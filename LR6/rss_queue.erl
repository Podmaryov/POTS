-module(rss_queue).
-include("logging.hrl").
-include_lib("xmerl/include/xmerl.hrl").
-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3, subscribe/2]).
-export([start/1, start/2, add_feed/2, get_all/1]).

-record(rssQ,{queue,subscribers}).
% ������� �� ����� �������� ������ ��������� = 10 �.
-define(TIMEOUT,10000).

% @type rssDoc() = #xmlElement{name=rss,
% attributes=[xmlAttr()],
% content=[xmlAny()]}.
% �������� ������� xmerl ����� �������� RSS 2.0.
% @type rssItem() = #xmlElement{name=item}.
% ������� ����� �������� RSS 2.0.
% @type filename() = string().
% �������� �����.
% @type url() = string() | binary() | term().
% URL ������.
% @type name() = atom() | term().
% ��� �������.
% @type msg() = {atom() | term()}.
% ��������� ����: {'DOWN', Ref, process, Pid2, Reason} ��� {'EXIT', Pid, Reason}.
% ��������� ��������.
% @type serverRef() = atom() | {atom(), atom()} | {atom(), term()} | pid().
% ������ �� ������.

% @doc ������ gen_server �������� � ������ Name (���������� �����).
% @spec start(Name::name()) -> term()
%

start(Name) -> 
	gen_server:start({local, Name}, ?MODULE, [], []).

% @doc ������ gen_server �������� � ������ Name c ������� URL.
% @spec start(Name::name(), Url::url()) -> term()
%

start(Name,Url) -> 
	gen_server:start({local, Name}, ?MODULE, [Url], []).

% @doc ������� ������������� ������� � ���������� ���������� ���������,
%      � ������� �������� ������ (�����������).
% @spec init([term()]) -> {ok, record()}
%

init([]) ->
	process_flag(trap_exit,true),
	{ok, #rssQ{queue = [], subscribers = sets:new()}};


init([Url]) -> 
	State = #rssQ{queue = [], subscribers = sets:new()},
	process_flag(trap_exit,true),
	rss_reader:start(Url, self()),
	{ok, State}.

% @doc ���������� �������� �� ������� gen_server:call
%      ��� gen_server:multi_call.
% @spec handle_call(_Request::{atom(),term()}, _From::term(), State::record()) -> {reply, term(), record()}
%

handle_call(_Request = {subscribe,QPid}, _From, State = #rssQ{queue = Q,subscribers = Subscribers}) ->
	{Reply,NewState} = case sets:is_element(QPid,Subscribers) of
		true -> 
			{{error,already_subscribed},State};
    	false ->  
    		erlang:monitor(process,QPid),
    		?INFO("New subscriber ~p to ~p~n",[QPid,self()]),
    		[add_item(QPid,Item) || Item <- Q],
    		{ok, State#rssQ{subscribers = sets:add_element(QPid,Subscribers)}}
  	end,
  	{reply, Reply, NewState};

handle_call(_Request = {get_all}, _From, State = #rssQ{queue = Q}) -> 
	{reply,Q,State};
handle_call(_Request, _From, State) -> 
	{reply,{error,{unknown_request,_Request}}, State}.

% @doc ���������� �������� �� ������� gen_server:cast � gen_server:abcast.
% @spec handle_cast(_Msg::{atom(), term()}, State::record()) -> {noreply, record()}
%

handle_cast(_Msg = {add_item, RSSItem = #xmlElement{name = item}}, State = #rssQ{queue = Q,subscribers = Subscribers}) -> 
 	NewQ = add_item_to_q(RSSItem,Q,Subscribers),
 	{noreply,State#rssQ{queue = sort(NewQ)}};

handle_cast(_Msg = {unsubscribe,QPid}, State = #rssQ{subscribers = Subscribers}) -> 
 	{noreply,State#rssQ{subscribers = sets:del_element(QPid,Subscribers)}};

handle_cast(_Msg, State) -> 
	?WARN("Unknown msg {~p} to Q{~p}",[_Msg,State]),
	{noreply, State}.

% @doc ���������� ���������� �������� �������� timeout ��������� gen_server.
% @spec handle_info(_Info::msg(), State::record()) -> {noreply, record()}
%

handle_info(_Info = {'DOWN',_,_,QPid,_Reason}, State = #rssQ{subscribers = Subscribers}) ->
	{noreply, State#rssQ{subscribers=sets:del_element(QPid,Subscribers)}};

handle_info(_Info = {'EXIT',FromPid,_Reason}, State) ->
	?ERROR("RSS Reader ~p died for ~p with reason ~n",[FromPid,self(),_Reason]),
	{noreply, State};

handle_info(_Info, State) -> {noreply, State}.

% @doc ��� ������� ���������� ����� ����������� gen_server ��������
%      � �������� ������������������ ������� Module:init/1, ����������� ����������� �������.
% @spec terminate(_Reason::term(), _State::record()) -> ok
%

terminate(_Reason, _State) -> ok.

% @doc ��� ������� ���������� ����� ���������� 
% ����������/����� ������ gen_server �������� 
% � ��� ���������� �������� ���� ���������� ���������, 
% ��� ����������� ������� ����������� ����.
% @spec code_change(_OldVsn::term(), State::record(), _Extra::term()) -> {ok, record()}

code_change(_OldVsn, State, _Extra) -> {ok, State}.

% @doc ��� ������� ����������� ������� Q1 �� ��������� ������� Q2.
% @spec subscribe(Q1::serverRef(), Q2::serverRef()) -> term()
%

subscribe(Q1, Q2) -> gen_server:call(Q2,{subscribe, Q1}).

% @doc ��������������� �������, ���������� ��������� �������� �������� � �������;
%      QPid - ��� PID �������� �������, � Item - ��� ������� ����������� � �������.
% @spec add_item(QPid::pid(), Item::rssItem()) -> ok
%

add_item(QPid, Item) -> 
	ok = gen_server:cast(QPid ,{add_item,Item}), ok.

% @doc ��� ������� ������ ��������� ��� �������� �� ��������� �����, 
% � ���������� ��� �������� �� ������� � �������.
% @spec add_feed(QPid::pid(), RSS2Feed::rssDoc()) -> ok | error
%

add_feed(QPid,RSS2Feed) when is_pid(QPid) ->
	case rss_parse:is_rss2_feed(RSS2Feed) of
		true ->
			Items = rss_parse:get_feed_items(RSS2Feed),
			[add_item(QPid,Item) || Item <- Items],
			io:format("Size of RSS2Feed: ~p ~n", [length(Items)]),
			ok;
		false ->
			?ERROR("Version not 2.0! PID: ~p~n", [QPid]),
			error
	end.

% @doc ��� ��������������� ������� �������� ��������� ��������� ������ ��������� 
%      ����� �� ��������.
% @spec get_all(QPid::pid()) -> term()
%

get_all(QPid) when is_pid(QPid) -> 
	gen_server:call(QPid, {get_all}).

% @private
% @doc ��������������� ������� ������������ ������� ���������� 
%      ������ �������� ����� � �������.
% @spec add_item_to_q(NewItem::rssItem(), Q::[rssItem()], Subscribers::set()) -> [rssItem()]
%

add_item_to_q(NewItem, Q, Subscribers) ->
	add_item_to_q(NewItem, [], Q, Subscribers).

% @private
% @doc ��������������� �������, �������������� ���������� 
%      ������ �������� � ������ �������.
% @spec add_item_to_q(NewItem::rssItem(), L1::[rssItem()], [], Subscribers::set()) -> [rssItem()]
%

add_item_to_q(NewItem, L1, [], Subscribers) ->
	?INFO("New item added to Queue. PID: ~p~n", [self()]), 
	[add_item(Pid, NewItem) || Pid <- sets:to_list(Subscribers)],
	L1++[NewItem];

% @private
% @doc ��������������� �������, �������������� ���������� 
%      ������ �������� � �������� ������� �� �������:
%      ���� ��� ��������� ������� ���� same - ����� ������� �������������,
%      ���� ������� ���� updated - ������ ������ ���������, � ����� ������������ �������������� ����,
%      ���� ������� ���� different - ����� ������� ��������� � ������� ����������� ����.
% @spec add_item_to_q(NewItem::rssItem(), L1::[rssItem()], L::[rssItem()], Subscribers::set()) -> [rssItem()]
%

add_item_to_q(NewItem, L1, L = [OldItem | Rest], Subscribers) ->
	case rss_parse:compare_feed_items(OldItem, NewItem) of
		same -> 
			?INFO("Same items. Item ignored. PID: ~p~n",[self()]),
			L1++L;
		updated -> 
			?INFO("Updated item. PID: ~p~n",[self()]),
			[add_item(Pid, NewItem) || Pid <- sets:to_list(Subscribers)],
			L1++Rest++[NewItem];
		different -> 
			add_item_to_q(NewItem, L1++[OldItem], Rest, Subscribers)
	end.

% @private
% @doc ������� ���������� ������ ��������� �������
%      �� ����������� ���� ����������.
% @spec sort([rssItem()]) -> [rssItem()]
%

sort([]) -> [];
sort([H|T]) -> sort([X || X <- T, rss_parse:get_item_time(X) < rss_parse:get_item_time(H)]) 
					++ [H] 
					++ sort([X || X <-T , rss_parse:get_item_time(X) >= rss_parse:get_item_time(H)]).