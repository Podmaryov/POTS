-module(rss_queue).
-export([start/0, server/1, add_item/2, add_feed/2, get_all/1]).

-define(INFO(Format, Data),
    error_logger:info_msg("~p ~p:  " ++ Format, [?MODULE, self()] ++ Data)).
-define(WARN(Format, Data),
    error_logger:warning_msg("~p ~p:  " ++ Format, [?MODULE, self()] ++ Data)).
-define(ERROR(Format, Data),
	error_logger:error_msg("~p ~p:  " ++ Format, [?MODULE, self()] ++ Data)).


-define(TIMEOUT,10000).



start() ->
	Q = [],
	spawn(?MODULE,server,[Q]).


server(Q) ->
	receive
		{add_item,RSSItem} ->
	    	NewQ = add_item_to_q(RSSItem,Q), 
	    	SortQ = sort(NewQ),
	    	server(SortQ);

	    {get_all,ReqPid} ->
	      	ReqPid ! {self(),Q},
	      	?INFO("Sent rss items to ~p~n",[ReqPid]),
	      	server(Q);

	    _Msg -> ?ERROR("Unknown message: ~p~n",[_Msg]), {unknown_msg,_Msg}
	end.



add_item(QPid, Item) when is_pid(QPid) ->
 	QPid ! {add_item, Item}, ok.


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


get_all(QPid) when is_pid(QPid) ->
 	QPid ! {get_all, self()},
 	receive
 		{QPid, Q} -> 
 			io:format("Queue size = ~p items from the feed to ~p ~n", [length(Q),QPid]), 
 			{ok, Q};
 		_Msg -> ?ERROR("Unknown message: ~p~n",[_Msg]), {unknown_msg,_Msg}
 	after 
 		?TIMEOUT -> 
 			?ERROR("Timeout exceeded: module ~p, ~p in line ~p.~n", 
 					[?MODULE_STRING, ?FUNCTION_NAME, ?LINE]),
 			{error,timeout}
 	end.


add_item_to_q(NewItem, Q) ->
	add_item_to_q(NewItem, [], Q).


add_item_to_q(NewItem, L1, []) ->
	?INFO("New item added to Queue. PID: ~p~n", [self()]), 
	L1++[NewItem];


add_item_to_q(NewItem, L1, L = [OldItem | Rest]) ->
	case rss_parse:compare_feed_items(OldItem, NewItem) of
		same -> 
			?INFO("Same items. Item ignored. PID: ~p~n",[self()]),
			L1++L;
		updated -> 
			?INFO("Updated item. PID: ~p~n",[self()]),
			L1++Rest++[NewItem];
		different -> 
			add_item_to_q(NewItem, L1++[OldItem], Rest)
	end.


sort([]) -> [];
sort([H|T]) -> sort([X || X <- T, rss_parse:get_item_time(X) < rss_parse:get_item_time(H)]) 
					++ [H] 
					++ sort([X || X <-T , rss_parse:get_item_time(X) >= rss_parse:get_item_time(H)]).