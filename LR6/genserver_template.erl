% Шаблон для реализации модулей обратного вызова gen_server.

-module(genserver_template).

% Сервер реализует поведение gen_server.

-behaviour(gen_server).
-export([init/1, handle_call/3, handle_cast/2, handle_info/2, terminate/2,
         code_change/3]).

% Дополнительные вспомогательные функции, экспортируемые модулем % обратного вызова.

-export([start/1]).

start(Name) -> gen_server:start({local, Name}, ?MODULE, [], []).

init([]) -> {ok, todo_state}.
handle_call(_Request, _From, State) -> {reply, todo_reply, State}.
handle_cast(_Msg, State) -> {noreply, State}.
handle_info(_Info, State) -> {noreply, State}.
terminate(_Reason, _State) -> ok.
code_change(_OldVsn, State, _Extra) -> {ok, State}.