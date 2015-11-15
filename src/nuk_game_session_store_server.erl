%%%-------------------------------------------------------------------
%% @doc nuk game session storage server
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_session_store_server).

-behaviour(nuk_game_session_storage).

%% API
-export([get/1, delete/1, list/0]).

%%====================================================================
%% API
%%====================================================================

-spec get(SessionId :: string()) ->
    {ok, nuk_game_session:session()}.
get(SessionId) ->
    Pid = list_to_pid(SessionId),
    nuk_game_server:get_session(Pid).

-spec delete(SessionId :: string()) -> ok.
delete(SessionId) ->
    Pid = list_to_pid(SessionId),
    nuk_game_server:finish(Pid).

-spec list() -> [nuk_game_session:session()].
list() ->
    GameProcesses = supervisor:which_children(nuk_game_sup),
    lists:map(fun({_, Pid, worker, [nuk_game_server]}) ->
                  {ok, Session} = nuk_game_server:get_session(Pid),
                  Session end,
              GameProcesses).
