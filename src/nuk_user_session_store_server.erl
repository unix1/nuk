%%%-------------------------------------------------------------------
%% @doc nuk user session storage server
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user_session_store_server).

-behaviour(nuk_user_session_storage).

%% API
-export([get/1, delete/1, list/0]).

%%====================================================================
%% API
%%====================================================================

-spec get(SessionId :: string()) ->
    {ok, nuk_user_session:session()}.
get(SessionId) ->
    Pid = list_to_pid(SessionId),
    nuk_user_server:get_session(Pid).

-spec delete(SessionId :: string()) -> ok.
delete(SessionId) ->
    Pid = list_to_pid(SessionId),
    nuk_user_server:logout(Pid).

-spec list() -> [nuk_user_session:session()].
list() ->
    UserProcesses = supervisor:which_children(nuk_user_sup),
    lists:map(fun({_, Pid, worker, [nuk_user_server]}) ->
                  {ok, Session} = nuk_user_server:get_session(Pid),
                  Session end,
              UserProcesses).
