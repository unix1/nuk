%%%-------------------------------------------------------------------
%% @doc nuk user session storage server
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user_session_store_server).

-behaviour(nuk_user_session_storage).

%% API
-export([get_pid/1, put/1, delete/1, list/0]).

%%====================================================================
%% API
%%====================================================================

-spec get_pid(SessionId :: string()) ->
    {ok, pid()} |
    {error, user_session_not_found, Extra :: string()}.
get_pid(SessionId) ->
    try list_to_pid(SessionId) of
        Pid -> {ok, Pid}
    catch
        error:badarg -> {error, user_session_not_found, SessionId}
    end.

-spec put(Pid :: pid()) -> SessionId :: string().
put(Pid) when is_pid(Pid) ->
    pid_to_list(Pid).

-spec delete(SessionId :: string()) -> ok.
delete(SessionId) ->
    ok.

-spec list() -> [nuk_user_session:session()].
list() ->
    UserProcesses = supervisor:which_children(nuk_user_sup),
    lists:map(fun({_, Pid, worker, [nuk_user_server]}) ->
                  nuk_user_server:get_session(Pid) end,
              UserProcesses).
