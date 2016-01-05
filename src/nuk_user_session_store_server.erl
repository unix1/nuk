%%%-------------------------------------------------------------------
%% @doc nuk user session storage server
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user_session_store_server).

-behaviour(nuk_user_session_storage).

%% API
-export([get/1, put/1, delete/1, list/0]).

%%====================================================================
%% API
%%====================================================================

-spec get(SessionId :: string()) ->
    {ok, nuk_user_session:session()} |
    {error, user_session_not_found, Extra :: string()}.
get(SessionId) ->
    try list_to_pid(SessionId) of
        Pid -> {ok, nuk_user_server:get_session(Pid)}
    catch
        error:badarg -> {error, user_session_not_found, SessionId}
    end.

-spec put(Pid :: pid()) -> SessionId :: string().
put(Pid) when is_pid(Pid) ->
    pid_to_list(Pid).

-spec delete(SessionId :: string()) -> ok.
delete(SessionId) ->
    Pid = list_to_pid(SessionId),
    %% TODO move this out to nuk_user_sessions
    nuk_user_server:logout(Pid).

-spec list() -> [nuk_user_session:session()].
list() ->
    UserProcesses = supervisor:which_children(nuk_user_sup),
    lists:map(fun({_, Pid, worker, [nuk_user_server]}) ->
                  nuk_user_server:get_session(Pid) end,
              UserProcesses).
