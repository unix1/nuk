%%%-------------------------------------------------------------------
%% @doc `nuk_user_session_store_server' module
%%
%% This is an implementation of {@link nuk_user_session_storage} behavior. It
%% is meant for testing and proof of concept purposes only.
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user_session_store_server).

-behaviour(nuk_user_session_storage).

%% API
-export([get_pid/1, put/1, delete/1, list/0]).

%%====================================================================
%% API
%%====================================================================

%% @doc Get game session process ID
%%
%% Given the session identifier string, returns the game session `pid()' which
%% then can be used to interface with {@link nuk_user_server} functions.
%% @end
-spec get_pid(SessionId :: string()) ->
    {ok, pid()} |
    {error, user_session_not_found, Extra :: string()}.
get_pid(SessionId) ->
    try list_to_pid(SessionId) of
        Pid -> {ok, Pid}
    catch
        error:badarg -> {error, user_session_not_found, SessionId}
    end.

%% @doc Create a new user session identifier
%%
%% Given a user session process ID, creates a new session identifier string.
%% @end
-spec put(Pid :: pid()) -> SessionId :: string().
put(Pid) when is_pid(Pid) ->
    pid_to_list(Pid).

%% @doc Delete a session ID mapping
%%
%% Given a session identifier string, deletes its mapping to the user session
%% process ID, so that next call to {@link nuk_user_session_store_server:get/1}
%% results in `game_session_not_found' response.
%% @end
-spec delete(SessionId :: string()) -> ok.
delete(SessionId) ->
    ok.

%% @doc List all sessions
%%
%% Returns a list of all user sessions. Used in tests.
%% @end
-spec list() -> [nuk_user_session:session()].
list() ->
    UserProcesses = supervisor:which_children(nuk_user_sup),
    lists:map(fun({_, Pid, worker, [nuk_user_server]}) ->
                  nuk_user_server:get_session(Pid) end,
              UserProcesses).
