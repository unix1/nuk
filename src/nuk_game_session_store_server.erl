%%%-------------------------------------------------------------------
%% @doc nuk_game_session_store_server module
%%
%% This is an implementation of {@link nuk_game_session_storage} behavior. It
%% is meant for testing and proof of concept purposes only.
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_session_store_server).

-behaviour(nuk_game_session_storage).

%% API
-export([get_pid/1, put/1, delete/1]).

%%====================================================================
%% API
%%====================================================================

%% @doc Get game session process ID
%%
%% Given the session identifier string, returns the game session `pid()' which
%% then can be used to interface with {@link nuk_game_server} functions.
%% @end
-spec get_pid(SessionId :: string()) ->
    {ok, pid()} |
    {error, game_session_not_found, Extra :: string()}.
get_pid(SessionId) ->
    try list_to_pid(SessionId) of
        Pid -> {ok, Pid}
    catch
        error:badarg -> {error, game_session_not_found, SessionId}
    end.

%% @doc Create a new game session identifier
%%
%% Given a game session process ID, creates a new session identifier string.
%% @end
-spec put(Pid :: pid()) -> SessionId :: string().
put(Pid) when is_pid(Pid) ->
    pid_to_list(Pid).

%% @doc Delete a session ID mapping
%%
%% Given a session identifier string, deletes its mapping to the game session
%% process ID, so that next call to {@link nuk_game_session_store_server:get/1}
%% results in `game_session_not_found' response.
%% @end
-spec delete(SessionId :: string()) -> ok.
delete(SessionId) ->
    Pid = list_to_pid(SessionId),
    nuk_game_server:finish(Pid).
