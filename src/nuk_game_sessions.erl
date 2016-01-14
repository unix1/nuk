%%%-------------------------------------------------------------------
%% @doc `nuk_game_sessions' module
%%
%% This module should be used as an API for mapping game session identifiers
%% to process IDs:
%% - given a game process `pid()' create a new unique session identifier
%% - translate a given unique session identifier to the game session `pid()'
%%
%% The backend implementation of this is swappable. See
%% {@link nuk_game_session_storage} behavior and
%% {@link nuk_game_session_store_server} default implementation.
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_sessions).

%% API
-export([get_pid/1, put/1, delete/1]).

%% @doc Get a process ID
%%
%% Given a previously created session identifier, retrieve a process ID.
%% @end
-spec get_pid(SessionId :: string()) ->
    {ok, Pid :: pid()} |
    {error, game_session_not_found, Extra :: string()}.
get_pid(SessionId) ->
    SessionStorageModule = nuk_app:get_storage_module(game_sessions),
    SessionStorageModule:get_pid(SessionId).

%% @doc Create a new session
%%
%% Given a process ID, create a new unique session identifier.
%% @end
-spec put(Pid :: pid()) -> SessionId :: string().
put(Pid) when is_pid(Pid) ->
    SessionStorageModule = nuk_app:get_storage_module(game_sessions),
    SessionStorageModule:put(Pid).

%% @doc Delete a session
%%
%% Delete the session associated with the given session identifier.
%% @end
-spec delete(SessionId :: string()) -> ok.
delete(SessionId) ->
    SessionStorageModule = nuk_app:get_storage_module(game_sessions),
    SessionStorageModule:delete(SessionId).
