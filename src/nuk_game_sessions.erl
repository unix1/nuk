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
    SessionStorageModule = get_storage_module(),
    SessionStorageModule:get_pid(SessionId).

%% @doc Create a new session
%%
%% Given a process ID, create a new unique session identifier.
%% @end
-spec put(Pid :: pid()) -> SessionId :: string().
put(Pid) when is_pid(Pid) ->
    SessionStorageModule = get_storage_module(),
    SessionStorageModule:put(Pid).

%% @doc Delete a session
%%
%% Delete the session associated with the given session identifier.
%% @end
-spec delete(SessionId :: string()) -> ok.
delete(SessionId) ->
    SessionStorageModule = get_storage_module(),
    SessionStorageModule:delete(SessionId).

%%====================================================================
%% Internal functions
%%====================================================================

%% @doc get the storage module
%% @private
%%
%% Retrieves the storage module for game sessions from application settings.
%% If not defined the default {@link nuk_game_session_store_server} will be
%% returned.
-spec get_storage_module() -> atom().
get_storage_module() ->
    case application:get_env(game_session_storage) of
        undefined ->
            nuk_game_session_store_server;
        Module ->
            Module
    end.
