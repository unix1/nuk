%%%-------------------------------------------------------------------
%% @doc `nuk_user_sessions' module
%%
%% This module should be used as an API for mapping user session identifiers
%% to process IDs:
%% - given a user process `pid()' create a new unique session identifier
%% - translate a given unique session identifier to the user session `pid()'
%%
%% It also provides several convenience functions for getting and extracting
%% data from user sessions.
%%
%% The backend implementation of this is swappable. See
%% {@link nuk_user_session_storage} behavior and
%% {@link nuk_user_session_store_server} default implementation.
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user_sessions).

%% API
-export([get/1, get_pid/1, get_user/1, put/1, logout/1, delete/1, list/0]).

%% @doc Get session
%%
%% Given a previously created session identifier, retrieve a process ID. Then
%% call the process to retrive its session {@link nuk_user_session:session()}
%% data type.
%% @end
-spec get(SessionId :: string()) ->
    {ok, nuk_user_session:session()} |
    {error, user_session_not_found, Extra :: string()}.
get(SessionId) ->
    case get_pid(SessionId) of
        {ok, Pid} ->
            {ok, nuk_user_server:get_session(Pid)};
        {error, user_session_not_found, Reason} ->
            {error, user_session_not_found, Reason}
    end.

%% @doc Get a process ID
%%
%% Given a previously created session identifier, retrieve a process ID.
%% @end
-spec get_pid(SessionId :: string()) ->
    {ok, pid()} |
    {error, user_session_not_found, Extra :: string()}.
get_pid(SessionId) ->
    nuk_user_session_store_server:get_pid(SessionId).

%% @doc Get user
%%
%% Gets user that is associated with this session identifier.
%% @end
-spec get_user(SessionId :: string()) ->
    {ok, nuk_user:user()} |
    {error, user_session_not_found, Extra :: string()}.
get_user(SessionId) ->
    case nuk_user_sessions:get(SessionId) of
        {ok, Session} ->
            {ok, nuk_user_session:get_user(Session)};
        {error, user_session_not_found, Reason} ->
            {error, user_session_not_found, Reason}
    end.

%% @doc Create a new session
%%
%% Given a process ID, create a new unique session identifier.
%% @end
-spec put(Pid :: pid()) -> SessionId :: string().
put(Pid) when is_pid(Pid) ->
    nuk_user_session_store_server:put(Pid).

%% @doc Log out a user session
%%
%% Logs out the given user session. Note that the {@link delete/1} happens
%% after the {@link nuk_user_server} terminates successfully.
%% @end
-spec logout(SessionId :: string()) -> ok.
logout(SessionId) ->
    {ok, Pid} = get_pid(SessionId),
    nuk_user_server:logout(Pid).

%% @doc Delete a session
%%
%% Delete the session associated with the given session identifier.
%% @end
-spec delete(SessionId :: string()) -> ok.
delete(SessionId) ->
    nuk_user_session_store_server:delete(SessionId).

%% @doc List all sessions
%%
%% Returns a list of all user sessions. Used in tests.
%% @end
-spec list() -> [nuk_user_session:session()].
list() ->
    nuk_user_session_store_server:list().
