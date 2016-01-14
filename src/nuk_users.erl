%%%-------------------------------------------------------------------
%% @doc `nuk_users' module
%%
%% This module should be used as an API to interacting with all user related
%% actions.
%% @end
%%%-------------------------------------------------------------------

-module(nuk_users).

%% API
-export([delete/1, get/1, put/1, login/2, logout/1, list/0]).

%% @doc Delete an existing user
%%
%% This deletes an existing user from user storage.
%% @end
-spec delete(Username :: string()) -> ok.
delete(Username) ->
    ok = nuk_user_store_server:delete(Username).

%% @doc Get a user
%%
%% Retrieves a user from user data storage given a username. The return is a
%% {@link nuk_user:user()} data type. Use {@link nuk_user} module to extract
%% needed information.
%% @end
-spec get(Username :: string()) ->
    {ok, nuk_user:user()} |
    {error, user_not_found, string()}.
get(Username) ->
    nuk_user_store_server:get(Username).

%% @doc Create new or update an existing user
%%
%% Performs a put operation. If a user with the given username exists it will
%% overwrite with the new data. If a new username is given, a new user will be
%% created. Create a {@link nuk_user:user()} data type and use it here.
%% @end
-spec put(User :: nuk_user:user()) -> ok.
put(User) ->
    ok = nuk_user_store_server:put(User).

%% @doc Login a user
%%
%% Given a username and a password attempt to login a user. If login is
%% successful, a new user session identifier is returned; otherwise, an error
%% is returned.
%% @end
-spec login(Username :: string(), Password :: string()) ->
    {ok, SessionId :: string()} |
    {error, ErrorCode :: atom(), Extra :: string()}.
login(Username, Password) ->
    nuk_user_server:login(Username, Password).

%% @doc Log out a user session
%% @equiv nuk_user_sessions:logout(SessionId)
%% @end
-spec logout(SessionId :: string()) -> ok.
logout(SessionId) ->
    nuk_user_sessions:logout(SessionId).

%% @doc List all users
%%
%% Get all users from user storage. The return is a list of
%% {@link nuk_user:user()} data types. Use {@link nuk_user} modue to extract
%% needed information.
%% @end
-spec list() -> [nuk_user:user()].
list() ->
    nuk_user_store_server:list().
