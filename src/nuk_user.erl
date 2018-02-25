%%%-------------------------------------------------------------------
%% @doc `nuk_user' module
%%
%% This module is used to operate on {@link nuk_user:user()} data type. This
%% data type is used when dealing with users in nuk, for example, when calling
%% {@link nuk_users:put/1} and {@link nuk_users:get/1}.
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user).

%% API
-export([new/2, check_password/2, get_username/1]).
-export_type([user/0]).
-export_type([username/0]).

-opaque user() :: #{username => username(), password => string()}.
%% Data type used to operate on users in nuk. Use functions in this module to
%% operate on this data type.

-type username() :: string() | anonymous.
%% Data type for user name, can be any string or atom `anonymous'.

%%====================================================================
%% API
%%====================================================================

%% @doc Create a new {@link nuk_user:user()} data type
%%
%% `Username' is a string that uniquely identifies a user in the user storage.
%% `Password' is a string used to validate user for login. Note that the
%% password is here for proof of concept and testing purposes only. It does not
%% provide hashing or secure password storage. When using
%% {@link nuk_user_storage} implementations for the hashed password is likely
%% stored there, so this password shouldn't be used in that case.
%% @end
-spec new(Username :: string(), Password :: string()) -> user().
new(Username, Password) ->
    #{username => Username, password => Password}.

%% @doc Verify user's password
%%
%% This function is only here for testing and proof of concept purposes. In
%% production scenarios where users are stored in external
%% {@link nuk_user_storage} implementations the authentication should be
%% performed by that system, and this function should not be used.
%% @end
-spec check_password(user(), EnteredPassword :: string()) -> boolean().
check_password(#{password := StoredPassword}, EnteredPassword) ->
    StoredPassword =:= EnteredPassword.

%% @doc Get username
%%
%% Extract username from {@link nuk_user:user()} data type.
%% @end
-spec get_username(user()) -> username().
get_username(#{username := Username}) ->
    Username.
