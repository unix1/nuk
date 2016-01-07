%%%-------------------------------------------------------------------
%% @doc `nuk_user_storage' module
%%
%% This behavior allows to extend the storage service for registered users.
%% When a new user is created engine with nuk via
%% {@link nuk_users:put/1} it is stored internally by the system.
%% Implementing this behavior allows a custom storage backend to be defined.
%% The default simple implementation is provided with
%% {@link nuk_user_store_server}.
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user_storage).

-callback delete(Username :: string()) ->
    'ok'.

-callback get(Username :: string()) ->
    {ok, nuk_user:user()} |
    {error, ErrorCode :: user_not_found, ErrorText :: string()}.

-callback put(User :: nuk_user:user()) ->
    'ok'.

-callback validate(Username :: string(), Password :: string()) ->
    {ok, User :: nuk_user:user()} |
    {error, ErrorCode :: wrong_password | user_not_found, ErrorText :: string()}.

-callback list() ->
    [nuk_user:user()].
