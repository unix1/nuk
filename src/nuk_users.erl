%%%-------------------------------------------------------------------
%% @doc nuk users
%% @end
%%%-------------------------------------------------------------------

-module(nuk_users).

%% API
-export([delete/1, get/1, put/1, login/2]).

-spec delete(Username :: string()) -> ok.
delete(Username) ->
    ok = nuk_user_store_server:delete(Username).

-spec get(Username :: string()) ->
    {ok, nuk_user:user()} |
    {error, user_not_found, string()}.
get(Username) ->
    nuk_user_store_server:get(Username).

-spec put(User :: nuk_user:user()) -> ok.
put(User) ->
    ok = nuk_user_store_server:put(User).

-spec login(Username :: string(), Password :: string()) ->
    {ok, string()} |
    {error, atom(), string()}.
login(Username, Password) ->
    nuk_user_server:login(Username, Password).
