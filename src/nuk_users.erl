%%%-------------------------------------------------------------------
%% @doc nuk users
%% @end
%%%-------------------------------------------------------------------

-module(nuk_users).

%% API
-export([login/2]).

-spec login(Username :: string(), Password :: string()) ->
    {ok, string()} |
    {error, atom(), string()}.
login(Username, Password) ->
    nuk_user_server:login(Username, Password).
