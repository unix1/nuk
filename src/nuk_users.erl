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
    {ok, ServerPid} = supervisor:start_child(nuk_user_sup, []),
    nuk_user_server:login(ServerPid, Username, Password).
