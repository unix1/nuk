%%%-------------------------------------------------------------------
%% @doc nuk user
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user).

%% API
-export([new/2, check_password/2, get_username/1]).
-export_type([user/0]).

-opaque user() :: #{username => string(), password => string()}.

%%====================================================================
%% API
%%====================================================================

-spec new(Username :: string(), Password :: string()) -> user().
new(Username, Password) ->
    #{username => Username, password => Password}.

-spec check_password(user(), EnteredPassword :: string()) -> boolean().
check_password(#{password := StoredPassword}, EnteredPassword) ->
    StoredPassword =:= EnteredPassword.

-spec get_username(user()) -> string().
get_username(#{username := Username}) ->
    Username.
