%%%-------------------------------------------------------------------
%% @doc nuk user session
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user_session).

%% API
-export([new/0, get_user/1, set_user/2]).
-export_type([session/0]).

-opaque session() :: #{username => string(), user => nuk_user:user()}.

%%====================================================================
%% API
%%====================================================================

-spec new() -> session().
new() ->
    #{username => anonymous, user => #{}}.

-spec get_user(session()) -> nuk_user:user().
get_user(#{user := User}) ->
    User.

-spec set_user(Session :: session(), User :: nuk_user:user()) -> session().
set_user(Session, User) ->
    Username = nuk_user:get_username(User),
    Session#{username := Username, user := User}.
