%%%-------------------------------------------------------------------
%% @doc `nuk_user_session' module
%%
%% This module is used to operate on {@link nuk_user_session:session()} data
%% type. This data type is used when dealing with user sessions in nuk, for
%% example, when calling {@link nuk_user_sessions:get/1}.
%%
%% For public API the {@link nuk_user_sessions} module should be used which
%% already provides convenience functions for extracting information from
%% user sessions.
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

%% @doc Create a new {@link nuk_user_session:session()} data type
%%
%% Creates a new data type with default values.
%% @end
-spec new() -> session().
new() ->
    #{username => anonymous, user => #{}}.

%% @doc Get user
%%
%% Extracts a {@link nuk_user:user()} stored in the session.
%% @end
-spec get_user(session()) -> nuk_user:user().
get_user(#{user := User}) ->
    User.

%% @doc Set user
%%
%% Set a specified {@link nuk_user:user()} in the session.
%% @end
-spec set_user(Session :: session(), User :: nuk_user:user()) -> session().
set_user(Session, User) ->
    Username = nuk_user:get_username(User),
    Session#{username := Username, user := User}.
