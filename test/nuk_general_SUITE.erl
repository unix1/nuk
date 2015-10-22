-module(nuk_general_SUITE).

-include_lib("common_test/include/ct.hrl").

%% ct functions
-export([all/0]).
-export([init_per_suite/1]).
-export([end_per_suite/1]).
-export([init_per_testcase/2]).
-export([end_per_testcase/2]).

%% Tests
-export([
    nuk_users_get/1,
    nuk_users_delete/1,
    nuk_user_store_server_store_validate/1,
    nuk_users_login_bad/1,
    nuk_users_login_good/1,
    nuk_users_list/1,
    nuk_user_sessions_list/1,
    nuk_user_session_set_user/1,
    nuk_user_sessions_get/1
]).

%%====================================================================
%% ct functions
%%====================================================================

all() ->
    [
        nuk_users_get,
        nuk_users_delete,
        nuk_user_store_server_store_validate,
        nuk_users_login_bad,
        nuk_users_login_good,
        nuk_users_list,
        nuk_user_sessions_list,
        nuk_user_session_set_user,
        nuk_user_sessions_get
    ].

init_per_suite(Config) ->
    ok = application:start(nuk),
    Config.

end_per_suite(_) ->
    application:stop(gen_node),
    ok.

init_per_testcase(_, Config) ->
    Config.

end_per_testcase(_, _Config) ->
    ok.

%%====================================================================
%% Tests
%%====================================================================

nuk_users_get(_) ->
    ok = nuk_users:put(nuk_user:new("GoodUser1", "GoodPass1")),
    {error, user_not_found, _Extra} = nuk_users:get("BadUser"),
    {ok, User} = nuk_users:get("GoodUser1").

nuk_users_delete(_) ->
    ok = nuk_users:put(nuk_user:new("GoodUser1", "GoodPass1")),
    ok = nuk_users:delete("GoodUser1"),
    {error, user_not_found, _} = nuk_users:get("GoodUser1").

nuk_user_store_server_store_validate(_) ->
    ok = nuk_users:put(nuk_user:new("GoodUser1", "GoodPass1")),
    ok = nuk_users:put(nuk_user:new("GoodUser2", "GoodPass2")),
    {ok, _User1} = nuk_user_store_server:validate("GoodUser1", "GoodPass1"),
    {ok, _User2} = nuk_user_store_server:validate("GoodUser2", "GoodPass2"),
    {error, wrong_password, _} = nuk_user_store_server:validate("GoodUser1", "BadPass"),
    {error, user_not_found, _} = nuk_user_store_server:validate("BadUser1", "GoodPass1").

nuk_users_login_bad(_) ->
    {error, user_not_found, "BadUser"} = nuk_users:login("BadUser", "BadPass").

nuk_users_login_good(_) ->
    ok = nuk_users:put(nuk_user:new("GoodUser1", "GoodPass1")),
    {ok, _SessionId} = nuk_users:login("GoodUser1", "GoodPass1").

nuk_users_list(_) ->
    User1 = nuk_user:new("GoodUser1", "GoodPass1"),
    User2 = nuk_user:new("GoodUser2", "GoodPass2"),
    ok = nuk_users:put(User1),
    ok = nuk_users:put(User2),
    %% TODO list doesn't have to be in same order
    [User1, User2] = nuk_users:list().

nuk_user_sessions_list(_) ->
    nuk_users:put(nuk_user:new("GoodUser1", "GoodPass1")),
    nuk_users:put(nuk_user:new("GoodUser2", "GoodPass2")),
    {ok, SessionId1} = nuk_users:login("GoodUser1", "GoodPass1"),
    {ok, SessionId2} = nuk_users:login("GoodUser2", "GoodPass2"),
    [SessionId1, SessionId2] = nuk_user_sessions:list().

nuk_user_session_set_user(_) ->
    User1 = nuk_user:new("GoodUser1", "GoodPass1"),
    Session1 = nuk_user_session:new(),
    Session1_New = nuk_user_session:set_user(Session1, User1),
    User1 = nuk_user_session:get_user(Session1_New).

nuk_user_sessions_get(_) ->
    User1 = nuk_user:new("GoodUser1", "GoodPass1"),
    ok = nuk_users:put(User1),
    {ok, SessionId1} = nuk_users:login("GoodUser1", "GoodPass1"),
    {ok, Session1} = nuk_user_sessions:get(SessionId1),
    User1 = nuk_user_session:get_user(Session1).
