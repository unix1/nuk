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
    nuk_user_store_server_get/1,
    nuk_user_store_server_delete/1,
    nuk_user_store_server_validate_false/1,
    nuk_user_store_server_store_validate/1
]).

%% ============================================================================
%% ct functions
%% ============================================================================

all() ->
    [
        nuk_user_store_server_get,
        nuk_user_store_server_delete,
        nuk_user_store_server_validate_false,
        nuk_user_store_server_store_validate
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

%% ============================================================================
%% Tests
%% ============================================================================

nuk_user_store_server_get(_) ->
    ok = nuk_user_store_server:put(<<"GoodUser1">>, <<"GoodPass1">>),
    {error, user_not_found, _Extra} = nuk_user_store_server:get(<<"BadUser">>),
    {ok, User} = nuk_user_store_server:get(<<"GoodUser1">>).

nuk_user_store_server_delete(_) ->
    ok = nuk_user_store_server:put(<<"GoodUser1">>, <<"GoodPass1">>),
    ok = nuk_user_store_server:delete(<<"GoodUser1">>),
    {error, user_not_found, _} = nuk_user_store_server:get(<<"GoodUser1">>).

nuk_user_store_server_validate_false(_) ->
    {error, user_not_found, _Extra} = nuk_user_store_server:validate(<<"BadUser">>, <<"BadPass">>).

nuk_user_store_server_store_validate(_) ->
    ok = nuk_user_store_server:put(<<"GoodUser1">>, <<"GoodPass1">>),
    {ok, User} = nuk_user_store_server:validate(<<"GoodUser1">>, <<"GoodPass1">>),
    {error, wrong_password, _} = nuk_user_store_server:validate(<<"GoodUser1">>, <<"BadPass">>),
    {error, user_not_found, _} = nuk_user_store_server:validate(<<"BadUser1">>, <<"GoodPass1">>).
