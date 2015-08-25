%%%-------------------------------------------------------------------
%% @doc nuk user storage server
%% @end
%%%-------------------------------------------------------------------

-module(nuk_user_store_server).

-behaviour(gen_server).

%% Supervision
-export([start_link/0, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([delete/1, get/1, put/2, validate/2]).

%%====================================================================
%% Supervision
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{data => #{}}}.

%%====================================================================
%% API
%%====================================================================

delete(Username) ->
    ok = gen_server:call(?MODULE, {delete, Username}).

get(Username) ->
    gen_server:call(?MODULE, {get, Username}).

put(Username, Password) ->
    ok = gen_server:call(?MODULE, {put, Username, Password}).

validate(Username, Password) ->
    gen_server:call(?MODULE, {validate, Username, Password}).

%%====================================================================
%% Behavior callbacks
%%====================================================================

handle_call({delete, Username}, _From, #{data := Data} = State) ->
    NewData = delete_user(Username, Data),
    NewState = State#{data := NewData},
    {reply, ok, NewState};

handle_call({get, Username}, _From, #{data := Data} = State) ->
    {reply, lookup_user(Username, Data), State};

handle_call({put, Username, Password}, _From, #{data := Data} = State) ->
    NewState = State#{data := Data#{Username => nuk_user:new(Username, Password)}},
    {reply, ok, NewState};

handle_call({validate, Username, Password}, _From, #{data := Data} = State) ->
    {reply, validate_user_with_password(Username, Password, Data), State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

delete_user(Username, Data) ->
    maps:remove(Username, Data).

lookup_user(Username, Data) ->
    try maps:get(Username, Data) of
        User -> {ok, User}
    catch
        error:{badkey, Username} -> {error, user_not_found, Username}
    end.

check_password(User, EnteredPassword) ->
    StoredPassword = nuk_user:get_password(User),
    case StoredPassword =:= EnteredPassword of
        true -> true;
        _ -> false
    end.

validate_user_with_password(Username, Password, Data) ->
    case lookup_user(Username, Data) of
        {ok, User} ->
            case check_password(User, Password) of
                true -> {ok, User};
                _ -> {error, wrong_password, <<"">>}
            end;
        {error, Reason, Extra} -> {error, Reason, Extra}
    end.
