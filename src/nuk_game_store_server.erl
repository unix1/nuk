%%%-------------------------------------------------------------------
%% @doc nuk game storage server
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_store_server).

-behaviour(gen_server).
-behaviour(nuk_game_storage).

%% Supervision
-export([start_link/0, init/1]).

%% Behavior callbacks
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).

%% API
-export([delete/1, get/1, list/0, put/1]).

%%===================================================================
%% Supervision
%%====================================================================

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

init([]) ->
    {ok, #{data => #{}}}.

%%====================================================================
%% API
%%====================================================================

-spec delete(GameName :: string()) -> ok.
delete(GameName) ->
    ok = gen_server:call(?MODULE, {delete, GameName}).

-spec get(GameName :: string()) ->
    {ok, nuk_game:game()} |
    {error, game_not_found, string()}.
get(GameName) ->
    gen_server:call(?MODULE, {get, GameName}).

-spec list() -> [nuk_game:game()].
list() ->
    gen_server:call(?MODULE, {list}).

-spec put(Game :: nuk_game:game()) -> ok.
put(Game) ->
    ok = gen_server:call(?MODULE, {put, Game}).

%%====================================================================
%% Behavior callbacks
%%====================================================================

handle_call({delete, GameName}, _From, #{data := Data} = State) ->
    NewData = delete_game(GameName, Data),
    NewState = State#{data := NewData},
    {reply, ok, NewState};

handle_call({get, GameName}, _From, #{data := Data} = State) ->
    {reply, lookup_game(GameName, Data), State};

handle_call({list}, _From, #{data := Data} = State) ->
    {reply, list_games(Data), State};

handle_call({put, Game}, _From, #{data := Data} = State) ->
    GameName = nuk_game:get_name(Game),
    NewState = State#{data := Data#{GameName => Game}},
    {reply, ok, NewState}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.

%%====================================================================
%% Internal functions
%%====================================================================

delete_game(GameName, Data) ->
    maps:remove(GameName, Data).

list_games(Data) ->
    maps:values(Data).

lookup_game(GameName, Data) ->
    try maps:get(GameName, Data) of
        Game -> {ok, Game}
    catch
        error:{badkey, GameName} -> {error, game_not_found, GameName}
    end.
