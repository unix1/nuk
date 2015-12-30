%%%-------------------------------------------------------------------
%% @doc `nuk_game_store_server' module
%%
%% This is an implementation of {@link nuk_game_storage} behavior. It is meant
%% for testing and proof of concept purposes only.
%%
%% This is a `gen_server' that's started by the {@link nuk_game_store_sup}
%% supervisor. It provides storage interface to registered games. For public
%% API the {@link nuk_games} module should be used which, in turn, will use the
%% appropriate storage backend.
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

%% @doc Delete a game from registry
%%
%% Deletes a game by its name from the game registration database.
%% @end
-spec delete(GameName :: string()) -> ok.
delete(GameName) ->
    ok = gen_server:call(?MODULE, {delete, GameName}).

%% @doc Get a game
%%
%% Retrieves a game registration by its name from the database.
%% @end
-spec get(GameName :: string()) ->
    {ok, nuk_game:game()} |
    {error, game_not_found, string()}.
get(GameName) ->
    gen_server:call(?MODULE, {get, GameName}).

%% @doc List all games
%%
%% Lists all registered games from the registration database.
%% @end
-spec list() -> [nuk_game:game()].
list() ->
    gen_server:call(?MODULE, {list}).

%% @doc Create or replace a game
%%
%% If a game by the name is already registered, replaces that registration;
%% otherwise creates a new registration.
%% @end
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

%% @doc Delete a game from data storage
%% @private
%%
%% Deletes a game by its name from the internal map.
%% @end
delete_game(GameName, Data) ->
    maps:remove(GameName, Data).

%% @doc List all games
%% @private
%%
%% Returns all games stored inside the map.
%% @end
list_games(Data) ->
    maps:values(Data).

%% @doc Look up a game
%% @private
%%
%% Search for a game by its name in the map.
%% @end
lookup_game(GameName, Data) ->
    try maps:get(GameName, Data) of
        Game -> {ok, Game}
    catch
        error:{badkey, GameName} -> {error, game_not_found, GameName}
    end.
