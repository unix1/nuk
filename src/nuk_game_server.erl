%%%-------------------------------------------------------------------
%% @doc nuk game server
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_server).

-behaviour(gen_server).

-export([start_link/0, init/1]).
-export([code_change/3, handle_call/3, handle_cast/2, handle_info/2, terminate/2]).
-export([game_start/3, game_end/1]).

%%%%% Supervision functions %%%%%

start_link() ->
    gen_server:start_link(?MODULE, [], []).

init([]) ->
    {ok, 0}.

%%%%% User functions %%%%%

%% start a game.
game_start(Pid, Players, Game) ->
    ok = gen_server:call(Pid, {game_start, Players, Game}).

%% end a game
game_end(Pid) ->
    ok = gen_server:call(Pid, {game_end}).

%%%%% Server callbacks %%%%%
handle_call({game_start, _Players, _Game}, _From, State) ->
    {reply, ok, State};
handle_call({game_end}, _From, State) ->
    {reply, ok, State}.

handle_cast(_Msg, State) -> {noreply, State}.

handle_info(_Msg, State) -> {noreply, State}.

terminate(_Reason, _State) -> ok.

code_change(_OldVersion, State, _Extra) -> {ok, State}.
