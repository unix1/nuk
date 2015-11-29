%%%-------------------------------------------------------------------
%% @doc nuk game coin flip
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_coinflip).

-behaviour(nuk_game_engine).

-export([initialize/2, player_join/2, player_leave/2, start/1, turn/3, finish/1]).

%% TODO errors should probably also return state

initialize(User, []) ->
    {ok, #{turn_number => 0, wins => 0, max_turns => 3, user => User}}.

player_join(_User, State) -> {ok, State}.

player_leave(User, #{user := User} = State) ->
    % player leaves before the game is over
    {ok, complete, [], [], State}.

start(#{user := User} = State) ->
    {ok, await_turn, [User], State}.

turn(User, Turn, #{turn_number := TurnNumber,
                   wins := Wins,
                   losses := Losses,
                   max_turns := MaxTurns,
                   user := User} = State) when Turn =:= heads; Turn =:= tails ->
    NewTurnNumber = TurnNumber + 1,
    [Win, Loss] = process_turn(Turn),
    NewWins = Wins + Win,
    NewLosses = Losses + Loss,
    NewState = State#{turn_number := NewTurnNumber,
                      wins := NewWins,
                      losses := NewLosses},
    case NewTurnNumber of
        MaxTurns ->
            if
                NewWins > MaxTurns / 2 ->
                    {ok, complete, [User], []};
                true ->
                    {ok, complete, [], [User]}
            end;
        _ ->
            {ok, await_turn, [User], NewState}
    end;
turn(_User, _Turn, _State) ->
    {error, invalid_turn, "Invalid turn"}.

finish(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

process_turn(Turn) when Turn =:= heads; Turn =:= tails ->
    Toss = rand:uniform(2),
    case turn_to_number(Turn) of
        Toss ->
            [1, 0];
        _ ->
            [0, 1]
    end.

turn_to_number(Turn) when Turn =:= heads -> 1;
turn_to_number(Turn) when Turn =:= tails -> 2.
