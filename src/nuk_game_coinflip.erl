%%%-------------------------------------------------------------------
%% @doc nuk game coin flip
%% @end
%%%-------------------------------------------------------------------

-module(nuk_game_coinflip).

-behaviour(nuk_game_engine).

-export([initialize/2, player_join/2, player_leave/2, start/1, turn/3, finish/1]).

%% TODO errors should probably also return state

initialize(User, []) ->
    {ok, #{turn_number => 0, wins => 0, max_turns => 10, user => User}}.

player_join(User, State) ->
    % single player game, we dont' allow more players to join
    case get_user(State) of
        User ->
            {error, already_joined, "User already joined the game"};
        _ ->
            {error, max_users_reached, "This is a 1 player game"}
    end.

player_leave(User, State) ->
    case get_user(State) of
        User ->
            {ok, complete, [], [], State};
        _ ->
            {error, unknown_user, "Couldn't leave because user was not found"}
    end.

start(State) ->
    {ok, await_turn, [get_user(State)], State}.

turn(_User, Turn, State) when Turn =:= heads; Turn =:= tails ->
    NewTurnNumber = get_turn_number(State) + 1,
    MaxTurns = get_max_turns(State),
    case NewTurnNumber of
        MaxTurns ->
            WinsNumber = get_wins(State),
            if
                WinsNumber > MaxTurns / 2 ->
                    {ok, complete, [get_user(State)], [], "Game over. You win."};
                true ->
                    {ok, complete, [], [get_user(State)], "Game over. You lose."}
            end;
        _ ->
            NewState = State#{turn_number := NewTurnNumber},
            {ok, await_turn, [get_user(State)], NewState}
    end;
turn(_User, _Turn, _State) ->
    {error, invalid_turn, "Invalid turn"}.

finish(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

get_user(#{user := User}) ->
    User.

get_turn_number(#{turn_number := TurnNumber}) ->
    TurnNumber.

get_max_turns(#{max_turns := MaxTurns}) ->
    MaxTurns.

get_wins(#{wins := Wins}) ->
    Wins.
