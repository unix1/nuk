Implementing a Game
===================

Before implementing a game please read the [guide](guide.md) and [developer overview](developer-overview.md) chapters. They set a useful context for this topic.

The aim of nuk is to provide a framework for turn based games, so that impelmentation of each game does not have to deal with general functionality. Each implementation only need to concern itself with the game specific details. To that end, nuk:
- tracks commonly used game session related data
- provides handling, authentication and validation for common player actions
- invokes game engine via behavior callbacks

Assuming you have decided on the rules of your game, then implementing a game engine in nuk is a matter of implementing the `nuk_game_engine` behavior callbacks. Generally speaking, these callbacks:
- represent actions or events occurring during the game
- take current state(s) of the game session and the action being performed
- are expected to return the new state(s) of the game session

States
------

There are 2 states to be aware of when implementing `nuk_game_engine` callbacks.

`nuk_game_state:state()` is a general game data that is tracked by nuk automatically. The game engines need not track this data on their own. The `nuk_game_state` module can be used to operate on this data type.

`nuk_game_engine_state` is a game specific state. This contains data that's relevant to a specific game engine and is not generally commonly shared across different games. This data type has 3 components:
- private: game session data that stays private to the game engine; it is never shared with players
- public: game session data that's shared with all players
- players: a map of player username => data that's only shared with a specific player

Since this data is specific to a game being implemented the game engines are entirely responsible for the contents of the `nuk_game_engine_state:state()` data type. `nuk_game_engine_state` module must be used to operate on this data type.

Common Checks
-------------

Prior to invoking callbacks, nuk automatically performs the following checks, as applicable:
- given user session is authenticated
- given game session is valid

In addition, nuk automatically performs few other checks specific to the action being performed. Below are details on `nuk_game_engine` callbacks.

initialize/2
------------

Invoked: when a user attempts to create a new game session via `nuk_games:create/2-3`.

Arguments:
- `nuk_user:user()` that's trying to create a game session and a list of options.

This callback is useful for game engines to parse initial options and set up initial game session states. Valid returns are:
- `{ok, nuk_game_engine_state:state()}` when successful
- `{error, invalid_options, string()}` when invalid option was specified

player_join/3
-------------

Invoked: when a user attempts to join an existing game session via `nuk_games:join/2`.

Arguments:
- `nuk_user:user()` that's trying to join the game session
- `nuk_game_engine_state:state()` current game engine state
- `nuk_game_state:state()` current nuk state for this game session

In addition to common checks, prior to invoking callback, nuk verifies:
- user is not currently part of this game session
- maximum number of players allowed by the game has not been reached

This callback is useful for game engines to set up initial player state. Valid return is:
- `{ok, nuk_game_engine_state:state()`

player_leave/3
--------------

Invoked: when a user attempts to leave an existing game session via `nuk_games:leave/2`.

Arguments:
- `nuk_user:user()` that's trying to leave the game session
- `nuk_game_engine_state:state()` current game engine state
- `nuk_game_state:state()` current nuk state for this game session

In addition to common checks, prior to invoking callback, nuk verifies:
- user is currently part of this game session

This callback is useful for game engines to process player's request to leave the game. Depending on the game's rules and the session state, it might result in: (1) game staying in `initialized` state if it hadn't yet started; (2) game ending because it is impossible to continue the game without this player; (3) game being allowed to continue in `await_turn` state; or in rare cases (4) returning an error to the user. Valid returns are:
- `{ok, initialized, nuk_game_engine_state:state()}` when the game has not yet been started and may remain in `initialized` state
- `{ok, await_turn, [nuk_user:user()], nuk_game_engine_state:state()}` when game has already started but may continue without the leaving player; in this case the return contains the list of users whose turn it is next, and the new engine state
- `{ok, complete, [nuk_user:user()], [nuk_user:user()], nuk_game_engine_state:state()}` when the game cannot continue, returning the list of winners and losers
- `{error, game_already_started, string()}` when the game engine needs to return an error to the player

start/2
-------

Invoked: when a user attempts to start an existing game session via `nuk_games:start/2`.

Arguments:
- `nuk_game_engine_state:state()` current game engine state
- `nuk_game_state:state()` current nuk state for this game session

In addition to common checks, prior to invoking callback, nuk verifies:
- user is currently part of this game session

This callback is useful for game engines to start the game play. After this callback the game session status changes from `initialized` to `await_turn` and game engine must return a list of players whose turn it is next. Valid return is:
- `{ok, await_turn, [nuk_user:user()], nuk_game_engine_state:state()}`

turn/3
------

Invoked: when a player attempts to make a turn via `nuk_games:turn/2`.

Arguments:
- `nuk_user:user()` who is making a turn
- `term()` the arbitrary game specific turn data
- `nuk_game_engine_state:state()` current game engine state
- `nuk_game_state:state()` current nuk state for this game session

In addition to common checks, prior to invoking callback, nuk verifies:
- user is currently part of this game session
- this user is in the list of players whose turn it is next

This callback allows game engines to process a turn. The turn data is completely arbitrary information that's dictated by the game engine. There are 3 possible outcomes as a result of this action: (1) the turn was successfully made which resulted in updated game session state and new turn order; (2) the turn resulted in game being completed and there are winners and/or losers; or (3) the turn data was invalid and the player should get an error. Valid returns are:
- `{ok, await_turn, [nuk_user:user()]}` upon successful turn when game is still in progress
- `{ok, complete, [nuk_user:user()], [nuk_user:user()]}` when the turn completes the game and winners and losers are returned
- `{error, invalid_turn, string()}` when turn contains invalid data

finish/2
--------

Invoked: when a game session is about to terminate.

Arguments:
- `nuk_game_engine_state:state()` current game engine state
- `nuk_game_state:state()` current nuk state for this game session

This is a callback invoked by the system prior to the termination of the `nuk_game_server` process. It is useful for game engines to perform any game session related cleanup (closing connections, removing temporary files, etc.). Valid return is
- `ok`
