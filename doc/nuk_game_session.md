

# Module nuk_game_session #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

`nuk_game_session` module.

<a name="description"></a>

## Description ##

This module is used to operate on [`nuk_game_session:session()`](nuk_game_session.md#type-session) data
type. This data type is used when retrieving the game session state from
[`nuk_games:get_game_session/1`](nuk_games.md#get_game_session-1). It tracks the following data:

*Game* [`nuk_game:game()`](nuk_game.md#type-game) which this session is for

*nuk's general state* of the game session containing following
- `status`: game session status, default `nil`
- `turn_number`: current turn number, default `0`
- `players`: list of players currently in the game session, default `[]`
- `players_turn`: list of players who should make turn(s) next,
default `[]`
- `players_winners`: list of players who won the game, only populated
after the game completes, default `[]`
- `players_losers`: list of players who lost the game, only populated
after the game completes, default `[]`

*game engine's arbitrary state* set by the game engine being played. Since
this state is specific to the game engine, this module can only help get it
as a whole value. Extracting any information out of that state is the
responsibility of the game engine itself.
<a name="types"></a>

## Data Types ##




### <a name="type-nuk_game_session_status">nuk_game_session_status()</a> ###


<pre><code>
nuk_game_session_status() = nil | initialized | await_turn | complete
</code></pre>

 General game session status tracked by nuk.



### <a name="type-nuk_state">nuk_state()</a> ###


__abstract datatype__: `nuk_state()`

 Data type containing nuk's general game state. This is part of
[`nuk_game_session:get_game_session()`](nuk_game_session.md#type-get_game_session) data type.



### <a name="type-session">session()</a> ###


__abstract datatype__: `session()`

 Data type used to represent a game session state. Use functions in this
module to operate on this data type.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#add_player-2">add_player/2</a></td><td>Add a player to the game session.</td></tr><tr><td valign="top"><a href="#get_game-1">get_game/1</a></td><td>Get game.</td></tr><tr><td valign="top"><a href="#get_game_state-1">get_game_state/1</a></td><td>Get game engine arbitrary state.</td></tr><tr><td valign="top"><a href="#get_players-1">get_players/1</a></td><td>Get players currently in the game session.</td></tr><tr><td valign="top"><a href="#get_players_count-1">get_players_count/1</a></td><td>Get number of players currently in the game session.</td></tr><tr><td valign="top"><a href="#get_players_turn-1">get_players_turn/1</a></td><td>Get players who's turn it is next.</td></tr><tr><td valign="top"><a href="#get_status-1">get_status/1</a></td><td>Get game session status.</td></tr><tr><td valign="top"><a href="#get_turn_number-1">get_turn_number/1</a></td><td>Get turn number.</td></tr><tr><td valign="top"><a href="#get_winners_losers-1">get_winners_losers/1</a></td><td>Get winners and losers lists.</td></tr><tr><td valign="top"><a href="#has_player-2">has_player/2</a></td><td>Is a player a member of this game session?.</td></tr><tr><td valign="top"><a href="#increment_turn_number-1">increment_turn_number/1</a></td><td>Increments the internal turn number.</td></tr><tr><td valign="top"><a href="#is_players_turn-2">is_players_turn/2</a></td><td>Is it a given player's turn?.</td></tr><tr><td valign="top"><a href="#new-1">new/1</a></td><td>Create a new <a href="nuk_game_session.md#type-get_game_session"><code>nuk_game_session:get_game_session()</code></a> data type.</td></tr><tr><td valign="top"><a href="#remove_player-2">remove_player/2</a></td><td>Remove a player from the game session.</td></tr><tr><td valign="top"><a href="#set_game_state-2">set_game_state/2</a></td><td>Sets game engine state in the session.</td></tr><tr><td valign="top"><a href="#set_players-2">set_players/2</a></td><td>Set a list of players to the current game session.</td></tr><tr><td valign="top"><a href="#set_players_turn-2">set_players_turn/2</a></td><td>Set players who's turn it is next.</td></tr><tr><td valign="top"><a href="#set_status-2">set_status/2</a></td><td>Set game session status.</td></tr><tr><td valign="top"><a href="#set_turn_number-2">set_turn_number/2</a></td><td>Set turn number.</td></tr><tr><td valign="top"><a href="#set_winners_losers-3">set_winners_losers/3</a></td><td>Set winners and losers.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="add_player-2"></a>

### add_player/2 ###

<pre><code>
add_player(Session::<a href="#type-session">session()</a>, Player::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

Add a player to the game session

Whenver a players joins a game, nuk uses this function to add that player to
the game session.

<a name="get_game-1"></a>

### get_game/1 ###

<pre><code>
get_game(Session::<a href="#type-session">session()</a>) -&gt; <a href="nuk_game.md#type-game">nuk_game:game()</a>
</code></pre>
<br />

Get game

Returns the [`nuk_game:game()`](nuk_game.md#type-game) data type to which this session belongs.

<a name="get_game_state-1"></a>

### get_game_state/1 ###

<pre><code>
get_game_state(Session::<a href="#type-session">session()</a>) -&gt; term()
</code></pre>
<br />

Get game engine arbitrary state

Returns an arbitrary game state set by the game engine. Since this value
is specific to a specific game engine, it is the responsibility of the
respective game engine to provide functions to extract information
from this value.

<a name="get_players-1"></a>

### get_players/1 ###

<pre><code>
get_players(Session::<a href="#type-session">session()</a>) -&gt; [<a href="nuk_user.md#type-user">nuk_user:user()</a>]
</code></pre>
<br />

Get players currently in the game session

Returns a list of [`nuk_user:user()`](nuk_user.md#type-user) data types that represent a list
of players currently joined to this game session.

<a name="get_players_count-1"></a>

### get_players_count/1 ###

<pre><code>
get_players_count(Session::<a href="#type-session">session()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

Get number of players currently in the game session

Returns number of players currently in this game session.

<a name="get_players_turn-1"></a>

### get_players_turn/1 ###

<pre><code>
get_players_turn(Session::<a href="#type-session">session()</a>) -&gt; [<a href="nuk_user.md#type-user">nuk_user:user()</a>]
</code></pre>
<br />

Get players who's turn it is next

Returns a list of [`nuk_user:user()`](nuk_user.md#type-user) data types that represent a list
of players who the game engine is expecting to make the turn(s) next. i.e.
the answer to "who's turn is it?" question.

<a name="get_status-1"></a>

### get_status/1 ###

<pre><code>
get_status(Session::<a href="#type-session">session()</a>) -&gt; <a href="#type-nuk_game_session_status">nuk_game_session_status()</a>
</code></pre>
<br />

Get game session status

Returns an atom status of the game session

<a name="get_turn_number-1"></a>

### get_turn_number/1 ###

<pre><code>
get_turn_number(Session::<a href="#type-session">session()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

Get turn number

Every time any player makes a turn nuk increments an internal turn counter.
This returns the current turn number from the game session.

<a name="get_winners_losers-1"></a>

### get_winners_losers/1 ###

<pre><code>
get_winners_losers(Session::<a href="#type-session">session()</a>) -&gt; {Winners::[<a href="nuk_user.md#type-user">nuk_user:user()</a>], Losers::[<a href="nuk_user.md#type-user">nuk_user:user()</a>]}
</code></pre>
<br />

Get winners and losers lists

Returns two lists of [`nuk_user:user()`](nuk_user.md#type-user) data types that represent a
list of players who have won and who have lost the game. This is only
relevant once the game has completed. In all other cases these lists will
be empty.

<a name="has_player-2"></a>

### has_player/2 ###

<pre><code>
has_player(Session::<a href="#type-session">session()</a>, Player::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; boolean()
</code></pre>
<br />

Is a player a member of this game session?

This is useful for checking whether a given user is a player in this game
session.

<a name="increment_turn_number-1"></a>

### increment_turn_number/1 ###

<pre><code>
increment_turn_number(Session::<a href="#type-session">session()</a>) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

Increments the internal turn number

nuk uses an internal turn counter every time any player makes a turn. This
is used to increment that turn counter.

<a name="is_players_turn-2"></a>

### is_players_turn/2 ###

<pre><code>
is_players_turn(Session::<a href="#type-session">session()</a>, Player::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; boolean()
</code></pre>
<br />

Is it a given player's turn?

This is useful to checking whether it is OK for a given player to make a
turn.

<a name="new-1"></a>

### new/1 ###

<pre><code>
new(Game::<a href="nuk_game.md#type-game">nuk_game:game()</a>) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

Create a new [`nuk_game_session:get_game_session()`](nuk_game_session.md#type-get_game_session) data type.

`Game` is a [`nuk_game:game()`](nuk_game.md#type-game) data type which is stored inside the
session. All other values are set to their defaults. For default values see
the top description of this module.

<a name="remove_player-2"></a>

### remove_player/2 ###

<pre><code>
remove_player(Session::<a href="#type-session">session()</a>, Player::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

Remove a player from the game session

Whenver a player leaves a game, nuk uses this function to remove that player
from the game session.

<a name="set_game_state-2"></a>

### set_game_state/2 ###

<pre><code>
set_game_state(Session::<a href="#type-session">session()</a>, GameState::term()) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

Sets game engine state in the session

With every successful [`nuk_game_engine`](nuk_game_engine.md) callback the game engine
returns its new state. This function is used to then store that state in the
game session.

<a name="set_players-2"></a>

### set_players/2 ###

<pre><code>
set_players(Session::<a href="#type-session">session()</a>, Players::[<a href="nuk_user.md#type-user">nuk_user:user()</a>]) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

Set a list of players to the current game session

nuk uses this function to set the initial players to the game session.

<a name="set_players_turn-2"></a>

### set_players_turn/2 ###

<pre><code>
set_players_turn(Session::<a href="#type-session">session()</a>, Players::[<a href="nuk_user.md#type-user">nuk_user:user()</a>]) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

Set players who's turn it is next

In cases where [`nuk_game_engine`](nuk_game_engine.md) callback returns the players who's
turn it is next, nuk uses this function to update them in its session state.

<a name="set_status-2"></a>

### set_status/2 ###

<pre><code>
set_status(Session::<a href="#type-session">session()</a>, Status::<a href="#type-nuk_game_session_status">nuk_game_session_status()</a>) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

Set game session status

nuk uses this function to update the general game status.

<a name="set_turn_number-2"></a>

### set_turn_number/2 ###

<pre><code>
set_turn_number(Session::<a href="#type-session">session()</a>, TurnNumber::non_neg_integer()) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

Set turn number

Sets turn number to a specified integer.

<a name="set_winners_losers-3"></a>

### set_winners_losers/3 ###

<pre><code>
set_winners_losers(Session::<a href="#type-session">session()</a>, Winners::[<a href="nuk_user.md#type-user">nuk_user:user()</a>], Losers::[<a href="nuk_user.md#type-user">nuk_user:user()</a>]) -&gt; <a href="#type-session">session()</a>
</code></pre>
<br />

Set winners and losers

nuk uses this function to set winners and losers in the game session. This
is used once the game has completed.

