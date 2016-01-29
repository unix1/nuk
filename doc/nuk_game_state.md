

# Module nuk_game_state #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

`nuk_game_state` module.

<a name="description"></a>

## Description ##
This module is used to operate on [`nuk_game_state:state()`](nuk_game_state.md#type-state) data type.
This data type is used when retrieving nuk's general game session state from
[`nuk_game_session:get_nuk_state/1`](nuk_game_session.md#get_nuk_state-1).
<a name="types"></a>

## Data Types ##




### <a name="type-state">state()</a> ###


__abstract datatype__: `state()`

 Data type containing nuk's general game state. This is part of
[`nuk_game_session:session()`](nuk_game_session.md#type-session) data type. Functions in this module can
be used to operate on the following data:
- `status`: game session status, default `nil`, use [`get_status/1`](#get_status-1) to
extract
- `turn_number`: current turn number, default `0`, use
[`get_turn_number/1`](#get_turn_number-1) to extract
- `players`: list of players currently in the game session, default `[]`,
use [`get_players/1`](#get_players-1) to extract
- `players_turn`: list of players who should make turn(s) next,
default `[]`, use [`get_players_turn/1`](#get_players_turn-1) to extract
- `players_winners`: list of players who won the game, only populated
after the game completes, default `[]`, use [`get_winners_losers/1`](#get_winners_losers-1)
to extract
- `players_losers`: list of players who lost the game, only populated
after the game completes, default `[]`, use [`get_winners_losers/1`](#get_winners_losers-1)
to extract



### <a name="type-status">status()</a> ###


<pre><code>
status() = nil | initialized | await_turn | complete
</code></pre>

 General game session status tracked by nuk.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_players-1">get_players/1</a></td><td>Get players currently in the game session.</td></tr><tr><td valign="top"><a href="#get_players_turn-1">get_players_turn/1</a></td><td>Get players whose turn it is next.</td></tr><tr><td valign="top"><a href="#get_status-1">get_status/1</a></td><td>Get game session status.</td></tr><tr><td valign="top"><a href="#get_turn_number-1">get_turn_number/1</a></td><td>Get turn number.</td></tr><tr><td valign="top"><a href="#get_winners_losers-1">get_winners_losers/1</a></td><td>Get winners and losers lists.</td></tr><tr><td valign="top"><a href="#new-0">new/0</a></td><td>Create a new <a href="#type-state"><code>state()</code></a> data type.</td></tr><tr><td valign="top"><a href="#set_players-2">set_players/2</a></td><td>Set a list of players to the current game session.</td></tr><tr><td valign="top"><a href="#set_players_turn-2">set_players_turn/2</a></td><td>Set players whose turn it is next.</td></tr><tr><td valign="top"><a href="#set_status-2">set_status/2</a></td><td>Set status.</td></tr><tr><td valign="top"><a href="#set_turn_number-2">set_turn_number/2</a></td><td>Set turn number.</td></tr><tr><td valign="top"><a href="#set_winners_losers-3">set_winners_losers/3</a></td><td>Set winners and losers.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_players-1"></a>

### get_players/1 ###

<pre><code>
get_players(State::<a href="#type-state">state()</a>) -&gt; [<a href="nuk_user.md#type-user">nuk_user:user()</a>]
</code></pre>
<br />

Get players currently in the game session

Returns a list of [`nuk_user:user()`](nuk_user.md#type-user) data types that represent a list
of players currently joined to this game session.

<a name="get_players_turn-1"></a>

### get_players_turn/1 ###

<pre><code>
get_players_turn(State::<a href="#type-state">state()</a>) -&gt; [<a href="nuk_user.md#type-user">nuk_user:user()</a>]
</code></pre>
<br />

Get players whose turn it is next

Returns a list of [`nuk_user:user()`](nuk_user.md#type-user) data types that represent a list
of players who the game engine is expecting to make the turn(s) next. i.e.
the answer to "whose turn is it?" question.

<a name="get_status-1"></a>

### get_status/1 ###

<pre><code>
get_status(State::<a href="#type-state">state()</a>) -&gt; <a href="#type-status">status()</a>
</code></pre>
<br />

Get game session status

Returns an atom status of the game session.

<a name="get_turn_number-1"></a>

### get_turn_number/1 ###

<pre><code>
get_turn_number(State::<a href="#type-state">state()</a>) -&gt; non_neg_integer()
</code></pre>
<br />

Get turn number

Every time any player makes a turn nuk increments an internal turn counter.
This returns the current turn number from the game session.

<a name="get_winners_losers-1"></a>

### get_winners_losers/1 ###

<pre><code>
get_winners_losers(State::<a href="#type-state">state()</a>) -&gt; {Winners::[<a href="nuk_user.md#type-user">nuk_user:user()</a>], Losers::[<a href="nuk_user.md#type-user">nuk_user:user()</a>]}
</code></pre>
<br />

Get winners and losers lists

Returns two lists of [`nuk_user:user()`](nuk_user.md#type-user) data types that represent a
list of players who have won and who have lost the game. This is only
relevant once the game has completed. In all other cases these lists are
likely to be empty.

<a name="new-0"></a>

### new/0 ###

<pre><code>
new() -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Create a new [`state()`](#type-state) data type.

Creates a new state with default values.

<a name="set_players-2"></a>

### set_players/2 ###

<pre><code>
set_players(State::<a href="#type-state">state()</a>, Players::[<a href="nuk_user.md#type-user">nuk_user:user()</a>]) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Set a list of players to the current game session

Useful for setting initial set of players for the game session.

<a name="set_players_turn-2"></a>

### set_players_turn/2 ###

<pre><code>
set_players_turn(State::<a href="#type-state">state()</a>, Players::[<a href="nuk_user.md#type-user">nuk_user:user()</a>]) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Set players whose turn it is next

Sets a new list of players, replacing an existing list.

<a name="set_status-2"></a>

### set_status/2 ###

<pre><code>
set_status(State::<a href="#type-state">state()</a>, Status::<a href="#type-status">status()</a>) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Set status

Sets a new status in the game state.

<a name="set_turn_number-2"></a>

### set_turn_number/2 ###

<pre><code>
set_turn_number(State::<a href="#type-state">state()</a>, TurnNumber::non_neg_integer()) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Set turn number

Sets turn number to a specified integer.

<a name="set_winners_losers-3"></a>

### set_winners_losers/3 ###

<pre><code>
set_winners_losers(State::<a href="#type-state">state()</a>, Winners::[<a href="nuk_user.md#type-user">nuk_user:user()</a>], Losers::[<a href="nuk_user.md#type-user">nuk_user:user()</a>]) -&gt; <a href="#type-state">state()</a>
</code></pre>
<br />

Set winners and losers

This is typically used once the game has completed.

