

# Module nuk_game #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

`nuk_game` module.

<a name="description"></a>

## Description ##
This module is used to operate on the [`nuk_game:game()`](nuk_game.md#type-game) data type.
This data type is used during game registration in nuk - i.e. when calling
[`nuk_games:register/1`](nuk_games.md#register-1), [`nuk_games:get/1`](nuk_games.md#get-1),
[`nuk_games:list/0`](nuk_games.md#list-0).
<a name="types"></a>

## Data Types ##




### <a name="type-game">game()</a> ###


__abstract datatype__: `game()`

 Data type used to register games in nuk.  Use functions in this module to
operate on this data type.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_max_players-1">get_max_players/1</a></td><td>Get maximum number of players.</td></tr><tr><td valign="top"><a href="#get_min_players-1">get_min_players/1</a></td><td>Get minimum number of players.</td></tr><tr><td valign="top"><a href="#get_module-1">get_module/1</a></td><td>Get module name.</td></tr><tr><td valign="top"><a href="#get_name-1">get_name/1</a></td><td>Get game name.</td></tr><tr><td valign="top"><a href="#new-4">new/4</a></td><td>Create a new <a href="nuk_game.md#type-game"><code>nuk_game:game()</code></a> data type.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_max_players-1"></a>

### get_max_players/1 ###

<pre><code>
get_max_players(Game::<a href="#type-game">game()</a>) -&gt; integer()
</code></pre>
<br />

Get maximum number of players

Extract maximum number of players from [`nuk_game:game()`](nuk_game.md#type-game) data type.

<a name="get_min_players-1"></a>

### get_min_players/1 ###

<pre><code>
get_min_players(Game::<a href="#type-game">game()</a>) -&gt; integer()
</code></pre>
<br />

Get minimum number of players

Extract minimum number of players from [`nuk_game:game()`](nuk_game.md#type-game) data type.

<a name="get_module-1"></a>

### get_module/1 ###

<pre><code>
get_module(Game::<a href="#type-game">game()</a>) -&gt; atom()
</code></pre>
<br />

Get module name

Extract module name from [`nuk_game:game()`](nuk_game.md#type-game) data type.

<a name="get_name-1"></a>

### get_name/1 ###

<pre><code>
get_name(Game::<a href="#type-game">game()</a>) -&gt; string()
</code></pre>
<br />

Get game name

Extract game name from [`nuk_game:game()`](nuk_game.md#type-game) data type.

<a name="new-4"></a>

### new/4 ###

<pre><code>
new(GameName::string(), Module::atom(), MinPlayers::integer(), MaxPlayers::integer) -&gt; <a href="#type-game">game()</a>
</code></pre>
<br />

Create a new [`nuk_game:game()`](nuk_game.md#type-game) data type

`GameName` is a string used to identify the game during registration.
`Module` is the name of the module implementing the
[`nuk_game_engine`](nuk_game_engine.md) behavior. `MinPlayers` and `MaxPlayers` are integers
used for game registration. They are used by nuk to validate correct
number of players before starting a game.

