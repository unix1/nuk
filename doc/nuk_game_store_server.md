

# Module nuk_game_store_server #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

nuk game storage server.

__Behaviours:__ [`gen_server`](gen_server.md), [`nuk_game_storage`](nuk_game_storage.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#code_change-3">code_change/3</a></td><td></td></tr><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td></td></tr><tr><td valign="top"><a href="#handle_call-3">handle_call/3</a></td><td></td></tr><tr><td valign="top"><a href="#handle_cast-2">handle_cast/2</a></td><td></td></tr><tr><td valign="top"><a href="#handle_info-2">handle_info/2</a></td><td></td></tr><tr><td valign="top"><a href="#init-1">init/1</a></td><td></td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td></td></tr><tr><td valign="top"><a href="#put-1">put/1</a></td><td></td></tr><tr><td valign="top"><a href="#start_link-0">start_link/0</a></td><td></td></tr><tr><td valign="top"><a href="#terminate-2">terminate/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="code_change-3"></a>

### code_change/3 ###

`code_change(OldVersion, State, Extra) -> any()`

<a name="delete-1"></a>

### delete/1 ###

<pre><code>
delete(GameName::string()) -&gt; ok
</code></pre>
<br />

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(GameName::string()) -&gt; {ok, <a href="nuk_game.md#type-game">nuk_game:game()</a>} | {error, game_not_found, string()}
</code></pre>
<br />

<a name="handle_call-3"></a>

### handle_call/3 ###

`handle_call(X1, From, State) -> any()`

<a name="handle_cast-2"></a>

### handle_cast/2 ###

`handle_cast(Msg, State) -> any()`

<a name="handle_info-2"></a>

### handle_info/2 ###

`handle_info(Msg, State) -> any()`

<a name="init-1"></a>

### init/1 ###

`init(X1) -> any()`

<a name="list-0"></a>

### list/0 ###

<pre><code>
list() -&gt; [<a href="nuk_game.md#type-game">nuk_game:game()</a>]
</code></pre>
<br />

<a name="put-1"></a>

### put/1 ###

<pre><code>
put(Game::<a href="nuk_game.md#type-game">nuk_game:game()</a>) -&gt; ok
</code></pre>
<br />

<a name="start_link-0"></a>

### start_link/0 ###

`start_link() -> any()`

<a name="terminate-2"></a>

### terminate/2 ###

`terminate(Reason, State) -> any()`

