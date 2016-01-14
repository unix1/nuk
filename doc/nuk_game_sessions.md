

# Module nuk_game_sessions #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

`nuk_game_sessions` module.

<a name="description"></a>

## Description ##

This module should be used as an API for mapping game session identifiers
to process IDs:
- given a game process `pid()` create a new unique session identifier
- translate a given unique session identifier to the game session `pid()`

The backend implementation of this is swappable. See
[`nuk_game_session_storage`](nuk_game_session_storage.md) behavior and
[`nuk_game_session_store_server`](nuk_game_session_store_server.md) default implementation.<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td>Delete a session.</td></tr><tr><td valign="top"><a href="#get_pid-1">get_pid/1</a></td><td>Get a process ID.</td></tr><tr><td valign="top"><a href="#put-1">put/1</a></td><td>Create a new session.</td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-1"></a>

### delete/1 ###

<pre><code>
delete(SessionId::string()) -&gt; ok
</code></pre>
<br />

Delete a session

Delete the session associated with the given session identifier.

<a name="get_pid-1"></a>

### get_pid/1 ###

<pre><code>
get_pid(SessionId::string()) -&gt; {ok, Pid::pid()} | {error, game_session_not_found, Extra::string()}
</code></pre>
<br />

Get a process ID

Given a previously created session identifier, retrieve a process ID.

<a name="put-1"></a>

### put/1 ###

<pre><code>
put(Pid::pid()) -&gt; SessionId::string()
</code></pre>
<br />

Create a new session

Given a process ID, create a new unique session identifier.

