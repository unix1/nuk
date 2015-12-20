

# Module nuk_game_sessions #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

nuk game sessions.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#get_pid-1">get_pid/1</a></td><td></td></tr><tr><td valign="top"><a href="#put-1">put/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="get_pid-1"></a>

### get_pid/1 ###

<pre><code>
get_pid(SessionId::string()) -&gt; {ok, Pid::pid()} | {error, game_session_not_found, Extra::string()}
</code></pre>
<br />

<a name="put-1"></a>

### put/1 ###

<pre><code>
put(Pid::pid()) -&gt; SessionId::string()
</code></pre>
<br />

