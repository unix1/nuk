

# Module nuk_user_sessions #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

nuk sessions.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td></td></tr><tr><td valign="top"><a href="#get_user-1">get_user/1</a></td><td></td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-1"></a>

### delete/1 ###

<pre><code>
delete(SessionId::string()) -&gt; ok
</code></pre>
<br />

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(SessionId::string()) -&gt; {ok, <a href="nuk_user_session.md#type-session">nuk_user_session:session()</a>} | {error, user_session_not_found, Extra::string()}
</code></pre>
<br />

<a name="get_user-1"></a>

### get_user/1 ###

<pre><code>
get_user(SessionId::string()) -&gt; {ok, <a href="nuk_user.md#type-user">nuk_user:user()</a>} | {error, user_session_not_found, Extra::string()}
</code></pre>
<br />

<a name="list-0"></a>

### list/0 ###

<pre><code>
list() -&gt; [<a href="nuk_user_session.md#type-session">nuk_user_session:session()</a>]
</code></pre>
<br />

