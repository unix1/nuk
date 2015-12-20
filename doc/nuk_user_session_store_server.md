

# Module nuk_user_session_store_server #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

nuk user session storage server.

__Behaviours:__ [`nuk_user_session_storage`](nuk_user_session_storage.md).

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td></td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td></td></tr></table>


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

<a name="list-0"></a>

### list/0 ###

<pre><code>
list() -&gt; [<a href="nuk_user_session.md#type-session">nuk_user_session:session()</a>]
</code></pre>
<br />

