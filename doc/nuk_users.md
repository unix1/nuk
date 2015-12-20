

# Module nuk_users #
* [Description](#description)
* [Function Index](#index)
* [Function Details](#functions)

nuk users.

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#delete-1">delete/1</a></td><td></td></tr><tr><td valign="top"><a href="#get-1">get/1</a></td><td></td></tr><tr><td valign="top"><a href="#list-0">list/0</a></td><td></td></tr><tr><td valign="top"><a href="#login-2">login/2</a></td><td></td></tr><tr><td valign="top"><a href="#put-1">put/1</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="delete-1"></a>

### delete/1 ###

<pre><code>
delete(Username::string()) -&gt; ok
</code></pre>
<br />

<a name="get-1"></a>

### get/1 ###

<pre><code>
get(Username::string()) -&gt; {ok, <a href="nuk_user.md#type-user">nuk_user:user()</a>} | {error, user_not_found, string()}
</code></pre>
<br />

<a name="list-0"></a>

### list/0 ###

<pre><code>
list() -&gt; [<a href="nuk_user.md#type-user">nuk_user:user()</a>]
</code></pre>
<br />

<a name="login-2"></a>

### login/2 ###

<pre><code>
login(Username::string(), Password::string()) -&gt; {ok, string()} | {error, atom(), string()}
</code></pre>
<br />

<a name="put-1"></a>

### put/1 ###

<pre><code>
put(User::<a href="nuk_user.md#type-user">nuk_user:user()</a>) -&gt; ok
</code></pre>
<br />

