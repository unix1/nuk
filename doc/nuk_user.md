

# Module nuk_user #
* [Description](#description)
* [Data Types](#types)
* [Function Index](#index)
* [Function Details](#functions)

nuk user.

<a name="types"></a>

## Data Types ##




### <a name="type-user">user()</a> ###


__abstract datatype__: `user()`

<a name="index"></a>

## Function Index ##


<table width="100%" border="1" cellspacing="0" cellpadding="2" summary="function index"><tr><td valign="top"><a href="#check_password-2">check_password/2</a></td><td></td></tr><tr><td valign="top"><a href="#get_username-1">get_username/1</a></td><td></td></tr><tr><td valign="top"><a href="#new-2">new/2</a></td><td></td></tr></table>


<a name="functions"></a>

## Function Details ##

<a name="check_password-2"></a>

### check_password/2 ###

<pre><code>
check_password(X1::<a href="#type-user">user()</a>, EnteredPassword::string()) -&gt; boolean()
</code></pre>
<br />

<a name="get_username-1"></a>

### get_username/1 ###

<pre><code>
get_username(X1::<a href="#type-user">user()</a>) -&gt; string()
</code></pre>
<br />

<a name="new-2"></a>

### new/2 ###

<pre><code>
new(Username::string(), Password::string()) -&gt; <a href="#type-user">user()</a>
</code></pre>
<br />

