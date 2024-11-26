rebar_auto_plugin
=====

A rebar3 plugin for auto running compile on source file change reloading modules in the shell.

Prerequisite
-----
On Linux you need to install inotify-tools.

```
-m: 1: inotifywait: not found
```

Use
---

Add the plugin only to your user local rebar config in `~/.config/rebar3/rebar.config`:

```erlang
{plugins, [rebar3_auto]}.
```

If you add it to your project rebar.config, it will get unloaded each time compilation occurs, thus breaking it.

Then run
```
    $ rebar3 compile
```

Then just call your plugin directly in an existing application:


```
(relx) $ rebar3 auto
===> Compiling rebar3_auto
Setting up watches.  Beware: since -r was given, this may take a while!
Watches established.
Erlang/OTP 18 [erts-7.1] [source] [64-bit] [smp:4:4] [async-threads:0] [hipe] [kernel-poll:false]

Eshell V7.1  (abort with ^G)
1> ===> This feature is experimental and may be modified or removed at any time.
Compiling rebar3_auto
Verifying dependencies...
Compiling relx
Compiling rebar3_auto
Verifying dependencies...
Compiling relx

1>
```

Custom extensions, thanks abxy.  
Regex matches are supported, "$" is suffixed automatically, thanks xuchaoqian.  
```
re:run(<<"file_name.erl_ab">>, <<ExtMatch/binary, "$">>)
```


To extend the list add an option to your rebar.config like so:
```
{auto, [
	{extra_extensions, [".alp", ".hterl", ".erl(.*?)"]}
]}.
```

Extra directories.
To extend the list of directories to be watched add an option to your rebar.config like so:
```
{auto, [
	{extra_dirs, ["priv/dtl"]}
]}.
```