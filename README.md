rebar_auto_plugin
=====

A rebar3 plugin for auto running compile on source file change reloading modules in the shell.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config, since it is a developer tool and not necessary for building any project you work on I put it in `~/config/.rebar3/rebar.config`:

    {plugins, [rebar3_auto]}.

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
