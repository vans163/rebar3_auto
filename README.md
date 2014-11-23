rebar_auto_plugin
=====

A rebar3 plugin for auto running tasks on source file change.

Build
-----

    $ rebar3 compile

Use
---

Add the plugin to your rebar config:

    {plugins, [
        {rebar_auto_plugin, ".*", {git, "https://github.com/tsloughter/rebar_auto_plugin.git", {branch, "master"}}}
    ]}.

Then just call your plugin directly in an existing application:


    $ rebar3 auto
    ===> Fetching rebar_auto_plugin
    ===> Compiling rebar_auto_plugin
