%% @doc
%% Add the plugin to your rebar config, since it is a developer tool and not
%% necessary for building any project you work on I put it in
%% `~/config/.rebar3/rebar.config`:
%%
%% ```
%% {plugins, [rebar3_auto]}.'''
%%
%% Then just call your plugin directly in an existing application:
%%
%% ```
%% $ rebar3 auto
%% ===> Fetching rebar_auto_plugin
%% ===> Compiling rebar_auto_plugin'''
%%
-module(rebar3_auto).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, auto).
-define(DEPS, [app_discovery]).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},        % The 'user friendly' name of the task
            {module, ?MODULE},        % The module implementation of the task
            {bare, true},             % The task can be run by the user, always true
            {deps, ?DEPS},            % The list of dependencies
            {example, "rebar3 auto"}, % How to use the plugin
            {opts, []},               % list of options understood by the plugin
            {short_desc, "Automatically run compile task on change of source file."},
            {desc, ""}
    ]),
    {ok, rebar_state:add_provider(State, Provider)}.

-spec do(rebar_state:t()) -> {ok, rebar_state:t()} | {error, string()}.
do(State) ->
    auto(State).

-spec format_error(any()) ->  iolist().
format_error(Reason) ->
    io_lib:format("~p", [Reason]).

auto(State) ->
    Task = case rebar_state:command_args(State) of
               [T | _] ->
                   list_to_atom(T);
               _ ->
                   compile
           end,

    State1 = rebar_state:set(State, task, Task),
    State2 = rebar_state:command_args(State1, []),

    listen_on_project_apps(State2),
    State3 = remove_from_plugin_paths(State2),

    case rebar_core:process_command(State3, Task) of
        {ok, State4} ->
            auto(Task, State4);
        Error ->
            Error
    end.

auto(Provider, State) ->
    flush(),
    receive
        _Msg ->
            ok
    end,
    {ok, State1} = rebar_core:do([Provider], State),
    auto(Provider, State1).

flush() ->
    receive
        _ ->
            flush()
    after
        0 -> ok
    end.

listen_on_project_apps(State) ->
    ProjectApps = rebar_state:project_apps(State),
    lists:foreach(fun(AppInfo) ->
                          SrcDir = filename:join(rebar_app_info:dir(AppInfo), "src"),
                          enotify:start_link(SrcDir)
                  end, ProjectApps).

remove_from_plugin_paths(State) ->
    PluginPaths = rebar_state:code_paths(State, all_plugin_deps),
    PluginsMinusAuto = lists:filter(fun(Path) ->
                                            Name = filename:basename(Path, "/ebin"),
                                            not (list_to_atom(Name) =:= rebar_auto_plugin
                                                orelse list_to_atom(Name) =:= enotify)
                                    end, PluginPaths),
    rebar_state:code_paths(State, all_plugin_deps, PluginsMinusAuto).
