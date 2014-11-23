-module(rebar_auto_plugin).
-behaviour(provider).

-export([init/1, do/1, format_error/1]).

-define(PROVIDER, auto).
-define(DEPS, []).

%% ===================================================================
%% Public API
%% ===================================================================
-spec init(rebar_state:t()) -> {ok, rebar_state:t()}.
init(State) ->
    Provider = providers:create([
            {name, ?PROVIDER},          % The 'user friendly' name of the task
            {module, ?MODULE},          % The module implementation of the task
            {bare, true},               % The task can be run by the user, always true
            {deps, ?DEPS},              % The list of dependencies
            {example, "rebar auto"}, % How to use the plugin
            {opts, []},                  % list of options understood by the plugin
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
    rebar_file_monitor:start(),
    Apps = rebar_state:project_apps(State),

    Refs = lists:map(fun(App) ->
                             Dir = filename:join(rebar_app_info:dir(App), "src"),
                             {ok, _, Ref} = rebar_file_monitor:monitor_dir(Dir, self()),
                             Ref
                     end, Apps),

    State1 = rebar_state:set(State, task, Task),
    State2 = rebar_state:command_args(State1, []),
    case rebar_core:process_command(State2, Task) of
        {ok, State3} ->
            auto(State3, Task, Refs);
        Error ->
            Error
    end.

auto(State, Task, Refs) ->
    flush(),
    receive
        {file_monitor, _, _Msg} ->
            ok
    end,
    {ok, State1} = do(Task, State),
    auto(State1, Task, Refs).

do(ProviderName, State) ->
    Provider = providers:get_provider(ProviderName
                                     ,rebar_state:providers(State)),
    providers:do(Provider, State).

flush() ->
    receive
        _ ->
            flush()
    after
        0 -> ok
    end.
