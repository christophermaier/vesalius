%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <christopher.maier@gmail.com>
%% @copyright 2011 Christopher Maier.

-module(vesalius_options).

-export([
         get/2,
         get/3,
         process_args/1,
         usage/0
        ]).

-include("vesalius.hrl").

-define(OPTIONS,
        [{modules, $m, "modules", string, "A comma-delimited list of modules to report on"},
         {paths, $p, "paths", string, "A comma-delimited list of paths of ebin dirs"},
         {apps, $a, "apps", string, "The apps whose modules you want to report on.  Augments 'modules' if present"},

%         {app, $a, "app", string, "Whole app"},
         {libdir, $l, "libdir", string, "libdir"},
         {help, $h, "help", boolean, "Display brief help"}

        ]).

%% @doc Treats a proplist like an updateable hash, replacing the first
%% value of `Key' in `Proplist' with `NewVal'.
update(Key, Proplist, NewVal) ->
    lists:keystore(Key, 1,
                   Proplist,
                   {Key, NewVal}).

%% @doc Displays usage information
usage() ->
    ScriptName = filename:basename(escript:script_name()),
    getopt:usage(?OPTIONS, ScriptName).


%% This isn't a "real" key, but rather a synthetic one
get(target_modules, Proplist) ->
    get(modules, Proplist, []) ++ get(app_modules, Proplist, []);

get(Key, Proplist) ->
    proplists:get_value(Key, Proplist).
get(Key, Proplist, Default) ->
    proplists:get_value(Key, Proplist, Default).

%% @doc split a string on comma characters
split(ArgString) ->
    string:tokens(ArgString, [$,]).

split_to_atoms(ArgString) ->
    [ list_to_atom(S) || S <- split(ArgString) ].

%-spec process_args( [ string() ]) -> [ tuple() ].
process_args([]) ->
    process_args(["--help"]);
process_args(Args) ->
    %% TODO: Need some more robust validation here
    {Parsed, NonOptArgs} = case getopt:parse(?OPTIONS, Args) of
                 {ok, ParsedArgs} ->
                     ParsedArgs;
                 {error, Reason} ->
                     case Reason of
                         {missing_option_arg, HasNoArg} ->
                             ?CONSOLE("Oops!  You forgot a value for the '~p' argument!~n", [HasNoArg]);
                         {invalid_option, BadOpt} ->
                             ?CONSOLE("Unknown option ~p!~n", [BadOpt])
                     end,
                     getopt:usage(?OPTIONS, "vesalius"),
                     {exit}
             end,
    ?CONSOLE("Parsed: ~p~n", [Parsed]),

    Processed = process_options(Parsed, [fun process_paths/1,  % process_paths MUST be first, since it alters the code path, which the latter depend on
                                         fun process_modules/1,
                                         fun process_apps/1]),

    {Processed, NonOptArgs}.

process_options(ParsedOptions, ProcessingFuns) ->
    lists:foldl(fun(Fun, Acc) ->
                         Fun(Acc)
                 end,
                 ParsedOptions,
                 ProcessingFuns).

%% @doc All `Paths' are directories relative to the current directory
%% (or absolutely specified) in which `ebin' directories are searched
%% for code.  Each directory is checked for existence.  Any wildcards
%% are expanded.  The final list of fully-qualified paths to ebin
%% directories is returned.
process_paths(Options) ->
    case get(paths, Options) of
        undefined ->
            Options;
    Paths0 ->
            Paths = split(Paths0),
            All = lists:flatmap(fun(Path) ->
                                        filelib:wildcard(filename:join(Path, "ebin"))
                                end,
                                Paths),
            P= [ filename:absname(D) || D <- All ],
            {ok, OldPath} = vesalius_util:add_to_path(P),
            O1 = update(paths, Options, P),
            update(orig_path, O1, OldPath)
    end.

%% TODO: bail if a module isn't on the code path
process_modules(Options) ->
    case get(modules, Options) of
        undefined ->
            Options;
        Modules0 ->
            Modules = split_to_atoms(Modules0),
            update(modules, Options, Modules)
    end.

%% TODO: bail if an app isn't actually on the code path
process_apps(Options) ->
    case get(apps, Options) of
        undefined ->
            Options;
        Apps0 ->
            Apps = split_to_atoms(Apps0),
            O1 = update(apps, Options, Apps),
            AppModules = lists:flatmap(fun(App) ->
                                               ok = application:load(App),
                                               {ok, Modules} = application:get_key(App, modules),
                                               Modules
                                       end,
                                       Apps),
            update(app_modules, O1, AppModules)
    end.
