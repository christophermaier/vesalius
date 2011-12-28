%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <christopher.maier@gmail.com>
%% @copyright 2011 Christopher Maier

-module(vesalius).

-export([
         main/1
        ]).

-include("vesalius.hrl").

main(Args0) ->
    ?CONSOLE("===== VESALIUS =====~n"),

    {Opts, Args} = vesalius_options:process_args(Args0),
    case vesalius_options:get(help, Opts) of
        undefined ->
            perform_analysis(Opts, Args);
        _ ->
            vesalius_options:usage()
    end.

perform_analysis(Opts, Args) ->
    ?CONSOLE("Parsed Opts: ~p~n", [Opts]),
    ?CONSOLE("Parsed Args: ~p~n", [Args]),

    %%Deps = vesalius_options:get(deps, Opts),
    Paths = vesalius_options:get(paths, Opts),

    %% TODO: Maybe make the build process a gen_server, with stuff
    %% like the project directory part of its state?  That way it
    %% wouldn't need to be passed through all the functions.
    setup_xref(code:get_path(), Paths),

    {ok, _OldPath} = vesalius_util:add_to_path(Paths),

    TargetModules = vesalius_options:get(target_modules, Opts),

    ReportModulesFun = vesalius_filter:keep_from_modules(TargetModules),

    analyze({"X - XU", "unused exports"},
            TargetModules,
            ReportModulesFun),

    analyze({"UU - X",
             "Local (non-exported) functions that have not been used; REMOVE THEM!"},
            TargetModules,
            ReportModulesFun),

    analyze({"LU * (X - XU)",
             "Exported functions, but only used locally (i.e., remove these from the exports list)"},
            TargetModules,
            ReportModulesFun),

    analyze({"DF - (XU + LU)",
             "Deprecated Functions that are no longer used anywhere, and can be removed"},
            TargetModules,
            ReportModulesFun),

    analyze({"DF * (XU + LU)",
             "Deprecated Functions that are still being used, either locally or externally"},
            TargetModules,
            ReportModulesFun),

    analyze({"X - XU", "Unexpected testing functions"},
            TargetModules,
            [ReportModulesFun,
             fun vesalius_filter:is_test_function/1]),

    ?CONSOLE("End of run~n").

setup_xref(LibPath, AnalysisPath) ->
    {ok, _Pid} = xref:start(?XREF_SERVER),
    xref:set_library_path(?XREF_SERVER, LibPath),
    [xref:add_directory(?XREF_SERVER, P) || P <- AnalysisPath ].

%% TODO: Make behaviour filtering optional?  Any use for that?
filter_results(Answer, Modules, Filter) ->
    [ {M,F,A} || {M,F,A} <- vesalius_filter:remove_functions(Answer, Modules, [behaviour, webmachine]),
                 Filter({M,F,A})].

print_results(Analysis, Modules, Answer, Description) ->
    ?CONSOLE("Analysis: ~s~n", [Description]),
    ?CONSOLE("Xref Query String: ~s~n", [Analysis]),
    ?CONSOLE("Modules: ~p~n", [Modules]),
    ?CONSOLE("==========~n"),
    [?CONSOLE("~p:~p/~p~n", [M,F,A]) || {M,F,A} <- Answer],
    ?CONSOLE("==========~n").

analyze({Query, Description}, Modules, FilterFuns) when is_function(FilterFuns) ->
    analyze({Query, Description}, Modules, [FilterFuns]);
analyze({Query, Description}, Modules, FilterFuns) when is_list(FilterFuns)->
    {ok, Answer} = xref:q(?XREF_SERVER, Query),
    Filtered = filter_results(Answer, Modules,
                              compose_filters(FilterFuns)),
    print_results(Query, Modules, Filtered, Description),
    Filtered.

compose_filters([]) ->
    fun(_) -> true end;
compose_filters(Fun) when is_function(Fun)->
    Fun;
compose_filters(Funs) when is_list(Funs) ->
    fun(MFA) ->
            short_circuit(lists:map(fun(F) ->
                                            F(MFA)
                                    end,
                                    Funs))
    end.

short_circuit([true])        -> true;
short_circuit([false|_Rest]) -> false;
short_circuit([true|Rest])   -> short_circuit(Rest).
