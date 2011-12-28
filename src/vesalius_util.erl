%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <christopher.maier@gmail.com>
%% @copyright 2011 Christopher Maier

-module(vesalius_util).

-export([
         add_to_path/1,
         get_ast/1,
         get_beam/1,
         get_exports/1,
         intersect/2,
         subtract/2,
         write_ast/2
        ]).


%% @doc Adds `Paths' to the end of the current code path.  Returns the previous code path
-spec add_to_path( list() ) -> {ok, list()}.
add_to_path(Paths) ->
    OldPath = code:get_path(),
    ok = code:add_pathsz(Paths),
    {ok, OldPath}.

%% -spec get_beam( atom() ) -> {ok, Beam} | non_existing.
get_beam(Module) ->
    case code:which(Module) of
        non_existing ->
            non_existing;
        Path ->
            file:read_file(Path)
    end.

get_ast(Module) ->
    {ok, Beam} = get_beam(Module),
    {ok, {Module,
          [{abstract_code,
            {raw_abstract_v1, Ast}}]}} = beam_lib:chunks(Beam, [abstract_code]),
    {ok, Ast}.

%% TODO: filter out module_info/0 and module_info/1?
-spec get_exports( module() ) -> [ mfa() ].
get_exports(Module) ->
    [ {Module, F, A} || {F, A} <- Module:module_info(exports) ].

set_utility(S1, S2, F) ->
    ordsets:to_list(
      F(ordsets:from_list(S1),
        ordsets:from_list(S2))).

-spec subtract( list(), list() ) -> list().
subtract(Source, ToRemove) ->
    set_utility(Source, ToRemove, fun ordsets:subtract/2).

-spec intersect( list(), list() ) -> list().
intersect(L1, L2) ->
    set_utility(L1, L2, fun ordsets:intersection/2).

write_ast(Filename, Module) ->
    file:write_file(Filename, iolist_to_binary(io_lib:format("~p~n", [get_ast(Module)]))).
