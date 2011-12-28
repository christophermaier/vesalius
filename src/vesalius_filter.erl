%% -*- erlang-indent-level: 4;indent-tabs-mode: nil; fill-column: 92-*-
%% ex: ts=4 sw=4 et
%% @author Christopher Maier <christopher.maier@gmail.com>
%% @copyright 2011 Christopher Maier.

-module(vesalius_filter).

-export([
         behaviour_callbacks/1,
         is_test_function/1,
         callbacks/2,
         keep_from_modules/1,
         remove_functions/3
        ]).

%% Standard 2-argument Webmachine resource callbacks... doesn't include `init/1' or `ping/2'
-define(WM_CALLBACKS, [
                       allow_missing_post,
                       allowed_methods,
                       charsets_provided,
                       content_types_accepted,
                       content_types_provided,
                       create_path,
                       delete_completed,
                       delete_resource,
                       encodings_provided,
                       expires,
                       finish_request,
                       forbidden,
                       generate_etag,
                       is_authorized,
                       is_conflict,
                       known_content_type,
                       last_modified,
                       malformed_request,
                       moved_permanently,
                       moved_temporarily,
                       multiple_choices,
                       options,
                       post_is_create,
                       previously_existed,
                       process_post,
                       resource_exists,
                       service_available,
                       uri_too_long,
                       valid_content_headers,
                       valid_entity_length,
                       variances
                      ]).

is_test_function({_M, test, 0}) -> true;
is_test_function(_MFA)          -> false.

-spec behaviour_callbacks( module() ) -> [ mfa() ].
behaviour_callbacks(Module) ->
    Attributes = Module:module_info(attributes),
    Behaviours = proplists:append_values(behaviour, Attributes), % can implement several behaviours
    lists:foldl(fun(B, Acc) ->
                        Acc ++ [{Module, F, A} ||
                                   {F,A} <- B:behaviour_info(callbacks)]
                end,
                [],
                Behaviours).

-spec is_webmachine_resource( module() ) -> boolean().
is_webmachine_resource(Module) ->
    {ok, Ast} = vesalius_util:get_ast(Module),
    IsResource = fun ({attribute, _, record, {wm_reqdata, _}}, _) ->
                         true;
                     (_, Flag) ->
                         Flag
                 end,
    lists:foldl(IsResource, false, Ast).

%% @doc Return a list of all standard Webmachine callbacks in this
%% module.  Does not currently include any functions specified in
%% `content_types_provided', `content_types_accepted',
%% `encodings_provided', `charsets_provided'
%% @end
-spec webmachine_callbacks( module() ) -> [ mfa() ].
webmachine_callbacks(Module) ->
    case is_webmachine_resource(Module) of
        true ->
            Callbacks = [ {Module, init, 1}, {Module, ping, 2}
                          | [ {Module, F, 2} || F <- ?WM_CALLBACKS ]],
            vesalius_util:intersect(Callbacks,
                                    vesalius_util:get_exports(Module));
        false ->
            []
    end.

function_class(Modules, Fun) ->
    lists:foldl(fun(Module, Acc) ->
                        Acc ++ Fun(Module)
                end,
                [],
                Modules).

callbacks(webmachine, Modules) ->
    function_class(Modules, fun webmachine_callbacks/1);
callbacks(behaviour, Modules) ->
    function_class(Modules, fun behaviour_callbacks/1).

remove_functions(MFAs, _Modules, []) ->
    MFAs;
remove_functions(MFAs, Modules, [CallbackType|Rest]) ->
    Leftover = vesalius_util:subtract(MFAs, callbacks(CallbackType, Modules)),
    remove_functions(Leftover, Modules, Rest).


keep_from_modules(Modules) ->
    fun ({M, _F, _A}) ->
            lists:member(M, Modules)
    end.
