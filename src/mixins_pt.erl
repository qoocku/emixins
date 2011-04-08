%%%-------------------------------------------------------------------
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @copyright (C) 2011, Damian T. Dobroczy\\'nski
%%% @doc Mixins Parse Transform.
%%%
%%% @since 2011-04-08
%%% @end
%%%-------------------------------------------------------------------

-module (mixins_pt).
-export ([parse_transform/2]).

-include_lib ("eunit/include/eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc Standard `parse_transform' function which compiles into the
%%      Erlang forms all uncommon exports from mixins given in
%%      `-mixin([atom()])' attribute.
%% @end
%%--------------------------------------------------------------------

parse_transform (Forms, _Options) ->
  % search for "mixins" attribute
  case find_mixins_attr(Forms) of
    [] ->
      Forms;
    Mixins ->
      insert_exports(insert_mixins_features(Forms, Mixins))
  end.

%%%===================================================================
%%% Internal functions
%%%===================================================================

find_mixins_attr (Forms) ->
  case lists:keysearch(mixins, 3, Forms) of
    {value, {attribute, _, mixins, Mixins}} ->
      Mixins;
    false -> []
  end.

insert_mixins_features (Forms, Mixins) ->
  {Forms1, LastLine} = goto_last_line(Forms),
  {Exports, Forms2}  = generate_mixins_features(Forms1, Mixins, LastLine),
  {Exports, Forms1 ++ Forms2 ++ [{eof, LastLine + length(Mixins)}]}.

insert_exports ({Exports, Forms}) ->
  {N, ThisExports} = get_exports(Forms),
  case ThisExports of
    [] -> 
      % the target module does not exports anything by itself,
      % so insert mixed-in exports just after the `-module' attribute
      {value,
       {attribute, N1, module, _}} = lists:keysearch(module, 3, Forms),
      {Before, After}              = lists:partition(fun 
                                                       ({attribute, _, module, _}) -> true;
                                                       (_) -> false
                                                     end, Forms),
      Before ++ [{attribute, N1, export, Exports}] ++ After;
    ThisExports ->
      lists:keyreplace(export, 3, Forms,
                       {attribute, N, export, Exports})
  end.
  
goto_last_line (Forms) ->
  [{eof, N} | Head] = lists:reverse(Forms),
  {lists:reverse(Head), N}.

generate_mixins_features (Forms, Mixins, StartLine) ->
  {_, Exports}  = get_exports(Forms),
  ModParams     = get_module_params(Forms),
  Mixins1       = concatenate_modules_with_params(Mixins, ModParams),
  {Lst, Exp, _} = lists:foldl(fun (Mixin, Acc) ->
                                  insert_one_mixin_feature(Mixin, Acc)
                              end, {[], Exports, StartLine}, Mixins1),
  {Exp, lists:reverse(lists:flatten(Lst))}.

get_exports (Forms) ->
  case lists:keysearch(export, 3, Forms) of
    {value, {attribute, N, export, Exports}} ->
      {N, Exports};
    false ->
      {-1, []}
  end.

get_module_params (Forms) ->
  case lists:keysearch(module, 3, Forms) of
    {value, {attribute, _, module, {_, Params}}} ->
      Params;
    _ ->
      []
  end.

concatenate_modules_with_params (Mixins, []) ->
  [{Mixin, undefined} || Mixin <- Mixins];
concatenate_modules_with_params (Mixins, MyParams) ->
  {Xs, _} = lists:foldl(fun (Mixin, {Acc, Params = [P|Ps]}) ->
                            case is_abstract(Mixin) of
                              true  -> {[{Mixin, P}|Acc], Ps};
                              false -> {[{Mixin, undefined} | Acc], Params}
                            end
                        end, {[], MyParams}, Mixins),
  lists:reverse(Xs).

is_abstract (Mod) ->
  case proplists:get_value(abstract, Mod:module_info(attributes), [false]) of
    [true] ->
      true;
    [false] ->
      false
  end.

insert_one_mixin_feature ({Mixin, ModParam}, {Acc, Exp, N}) ->
  ArityFix     = case ModParam of
                   undefined -> 0;
                   ModParam  -> -1
                 end,
  MixinExports = sets:from_list(lists:filter(fun
                                               ({module_info, _}) -> false;
                                               ({instance, _}) -> false;
                                               (_) -> true
                                             end, Mixin:module_info(exports))),
  ToDefine     = [{F, A+ArityFix} || {F, A} <- sets:to_list(sets:subtract(MixinExports, sets:from_list(Exp)))],
  RealMixin    = case ModParam of
                   undefined -> Mixin;
                   ModParam  -> ModParam
                 end,
  Forms        = lists:foldl(fun ({Fun, Arity}, Acc0) ->
                                 Source          = generate_mixin_source(RealMixin, Fun, Arity),
                                 {ok, Tokens, _} = erl_scan:string(Source, N),
                                 {ok, Forms}     = erl_parse:parse_form(Tokens),
                                 Inlining        = {attribute, N+1, compile, [{inline, [{Fun, Arity}]}]},
                                 [Forms, Inlining | Acc0]
                             end, [], ToDefine),
  {[Forms | Acc], ToDefine ++ Exp, N+2}.

generate_mixin_source (Mod, Fun, Arity) ->
  Head = io_lib:format("~s(", [Fun]),
  Args = string:join([io_lib:format("A~p", [I]) || I <- lists:seq(1, Arity)], ","),
  Tail = io_lib:format(") -> ~s:~s(~s).", [Mod, Fun, Args]),
  lists:flatten([Head, Args, Tail]).
  
