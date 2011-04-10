%%%-------------------------------------------------------------------
%%% @author Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% @copyright (C) 2011, Damian T. Dobroczy\\'nski
%%% @doc Mixins Parse Transform.
%%%
%%% @since 2011-04-08
%%% @end
%%%-------------------------------------------------------------------

-module (mixins_pt).
-author ("Damian T. Dobroczy\\\\'nski <qoocku@gmail.com>").
-export ([parse_transform/2]).
-include ("vsn").

-include_lib ("eunit/include/eunit.hrl").

%%%===================================================================
%%% API
%%%===================================================================

-type fa     () :: {atom(), non_neg_integer()}.  % `Function/Arity' definition.
-type mixin  () :: atom() | {atom(), [fa()]}.    % One mixin specification.
-type mixins () :: [mixin()].                    % The content of the `-mixin' attribute.
-type form   () :: tuple().                      % Single form.
-type forms  () :: [form()].                     % List of forms.

-export_types ([mixin/0, mixins/0]).

%%--------------------------------------------------------------------
%% @type mixins () = [atom() | {atom(), [{atom(), non_neg_integer()}]}]. 
%%       List of mixin specifications.
%% @doc Standard `parse_transform' function which compiles into the
%%      Erlang forms all uncommon exports from mixins given in
%%      `-mixin([mixins()])' attribute.
%% @end
%%--------------------------------------------------------------------

-spec parse_transform (forms(), [atom() | {atom(), atom() | list()}]) -> forms().

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

%% @doc Returns the contents of the `-mixin' attribute.

find_mixins_attr (Forms) ->
  case lists:keysearch(mixins, 3, Forms) of
    {value, {attribute, _, mixins, Mixins}} ->
      Mixins;
    false -> []
  end.

%% @doc Just a high-level step in this parse transform ;)

insert_mixins_features (Forms, Mixins) ->
  {Forms1, LastLine} = goto_last_line(Forms),
  {Exports, Forms2}  = generate_mixins_features(Forms1, Mixins, LastLine),
  {Exports, Forms1 ++ Forms2 ++ [{eof, LastLine + length(Mixins)}]}.

%% @doc Inserts into the forms list exports specs given as a list.

-spec insert_exports ({[fa()], list()}) -> list().

insert_exports ({Exports, Forms}) ->
  {N, ThisExports} = get_exports(Forms),
  case ThisExports of
    [] -> 
      %% the target module does not exports anything by itself,
      %% so insert mixed-in exports just after the `-module' attribute
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
  
%% @doc Strip the forms off the `eof' and returns the stripped forms and the source code
%%      last line number.

goto_last_line (Forms) ->
  [{eof, N} | Head] = lists:reverse(Forms),
  {lists:reverse(Head), N}.

%% @doc No comments. `Forms' is the target module forms, `Mixins' is a list embbedded in the
%%      `-mixin' attribute, `StartLine' is typically an integer equals to the last line number
%%      of the target module original source code.

generate_mixins_features (Forms, Mixins, StartLine) ->
  {_, Exports}  = get_exports(Forms),
  ModParams     = get_module_params(Forms),
  Mixins1       = concatenate_modules_with_params(Mixins, ModParams),
  {Lst, Exp, _} = lists:foldl(fun (Mixin, Acc) ->
                                  insert_one_mixin_feature(Mixin, Acc)
                              end, {[], Exports, StartLine}, Mixins1),
  {Exp, lists:reverse(lists:flatten(Lst))}.

%% @doc Extracts from a module forms all of the explicit exports.

get_exports (Forms) ->
  case lists:keysearch(export, 3, Forms) of
    {value, {attribute, N, export, Exports}} ->
      {N, Exports};
    false ->
      {-1, []}
  end.

%% @doc Returns an abstract module parameters list given its forms.

get_module_params (Forms) ->
  case lists:keysearch(module, 3, Forms) of
    {value, {attribute, _, module, {_, Params}}} ->
      Params;
    _ ->
      []
  end.

%% @doc Generates list of pairs defining relation between a mixin module name
%%      and the target abstract module parameter names. The input parameters
%%      are a list of mixins specs (in form of an atom or a pair `{ModuleName,
%%      [{FunctionName, Arity}]}' and a list of the target module parameters
%%      names. It iterates over the mixins list and chooses a parameter from
%%      the parameter list which is reponsible for holding the mixin module
%%      instance.

-spec concatenate_modules_with_params (mixins(), [atom()]) ->[{atom(), atom()}].

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

-define (UNDEF_ERROR, error("Cannot get module_info from a mixin -"
                            " try to add path to it while compiling")).

%% @doc Tests if a module `Mod' is an abstract one.

-spec is_abstract (mixin()) -> boolean().

is_abstract ({Mod, _}) ->
  is_abstract(Mod);
is_abstract (Mod) ->
  try proplists:get_value(abstract, Mod:module_info(attributes), [false]) of
    [true] ->
      true;
    [false] ->
      false
  catch
    error:undef -> ?UNDEF_ERROR
  end.

%% @doc Insert forms for one mixin into the forms accumulator.
%%      It interprets one mixin spec from `-mixins(List)' attribute list.
%%      The `List' contains atoms for mixed-in modules and/or a pair 
%%      `{Module::atom(), [Fun/Arity]}' which specifies explicit imports
%%      from the mixed-in module `Mod'. In both cases the unnecessary imports
%%      are eliminated (which are compiler generated special functions like
%%      `instance(...)' or `module_info(...)' and all functions already exported
%%      from the target module with the name and arity like in the mixin imports). 

-type acc () :: {ListOfForms::list(),
                 AlreadyDefinedExports::[fa()],
                 LineNumber::pos_integer()}.

-spec insert_one_mixin_feature ({{atom() | {atom(), [fa()]}}, atom()}, acc()) -> acc().

insert_one_mixin_feature ({Mod, ModParam}, Accumulator) when is_atom(Mod) ->
  insert_one_mixin_feature({{Mod, []}, ModParam}, Accumulator);
insert_one_mixin_feature ({_Mixin = {Mod, Imports}, ModParam}, {Acc, Exp, N}) ->

  %% arity fix defines an integer which should be added to the
  %% target module exports specs in case of abstract mixin

  ArityFix     = case ModParam of
                   undefined -> 0;
                   ModParam  -> 1
                 end,

  %% select imports from a mixin

  MixinExports = try
                   Mod:module_info(exports)
                 catch
                   error:undef -> ?UNDEF_ERROR % obviously `module_info/1' is not available
                 end,

  MixinImports = case Imports of
                   [] -> 
                     %% implicit imports
                     MixinExports;
                   Imports ->
                     %% explicit imports without unknown functions
                     case lists:partition(fun (ExpImp) ->
                                              not lists:member(ExpImp, MixinExports)
                                          end, Imports) of
                       {[], _} ->
                         Imports;
                       {Lst, Imports1} ->
                         io:format("*** " ?MODULE_STRING ": Some functions are not"
                                   " exported from '~p' module:~n"
                                   "~s~n", [Mod, lists:flatten([io_lib:format("\t~p/~p~n", [F,A]) 
                                                                || {F,A} <- Lst])]),
                         Imports1
                     end
                 end,

  %% we should figure out what exatctly has to be mixed-in.
  %% we do not want to "inherit" some compiler generated
  %% functions.

  RealImports = sets:from_list(lists:filter(fun
                                              ({module_info, _}) -> false;
                                              ({instance, _})    -> false;
                                              (_)                -> true
                                            end, MixinImports)),

  %% create a list of final exports

  ToDefine     = [{F, A-ArityFix}
                  || {F, A} <- sets:to_list(lists:foldl(fun(Set0, Set) ->
                                                            sets:subtract(Set, Set0)
                                                        end, RealImports,
                                                        [sets:from_list(Exp)]))],

  %% if a abstract module parameter has been linked to the mixed-in module
  %% it should be used in the mixed-in function call. If not - use the mixed-in 
  %% module name.
  
  RealMixin    = case ModParam of
                   undefined -> Mod;
                   ModParam  -> ModParam
                 end,

  %% mixins code generation

  Forms        = lists:foldl(fun ({Fun, Arity}, Acc0) ->
                                 Source          = generate_mixin_source(RealMixin, Fun, Arity),
                                 {ok, Tokens, _} = erl_scan:string(Source, N),
                                 {ok, Forms}     = erl_parse:parse_form(Tokens),
                                 Inlining        = {attribute, N+1, compile, [{inline, [{Fun, Arity}]}]},
                                 [Forms, Inlining | Acc0]
                             end, [], ToDefine),
  {[Forms | Acc], ToDefine ++ Exp, N+2}.

%% @doc Generates mixin function call source code for the target module.
%%      Generaly, if one imports from a mixin a function `Fun/Arity' the
%%      generated code would look like:
%%      <pre>
%%        Fun () -> MixedInModule:Fun().
%%      </pre>
%%      for `Arity = 0' or:
%%      <pre>
%%        Fun (A1, ..., An) -> MixedInModule:Fun(A1, ..., An).
%%      </pre>
%%      for `Arity = An'.

-spec generate_mixin_source (atom(), atom(), non_neg_integer()) -> string().

generate_mixin_source (Mod, Fun, Arity) ->
  Head = io_lib:format("~s(", [Fun]),
  Args = string:join([io_lib:format("A~p", [I]) || I <- lists:seq(1, Arity)], ","),
  Tail = io_lib:format(") -> ~s:~s(~s).", [Mod, Fun, Args]),
  lists:flatten([Head, Args, Tail]).  

  
