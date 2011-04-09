-module (tp2, [MIXIN, T]).
-compile ([{parse_transform, mixins_pt}]).
-mixins ([p, b]).
-export ([new/1, f/1]).

new (T0) ->
  ?MODULE:instance(p:new(T0+10), T0).

f (A) ->
  A+T.
