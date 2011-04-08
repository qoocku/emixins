-module (tp3, [MIXIN, T]).
-compile ([{parse_transform, mixins_pt}]).
-mixins ([b, p, b2]).
-export ([new/1, f/1, g/3]).

new (T0) ->
  ?MODULE:instance(p:new(T0+10), T0).

f (A) ->
  A+T.

g (A, B, C) ->
  A+B+C+THIS:x().
