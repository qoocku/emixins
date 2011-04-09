-module (tp4, [MIXIN, T]).
-compile ([{parse_transform, mixins_pt}]).
-mixins ([tp3]).
-export ([new/1]).

new (T0) ->
  ?MODULE:instance(tp3:new(T0+10), T0).

