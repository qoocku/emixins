%% An example of explicit import with undefined exports in mixin module.
-module (t3).
-compile ([{parse_transform, mixins_pt}]).
-mixins ([{a, [a/1, unknown/3, a/3, a/4]},
          b, b2]).
