%% An example of explicit import with undefined exports in mixin module.
-module (t3).
-compile ([{parse_transform, mixins_pt}]).
-mixins ([b, {b2, {exclude, [f2/0]}}]).
