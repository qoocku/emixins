Introduction
============

Current version of Erlan/OTP has had a rather experimental feature
which resembles a "mixin" concept of several OO languages. The feature
is enabled by inserting `-extends(a_mixin)` attribute into a module
code. This feature is during run-time -- standard `error_handler`
module catches all cases of undefined function call and tries to call
the lacking function using the module mentioned in the `extends`
attribute. It's rather, like err..., pretty cool BUT one may define
only ONE mixin.

The `emixins` application make many mixins possible BUT the drawback
is the fact that the mixins "mixing" is done during module compilation
using `parse_transform` mechanism. This means that:

* all of the mixed-in modules SHOULD be compiled before;
* reloading of a mixed-in module does not affect the target module
  exported functions list -- only the function seen in a mixin during
  compilation will be available; one have to recompile the target
  module to obtain new functions (if such has been added to a mixin)
  mixed-in.
  
Example of usage
================

Let's say we have a "base" module `b.erl`:

    -module (b).
    -export ([f/0]).
  
    f () -> 1.
  
and another "base" module `b2.erl`:

    -module (b2).
    -export ([f2/0]).
  
    f2 () -> 2.
  
The target module is called `t.erl`:

    -module (t).
    -compile ([{parse_transform, mixins_pt}]).
    -mixins ([b]).
  
Now, the `-mixins(list())` attribute defines a list of mixed-in
modules that will participate in function calls of the `t` module.
After compilation of the modules you should be able to call:

    t:f().
  
to obtain great value of `1`. If you type:

    t:module_info(exports).
  
you should obtain something like this:

    [{f, 0}, ...]
  
Mixing more than one module
---------------------------

Now, change the `t` code like this:

    -mixins ([b, b2]).
  
If you look at the `t:module_info(exports)` result you can see this:

    [{f, 0}, {f2, 0}, ...]
  
which mean that you may safely call `f2/0` function with the `t` module.
