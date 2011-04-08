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
  
