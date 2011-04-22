%%% Author: Damian T. Dobroczy\\'nski <qoocku@gmail.com>
%%% Since: 2011-04-22
%%% Synopsis: Includes emixins compilation attribute.

-ifndef (EMIXINS_HRL).
-define (EMIXINS_HRL, true).

-compile ({parse_transform, mixins_pt}).

-endif.