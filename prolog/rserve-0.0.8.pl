:- <- {|r||
       overline = identity
       |}.

:- <- {|r||
       instead_of = function(instead, of)
           return(instead)
       |}.

:- <- {|r||
       omit_left = function(l, op, r)
           return(r)
       |}.

:- <- {|r||
       omit_right = function(l, op, r)
           return(l)
       |}.

:- <- {|r||
       denoting = function(x, y, desc)
           return(y)
       |}.

:- <- {|r||
       frac = `/`
       |}.

:- <- {|r||
       dfrac = `/`
       |}.

:- <- {|r||
       format_tratio = function(x)
           sprintf(ifelse(abs(x) > 10, '%.1f', '%.2f'), x)
       |}.
