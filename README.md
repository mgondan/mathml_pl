# mathml



Prolog package to convert mathematical expressions to MathML that can be rendered by some 
browsers. Actually, it's mostly Firefox, all the other browsers I tried did not yield 
satisfactory results.



You can find a SWISH demo here, https://swish.swi-prolog.org/p/mathml-0.1.swinb



# Installation and usage



Simple usage example from the SWI-Prolog console:



```
pack_install(mathml).
use_module(library(mathml)).
use_module(library(http/html_write)).
mathml(sin(x), M), html(html(math(M)), Tokens, []), print_html(Tokens).
```

More fancy things are given as an example:

```
mathml:example.
```



At the moment, the library is quite limited, but is is easily extended. So if you need more 
stuff, just write me a little email and I will see if I can implement it.
