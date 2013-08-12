# nanfott

A programming language for an extremely specific child. 
Deployed [here](http://podsnap.com/nanfott/nanfott.html)

Nanfott provides immediate gratification for child who's basically illiterate, with a
path towards Turing completeness when she gets bored.
Nanfott was inspired by demo applications for [Isla](https://github.com/maryrosecook/isla), but obviously takes
a much different approach to the language. A conscious decision was made to avoid object-orientation and keep the
most basic interface purely imperative. In fact every statement starts with an imperative verb, which is
followed by a number of parameters, the result of which is some change to the image on the web page.  We can think
of this as a sort of crippled lisp, suggesting that we can at some point uncripple it by involving parentheses.


In the short term, we can do things like this:

~~~
make triangle
color it blue
name it tom
turn tom right
move tom up
move tom up 3
~~~

However, magic lurks below the surface once parentheses are discovered:

~~~
(move tom up (* 3 5))
~~~

and even

~~~
(repeatedly 5 (fn (move tom up 1)))
~~~

(No arguments implemented yet!)

Since clojurescript doesn't have eval, this "real clojure" stuff is implemented with a
simple lispish parser.


## License

Copyright © 2013 Peter Fraenkel

Distributed under the Eclipse Public License, the same as Clojure.
