# nanfott

A programming language for an extremely specific child.

Nanfott provides immediate gratification for child who's basically illiterate, with a
path towards Turing completeness when she gets bored.

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
