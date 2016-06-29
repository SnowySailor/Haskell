#!/bin/bash

pygmentize -f html -o "Geometry.html" -O style=colorful "Geometry.hs"
pygmentize -f html -o "higher-order-functions.html" -O style=colorful "higher-order-functions.hs"
pygmentize -f html -o "input-and-output.html" -O style=colorful "input-and-output.hs"
pygmentize -f html -o "making-our-own-types-and-typeclasses.html" -O style=colorful "making-our-own-types-and-typeclasses.hs"
pygmentize -f html -o "recursion.html" -O style=colorful "recursion.hs"
pygmentize -f html -o "syntax-in-functions.html" -O style=colorful "syntax-in-functions.hs"
pygmentize -f html -o "modules.html" -O style=colorful "modules.hs"
pygmentize -f html -o "types-and-typeclasses.html" -O style=colorful "types-and-typeclasses.hs"