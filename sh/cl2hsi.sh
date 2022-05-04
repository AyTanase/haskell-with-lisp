basedir=$(dirname $0)/..
ros run --load "$basedir/lisp/interactive.lisp" --eval "(hs:repl)" --quit | stack ghci --ghci-options "-ghci-script \"$basedir/.ghci\""
