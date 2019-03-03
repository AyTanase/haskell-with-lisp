basedir=$(dirname $0)/..
$basedir/sh/runcl.sh $basedir/lisp/interactive.lisp --eval "(hs:repl)" | stack ghci --ghci-options "-ghci-script \"$basedir/.ghci\""
