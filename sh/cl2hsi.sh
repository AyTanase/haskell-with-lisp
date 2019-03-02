basedir=$(dirname $0)
$basedir/runcl.sh $basedir/../lisp/interactive.lisp --eval "(hs:repl)" | stack ghci --ghci-options "-ghci-script $basedir/../bat/.ghci"
