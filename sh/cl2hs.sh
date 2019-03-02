basedir=$(dirname $0)
cl-eval() {
    $basedir/runcl.sh $basedir/../lisp/haskell.lisp --eval $1
}

if test $1 = "all"; then
    files=$(find . -name *.hl | while read line; do
                echo -n "\"$line\""
            done)
    cl-eval "(hs:compile-all$files)"
else
    cl-eval "(hs:compile\"$1\")"
fi
