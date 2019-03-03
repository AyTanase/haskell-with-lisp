cl-eval() {
    ros run -e "(load\"$(dirname $0)/../haskell-with-lisp.asd\")" -e "(require\"haskell-with-lisp\")" -e $1 -q
}

if test $1 = "all"; then
    files=$(find . -name *.hl | while read line; do
                echo -n "\"$line\""
            done)
    cl-eval "(hs:compile-all$files)"
else
    cl-eval "(hs:compile\"$1\")"
fi
