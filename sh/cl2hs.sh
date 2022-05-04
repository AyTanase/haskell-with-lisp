cl-eval() {
    ros run --eval "(load\"$(dirname $0)/../haskell-with-lisp.asd\")" --eval "(require\"haskell-with-lisp\")" --eval $1 --quit
}

if test $1 = "all"; then
    files=$(find . -name "*.hl" | while read line; do
                echo -n "\"$line\""
            done)
    cl-eval "(hs:compile-all$files)"
else
    cl-eval "(hs:compile\"$1\")"
fi
