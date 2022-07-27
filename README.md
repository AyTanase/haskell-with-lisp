# Haskell with Lisp
Common Lisp macros for generating Haskell code

## Command Line
```shell
cl2hs FILE
cl2hs all  # compile ./**/*.hl
cl2hsi     # GHCi with Lisp syntax
```
If you do not use roswell, you have to modify `bat/*.bat` or `sh/*.sh`.


## The Default External Formats for Command Line Tools
```lisp
(setf hs:*external-format* :default)
```
