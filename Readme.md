# Algorithms from Dasgupta book - haskell in heart

a) Fibonacci : matrix way to find the solution.

b) kth smallest number : helpful for median.

c) Depth first search

d) Longest edge path in a DAG

## build with :

```
stack build --file-watch --executable-profiling --library-profiling --ghc-options="-fforce-recomp -fprof-auto -rtsopts"
```

## run with :

```
$ head -n 1 profs/map.txt 
1000 1000
$ stack exec -- hsalgos-exe +RTS -s -hy
profs/map.txt
```
