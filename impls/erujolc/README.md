# Erujolc

There was already a Clojure implementation of Mal, so for lack of a clever alternative, I named my Mal implementation "erujolc".

1. This is my first LISP implementation, and I deliberately chose to use boxed types (MalDatum) as a forcing function for learning and comprehension.
2. I used Clojure protocols for printing and the environment implementation, but it would be interesting to think about how to use protocols more fully. How far could I go?
3. The freshly completed erujolc Mal is needlessly cumbersome.
4. The freshly completed erujolc Mal is outdated since I most recently rebased to upstream in 2022.

## Tests

Use the self-hosted run script:

```
$ ls -l impls/erujolc/run
lrwxrwxrwx. 1 moquist moquist 15 Apr 28 17:41 impls/erujolc/run -> run-self-hosted
```

Run all the tests:

```
$ for x in 0 1 2 3 4 6 7 8 9 A; do echo step$x: $(make test^erujolc^step$x |& grep 'failing tests'); done
step0: 0: soft failing tests 0: failing tests
step1: 0: soft failing tests 0: failing tests
step2: 0: soft failing tests 0: failing tests
step3: 0: soft failing tests 0: failing tests
step4: 0: soft failing tests 0: failing tests


step6: 0: soft failing tests 0: failing tests
step7: 0: soft failing tests 0: failing tests
step8: 0: soft failing tests 0: failing tests
step9: 0: soft failing tests 0: failing tests
stepA: 77: soft failing tests 0: failing tests
```


## Notes for self
Bugs found while fixing self-hosting

needed to move several items (so far: atom, deref, reset!, cons, concat, read-string, slurp, and vec) out of EVAL as special forms and turn them into core fns

(1 2
-- unbalanced parens crashed with an exception

 ;; whole line comment (not an exception)
-- crashed with an exception because my read-string returned ::peeked-into-the-abyss
  -- test both the whole-line comment beginning with a space,
  -- AND that read-string returns nil on a comment line
