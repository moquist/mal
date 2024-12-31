Bugs found while fixing self-hosting

needed to move several items (so far: atom, deref, reset!, cons, concat, read-string, slurp, and vec) out of EVAL as special forms and turn them into core fns

(1 2
-- unbalanced parens crashed with an exception

 ;; whole line comment (not an exception)
-- crashed with an exception
