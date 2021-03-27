```bash
# run
hpack; cabal run
```

Example (look example.erl):

```erlang
not(true) -> false;
not(false) -> true.

eq(X, X) -> true;
eq(_, _) -> false.

dedup([H,H|T]) -> dedup([H|T]);
dedup([H|T]) -> [H|dedup(T)];
dedup([]) -> [].

length([_|T]) -> plus(1, length(T));
length([]) -> 0.

'without dups'(L) -> eq(L, dedup(L)).

main() ->
  A = not(true),
  print("A = ", A),
  print("not(A) = ", not(A)),
  print("A == not(A) = ", eq(A, not(A))),
  print("A == not(not(A)) = ", eq(A, not(not(A)))),

  print(),
  B = [1, 1, true, "hello" | ["hello"]],
  print("B = ", B),
  print("length(B) = ", length(B)),
  print("dedup(B) = ", dedup(B)),
  print("length(dedup(B)) = ", length(dedup(B))),
  print("is B without dups: ", 'without dups'(B)),
  print("is [1, 2, 3] without dups: ", 'without dups'([1, 2, 3])).

% Output:
% A = false
% not(A) = true
% A == not(A) = false
% A == not(not(A)) = true

% B = [1,1,true,hello,hello]
% length(B) = 5
% dedup(B) = [1,true,hello]
% length(dedup(B)) = 3
% is B without dups: false
% is [1, 2, 3] without dups: true
```
