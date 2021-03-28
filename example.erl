print_server(Prefix) ->
  receive
    {print, Message} ->
      print(Prefix, ": ", Message),
      print_server(Prefix);
    {stop, Pid} ->
      Pid ! {stoped, self()}
  end.

wait(N) -> receive after N -> ok end.

% swap between A and B
swap(A, B, A) -> B;
swap(A, B, B) -> A.

balancer(S1, S2, M) -> balancer(S1, S2, M, S1).
balancer(S1, S2, [H|T], S) ->
  S ! {print, H},
  wait(500),
  balancer(S1, S2, T, swap(S1, S2, S));
balancer(_, _, [], _) -> ok.

start() ->
  S1 = spawn(main, print_server, ["server 1"]),
  S2 = spawn(main, print_server, ["server 2"]),
  balancer(S1, S2, [hello, world, kana, other, message, 42]),
  S1 ! {stop, self()}, receive {stoped, S1} -> ok end,
  S2 ! {stop, self()}, receive {stoped, S2} -> ok end,
  print("done").
