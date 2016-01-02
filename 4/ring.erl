-module(ring).
-compile(export_all).

start(N, M, Message) ->
  Pids = start_procs(N),
  Ring = Pids ++ [hd(Pids)],
  send_message(Ring, Ring, M, Message),
  stop_ring(Ring).

start_procs(0) ->
  [];
start_procs(N) ->
  [spawn(ring, start_proc, [])|start_procs(N - 1)].

start_proc() ->
  receive
    stop ->
      io:format("~w: stopping...~n", [self()]),
      true;
    Msg ->
      io:format("~w: ~ts~n", [self(), Msg]),
      start_proc()
  end.

send_message(Ring, [Pid|[]], M, Message) ->
  ping(Pid, Message),
  send_message(Ring, tl(Ring), M - 1, Message);

send_message(_, [Pid|_], 1, Message) ->
  ping(Pid, Message);

send_message(Ring, [Pid|Pids], M, Message) ->
  ping(Pid, Message),
  send_message(Ring, Pids, M - 1, Message).

ping(Pid, Message) ->
  Pid ! Message.

stop_ring([]) ->
  ok;
stop_ring([Pid|Pids]) ->
  ping(Pid, stop),
  stop_ring(Pids).
