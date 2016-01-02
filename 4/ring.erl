-module(ring).
-compile(export_all).

start(N, M, Message) ->
  Pids = start_procs(N),
  send_message(Pids, Pids, M, Message),
  stop_ring(Pids).

start_procs(0) ->
  [];
start_procs(N) ->
  [spawn(ring, start_proc, [])|start_procs(N - 1)].

start_proc() ->
  receive
    {From, stop} ->
      io:format("~w: stopping...~n", [self()]),
      ping(From, {self(), stopped}),
      true;
    {From, Msg} ->
      io:format("~w: ~ts~n", [self(), Msg]),
      ping(From, {self(), message_delivered}),
      start_proc()
  end.

send_message(_, [Pid|[]], 1, Message) ->
  wait_and_then_quit(Pid, Message);
send_message(Ring, [Pid|[]], M, Message) ->
  wait_and_then_send_message(Pid, Ring, Ring, M - 1, Message);
send_message(_, [Pid|_], 1, Message) ->
  wait_and_then_quit(Pid, Message);
send_message(Ring, [Pid|Pids], M, Message) ->
  wait_and_then_send_message(Pid, Ring, Pids, M - 1, Message).

ping(Pid, Message) ->
  Pid ! {self(), Message}.

stop_ring([]) ->
  ok;
stop_ring([Pid|Pids]) ->
  ping(Pid, stop),
  receive
    {_, {Pid, stopped}} ->
      stop_ring(Pids)
  end.

wait_and_then_send_message(Pid, Ring, Rest, M, Message) ->
  ping(Pid, Message),
  receive
    {_, {Pid, message_delivered}} ->
      send_message(Ring, Rest, M, Message)
  end.

wait_and_then_quit(Pid, Message) ->
  ping(Pid, Message),
  receive
    {_, {Pid, message_delivered}} ->
      ok
  end.
