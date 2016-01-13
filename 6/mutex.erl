%% Code from
%%   Erlang Programming
%%   Francecso Cesarini and Simon Thompson
%%   O'Reilly, 2008
%%   http://oreilly.com/catalog/9780596518189/
%%   http://www.erlangprogramming.org/
%%   (c) Francesco Cesarini and Simon Thompson

-module(mutex).
-export([start/0, stop/0]).
-export([wait/0, signal/0]).
-export([init/0]).

start() ->
  Pid = spawn(?MODULE, init, []),
  register(mutex, Pid),
  try link(Pid)
  catch
    exit:Reason -> {'EXIT', Reason}
  end,
  ok.

stop() ->
  mutex ! stop.

wait() ->
  process_flag(trap_exit, true),
  mutex ! {wait, self()},
  receive
    ok ->
      ok;
    {'EXIT', _, _} ->
      free()
  end.

signal() ->
  mutex ! {signal, self()}, ok.

init() ->
  free().

free() ->
  receive
    {wait, Pid} ->
      Pid ! ok,
      busy(Pid);
    stop ->
      terminate()
  end.

busy(Pid) ->
  receive
    {signal, Pid} ->
      free()
  end.

terminate() ->
  receive
    {wait, Pid} ->
      exit(Pid, kill),
      terminate()
  after
    0 -> ok
  end.
