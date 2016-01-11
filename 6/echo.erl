-module(echo).
-export([start/0, loop/0, print/1, stop/0, start_check/0]).

start() ->
  case start_check() of
    {error, _} ->
      register(echo, spawn_link(echo, loop, [])),
      ok;
    started ->
      {error, "The process is already started"}
  end.

loop() ->
  receive
    {print, Msg} ->
      io:format("~ts~n", [Msg]),
      loop();
    terminate ->
      exit(terminated)
  end.

print(Msg) ->
  case start_check() of
    {error, ErrorMsg} ->
      {error, ErrorMsg};
    started ->
      echo ! {print, Msg}
  end.

stop() ->
  case start_check("The process is already stopped.") of
    {error, Msg} ->
      {error, Msg};
    started ->
      process_flag(trap_exit, true),
      echo ! terminate,
      receive
        {'EXIT', _, Reason} ->
          {stop, Reason}
      end
  end.

start_check(ErrorMsg) ->
  IsStarted = is_pid(whereis(echo)),
  if
    IsStarted =:= true ->
      started;
    true ->
      {error, ErrorMsg}
  end.

start_check() ->
  start_check("The process is stopped.").
