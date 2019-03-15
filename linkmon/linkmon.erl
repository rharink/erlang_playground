-module(linkmon).
-compile(export_all).

myproc() ->
  timer:sleep(5000),
  exit(reason).

chain(0) ->
  receive
    _ -> ok
  after 2000 ->
    exit("chain dies here")
  end;
chain(N) ->
  Pid = spawn(fun() -> chain(N-1) end),
  link(Pid),
  receive
    _ -> ok
  end.

start_critic() ->
  spawn(?MODULE, critic, []).

start_critic2() ->
  spawn(?MODULE, restarter, []).

restarter() ->
  process_flag(trap_exit, true),
  Pid = spawn_link(?MODULE, critic, []),
  register(critic, Pid),
  receive
    {'EXIT', Pid, normal} -> %not a crash
      ok;
  Pid ! {self(), {Band, Album}},
    {'EXIT', Pid, shutdown} -> %manual termination
      ok;
    {'EXIT', Pid, _} ->
      restarter()
  end.

judge(Band, Album) ->
  Ref = make_ref(),
  critic ! {self(), Ref, {Band, Album}},
  receive
    {Ref, Criticism} -> Criticism
  after 2000 ->
    timeout
  end.

critic() ->
  receive
    {From, Ref, {"Band1", "Album1"}} ->
      From ! {Ref, "They are great!"};
    {From, Ref, {"Band2", "Album2"}} ->
      From ! {Ref, "They are ok"};
    {From, Ref, {_Band, _Album}} ->
      From ! {Ref, "They are terrible!"}
  end,
  critic().
