-module(counter).
-export([init/1]).

init([Ms]) -> init(Ms);
init([Ms|T]) -> init(Ms), init(T);
init(Ms) -> initStopped(0, Ms).

initStopped(V, Ms) ->
	Counter = spawn(fun() -> preStopped(V, Ms) end),
	Gui = counterGui:start(V, Counter),
	Counter!{guipid, Gui}.

initStarted(V, Ms) ->
	Counter = spawn(fun() -> preStarted(V, Ms) end),
	Gui = counterGui:start(V, Counter),
	Counter!{guipid, Gui}.

preStopped(N, Ms) ->
	receive
		{guipid, Gui} -> stopped(N, Ms, Gui)
	end.

preStarted(N, Ms) ->
	receive
		{guipid, Gui} -> started(N, Ms, Gui)
	end.

started(N, Ms, Gui) -> 
	receive
		stop -> stopped(N, Ms, Gui);
		{close, Pid} -> exit(-1);
		copy -> initStarted(N, Ms), started(N, Ms, Gui);
		_ -> started(N, Ms, Gui)

		after Ms ->	Gui!{setValue, N}, started(N+1, Ms, Gui)
	end.

stopped(N, Ms, Gui) ->
	receive
		start -> started(N, Ms, Gui);
		{close, Pid} -> exit(-1);
		copy -> initStopped(N, Ms), stopped(N, Ms, Gui);
		_ -> stopped(N, Ms, Gui)
	end.