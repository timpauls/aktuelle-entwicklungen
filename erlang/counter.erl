-module(counter).
-export([init/1]).

init([Ms]) -> init(Ms);
init([Ms|T]) -> init(Ms), init(T);
init(Ms) -> init(0, Ms, fun(X, Y) -> preStopped(X, Y) end).

init(V, Ms, Func) ->
	Counter = spawn(fun() -> Func(V, Ms) end),
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
		copy -> init(N, Ms, fun(X, Y) -> preStarted(X, Y) end), started(N, Ms, Gui);
		_ -> started(N, Ms, Gui)

		after Ms ->	Gui!{setValue, N}, started(N+1, Ms, Gui)
	end.

stopped(N, Ms, Gui) ->
	receive
		start -> started(N, Ms, Gui);
		{close, Pid} -> exit(-1);
		copy -> init(N, Ms, fun(X, Y) -> preStopped(X, Y) end), stopped(N, Ms, Gui);
		_ -> stopped(N, Ms, Gui)
	end.