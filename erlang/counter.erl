-module(counter).
-export([init/1]).

init([Ms]) -> init(Ms);
init([Ms|T]) -> init(Ms), init(T);
init(Ms) -> init(0, Ms, fun(X, Y) -> preStopped(X, Y) end).

init(V, Ms, Func) ->
	Counter = spawn(fun() -> Func(V, Ms) end),
	Gui = counterGui:start(V, Counter),
	Counter!{guipid, Gui}.

initClone(V, Counter) ->
	Gui = counterGui:start(V, Counter),
	Counter!{guipid, Gui}.

preStopped(N, Ms) ->
	receive
		{guipid, Gui} -> stopped(N, Ms, [Gui])
	end.

preStarted(N, Ms) ->
	receive
		{guipid, Gui} -> started(N, Ms, [Gui])
	end.

closeGuis([Gui]) -> Gui!{gs, lol, destroy, lol, lol};
closeGuis([Gui|T]) -> closeGuis([Gui]), closeGuis(T).

messageGuis([Gui], N) -> Gui!{setValue, N};
messageGuis([Gui|T], N) -> messageGuis([Gui], N), messageGuis(T, N).

started(N, Ms, Gui) -> 
	receive
		stop -> stopped(N, Ms, Gui);
		{close, Pid} -> closeGuis(Gui), exit(-1);
		copy -> init(N, Ms, fun(X, Y) -> preStarted(X, Y) end), started(N, Ms, Gui);
		clone -> initClone(N, self()), started(N, Ms, Gui);
		{guipid, Pid} -> started(N, Ms, Gui ++ [Pid]);
		_ -> started(N, Ms, Gui)

		after Ms ->	messageGuis(Gui, N), started(N+1, Ms, Gui)
	end.

stopped(N, Ms, Gui) ->
	receive
		start -> started(N, Ms, Gui);
		{close, Pid} -> closeGuis(Gui), exit(-1);
		copy -> init(N, Ms, fun(X, Y) -> preStopped(X, Y) end), stopped(N, Ms, Gui);
		clone -> initClone(N, self()), stopped(N, Ms, Gui);
		{guipid, Pid} -> stopped(N, Ms, Gui ++ [Pid]);
		_ -> stopped(N, Ms, Gui)
	end.