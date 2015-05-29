-module(diningPhils).

-export([start/1]).

start(N) when N>=2 ->
    Sticks = createSticks(N),
    startPhils([lists:last(Sticks)|Sticks],0).

createSticks(0) ->
    [];
createSticks(N) when N>0 ->
    Stick  = spawn(fun() -> stickDown() end),
    Sticks = createSticks(N-1),
    [Stick|Sticks].

startPhils([_],_) ->
    ok;
startPhils([SL,SR|Sticks],N) ->
    spawn(fun() -> phil(SL,SR,N) end),
    startPhils([SR|Sticks],N+1).

stickDown() ->
    receive
	{take,P} -> P!took,
		    stickUp()
    end.

stickUp() ->
    receive
	put -> stickDown()
    end.

take(Stick) ->
    Stick!{take,self()},
    receive
	took -> ok
    end.

put(Stick) ->
    Stick!put.

phil(SL,SR,N) ->
    base:putStrLn(base:show(N)++" is thinking"),
    timer:sleep(10+N),
    take(SL),
    take(SR),
    base:putStrLn(base:show(N)++" is eating"),
    put(SL),
    put(SR),
    phil(SL,SR,N).
