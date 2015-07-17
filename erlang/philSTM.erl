-module(philSTM).
-export([start/0]).
-import(stm,[newTVar/1,readTVar/1,writeTVar/2,atomically/1,retry/0]).

start() -> S1 = atomically(fun() -> newStick() end),
           S2 = atomically(fun() -> newStick() end),
           S3 = atomically(fun() -> newStick() end),
           S4 = atomically(fun() -> newStick() end),
           S5 = atomically(fun() -> newStick() end),
           spawn(fun() -> phil(1,S1,S2) end),
           spawn(fun() -> phil(2,S2,S3) end),
           spawn(fun() -> phil(3,S3,S4) end),
           spawn(fun() -> phil(4,S4,S5) end),
	   receive
           after 1000 -> ok
           end,
           phil(5,S5,S1).

newStick() -> newTVar(true).

take(S) -> B = readTVar(S),
           case B of
             true  -> writeTVar(S,false);
             false -> retry()
           end.

put(S) -> writeTVar(S,true).

phil(N,SL,SR) -> %base:putStrLn("Philosopher "++base:show(N)++" is thinking"),
                 base:putStr(base:show(N)),
                 %atomically(fun() -> take(SL) end),
                 %atomically(fun() -> take(SR) end),
                 atomically(fun() -> take(SL),
                                     take(SR) end),
                 base:putStr(base:show(N)),
                 %base:putStrLn("Philosopher "++base:show(N)++" is eating"),
                 atomically(fun() -> put(SL),
                                     put(SR) end),
                 phil(N,SL,SR).
