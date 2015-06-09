-module(primes).
-export([loop/0]).

loop() -> loop2(init()).

loop2(Gen) ->
	receive
		after 500 ->
			base:putStrLn(base:show(give(Gen))),
			loop2(Gen)
	end.

init() -> spawn(fun() -> collect(initAllNumbers(2)) end).
initAllNumbers(N) -> spawn(fun() -> allNumbers(N) end).
initSieve(N, Gen) -> spawn(fun() -> sieve(Gen, N) end).

collect(Gen) ->
	receive
		{give, Who} ->
			N = give(Gen),
			Who!N,
			collect(initSieve(N, Gen))
	end.

allNumbers(N) ->
	receive
		{give, Who} -> Who!N
	end,
	allNumbers(N+1).

give(Np) ->
	Np!{give, self()},
	receive
		N -> N
	end.

sieve(Gen, N) ->
	receive
		{collect, Who} -> Who!N, sieve(Gen, N);
		{give, Who} ->
			Who!sieveNext(N, give(Gen), Gen),
			sieve(Gen, N)
	end.

sieveNext(N, Actual, Gen) when Actual rem N == 0 -> sieveNext(N, give(Gen), Gen);  
sieveNext(_, Actual, _) -> Actual.
