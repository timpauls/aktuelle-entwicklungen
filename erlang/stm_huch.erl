-module(stm).

-export([newTVar/1,readTVar/1,writeTVar/2,atomically/1,retry/0]).
-import(gb_trees,[empty/0,enter/3,to_list/1,lookup/2]).
-import(lists,[map/2]).

tvar(V,Susps) ->
  receive
    {getVal,P} -> P!{val,V},
                  tvar(V,Susps);
    {lock,P}   -> P!locked,
                  tvarLocked(V,Susps);
    {unsusp,P} -> tvar(V,lists:delete(P,Susps))
  end.

tvarLocked(V,Susps) ->
  receive
    {setVal,V1} -> map(fun(T) -> T!awake end, Susps),
                   tvarLocked(V1,[]);
    {getVal,P}  -> P!{val,V},
                   tvarLocked(V,Susps);
    unlock      -> tvar(V,Susps);
    {susp,P}    -> tvarLocked(V,[P|Susps])
  end.

coreGetVal(T) -> T!{getVal,self()},
                 receive {val,V} -> V end.

coreWrite(T,V) -> T!{setVal,V}.

coreLock(T) -> T!{lock,self()},
           receive locked -> ok end.

coreUnLock(T) -> T!unlock.

newTVar(Val) ->
  spawn(fun() -> tvar(Val,[]) end).

readTVar(T) ->
  {RS,WS} = get(state),
  case lookup(T,WS) of
    none       -> V = coreGetVal(T),
                  case lookup(T,RS) of
                    none       -> put(state,{enter(T,V,RS),WS}),
                                  V;
                    {value,V1} -> case V==V1 of
                                    true  -> V;
                                    false -> throw(rollback)
                                  end
                  end;
    {value,V1} -> V1
  end.

retry() ->
  throw(retry).

writeTVar(T,V) ->
  {RS,WS} = get(state),
  put(state,{RS,enter(T,V,WS)}),
  ok.

atomically(Trans) ->
  put(state,{empty(),empty()}),
  case catch Trans() of
    rollback -> atomically(Trans);
    retry    -> {RS,WS} = get(state),
                RL = to_list(RS), %base:print(RL),
                WL = to_list(WS), %base:print(WL),
                Ts = keymerge(RL,WL),
                lock(Ts),
                case validate(RL) of
                  true  -> Me = self(),
                           Helper = spawn(fun() ->
                              receive awake -> Me ! awake end end),
                           map(fun({T,_}) -> T!{susp,Helper} end,RL),
                           unlock(Ts),
                           receive
                             awake ->
                               base:putStrLn("AWAKE"),
                               map(fun({T,_}) -> T!{unsusp,Helper} end,RL),
                               atomically(Trans)
                           end;
                  false -> unlock(Ts),
                           atomically(Trans)
                end;
    {'EXIT',Err} -> throw({'EXIT',Err});
    V -> {RS,WS} = get(state),
         RL = to_list(RS), %base:print(RL),
         WL = to_list(WS), %base:print(WL),
         Ts = keymerge(RL,WL),
         lock(Ts),
         case validate(RL) of
           true  -> commit(WL),
                    unlock(Ts),
                    V;
           false -> unlock(Ts),
                    atomically(Trans)
         end
  end.

orElse(T1,T2) ->
  {_,WS0} = get(state),
  case catch T1() of
    rollback -> throw(rollback);
    retry    -> {RS1,_} = get(state),
                put(state,{RS1,WS0}),
                T2();
    {'EXIT',Err} -> throw({'EXIT',Err});
    V            -> V
  end.
lock(Ts) -> map(fun(T) -> coreLock(T) end, Ts).

unlock(Ts) -> map(fun(T) -> coreUnLock(T) end, Ts).

validate([]) -> true;
validate([{T,V} | TVs]) ->
  V1 = coreGetVal(T),
  case V==V1 of
    true  -> validate(TVs);
    false -> false
  end.

commit(WL) -> map(fun({T,V}) -> coreWrite(T,V) end,WL).

%%% auxilliary functions
keymerge([],Xs) -> map(fun({X,_}) -> X end,Xs);
keymerge(Xs,[]) -> map(fun({X,_}) -> X end,Xs);
keymerge([{X,_}|Xs],[{X,_}|Ys]) -> [X|keymerge(Xs,Ys)];
keymerge([{X,_}|Xs],[{Y,V2}|Ys])
  when X<Y -> [X|keymerge(Xs,[{Y,V2}|Ys])];
keymerge([{X,V1}|Xs],[{Y,_}|Ys])
  when Y<X -> [Y|keymerge([{X,V1}|Xs],Ys)].
