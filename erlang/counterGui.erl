-module(counterGui).
-export([start/2,counterWindow/2,main/0,outputMessages/0]).

% Call this function to create a new counter window.
% The first parameter is the initial value of the counter and the
% second parameter is the pid to which the events are send be sent.
start(V,ClientPid) ->
    spawn(counterGui,counterWindow,[V,ClientPid]).

counterWindow(V,ClientPid) ->
    GS = gs:start(),
    Win = gs:create(window,GS,[{map,true},{width,200},{height,100},
			       {configure,true},{title,"Counter"}]),
    gs:frame(packer,Win,[{packer_x,[{stretch,1},{stretch,1}]},
                           {packer_y,[{stretch,1},{stretch,1},
				      {stretch,1},{stretch,1}]}]),
    Text = gs:label(packer,[{label, {text,V}},{pack_xy,{{1,2},1}}]),
    StartButt = gs:button(packer,[{label, {text,"Start"}},{pack_xy,{1,2}}]),
    StopButt = gs:button(packer,[{label, {text,"Stop"}},{pack_xy,{2,2}}]),
    CopyButt = gs:button(packer,[{label, {text,"Copy"}},{pack_xy,{1,3}}]),
    CloneButt = gs:button(packer,[{label, {text,"Clone"}},{pack_xy,{2,3}}]),
    CloseButt = gs:button(packer,[{label, {text,"Close"}},{pack_xy,{1,4}}]),
    CloseAllButt = gs:button(packer,[{label, {text,"Close all"}},{pack_xy,{2,4}}]),
    gs:config(packer,[{width,200},{height,100}]),
    loop(Text,StartButt,StopButt,CopyButt,CloneButt,ClientPid,CloseButt,CloseAllButt).

loop(Text,StartButt,StopButt,CopyButt,CloneButt,ClientPid,CloseButt,CloseAllButt) ->
    receive
	{setValue, V} ->
	    gs:config(Text,[{label, {text,V}}]),
            loop(Text,StartButt,StopButt,CopyButt,CloneButt,
		 ClientPid,CloseButt,CloseAllButt);
	{newClient,P} ->
	    loop(Text,StartButt,StopButt,CopyButt,CloneButt,P,
		 CloseButt,CloseAllButt);
        {gs, StartButt, click, _, _} ->
	    ClientPid!start,
            loop(Text,StartButt,StopButt,CopyButt,CloneButt,
		 ClientPid,CloseButt,CloseAllButt);
        {gs, StopButt, click, _, _} ->
	    ClientPid!stop,
            loop(Text,StartButt,StopButt,CopyButt,CloneButt,
		 ClientPid,CloseButt,CloseAllButt);
        {gs, CopyButt, click, _, _} ->
	    ClientPid!copy,
            loop(Text,StartButt,StopButt,CopyButt,CloneButt,
		 ClientPid,CloseButt,CloseAllButt);
        {gs, CloneButt, click, _, _} ->
	    ClientPid!clone,
            loop(Text,StartButt,StopButt,CopyButt,CloneButt,
		 ClientPid,CloseButt,CloseAllButt);
        {gs, CloseAllButt, click, _, _} ->
	    ClientPid!closeAll,
	    gs:stop();
        {gs, CloseButt, click, _, _} ->
	    ClientPid!{close,self()},
	    exit(-1);
	{gs, _, destroy, _, _} ->
	    ClientPid!{close,self()},
	    exit(-1);
	_ -> loop(Text,StartButt,StopButt,CopyButt,CloneButt,
		  ClientPid,CloseButt,CloseAllButt)
    end.


% Just for testing

main() ->
    P=spawn(counterGui,outputMessages,[]),
    spawn(counterGui,start,[3,P]).

outputMessages() ->
    receive
	closeAll ->
	    io:format("Message: ~w~n",[closeAll]);
	M ->
	    io:format("Message: ~w~n",[M]),
	    outputMessages()	    
    end.
