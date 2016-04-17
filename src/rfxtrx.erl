-module(rfxtrx).
-author(skvamme).
-export([start/0,init/0,loop/2]).
-define(RESET,<<13,0,0,0,0,0,0,0,0,0,0,0,0,0>>).
-define(STATUS,<<13,0,0,1,2,0,0,0,0,0,0,0,0,0>>).
-define(BULBON,<<11,17,0,0,9241614:32,1,1,15,128>>).
-define(BULBOFF,<<11,17,0,0,9241614:32,1,0,0,128>>).
-define(X10BULBON,<<7,16,0,0,65,1,1,0>>).
-define(X10BULBOFF,<<7,16,0,0,65,1,5,0>>).
-define(RFXTRX_IP,"192.168.0.20").
-define(RFXTRX_PORT,10001).
%-define(BULBOFF,<<11,17,0,0,9949898:32,6,0,0,128>>).
%-compile(export_all).

start() -> Pid = spawn_link(?MODULE,init,[]), {ok,Pid}.

init() ->
        io:format("New process: ~p~n", [?MODULE]),
        % Uncomment if you want to upload sensor data to a websocket server
        % You have to have Gun installed as a dependancy, use erlang.mk
        %{ok, ConnPid} = gun:open("192.168.0.23", 8000),
        %{ok, _Protocol} = gun:await_up(ConnPid),
        %gun:ws_upgrade(ConnPid, "/event"),
        inets:start(),
        {ok,Socket} = gen_tcp:connect(?RFXTRX_IP, ?RFXTRX_PORT, [binary,{packet, 0}]),
        timer:sleep(500),
        ok = gen_tcp:send(Socket,?RESET),
        timer:sleep(500),
        ok = gen_tcp:send(Socket,?STATUS),
        timer:send_interval(60*1000, {timer}),
        loop(Socket,<<>>).

loop(Socket,Buffer) ->
        receive
                % Messages from our own timer
                {timer} -> case calendar:local_time() of
                        {_, {Hour, _Minute, _Second}} when Hour > 18 -> % Turn on the lights at 7 pm
                                ok = gen_tcp:send(Socket,?X10BULBON),
                                timer:sleep(500),
                                ok = gen_tcp:send(Socket,?BULBON);
                        _ -> ok = gen_tcp:send(Socket,?X10BULBOFF), % Turn off the lights at midnight
                                timer:sleep(500),
                                ok = gen_tcp:send(Socket,?BULBOFF)
                        end,
                        ?MODULE:loop(Socket,Buffer);
                % Data from rfxtrx
                {tcp,_Port,Data} -> Buffer1 = parse_buffer(<<Buffer/binary,Data/binary>>),
                        ?MODULE:loop(Socket,Buffer1);
% Uncomment if you are using Gun to upload sensor data to a websocket server
%               {gun_ws,_,Data} -> io:format("Got from gun ~p~n",[Data]),
%                       ?MODULE:loop(Socket,Buffer);
%               {gun_ws_upgrade,_ConnPid,ok,_} -> io:format("Websocket is up~n",[]),
%                       ?MODULE:loop(Socket,Buffer);
%               {gun_error,_,Data} -> io:format("Got from gun_error ~p~n",[Data]), exit(error);
%               {gun_down,_,ws,closed,[],[]} -> io:format("Got gun_down ~n",[]), exit(error);
                Any -> io:format("~p got unknown msg: ~p~n",[?MODULE, Any]),
                        ?MODULE:loop(Socket,Buffer)
 %               after 60*1000 -> exit(timeout) % Uncomment if you want to restart this module if silent for too long
        end.
%************************************************************************************************************
% parse_buffer(Buffer)
%************************************************************************************************************
% This clause discards 0-length messages
parse_buffer(<<0,Rest/binary>>) -> parse_buffer(Rest); 
% This clause is true for status messages
parse_buffer(<<13,1,_,_,_,_Connected_device,Firmwareversion,_Decodeflag1,_Decodeflag2,_Decodeflag3,_,_,_,_,Rest/binary>>) -> 
        io:format("Message 1 status, Firmware=~p~n",[Firmwareversion]), Rest;
% This clause is true for ack messages from frxtrc
parse_buffer(<<4,2,1,_X1,_X2,Rest/binary>>) -> %io:format("Message 2 ack~n",[]),
        Rest;
% Uncomment this clause if you have lighting1 devices such as X10
parse_buffer(<<7,16,Type,Seqnr,Housecode,Unitcode,Command,0,Rest/binary>>) -> 
        io:format("Message 16 lighting1 and x10, Type=~p Seqnr=~p House=~p Unit=~p Command=~p~n",
                [Type,Seqnr,Housecode,Unitcode,Command]), Rest;
% Uncomment this clause if you have lighting2 devices
parse_buffer(<<11,17,Type,Seqnr,Housecode:32,Unitcode,Command,Level,Rssi,Rest/binary>>) ->
        io:format("Message 17 lighting2, Type=~p Seqnr=~p House=~p Unit=~p Command=~p Level=~p Rssi=~p~n",
                [Type,Seqnr,Housecode,Unitcode,Command,Level,Rssi]), Rest;
% Uncomment this clause if you have lighting5 devices
parse_buffer(<<11,20,Type,Seqnr,Housecode:32,Unitcode,Command,Level,Rssi,Rest/binary>>) -> 
        io:format("Message 20 lighting5, Type=~p Seqnr=~p House=~p Unit=~p Command=~p Level=~p Rssi=~p~n",
                [Type,Seqnr,Housecode,Unitcode,Command,Level,Rssi]), Rest;
% Uncomment this clause if you have security devices
parse_buffer(<<8,32,1,_Seqnr,_Id1,_Id2,_Id3,_Status,_Battery:4,_Rssi:4,Rest/binary>>) -> 
        io:format("Message 32 sequrity~n",[]), Rest;
% Uncomment this clause if you have temperature devices
parse_buffer(<<8,80,Type,Seqnr,Id:16,Temp:16,Battery:4,Rssi:4,Rest/binary>>) -> 
        io:format("Message 80 temperature, Type=~p, Seqnr=~p Id=~p Temp=~p Batteri=~p Rssi=~p~n",
                [Type,Seqnr,Id,Temp/10,Battery,Rssi]), Rest;
% Uncomment this clause if you have temperature and humidity devices
%parse_buffer(<<10,82,Type,Seqnr,Id:16,Temp:16,Humid,_,_,Rest/binary>>) -> 
%        io:format("Message 82 temperatyre&humidity, Type=~p, Seqnr=~p Id=~p Temp=~p Humid=~p~n",
%                [Type,Seqnr,Id,Temp/10,Humid]), Rest;
% Uncomment this clause if you have rain devices
%parse_buffer(<<11,85,Type,Seqnr,Id:16,Rainrate:16,Raintotal:24,Battery:4,Rssi:4,Rest/binary>>) -> 
%        io:format("Message 85 rain, Type=~p, Seqnr=~p Id=~p Rainrate=~p Raintotal=~p Battery=~p Rssi=~p~n",
%                [Type,Seqnr,Id,Rainrate,Raintotal,Battery,Rssi]), Rest;
% Uncomment this clause if you have wind devices
%parse_buffer(<<16,86,Type,Seqnr,Id:16,Direction:16,Average:16,Gust:16,_,_,_,_,Battery:4,Rssi:4,Rest/binary>>) -> 
%        io:format("Message 86 wind, Type=~p, Seqnr=~p Id=~p Direction=~p Average=~p Gust=~p Battery=~p Rssi=~p~n",
%                [Type,Seqnr,Id,Direction,Average,Gust,Battery,Rssi]), Rest;
% Uncomment this clause if you have energy devices
%parse_buffer(<<17,90,_,_,_,_,_,_,_,_,_,Rest/binary>>) -> io:format("Message 90 energy~n",[]), Rest;

% This clause is true for all unknown/irrelevant messages. They are discarded.
parse_buffer(<<Size,_U:Size/binary-unit:8,Rest/binary>>) -> 
        io:format("Message Unknown is ~p and Rest is ~p~n",[<<Size,_U/binary>>,Rest]), Rest;

% This clause is allways true. If a message is not completely downloaded yet, we just return the current buffer.
parse_buffer(Buffer) -> %io:format("Buffer is ~p~n",[Buffer]), 
        Buffer.





