-module (func).

-include("es_client.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export ([md5/1]).

-export ([get_last_position/1, save_logfile/1]).
-export ([save_msg/2, get_msg/0]).

%%
%% erlang md5 16进制字符串
%%
md5(Str) ->
    Sig = erlang:md5(Str),
    binary_to_list(iolist_to_binary([io_lib:format("~2.16.0b", [S]) || S <- binary_to_list(Sig)])).

get_last_position(NameMd5) ->
    Res = mnesia:transaction(fun() ->
        Q = qlc:q([[E#?LogFileTable.name_md5, E#?LogFileTable.last_position] || E <- mnesia:table(?LogFileTable), E#?LogFileTable.name_md5==NameMd5]),
        qlc:e(Q)
    end),
    case Res of
        {atomic,[]} ->
            0;
        {atomic, [[_Md5, Position]]} ->
            Position
    end.


save_logfile(Data) when is_record(Data, ?LogFileTable) ->
    mnesia:transaction(fun() ->
        mnesia:write(Data)
    end).


get_msg() ->
    ok.

save_msg(Md5, Msg_erlastic_json) ->
    mnesia:transaction(fun() ->
        Data = #?MsgSenderTable{
            msg_md5=Md5,
            msg=Msg_erlastic_json,
            timestamp=calendar:datetime_to_gregorian_seconds(erlang:universaltime())
        },
        mnesia:write(Data)
    end).
