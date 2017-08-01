-module (func).

-include("es_client.hrl").

-include_lib("stdlib/include/qlc.hrl").

-export ([md5/1, get_last_position/1, save_logfile/1, get_logfile/1]).

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

get_logfile(NameMd5) ->
    Res = mnesia:transaction(fun() ->
        Q = qlc:q([E || E <- mnesia:table(?LogFileTable), E#?LogFileTable.name_md5==NameMd5]),
        qlc:e(Q)
    end),
    case Res of
        {atomic,[]} ->
            [];
        {atomic, [Row]} ->
            Row
    end.

save_logfile(Data) when is_record(Data, ?LogFileTable) ->
    mnesia:transaction(fun() ->
        % Acc = #logfile{
        %     name_md5=Md5,
        %     last_position=0,
        %     multiline=Multiline, % Multiline 为 false 的表示单行匹配; 为 list 正则表达式
        %     separator=Separator, % Separator 为 "" 的标示为json格式数据
        %     keys=Keys,
        %     index=Index,
        %     file=File
        % },
        mnesia:write(Data)
    end).