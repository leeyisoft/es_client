-module (func).

-export ([md5/1

    , esc_split/2
    , esc_split/3
    , esc_zip/2
    , esc_read_line/3
]).

%%
%% erlang md5 16进制字符串
%%
md5(Str) ->
    Sig = erlang:md5(Str),
    binary_to_list(iolist_to_binary([io_lib:format("~2.16.0b", [S]) || S <- binary_to_list(Sig)])).

%% 从打开的文件里面读取一段数据
esc_read_line(Fd, Start, Offset) ->
    {ok, Row} = file:pread(Fd, Start, Offset),
    Row.


esc_split(Str, Separator) ->
    CheckSeparator = string:find(Separator, "["),
    if
        CheckSeparator==nomatch ->
            Vals = string:split(Str, Separator);
        true ->
            Vals = re:split(Str, Separator, [{return, list}])
    end.

esc_split(Str, Separator, Where) when is_atom(Where) ->
    CheckSeparator = string:find(Separator, "["),
    if
        CheckSeparator==nomatch ->
            Vals = string:split(Str, Separator, Where);
        true ->
            Vals = re:split(Str, Separator, [{return, list}])
    end.

esc_zip(Keys, Vals) when is_list(Keys), is_list(Vals), length(Keys)>0, length(Vals)>0 ->
    if
        length(Keys)>length(Vals) ->
            SubList = lists:sublist(Keys, length(Vals)),
            lists:zip(SubList, Vals);
        length(Keys)<length(Vals) ->
            SubList = lists:sublist(Vals, length(Keys)),
            lists:zip(Keys, SubList);
        true ->
            lists:zip(Keys, Vals)
    end.