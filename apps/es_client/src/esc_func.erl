-module (esc_func).

-export ([md5/1
    , esc_split/3
    , esc_zip/2
    , esc_read_line/3
    , timestamp_format/1
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

esc_split(Str, Separator, Where) when is_atom(Where) ->
    CheckSeparator = string:find(Separator, "["),
    if
        CheckSeparator==nomatch ->
            string:split(Str, Separator, Where);
        true ->
            re:split(Str, Separator, [{return, list}])
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


timestamp_format(Bin) when is_binary(Bin) ->
    timestamp_format(binary_to_list(Bin));
timestamp_format(Timestamp) ->
    List = [Head|_Tail] = string:tokens(Timestamp, "-+: /"),
    if
        length(Head)==2 andalso length(List)==7  ->
            [Day, Month, Year, Hour, Minute, Second, TZD] = List,
            Datetime = {
                {list_to_integer(Year), list_to_month(Month), list_to_integer(Day)},
                {list_to_integer(Hour), list_to_integer(Minute), list_to_integer(Second)}
            },
            format_datetime(Datetime, TZD);
        length(Head)==4 andalso length(List)==6  ->
            List2 = [list_to_integer(Num) || Num <- List ],
            [Year, Month, Day, Hour, Minute, Second] = List2,
            Datetime = {
                {Year, Month, Day},
                {Hour, Minute, Second}
            },
            format_datetime(Datetime, "");
        true ->
            Timestamp
    end.

%%====================================================================
%% Internal functions
%%====================================================================

format_datetime(Datetime, TZD) ->
    Datetime2 = case TZD of
        "0800" ->
            iso8601:add_time(Datetime, -8, 0, 0);
        "08" ->
            iso8601:add_time(Datetime, -8, 0, 0);
        _ ->
            calendar:local_time_to_universal_time(Datetime)
    end,
    binary_to_list(iso8601:format(Datetime2)).

list_to_month("Jan") -> 1;
list_to_month("Feb") -> 2;
list_to_month("Mar") -> 3;
list_to_month("Apr") -> 4;
list_to_month("May") -> 5;
list_to_month("Jun") -> 6;
list_to_month("Jul") -> 7;
list_to_month("Aug") -> 8;
list_to_month("Sep") -> 9;
list_to_month("Oct") -> 10;
list_to_month("Nov") -> 11;
list_to_month("Dec") -> 12.

% month_to_list(1)  -> "Jan";
% month_to_list(2)  -> "Feb";
% month_to_list(3)  -> "Mar";
% month_to_list(4)  -> "Apr";
% month_to_list(5)  -> "May";
% month_to_list(6)  -> "Jun";
% month_to_list(7)  -> "Jul";
% month_to_list(8)  -> "Aug";
% month_to_list(9)  -> "Sep";
% month_to_list(10) -> "Oct";
% month_to_list(11) -> "Nov";
% month_to_list(12) -> "Dec".

% day(1) -> "Mon";
% day(2) -> "Tue";
% day(3) -> "Wed";
% day(4) -> "Thu";
% day(5) -> "Fri";
% day(6) -> "Sat";
% day(7) -> "Sun".

