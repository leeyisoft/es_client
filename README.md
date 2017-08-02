# es_client

An OTP application
```
wget https://s3.amazonaws.com/rebar3/rebar3 && chmod +x rebar3
```

## Build
-----

    $ rebar3 compile


## Build and Run (In Erlang/OTP 20)

Erlang/OTP 20 [erts-9.0] [source] [64-bit] [smp:4:4] [ds:4:4:10] [async-threads:10] [hipe] [kernel-poll:false] [dtrace]

### 编译启动
```
% 编译
rm -rf _build && rm -rf rebar.lock && ./rebar3 compile

% 启动
erl -boot start_sasl -pa _build/default/lib/*/ebin -config config/sys.config


./rebar3 compile && erl -pa _build/default/lib/*/ebin -config config/sys.config -sname es_client -mnesia dir '"mnesia.db"'

./rebar3 compile && erl -pa _build/default/lib/*/ebin -config config/sys.config -sname es_client3 -mnesia dir '"mnesia.db3"'

需要添加 scan_files 配置项检查的功能，便于检查用户是否真的配置好了

observer:start().
application:start(es_client).

observer:stop(), observer:start().

erlang:is_process_alive(<0.100.0>).

whereis('90a1b746a382dc494c3a960b5d3bdba5').


application:start(mnesia).
rr("/Users/leeyi/workspace/erl/es_client/apps/es_client/include/es_client.hrl").
%% 查询 使用 qlc

mnesia:transaction(fun() ->
    Q = qlc:q([E || E <- mnesia:table(esc_logfile)]),
    qlc:e(Q)
end).

whereis(application_controller)

application:get_env(es_client, scan_flies).

{ok, [I1, I2]}=application:get_env(es_client, scan_flies).

 F1 = maps:get(file,I1).
 F2 = maps:get(file,I2).
 filelib:is_regular(F1).

Dir = filename:dirname(F1).
BaseName = filename:basename(F1).
filelib:fold_files(Dir, "."++BaseName, true, fun(F, AccIn) -> [F | AccIn] end, []).

```

### 引入 mnesia
```
mnesia:system_info(use_dir)

% mnesia检查数据库是否创建
% 确保先创建 schema 之后再启动 mnesia
% 确保已经 mnesia:start().
lists:member(esc_logfile, mnesia:system_info(tables)).

mnesia:create_table(esc_logfile, [{attributes, record_info(fields, esc_logfile)}]).

rr("/Users/leeyi/workspace/erl/es_client/apps/es_client/include/es_client.hrl").

%% 查询 使用 qlc

mnesia:transaction(fun() ->
    Q = qlc:q([E || E <- mnesia:table(esc_logfile)]),
    qlc:e(Q)
end).

%% 部分查询 使用 qlc

mnesia:transaction(fun() ->
    Q = qlc:q([[E#esc_logfile.name_md5, E#esc_logfile.last_position] || E <- mnesia:table(esc_logfile)]),
    qlc:e(Q)
end).

```

### 应用
```
% 创建索引
erlastic_search:create_index(<<"lee_index">>).

% 删除
erlastic_search:delete_index(<<"lee_index">>).
% 批量删除
erlastic_search:delete_index(<<"es_index_name*">>).
erlastic_search:delete_index(<<"*index_name*">>).


% 添加记录（自动生成 _id）
erlastic_search:index_doc(<<"lee_index">>, <<"type">>, [{<<"key1">>, <<"value1">>}]).

% 添加记录（自定义 _id）
erlastic_search:index_doc_with_id(<<"lee_index">>, <<"type">>, <<"id1">>, [{<<"key1">>, <<"value1">>}]).

% 查询
erlastic_search:search(<<"lee_index">>, <<"type">>, <<"key1:value1">>).


f().
File = "/Users/leeyi/workspace/tools/nginx/logs/8082backend-local-access.log",

{ok, S} = file:open(File, [read]),

Str = io:get_line(S, ''),

Sig = erlang:md5(Str),

RowId = iolist_to_binary([io_lib:format("~2.16.0b", [S]) || S <- binary_to_list(Sig)])

erlastic_search:index_doc_with_id(<<"lee_index">>, <<"doc">>, RowId, jsx:decode(list_to_binary(Str))).



f().
File = "/Users/leeyi/workspace/tools/nginx/logs/elk_access.log",

{ok, S} = file:open(File, [read]),

Str = io:get_line(S, ''),

Sig = erlang:md5(Str),

RowId = iolist_to_binary([io_lib:format("~2.16.0b", [S]) || S <- binary_to_list(Sig)])

erlastic_search:index_doc_with_id(<<"lee_index">>, <<"doc">>, RowId, jsx:decode(list_to_binary(Str))).


File = "/Users/leeyi/workspace/tools/nginx/logs/elk_access.log",
Msg = [File, false, "|"],
gen_server:cast({local, "fi"}, Msg).

File = "/Users/leeyi/workspace/tools/nginx/logs/elk_access.log",
{ok,Fd}=file:open(File,read),
file:position(Fd, {bof, 158}),
file:read_line(Fd).
file:read_line(Fd).


Position = file:position(Fd, {cur, 0}).

file_scaner:stop('90a1b746a382dc494c3a960b5d3bdba5').
observer:stop(), observer:start().

application:start(es_client).

esc_db:get_last_position("90a1b746a382dc494c3a960b5d3bdba5").

f(),
File = "/Users/leeyi/workspace/tools/nginx/logs/8085admin-local-error_test.log",
{ok,Fd} = file:open(File,read),
file:pread(Fd, 133, 800).

f(),
File = "/Users/leeyi/workspace/tools/nginx/logs/8085admin-local-error_test.log",
filelib:file_size(File).

application:start(es_client).

observer:start().

observer:stop(), observer:start().

StartPoistion = 133,
f(),
List = [[{0,19}],[{407,19}],[{558,19}]].
List2 = [Len || [{Len, _}] <- List],
List3 = lists:reverse(List2),
[_|List4] = List3,
[_|ListB] = List2,
ListA = lists:reverse(List4),
List5 = [ B - A || {B, A} <- lists:zip(ListB, ListA) ],

lists:zip(ListA, List5).

application:start(es_client).



f().
Val = "request: \"GET /img/a4.jpg HTTP/1.1\"",
Separator = ":",
KeyList = ["http_method","http_path", "http_protocol"],

[KeyName|Val2] = string:split(Val, Separator),
Separator2 = " ",
ValList = string:split(string:trim(Val2, both, Separator2), Separator2, all),
List = lists:sublist(ValList, length(KeyList)),
List2 = lists:zip(KeyList, List)


f().
Key = {name, "createtime", datetime}.
Val = "[ 192.168.2.178] ".

Re = "\\d{1,3}.\\d{1,3}.\\d{1,3}.\\d{1,3}",
{ok,MP} = re:compile(Re),
{match,[[Ip]]} = re:run(Val, MP, [{capture,all, list},global]),


f().
Separator = ":",
Val = " upstream: \"fastcgi://127.0.0.1:9000\"".
[KeyName|ValLi] = string:split(Val, Separator),
Val2 = lists:concat([lists:concat([Str, Separator]) || Str <- ValLi]),
string:trim(Val2, both, "\" :").

f().
Separator2 = " / ",
KeyList = ["http_method", "http_protocol"],
Key = {split, ":", {split, Separator2, KeyList}},
Val = " request: \"GET / HTTP/1.1 / abc \"",
[KeyName|Val2] = string:split(Val, ":"),
ValList = string:split(Val2, " / ", all),
List2 = lists:zip(KeyList, lists:sublist(ValList, length(KeyList))),
[{a, b} | List2].

```

### 调试
```
application:start(observer).
observer:start().
```

### 测试
```
./rebar3 eunit
```
