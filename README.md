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

./rebar3 compile && erl -boot start_sasl -pa _build/default/lib/*/ebin -config config/sys.config

./rebar3 compile && erl -pa _build/default/lib/*/ebin -config config/sys.config -sname es_client2 -mnesia dir '"mnesia.db"'

application:start(es_client).

observer:start().

observer:stop(), observer:start().

erlang:is_process_alive(<0.100.0>).

file_scaner:stop("90a1b746a382dc494c3a960b5d3bdba5")



application:start(mnesia).
rr("/Users/leeyi/workspace/erl/es_client/apps/es_client/include/es_client.hrl").
%% 查询 使用 qlc

mnesia:transaction(fun() ->
    Q = qlc:q([E || E <- mnesia:table(logfile)]),
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
lists:member(logfile, mnesia:system_info(tables)).

mnesia:create_table(logfile, [{attributes, record_info(fields, logfile)}]).

rr("/Users/leeyi/workspace/erl/es_client/apps/es_client/include/es_client.hrl").

%% 查询 使用 qlc

mnesia:transaction(fun() ->
    Q = qlc:q([E || E <- mnesia:table(logfile)]),
    qlc:e(Q)
end).

    {atomic,[]}
{atomic,[#logfile{name_md5 = "90a1b746a382dc494c3a960b5d3bdba5",
                  last_position = 0,
                  file = "/Users/leeyi/workspace/tools/nginx/logs/elk_access.log",
                  separator = "|",multiline = false,
                  keys = [#{"name" => "create_time","type" => datetime},
                          #{"name" => "level","type" => string},
                          #{"name" => "message","type" => string},
                          #{"name" => "other","type" => list}],
                  index_name = "test-afd-task"}]}
%% 部分查询 使用 qlc

mnesia:transaction(fun() ->
    Q = qlc:q([[E#logfile.name_md5, E#logfile.last_position] || E <- mnesia:table(logfile)]),
    qlc:e(Q)
end).


%% 使用 mnesia:write
    mnesia:transaction(fun() ->
        Acc = #logfile{
            name_md5="90a1b746a382dc494c3a960b5d3bdba5",
            last_position=1
        },
        mnesia:write(Acc)
    end).

mnesia:transaction(fun() ->
    Acc = #logfile{
        name_md5 = "90a1b746a382dc494c3a960b5d3bdba5",
        last_position=0,
        separator="|",
        keys=[
            #{
                "name"=>"create_time",
                "type"=>datetime
            },
            #{
                "name"=>"level",
                "type"=>string
            },
            #{
                "name"=>"message",
                "type"=>string
            },
            #{
                "name"=>"other",
                "type"=>list
            }
        ],
        multiline=false,
        index="test-afd-task",
        file="/Users/leeyi/workspace/tools/nginx/logs/elk_access.log"
    },
    mnesia:write(Acc)
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

File = "/Users/leeyi/workspace/tools/nginx/logs/elk_access.log",
{ok,Fd} = file:open(Fdile,read),
Line = file:read_line(Fd),
Position = file:position(Fd, {cur, 0}).

file_scaner:stop('90a1b746a382dc494c3a960b5d3bdba5').
observer:stop(), observer:start().

application:start(es_client).

rr("/Users/leeyi/workspace/erl/es_client/apps/es_client/include/es_client.hrl").
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
