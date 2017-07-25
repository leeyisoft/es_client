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
// 编译
rm -rf _build && rm -rf rebar.lock && ./rebar3 compile

// 启动
erl -boot start_sasl -pa _build/default/lib/*/ebin -config config/sys.config

./rebar3 compile && erl -boot start_sasl -pa _build/default/lib/*/ebin -config config/sys.config

./rebar3 compile && erl -pa _build/default/lib/*/ebin -config config/sys.config -boot es_client

application:start(es_client).

observer:start().

observer:stop(), observer:start().

erlang:is_process_alive(<0.100.0>).

file_scaner:stop("container1")
file_scaner:stop("container4")

whereis(application_controller)

application:get_env(es_client, scan_flies).

{ok, [I1, I2]}=application:get_env(es_client, scan_flies).

 F1 = maps:get(file,I1).
 F2 = maps:get(file,I2).
 filelib:is_regular(F1).

Dir = filename:dirname(F1).
BaseName = filename:basename(F1).
filelib:fold_files(Dir, "."++BaseName, true, fun(F, AccIn) -> [F | AccIn] end, []).



Str = "2017/06/07 09:44:02 [error] 33212#0: *253 FastCGI sent in stderr: \"PHP message: An Error occurred while handling another error:\nyii\baseInvalidRouteException: Unable to resolve the request \"site/error\". in /Users/leeyi/workspace/afd/app-afd/vendor/yiisoft/yii2/base/Module.php:460\nStack trace:\n#0 /Users/leeyi/workspace/afd/app-afd/vendor/yiisoft/yii2/web/ErrorHandler.php(93): yii\baseModule->runAction('site/error')\n#1 /Users/leeyi/workspace/afd/app-afd/vendor/yiisoft/yii2/base/ErrorHandler.php(109): yiiwebErrorHandler->renderException(Object(yiiwebNotFoundHttpException))\n#2 [internal function]: yii\baseErrorHandler->handleException(Object(yiiwebNotFoundHttpException))\n#3 {main}\nPrevious exception:\nyii\baseInvalidRouteException: Unable to resolve the request \"Cdorg/index\". in /Users/leeyi/workspace/afd/app-afd/vendor/yiisoft/yii2/base/Module.php:460\nStack trace:\n#0 /Users/leeyi/workspace/afd/app-afd/vendor/yiisoft/yii2/web/Application.php(87): yii\baseModule->runAction('Cdorg/index', Array)\n#1 /Users/leeyi/workspace/afd/app-afd/vendor/yiisoft/yii2/base/Applicati\" while reading upstream, client: 127.0.0.1, server: admin.afd56.local, request: \"GET /Cdorg/index HTTP/1.1\", upstream: \"fastcgi://127.0.0.1:9000\", host: \"127.0.0.1:8082\"".

f(ReStr), ReStr = "\\d{4}\\/\\d{2}\\/\\d{2} \\d{2}:\\d{2}:\\d{2} \\[\s+\\] *".

f(ReStr), ReStr = "[ , \, ]", re:split(Str, ReStr, [{return, list}]).
```

### 应用
```
// 创建索引
erlastic_search:create_index(<<"lee_index">>).

// 删除
erlastic_search:delete_index(<<"lee_index">>).
// 批量删除
erlastic_search:delete_index(<<"es_index_name*">>).
erlastic_search:delete_index(<<"*index_name*">>).


// 添加记录（自动生成 _id）
erlastic_search:index_doc(<<"lee_index">>, <<"type">>, [{<<"key1">>, <<"value1">>}]).

// 添加记录（自定义 _id）
erlastic_search:index_doc_with_id(<<"lee_index">>, <<"type">>, <<"id1">>, [{<<"key1">>, <<"value1">>}]).

// 查询
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

FileMd5 = '90a1b746a382dc494c3a960b5d3bdba5'.
            case whereis(FileMd5) of
                undefined ->
                    {error, atom_to_list(FileMd5) ++ " not registered"};
                Pid ->
                    io:format("Pid ~p~n", [Pid])
            end
```

file_scaner:stop('90a1b746a382dc494c3a960b5d3bdba5')
observer:stop(), observer:start().
gen_server:call('90a1b746a382dc494c3a960b5d3bdba5' , "dd").
### 调试
```
application:start(observer).
observer:start().
```

### 测试
```
./rebar3 eunit
```
