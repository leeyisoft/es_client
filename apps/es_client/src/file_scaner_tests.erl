-module (file_scaner_tests).

% eunit 引入 放在 include es_client 之后
-include_lib("eunit/include/eunit.hrl").

format_k_v_datetime_test() ->
    Key1 = {name, "createtime", datetime},
    Val1 = "[2017-06-02 10:38:59] ",
    ?assertEqual({"createtime", "2017-06-02 10:38:59"}, file_scaner:format_k_v(Key1, Val1)).


format_k_v_3_test() ->
    Key3 = {split, ":", {split, " ", [
                        {name,"request_method", string},
                        {name,"request_path", string},
                        {name,"http_protocol", string}
                    ]}},
    Val3 = " request: \"GET / HTTP/1.1\"",
    io:format("~p~n", [file_scaner:format_k_v(Key3, Val3)]).


taks_test() ->
    Str = "[2017-06-01 10:28:00] | SD.INFO | coroutine sql UPDATE a_sms_log SET response = '870467003841637376', response_time = '1496370480' WHERE id = '3347' | [] |[]\n",
    Keys = [
        {name, "createtime", datetime},
        {name, "level", string},
        {name, "message", string},
        {name, "other", string}
    ],
    Separator = "|",
    Vals = string:split(Str, Separator, all),
    Vals2 = lists:sublist(Vals, length(Keys)),
    % io:format("~p~n", [Vals]).
    Items = [file_scaner:format_k_v(Key, Val) || {Key, Val} <- lists:zip(Keys, Vals2)],
    io:format("~n~p~n", [ lists:flatten(Items)]).
    % [{list_to_binary(X),list_to_binary(Y)} || {X,Y} <- Items]

nginx_error_info2_test() ->
    Str = "2017/06/14 09:42:12 [info] 70237#0: *5527 kevent() reported that client prematurely closed connection, so upstream connection is closed too while sending request to upstream, client: 127.0.0.1, server: admin.afd56.local, request: \"GET /card/recharge-order/add HTTP/1.1\", upstream: \"fastcgi://127.0.0.1:9000\", host: \"127.0.0.1:8085\"\n",

    Keys = [
        {split,"[\\[|\\]]+", [
            {name, "createtime", datetime},
            {name, "level", string},
            {name, "message", string}
        ]},
        {split, ",", [
            {split, ":", ip},
            {split, ":", string},
            {split, ":", {split, " ", [
                {name,"request_method", string},
                {name,"request_path", string},
                {name,"http_protocol", string}
            ]}},
            {split, ":", string},
            {split, ":", string}
        ]}
      ],
    Separator = ", c",
    Vals = string:split(Str, Separator, all),
    Items = file_scaner:kv_to_erlastic_json(Keys, Vals, "ddd"),
    io:format("~n~p~n", [Items]).
    % [{list_to_binary(X),list_to_binary(Y)} || {X,Y} <- Items]


nginx_error_info_test() ->
    Str = "2017/07/13 17:32:45 [info] 70233#0: *24771 client closed connection while waiting for request, client: 127.0.0.1, server: 0.0.0.0:8085\n",
    Keys = [
        {split,"[\\[|\\]]+", [
            {name, "createtime", datetime},
            {name, "level", string},
            {name, "message", string}
        ]},
        {split, ",", [
            {split, ":", ip},
            {split, ":", string},
            {split, ":", {split, " ", [
                {name,"request_method", string},
                {name,"request_path", string},
                {name,"http_protocol", string}
            ]}},
            {split, ":", string},
            {split, ":", string}
        ]}
      ],
    Separator = ", c",
    Vals = string:split(Str, Separator, all),
    Items = file_scaner:kv_to_erlastic_json(Keys, Vals, "test file"),
    io:format("~n~p~n", [Items]).


nginx_error_msg_test() ->
    Str = "2017/06/29 10:20:38 [error] 70235#0: *16458 open() \"/Users/leeyi/workspace/afd/afd-admin/web/favicon.ico\" failed (2: No such file or directory), client: 127.0.0.1, server: admin.afd56.local, request: \"GET /favicon.ico HTTP/1.1\", host: \"127.0.0.1:8085\", referrer: \"http://127.0.0.1:8085/\"\n",
    Keys = [{name,"createtime",
                                                      datetime},
                                                     {name,"level",string},
                                                     {name,"message",string},
                                                     {name,"message2",string},
                                                     {split,":",ip},
                                                     {split,":",string},
                                                     {split,":",
                                                      {split," ",
                                                       [{name,
                                                         "request_method",
                                                         string},
                                                        {name,"request_path",
                                                         string},
                                                        {name,
                                                         "http_protocol",
                                                         string}]}},
                                                     {split,":",string},
                                                     {split,":",string}],
    Separator = "[,|\\[|\\]]+",
    % Vals = re:split(Str, Separator, [{return, list}]),
    % Items = file_scaner:kv_to_erlastic_json(Keys, Vals).

    Items = file_scaner:str_to_json(Str, Separator, Keys, "test file"),
    io:format("~n~p~n", [Items]).

