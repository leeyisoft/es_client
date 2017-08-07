-module (esc_scaner_tests).

% eunit 引入 放在 include es_client 之后
-include_lib("eunit/include/eunit.hrl").

format_k_v_datetime_test() ->
    Key1 = {name, "createtime", datetime},
    Val1 = "[2017-06-02 10:38:59] ",
    ?assertEqual({"createtime", "2017-06-02 10:38:59"}, esc_scaner:format_k_v(Key1, Val1)).


format_k_v_3_test() ->
    Key3 = {split, ":", {split, " ", [
                        {name,"request_method", string},
                        {name,"request_path", string},
                        {name,"http_protocol", string}
                    ]}},
    Val3 = " request: \"GET / HTTP/1.1\"",
    io:format("~p~n", [esc_scaner:format_k_v(Key3, Val3)]).


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
    Items = [esc_scaner:format_k_v(Key, Val) || {Key, Val} <- lists:zip(Keys, Vals2)],
    io:format("~n~p~n", [ lists:flatten(Items)]).
    % [{list_to_binary(X),list_to_binary(Y)} || {X,Y} <- Items]

nginx_error_info2_test() ->
    Str = "2017/07/24 10:38:25 [error] 4697#0: *58047 FastCGI sent in stderr: \"PHP message: PHP Fatal error:  Uncaught TypeError: Argument 1 passed to Topxia\\Common\\ArrayToolkit::column() must be of the type array, string given, called in /data/wwwroot/default/edu.afd56.com.cn/src/Topxia/Service/User/Impl/UserServiceImpl.php on line 272 and defined in /data/wwwroot/default/edu.afd56.com.cn/src/Topxia/Common/ArrayToolkit.php:15\nStack trace:\n#0 /data/wwwroot/default/edu.afd56.com.cn/src/Topxia/Service/User/Impl/UserServiceImpl.php(272): Topxia\\Common\\ArrayToolkit::column('<!DOCTYPE html>...', 'id')\n#1 /data/wwwroot/default/edu.afd56.com.cn/src/Topxia/WebBundle/Controller/SettingsController.php(191): Topxia\\Service\\User\\Impl\\UserServiceImpl->changeAvatar('57', '<!DOCTYPE html>...')\n#2 [internal function]: Topxia\\WebBundle\\Controller\\SettingsController->avatarCropModalAction(Object(Symfony\\Component\\HttpFoundation\\Request))\n#3 /data/wwwroot/default/edu.afd56.com.cn/vendor/symfony/symfony/src/Symfony/Component/HttpKernel/HttpKernel.php(144): call_user_func_array(Array, Array)\n#4 /data/ww\" while reading response header from upstream, client: 192.168.8.109, server: edu.dev.afd56.com.cn, request: \"POST /settings/avatar/crop/modal HTTP/1.1\", upstream: \"fastcgi://unix:/dev/shm/php-cgi.sock:\", host: \"edu.dev.afd56.com.cn\", referrer: \"http://edu.dev.afd56.com.cn/course/create\"\n",

    Keys = [{split,"[\\[|\\]]+",
                                                 [{name,"createtime",datetime},
                                                  {name,"level",string},
                                                  {name,"message",string}]},
                                                {split,",",
                                                 [{split,":",ip},
                                                  {split,":",string},
                                                  {split,":",
                                                   {split," ",
                                                    [{name,"request_method",
                                                      string},
                                                     {name,"request_path",string},
                                                     {name,"http_version",
                                                      string}]}},
                                                  {split,":",string},
                                                  {split,":",string}]}],

    Separator = ", cl",
    Vals = string:split(Str, Separator, all),
    Items = esc_scaner:kv_to_erlastic_json(Keys, Vals, "ddd"),
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
    Items = esc_scaner:kv_to_erlastic_json(Keys, Vals, "test file"),
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
    % Items = esc_scaner:kv_to_erlastic_json(Keys, Vals).

    Items = esc_scaner:str_to_json(Str, Separator, Keys, "test file"),
    io:format("~n~p~n", [Items]).

filter_index_name_test() ->
    Index = "test-index-{Ymd}-{Y-m-d}-{Ym}-{Y}",
    esc_scaner:filter_index_name(Index).

nginx_access_test() ->
    Str = "{ \"@timestamp\": \"19/Jul/2017:17:47:48 +0800\", \"http_host\": \"edu.dev.afd56.com.cn\", \"http_x_forwarded_for\": \"-\", \"request\": \"GET /assets/v2/img/righ_low.png HTTP/1.1\", \"status\": 200, \"remote_addr\": \"192.168.8.142\", \"remote_user\": \"-\", \"request_body\": \"-\", \"content_length\": \"-\", \"request_time\": 0.000, \"request_method\": \"GET\", \"http_referrer\": \"http://edu.dev.afd56.com.cn/assets/v2/css/main.css?7.5.22\", \"body_bytes_sent\": 472, \"http_user_agent\": \"Mozilla/5.0 (Linux; U; Android 4.4.4; zh-cn; HUAWEI Y635-CL00 Build/HuaweiY635-CL00) AppleWebKit/537.36 (KHTML, like Gecko)Version/4.0 Chrome/37.0.0.0 MQQBrowser/6.9 Mobile Safari/537.36\", \"upstream_response_time\": \"-\" }\n",
    jsx:decode(list_to_binary(Str)).

