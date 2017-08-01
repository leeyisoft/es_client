-module (file_scaner_tests).

-include("es_client.hrl").
% eunit 引入 放在 include es_client 之后
-include_lib("eunit/include/eunit.hrl").

format_k_v_test() ->
    Key1 = {name, "createtime", datetime},
    Val1 = "2017/06/07 11:27:35 ",
    ?assertEqual({"createtime", "2017/06/07 11:27:35"}, file_scaner:format_k_v(Key1, Val1)).

format_k_v_2_test() ->
    Key2 = {split, ":", string},
    Val2 = " upstream: \"fastcgi://127.0.0.1:9000\"",
    ?assertEqual({"upstream", "fastcgi://127.0.0.1:9000"}, file_scaner:format_k_v(Key2, Val2)).


format_k_v_3_test() ->
    Key3 = {split, ":", {split, " / ", ["http_method", "http_protocol"]}},
    Val3 = " request: \"GET / HTTP/1.1\"",
    ?assertEqual([{"request", "GET / HTTP/1.1"}, {"http_method", "GET"}, {"http_protocol", "HTTP/1.1"}], file_scaner:format_k_v(Key3, Val3)).

format_k_v_all_test() ->
    Str = "2017/06/07 11:27:35 [error] 70233#0: *436 FastCGI sent in stderr: \"PHP message: PHP Warning:  require(/Users/leeyi/workspace/afd/afd-admin/web/../vendor/autoload.php): failed to open stream: No such file or directory in /Users/leeyi/workspace/afd/afd-admin/web/index.php on line 15
PHP message: PHP Fatal error:  require(): Failed opening required '/Users/leeyi/workspace/afd/afd-admin/web/../vendor/autoload.php' (include_path='.:') in /Users/leeyi/workspace/afd/afd-admin/web/index.php on line 15\" while reading response header from upstream, client: 127.0.0.1, server: admin.afd56.local, request: \"GET / HTTP/1.1\", upstream: \"fastcgi://127.0.0.1:9000\", host: \"127.0.0.1:8085\"",
    Keys = [{name,"createtime",
          datetime},
         {name,"level",string},
         {name,"message",string},
         {split,":",ip},
         {split,":",string},
         {split,":",
          {split," ",
           ["http_method","http_path",
            "http_protocol"]}},
         {split,":",string},
         {split,":",string}],
    Separator = "[,|\\[|\\]]+",
    Vals = re:split(Str, Separator, [{return, list}]),

    Items = [file_scaner:format_k_v(Key, Val) || {Key, Val} <- lists:zip(Keys, Vals)],
    io:format("~n~p~n", [ lists:flatten(Items)]).
    % [{list_to_binary(X),list_to_binary(Y)} || {X,Y} <- Items]

