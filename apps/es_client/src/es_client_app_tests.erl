-module (es_client_app_tests).

% eunit 引入 放在 include es_client 之后
-include_lib("eunit/include/eunit.hrl").

start_test() ->
    es_client_app:start_worker(
        % Separator 为 "" 的标示为json格式数据
        % Multiline 为 false 的表示单行匹配; 为 list 正则表达式
        fun(File, Separator, Multiline, Keys, Index) ->
            % io:format("es_client start_worker : ~p~n", [[File, Multiline, Separator]]),
            FileMd5 = func:md5(File),
            % io:format("FileMd5 : ~p~n", [FileMd5]),
            Position = esc_db:get_last_position(FileMd5),
            esc_db:save_logfile(FileMd5, Position),

            % start_child
            StartArgs = {FileMd5, Position, File, Separator, Multiline, Keys, Index},
            io:format("StartArgs : ~p~n\n", [StartArgs])
        end
    ).