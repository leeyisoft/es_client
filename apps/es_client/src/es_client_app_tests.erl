-module (es_client_app_tests).

% eunit 引入 放在 include es_client 之后
-include_lib("eunit/include/eunit.hrl").

start_test() ->
    es_client_app:start_worker().