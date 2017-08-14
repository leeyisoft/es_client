-module (esc_func_tests).

% eunit 引入 放在 include es_client 之后
-include_lib("eunit/include/eunit.hrl").

% esc_func:timestamp_format(Timestamp).
timestamp_format_test() ->

    Timestamp = "12/Jun/2017:11:38:12 +0800",
    Dt = esc_func:timestamp_format(Timestamp),
    ?assertEqual("2017-06-12T03:38:12Z", Dt),

    Timestamp2 = "12/Jun/2017:07:38:12 +0800",
    Dt2 = esc_func:timestamp_format(Timestamp2),
    ?assertEqual("2017-06-11T23:38:12Z", Dt2),

    Timestamp3 = "2016/10/22 16:42:04",
    Dt3 = esc_func:timestamp_format(Timestamp3),
    ?assertEqual("2016-10-22T08:42:04Z", Dt3).
