-module (esc_loger_tests).

% eunit 引入 放在 include es_client 之后
-include_lib("eunit/include/eunit.hrl").


all_test() ->

    esc_loger:info("asdf中国"),
    esc_loger:info("asdf"),
    esc_loger:error("asdf"),
    esc_loger:info("info test asdf"),
    esc_loger:info("~p 你好 ~p", [ase, 33]),
    esc_loger:info("~p dddd ~p", [ase, 33]),
    esc_loger:error("~p dddd ~p", [ase, 33]).