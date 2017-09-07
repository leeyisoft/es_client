-module (esc_loger).

-export ([log/1, log/2, log/3
    ,error/1, error/2
    ,info/1, info/2
]).

error(Msg) ->
    log(Msg, [], error).
error(Msg, List) ->
    log(Msg, List, error).

info(Msg) ->
    log(Msg, [], info).
info(Msg, List) ->
    log(Msg, List, info).


log(Msg) ->
    log(Msg, [], info).

log(Msg, List) ->
    log(Msg, List, info).

%%%

% Leve [debug|info|warn|error|fatal ]
%
% Level debug 指出细粒度信息事件对调试应用程序是非常有帮助的。
% Level info 表明消息在粗粒度级别上突出强调应用程序的运行过程。
% Level warn 表明会出现潜在错误的情形。
% Level error 指出虽然发生错误事件，但仍然不影响系统的继续运行。
% Level fatal 指出每个严重的错误事件将会导致应用程序的退出。
%%%
log(Msg, List, Level) ->
    Path = case application:get_env(es_client, log_path) of
        undefined ->
            "./log";
        {ok, ""} ->
            "./log";
        {ok, Str} ->
            Str
    end,

    {{Year, Month, Day}, _} = calendar:local_time(),
    File = lists:flatten(io_lib:format("~s/~p_~4..0w~2..0w~2..0w.log", [Path,Level, Year, Month, Day])),

    IsDir = filelib:is_dir(Path),
    if
        IsDir==false ->
            file:make_dir(Path);
        true ->
            ok
    end,

    try
        {ok, Fd} = file:open(File,[append]),
        Msg2 = case List of
                [] ->
                    Msg;
                [_H|_T] ->
                    lists:flatten(io_lib:format(Msg, List))
        end,
        Msg3 = lists:flatten(io_lib:format("~t~n", [Msg2])),
        file:write(Fd, list_to_binary(Msg3))
    catch
        Exception:Reason ->
            % 本来就是在异常的情况下记录错误日志，这里再错了，就没有办法了
            % Msg 里面包含中文，会报异常
            io:format("esc_loger:log/3 Exception: ~p , Reason: ~p, Msg ~t ~n", [Exception, Reason, Msg])
    end.
