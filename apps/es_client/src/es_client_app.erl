%%%-------------------------------------------------------------------
%% @doc es_client public API
%% @end
%%%-------------------------------------------------------------------

-module(es_client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    io:format("es_client start ~n"),
    % 启动依赖APP
    application:start(sasl),
    application:start(crypto),
    application:start(asn1),
    application:start(public_key),
    application:start(ssl),
    application:start(unicode_util_compat),
    application:start(idna),
    application:start(mimerl),
    application:start(certifi),
    application:start(ssl_verify_fun),
    application:start(metrics),
    application:start(hackney),
    application:start(erlastic_search),

    Res = es_client_sup:start_link(),



    %% 处理文件
    %% File 这里的文件一定是存在的，在调用它之前已经判断了
    %% Multiline 要么是原子 false，要么是被设定的值
    %% Separator 要么是空列表，要么是被设定的list
    % handle_file = ,

    % send_cast(File, Multiline, _Separator) ->

    % 启动 worker
    start_worker(
        fun(File, Multiline, _Separator) ->
            case Multiline of
                false -> % 单行处理
                    % handle_file_json(File);
                    io:format("handle_file : ~p~n", [[File, Multiline, _Separator]]),
                    FileMd5 = func:md5(File),
                    Res2 = supervisor:start_child(es_client_sup, [FileMd5]),
                    io:format("handle_file res2 : ~p~n", [Res2]),
                    Res2;
                _ ->
                    {ok, normal}
            end
        end
    ),

    % 给 worker 发送异步消息 [File, Multiline, Separator]
    start_worker(
        fun(File, Multiline, Separator) ->
            Msg = [File, Multiline, Separator],
            io:format("Msg ~p~n", [Msg]),
            FileMd5 = func:md5(File),
            io:format("FileMd5 : ~p~n", [FileMd5]),
            case whereis(list_to_atom(FileMd5)) of
                undefined ->
                    {error, FileMd5 ++ " not registered"};
                Pid ->
                    Res3 = gen_server:cast(Pid, Msg),
                    io:format("Res3 ~p~n", [Res3])
            end
        end
    ),

    % io:format("app ~p~n", [Res]),
    Res.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

%%====================================================================
%% Internal functions
%%====================================================================

%% private
start_worker(Callback)->
    case application:get_env(es_client, scan_files) of
        {ok, List} ->
            % io:format("scan_files val: ~p~n", [List]),
            [analysis_files_item(Item, Callback) || Item <- List];
        undefined ->
            io:format("can't get scan_flies from es_client~n")
    end.



%% 分析 scan_files 配置项
analysis_files_item(Item, Callback) ->
    % 获取分隔符，如果为空，表示“每行记录数据为json格式”
    Separator = case maps:find(separator, Item) of
        {ok, Separator2} when is_list(Separator2) ->
            Separator2;
        {ok, _} ->
            [];
        error ->
            []
    end,
    % 多行匹配数据
    Multiline = case maps:find(multiline, Item) of
        {ok, Multiline2} when Multiline2==false ->
            false;
        {ok, Multiline2} ->
            Multiline2;
        error ->
            false
    end,
    % 获取文件
    case maps:find(file, Item) of
        {ok, File} ->
            IsFile = filelib:is_file(File),
            if
                IsFile==false ->
                    % 解析文件列表
                    Dir = filename:dirname(File),
                    BaseName = filename:basename(File),
                    FileList = filelib:fold_files(Dir, "."++BaseName, true, fun(F, AccIn) -> [F | AccIn] end, []),

                    [Callback(File2, Multiline, Separator) || File2 <- FileList ];
                true ->
                    Callback(File, Multiline, Separator)
            end;
        error ->
            []
    end.





