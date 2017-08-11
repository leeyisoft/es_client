%%%-------------------------------------------------------------------
%% @doc es_client public API
%% @end
%%%-------------------------------------------------------------------

-module(es_client_app).

-behaviour(application).

%% Application callbacks
-export([start/2
    , stop/1

    , start_scaner/0
]).

%%====================================================================
%% API
%%====================================================================

start(_StartType, _StartArgs) ->
    io:format("es_client start _StartType ~p ~n", [_StartType]),
    io:format("es_client start _StartArgs ~p ~n", [_StartArgs]),
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

    esc_db:start(),

    % 启动 es_client
    Res = es_client_sup:start_link(),

    % 启动 worker
    start_scaner(),

    % io:format("app ~p~n", [Res]),
    Res.

%%--------------------------------------------------------------------
stop(_State) ->
    ok.

start_scaner() ->
    Res = start_scaner(
        % Separator 为 "" 的标示为json格式数据
        % Multiline 为 false 的表示单行匹配; 为 list 正则表达式
        fun(File, Separator, Multiline, Keys, Index) ->
            % io:format("es_client start_scaner : ~p~n", [[File, Multiline, Separator]]),
            FileMd5 = list_to_atom(esc_func:md5(File)),
            % start_child
            StartArgs = {FileMd5, File, Separator, Multiline, Keys, Index},
            supervisor:start_child(es_client_sup, [StartArgs]),
            FileMd5
        end
    ),
    supervisor:start_child(es_client_sup, [{esc_check_scaner, {check_list, Res}}]),
    % io:format("start_scaner/0 : ~p~n", [Res]),
    ok.


%%====================================================================
%% Internal functions
%%====================================================================

start_scaner(Callback) when is_function(Callback) ->
    case application:get_env(es_client, scan_files) of
        {ok, List} ->
            % io:format("scan_files val: ~p~n", [List]),
            List2 = lists:flatten([analysis_files_item(Item, Callback) || Item <- List]),
            if
                List2==[] ->
                    io:format("can't find file from the es_client scan_flies~n"),
                    error;
                true ->
                    List2
            end;
        undefined ->
            io:format("can't get scan_flies from es_client~n"),
            error
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
    Keys = case maps:find(keys, Item) of
        {ok, Keys2} when is_list(Keys2) ->
            Keys2;
        {ok, _} ->
            "unset_keys";
        error ->
            "unset_keys"
    end,
    Index = case maps:find(index, Item) of
        {ok, Index2} when is_list(Index2) ->
            Index2;
        {ok, _} ->
            "index-name";
        error ->
            "index-name"
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
                    % io:format("FileList ~p~n", [FileList]),
                    [Callback(File2, Separator, Multiline, Keys, Index) || File2 <- FileList ];
                true ->
                    Callback(File, Separator, Multiline, Keys, Index)
            end;
        error ->
            []
    end.





