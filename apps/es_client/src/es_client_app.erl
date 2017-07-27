%%%-------------------------------------------------------------------
%% @doc es_client public API
%% @end
%%%-------------------------------------------------------------------

-module(es_client_app).

-behaviour(application).

%% Application callbacks
-export([start/2, stop/1]).

-include("es_client.hrl").

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

    % mnesia检查数据库是否创建
    % 确保先创建 schema 之后再启动 mnesia
    case mnesia:system_info(use_dir) of
        true ->
            alread_created_schema;
        _ ->
            % mnesia:delete_schema([node()]).
            mnesia:create_schema([node()])
    end,

    application:start(mnesia),

    % 创建表 ?LogFileTable
    % 确保已经 mnesia:start().
    case lists:member(?LogFileTable, mnesia:system_info(tables)) of
        false ->
            mnesia:create_table(?LogFileTable, [{type, set},
                           {disc_copies, [node()]}, % 磁盘 + 内存
                           {attributes, record_info(fields, ?LogFileTable)}]);
        _ ->
            alread_created_table
    end,

    % 启动 es_client
    Res = es_client_sup:start_link(),

    % 启动 worker
    start_worker(
        fun(File, Multiline, Separator, Item) ->
            % io:format("es_client start_worker : ~p~n", [[File, Multiline, Separator]]),
            FileMd5 = func:md5(File),
            % io:format("FileMd5 : ~p~n", [FileMd5]),
            Index = case maps:find(index, Item) of
                {ok, Index2} when is_list(Index2) ->
                    Index2;
                {ok, _} ->
                    "index-name";
                error ->
                    "index-name"
            end,
            Keys = case maps:find(keys, Item) of
                {ok, Keys2} when is_list(Keys2) ->
                    Keys2;
                {ok, _} ->
                    "index-name";
                error ->
                    "index-name"
            end,
            Data = #?LogFileTable{
                name_md5=FileMd5,
                last_position=func:get_last_position(FileMd5),
                multiline=Multiline, % Multiline 为 false 的表示单行匹配; 为 list 正则表达式
                separator=Separator, % Separator 为 "" 的标示为json格式数据
                keys=Keys,
                index=Index,
                file=File
            },
            func:save_logfile(Data),

            % start_child
            Res2 = supervisor:start_child(es_client_sup, [Data]),
            io:format("supervisor:start_child : ~p~n", [Res2]),
            Res2
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

                    [Callback(File2, Multiline, Separator, Item) || File2 <- FileList ];
                true ->
                    Callback(File, Multiline, Separator, Item)
            end;
        error ->
            []
    end.





