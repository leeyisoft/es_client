-module (esc_db).

-include_lib("stdlib/include/qlc.hrl").

-export ([start/0
    , get_last_position/1
    , save_logfile/2
]).

-export ([]).

% disc_copies 磁盘 + 内存; ram_copies 内存
% 生产环境用 disc_copies ， 开发环境用 ram_copies
-define(TableCopies, ram_copies).
-define (LogFileTable, esc_logfile).

-record(?LogFileTable, {
    name_md5, last_position
}).

start() ->
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
    Res = case lists:member(?LogFileTable, mnesia:system_info(tables)) of
        false ->
            % 创建表
            mnesia:create_table(?LogFileTable, [{type, set},
                           {?TableCopies, [node()]}, % disc_copies 磁盘 + 内存; ram_copies 内存
                           {attributes, record_info(fields, ?LogFileTable)}]);
        _ ->
            alread_created_table
    end,
    % 暂停10毫秒，等待创建、启动mnesia数据库
    timer:sleep(10),
    Res.

get_last_position(NameMd5) ->
    case mnesia:dirty_read(?LogFileTable, NameMd5) of
        [{?LogFileTable, NameMd5, Position}] ->
            Position;
        _ ->
            0
    end.


save_logfile(FileMd5, Position) when is_integer(Position), Position >=0 ->
    mnesia:transaction(fun() ->
        Data = #?LogFileTable{
            name_md5=FileMd5,
            last_position=Position
        },
        mnesia:write(Data)
    end).
