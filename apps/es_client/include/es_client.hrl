
-define(LogFileTable, logfile).

% disc_copies 磁盘 + 内存; ram_copies 内存
% 生产环境用 disc_copies ， 开发环境用 ram_copies
-define(TableCopies, ram_copies).

% 日志文件记录，比配置文件sys.config里面scan_files 的子项多2项: name_md5 last_position
-record(?LogFileTable, {
    name_md5, last_position,
    file, separator, multiline, keys, index
}).

% true 想在自己的代码上使用EUnit（例如使用?assert宏），但是你并不想产生测试。
% -define(NOTEST, true)