
-define(LogFileTable, logfile).

% 日志文件记录，比配置文件sys.config里面scan_files 的子项多2项: name_md5 last_position
-record(?LogFileTable, {
    name_md5, last_position,
    file, separator, multiline, keys, index
}).

