-module (func).

-export ([md5/1]).

%%
%% erlang md5 16进制字符串
%%
md5(Str) ->
    Sig = erlang:md5(Str),
    binary_to_list(iolist_to_binary([io_lib:format("~2.16.0b", [S]) || S <- binary_to_list(Sig)])).