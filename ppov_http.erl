-module(ppov_http).
-author('olivier@biniou.info').

-export([start/0]).
-export([encode_uuid/1, decode_uuid/1]).

%% Internal export
-export([init/0]).


start() ->
    spawn(?MODULE, init, []).


init() ->
    DocRoot = "./www",
    yaws:start_embedded(DocRoot,
			[
			 {listen, {0,0,0,0}}, {port, 44444},
			 {docroot, DocRoot}
			], %% sconf
			[
			 {include_dir, ["."]},
			 {cache_refresh_secs, 0}
			] %% gconf
		       ).

encode_uuid(UUID) when is_binary(UUID), size(UUID) =:= 16 ->
    binary_to_list(base64:encode(term_to_binary(UUID))).

decode_uuid(UUID) when is_list(UUID) ->
    binary_to_term(base64:decode(UUID), [safe]).
