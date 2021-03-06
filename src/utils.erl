%%%-------------------------------------------------------------------
%%% @author greg <>
%%% @copyright (C) 2013, greg
%%% @doc
%%%
%%% @end
%%% Created : 25 Mar 2013 by greg <>
%%%-------------------------------------------------------------------
-module(utils).


%% API
-export([get_timestamp/1
	 , set_task_meta/3
	 , get_correct_meta/3
	 , get_flv_path/3
	 , get_hls_folder/3
	 , get_frag_folder/3
	]).


%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Переводит erlang:now() в unix timestasmp.
%%
%% @spec get_timestamp() -> integer()
%% @end
%%--------------------------------------------------------------------
get_timestamp(Now) ->
    {Mega, Secs, _} = Now,
    Mega * 1000000 + Secs.


%%--------------------------------------------------------------------
%% @doc
%% Устанавливает кастомную метку на таск с определённым значением.
%%
%% @spec set_task_meta(string() | atom(), string(), record()) -> ok
%% @end
%%--------------------------------------------------------------------
set_task_meta(Key, Value, {Bucket, Task, S3Auth}=Auth) ->
    NMetadata = get_correct_meta(Key, Value, Auth),
    erlcloud_s3:put_object(Bucket, Task, <<"0">>, 
    			   [{meta, NMetadata}], S3Auth),
    ok.


%%--------------------------------------------------------------------
%% @doc
%% Make correct proplist of metadata from original S3 object info.
%%
%% @spec set_task_meta(string() | atom(), string(), record()) -> proplist()
%% @end
%%--------------------------------------------------------------------
get_correct_meta(Key, Value, {Bucket, Task, S3Auth}) ->
    Metadata = erlcloud_s3:get_object_metadata(Bucket, Task, [], S3Auth),
    NKey = case is_atom(Key) of true -> atom_to_list(Key); false -> Key end,
    Metadata2 = get_custom_meta(Metadata, NKey),
    [{NKey, Value} | Metadata2].
 

%%--------------------------------------------------------------------
%% @doc
%% Конструирнует и возвращает локальный путь до flv файла.
%%
%% @spec get_flv_path(string(), string(), string()) -> file:filename_all()
%% @end
%%--------------------------------------------------------------------
get_flv_path(AccountID, VideoID, StoragePath) ->
    Folder = filename:join([StoragePath, "video-flv", AccountID]),
    ok = filelib:ensure_dir(Folder),    
    filename:join([Folder, string:join([VideoID, ".flv"], "")]).


%%--------------------------------------------------------------------
%% @doc
%% Конструирнует и возвращает локальный путь до hls папки.
%%
%% @spec get_hls_folder(string(), string(), file:filename()) -> file:filename()
%% @end
%%--------------------------------------------------------------------
get_hls_folder(AccountID, VideoID, StoragePath) ->
    Folder = filename:join([StoragePath, "video-hls", AccountID, VideoID]),
    ok = filelib:ensure_dir(Folder),    
    Folder.


%%--------------------------------------------------------------------
%% @doc
%% Конструирнует и возвращает локальный путь до frag папки.
%%
%% @spec get_frag_folder(string(), string(), file:filename()) -> file:filename()
%% @end
%%--------------------------------------------------------------------
get_frag_folder(AccountID, VideoID, StoragePath) ->
    Folder = filename:join([StoragePath, "video-frag", AccountID, VideoID]),
    ok = filelib:ensure_dir(Folder),    
    Folder.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Берёт метаинформацию таска, выберает только кастомные значения и
%% возвращает proplist() вида [{Key, Value}, ...]. Причём, OKey не
%% должны попасть в возвращаемый результат.
%%
%% @spec get_custom_meta(proplist(), string()) -> proplist()
%% @end
%%--------------------------------------------------------------------
get_custom_meta(Metadata, Key) ->
    get_custom_meta(Metadata, Key, []).

get_custom_meta([{Key, Value} | Data], OKey, Acc) ->
    NKey = case is_atom(Key) of true -> atom_to_list(Key); _ -> Key end,
    case string:str(NKey, "x-amz-meta-")  of
	0 -> get_custom_meta(Data, OKey, Acc);
	_ ->
	    Meta = string:sub_string(NKey, 12),
	    case Meta == OKey of
		false -> get_custom_meta(Data, OKey, [{Meta, Value} | Acc]);
		true -> get_custom_meta(Data, OKey, Acc)
	    end
    end;

get_custom_meta([], _, Acc) -> Acc.
