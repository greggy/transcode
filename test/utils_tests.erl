%%% @author greg <>
%%% @copyright (C) 2013, greg
%%% @doc
%%%
%%% @end
%%% Created : 27 Mar 2013 by greg <>

-module(utils_tests).

-include_lib("eunit/include/eunit.hrl").


%%%------------------------------------------------------------------------
%%% Test list
%%%------------------------------------------------------------------------
transcode_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [
      fun test_paths/0
      , fun get_meta/0
     ]}.


%%%------------------------------------------------------------------------
%%% Fixtures
%%%------------------------------------------------------------------------
start() ->
    meck:new(erlcloud_s3),
    ok.

stop(ok) ->
    meck:unload(erlcloud_s3),
    ok.


%%%------------------------------------------------------------------------
%%% Tests
%%%------------------------------------------------------------------------
test_paths() ->
    Path1 = utils:get_flv_path("3", "1958", "/var/db/fftv_storage"),
    ?assertEqual(Path1, "/var/db/fftv_storage/video-flv/3/1958.flv"),
    Path2 = utils:get_hls_folder("3", "1958", "/var/db/fftv_storage"),
    ?assertEqual(Path2, "/var/db/fftv_storage/video-hls/3/1958"),
    Path3 = utils:get_frag_folder("3", "1958", "/var/db/fftv_storage"),
    ?assertEqual(Path3, "/var/db/fftv_storage/video-frag/3/1958").
    

get_meta() ->
     meck:expect(erlcloud_s3, get_object_metadata, fun(_, _, _, _) ->
    							  [
    							   {"last_modified", "Mar, 15 2013"},
    							   {"x-amz-meta-video_id", "3"},
   							   {"x-amz-meta-account_id", "1958"},
    							   {"x-amz-meta-metka", "node-2"},
    							   {"x-amz-meta-test1", "test1"},
    							   {"x-amz-meta-test2", "test2"}
    							  ]
    						  end),
    ?assertEqual([{"metka", "node-1"},
    		  {"test2", "test2"},
    		  {"test1", "test1"},
		  {"account_id", "1958"},
		  {"video_id", "3"}
		 ],
    		 utils:get_correct_meta(metka, "node-1", {"bucket", "task", "s3_url"})),
    ?assert(meck:validate(erlcloud_s3)),
  
    meck:expect(erlcloud_s3, put_object, fun(_, _, _, _, _) -> ok end),
    ?assertEqual(ok, utils:set_task_meta(metka, "node-1", {"bucket", "task", "s3_url"})),
    ?assert(meck:validate(erlcloud_s3)).
