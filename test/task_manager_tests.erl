%%% @author greg <>
%%% @copyright (C) 2013, greg
%%% @doc
%%%
%%% @end
%%% Created : 28 Mar 2013 by greg <>

-module(task_manager_tests).


-include_lib("eunit/include/eunit.hrl").


%%%------------------------------------------------------------------------
%%% Test list
%%%------------------------------------------------------------------------
transcode_test_() ->
    {foreach,
     fun start/0,
     fun stop/1,
     [
      fun test_manager/0
     ]}.


%%%------------------------------------------------------------------------
%%% Fixtures
%%%------------------------------------------------------------------------
start() ->
    application:start(sasl),
    lager:start(),
    application:start(transcode),
    ok.

stop(ok) ->
    application:stop(transcode),
    ok.


%%%------------------------------------------------------------------------
%%% Tests
%%%------------------------------------------------------------------------
test_manager() ->
    meck:expect(erlcloud_s3, list_objects, fun(_, _, _, _) ->
						   [{name,"test-bn-public-eu"}
						    ,{prefix,"task"},{marker,[]}
						    ,{delimiter,[]},{max_keys,1000}
						    ,{is_truncated,false}
						    ,{contents,
						      [[{key,"task/3-1966"}
							,{last_modified,{{2013,3,27},{12,29,50}}}
							,{etag,"\"cfcd208495d565ef66e7dff9f98764da\""}
							,{size,1}
							,{storage_class,"STANDARD"}
							,{owner,[{id,"51ec5d7fa4a77540530e3845431eb3f77a781cad0d286c28234cbbb9dd58dd6f"}
								 ,{display_name,"m.garanin"}
								]}
						       ]]
						     }]
					   end),
    meck:expect(erlcloud_s3, get_object_metadata, fun(_, _, _, _) ->
    							  [
    							   {"last_modified", "Mar, 15 2013"},
    							   {"x-amz-meta-video_id", "3"},
   							   {"x-amz-meta-account_id", "1958"},
    							   {"x-amz-meta-metka", "node-1"},
    							   {"x-amz-meta-test1", "test1"},
    							   {"x-amz-meta-test2", "test2"}
    							  ]
    						  end),
    ?assertEqual({ok, _}, ),
    ?assert(meck:validate(erlcloud_s3)),
    ok.


