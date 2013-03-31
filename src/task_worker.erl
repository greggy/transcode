%%%-------------------------------------------------------------------
%%% @author greg <>
%%% @copyright (C) 2013, greg
%%% @doc
%%%
%%% @end
%%% Created : 19 Mar 2013 by greg <>
%%%-------------------------------------------------------------------
-module(task_worker).

-behaviour(gen_server).

%% API
-export([start_link/1]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(TIMER, 10*1000).

-record(state, {
	  task
	  , s3_auth
	  , bucket
	  , num_tries
}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link(Task) ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [Task], []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Initializes the server
%%
%% @spec init(Args) -> {ok, State} |
%%                     {ok, State, Timeout} |
%%                     ignore |
%%                     {stop, Reason}
%% @end
%%--------------------------------------------------------------------
init([Task]) ->
    lager:debug("Start worker for task ~p", [Task]),
    {ok, Auth} = application:get_env(transcode, s3_auth),
    Data = erlcloud_s3:new(proplists:get_value(access_key_id, Auth),
                           proplists:get_value(secret_access_key, Auth),
                           proplists:get_value(host, Auth)),
    {ok, Bucket} = application:get_env(transcode, s3_bucket),
    {ok, NumTries} = application:get_env(transcode, num_tries),
    ok = gen_server:cast(?SERVER, start_transcode),
    {ok, _} = timer_timestamp(),
    {ok, #state{task=Task, bucket=Bucket, s3_auth=Data, num_tries=NumTries}}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling call messages
%%
%% @spec handle_call(Request, From, State) ->
%%                                   {reply, Reply, State} |
%%                                   {reply, Reply, State, Timeout} |
%%                                   {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, Reply, State} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_call(_Request, _From, State) ->
    Reply = ok,
    {reply, Reply, State}.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling cast messages
%%
%% @spec handle_cast(Msg, State) -> {noreply, State} |
%%                                  {noreply, State, Timeout} |
%%                                  {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_cast(start_transcode, #state{bucket=Bucket, task=Task, s3_auth=Auth}=State) ->
    Metadata = erlcloud_s3:get_object_metadata(Bucket, Task, [], Auth),
    VideoID = proplists:get_value("x-amz-meta-video_id", Metadata),
    AccountID = proplists:get_value("x-amz-meta-account_id", Metadata),
    {ok, StoragePath} = application:get_env(transcode, storage_path),
    FLVPath = utils:get_flv_path(AccountID, VideoID, StoragePath),
    HLSPath = case proplists:get_value("x-amz-meta-hls", Metadata) of
		  "NO" -> "NO";
		  _ -> utils:get_hls_folder(AccountID, VideoID, StoragePath)
	      end,
    FragFolder = utils:get_frag_folder(AccountID, VideoID, StoragePath),
    {ok, Script} = application:get_env(transcode, script_name),
    Command = filename:join([code:priv_dir(transcode), Script]),
    lager:debug("Command args ~p ~p ~p ~p ~p ~p ~p", [AccountID, VideoID, FLVPath, HLSPath,
						      StoragePath, Bucket, FragFolder]),
    _Pid = erlang:open_port({spawn_executable, Command}, [exit_status, {args, [AccountID, VideoID, FLVPath,
									       HLSPath, StoragePath,
									       Bucket, FragFolder]}]),
    {noreply, State};

handle_cast(set_timestamp, State) ->
    Timestamp = utils:get_timestamp(erlang:now()),
    utils:set_task_meta("timestamp", integer_to_list(Timestamp),
			{State#state.bucket
			 , State#state.task
			 , State#state.s3_auth}),
    {ok, _} = timer_timestamp(),
    {noreply, State};

handle_cast(_Msg, State) ->
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Handling all non call/cast messages
%%
%% @spec handle_info(Info, State) -> {noreply, State} |
%%                                   {noreply, State, Timeout} |
%%                                   {stop, Reason, State}
%% @end
%%--------------------------------------------------------------------
handle_info({_Port, {exit_status, Status}}, State) when Status == 0 ->
    utils:set_task_meta("status", "READY", {State#state.bucket
					    , State#state.task
					    , State#state.s3_auth}),
    {stop, normal, State};

handle_info({_Port, {exit_status, Status}}, State) when State#state.num_tries == 1 ->
    lager:debug("Status ~p", [Status]),
    utils:set_task_meta("status", "ERROR", {State#state.bucket
					    , State#state.task
					    , State#state.s3_auth}),
    lager:info("Task ~p was stoped with status ~p", [State#state.task, Status]),
    {stop, normal, State};

handle_info({_Port, {exit_status, _Status}}, State) ->
    lager:info("Task ~p wasn't done and will try again", [State#state.task]),
    {ok, _} = timer:apply_after(10*1000, gen_server, cast, [self(), start_transcode]),
    {noreply, State#state{num_tries=State#state.num_tries - 1}};

handle_info(_Info, State) ->
    lager:info("Unknown info ~p", [_Info]),
    {noreply, State}.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called by a gen_server when it is about to
%% terminate. It should be the opposite of Module:init/1 and do any
%% necessary cleaning up. When it returns, the gen_server terminates
%% with Reason. The return value is ignored.
%%
%% @spec terminate(Reason, State) -> void()
%% @end
%%--------------------------------------------------------------------
terminate(_Reason, State) ->
    lager:debug("Task was finished ~p", [State#state.task]),
    ok.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Convert process state when code is changed
%%
%% @spec code_change(OldVsn, State, Extra) -> {ok, NewState}
%% @end
%%--------------------------------------------------------------------
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

timer_timestamp() ->
    timer:apply_after(?TIMER, gen_server, cast, [self(), set_timestamp]).
