%%%-------------------------------------------------------------------
%%% @author greg <>
%%% @copyright (C) 2013, greg
%%% @doc
%%%
%%% @end
%%% Created : 19 Mar 2013 by greg <>
%%%-------------------------------------------------------------------
-module(task_manager).

-behaviour(gen_server).


%% API
-export([start_link/0]).

%% gen_server callbacks
-export([init/1, handle_call/3, handle_cast/2, handle_info/2,
	 terminate/2, code_change/3]).

-define(SERVER, ?MODULE). 
-define(TIMER, 30*1000).

-record(state, {
	  s3_auth
	  , bucket
	  , node_id
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
start_link() ->
    gen_server:start_link({local, ?SERVER}, ?MODULE, [], []).


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
init([]) ->
    {ok, Auth} = application:get_env(transcode, s3_auth),
    Data = erlcloud_s3:new(proplists:get_value(access_key_id, Auth),
                           proplists:get_value(secret_access_key, Auth),
                           proplists:get_value(host, Auth)),
    {ok, Bucket} = application:get_env(transcode, s3_bucket),
    {ok, NodeID} = application:get_env(transcode, node_id),
    {ok, _} = task_timer(),
    {ok, #state{s3_auth=Data, bucket=Bucket, node_id=NodeID}}.


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
handle_cast(check_tasks, State) ->
    Objects = erlcloud_s3:list_objects(State#state.bucket, [{prefix, <<"task">>}], State#state.s3_auth),
    tasks_dispatcher(Objects, State),
    {ok, _} = task_timer(),
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
handle_info(_Info, State) ->
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
terminate(_Reason, _State) ->
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

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Iterate over Objects proplist and return tasks list.
%% 
%% @spec tasks_list(Objects :: proplists()) -> [string()]
%% @end
%%--------------------------------------------------------------------
tasks_list(Objects) ->
    tasks_list(proplists:get_value(contents, Objects), []).

tasks_list([Object | TailObjects], Acc) ->
    Task = lists:flatten(proplists:get_value(key, Object)),
    tasks_list(TailObjects, [Task | Acc]);

tasks_list([], Acc) -> Acc.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Функция проверяет наличие 'metka' ключа в метадата проплисте и сверяет
%% с node_id, если они равны, то задача признаётся нашей и мы её отдаём в
%% работу. Если метки нет, то мы её ставим и оставляем задачу до следующей
%% проверке.
%%
%% @spec check_metka(Task :: string(), Metadata :: proplist(), State :: record()) -> 
%%                                      ok | {ok, pid()} | {error, Reason :: atom()}
%% @end
%%--------------------------------------------------------------------
check_metka(Task, Metadata, State) ->
    case proplists:get_value("x-amz-meta-metka", Metadata, false) of
	false -> 
	    utils:set_task_meta("metka", State#state.node_id, {State#state.bucket
							       , Task
							       , State#state.s3_auth
							      }),
	    lager:info("New task ~p found", [Task]),
	    ok;
	Metka ->
	    lager:debug("Found task ~p with metka ~p", [Metka, Task]),
	    case Metka == State#state.node_id of
		true ->
		    %% Here we start task_worker
		    lager:info("This task ~p is mine and we get it", [Task]),
		    task_worker_sup:start_worker(Task);
    		false ->
		    lager:debug("Task ~p is not mine, ignore it", [Task]),
		    ok
	    end
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Проверяет наличие кастомной метки 'timestamp', которую выставляет 
%% task_worker. Если метка есть и она старше чем 5*60 секунд, то считаем
%% что таск не обработан и запкускаем его снова не смотря на метки.
%%
%% @spec check_timestamp(Task :: string(), Metadata :: proplist(), State :: record()) -> 
%%                                         ok  | {ok, pid()} | {error, Reason :: atom()}
%% @end
%%--------------------------------------------------------------------
check_timestamp(Task, Metadata, State) ->
    NTimestamp = utils:get_timestamp(erlang:now()),
    Timestamp = list_to_integer(proplists:get_value("x-amz-meta-timestamp", Metadata, "0")),
    if 
	Timestamp == 0 -> %% setup new timestamp
	    check_metka(Task, Metadata, State);
	NTimestamp - Timestamp > 5*60 -> %% timestamp is very old
	    lager:info("Task ~p worker is dead and we start new worker", [Task]),
	    task_worker_sup:start_worker(Task);
	true -> %% task in work (i.e. timestamp is fresh)
	    lager:debug("Task ~p in work yet", [Task]),
	    ok
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Checks if AccountID in allowed accounts to transcode tasks. If accounts
%% is empty list that means every tasks can be transcoded.
%%
%% @spec check_account(Tasks :: list(), State :: record()) ->
%%                        ok | {ok, pid()} | {error, Reason :: atom()}
%% @end
%%--------------------------------------------------------------------
check_account(Task, Metadata, State) ->
    AccountID = proplists:get_value("x-amz-meta-account_id", Metadata, "0"),
    {ok, Accounts} = application:get_env(transcode, account_ids),
    case Accounts == [] of 
	true -> check_timestamp(Task, Metadata, State);
	false ->
	    case lists:member(AccountID, Accounts) of
		true -> check_timestamp(Task, Metadata, State);
		false -> ok
	    end 
    end.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Функция проверяет наличие ключа 'status' в метадата, если он есть, то
%% считается, что задача выполнена и ждёт удаления. Если ключа нет, то
%% отправляем задачу дальше на провернки.
%%
%% @spec check_tasks(Tasks :: list(), State :: record()) -> 
%%                       ok | {ok, pid()} | {error, Reason :: atom()}
%% @end
%%--------------------------------------------------------------------
check_tasks([Task | Tasks], State) ->
    Metadata = erlcloud_s3:get_object_metadata(State#state.bucket, Task, [], State#state.s3_auth),
    case proplists:get_value("x-amz-meta-status", Metadata, false) of 
	false -> check_account(Task, Metadata, State);
	_ -> 
	    lager:debug("Task ~p has status", [Task]),
	    ok
    end,
    check_tasks(Tasks, State);

check_tasks([], _) -> ok.


%%--------------------------------------------------------------------
%% @private
%% @doc
%% It starts up checkers for keys from Objects proplist().
%%
%% @spec tasks_dispatcher(Objects :: proplist(), State :: record()) -> 
%%                        ok | {ok, pid()} | {error, Reason :: atom()}
%% @end
%%--------------------------------------------------------------------
tasks_dispatcher(Objects, State) ->
    Tasks = tasks_list(Objects),
    check_tasks(Tasks, State).


task_timer() ->
    timer:apply_after(?TIMER, gen_server, cast, [self(), check_tasks]).
