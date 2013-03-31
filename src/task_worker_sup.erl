%%%-------------------------------------------------------------------
%%% @author greg <>
%%% @copyright (C) 2013, greg
%%% @doc
%%%
%%% @end
%%% Created : 21 Mar 2013 by greg <>
%%%-------------------------------------------------------------------
-module(task_worker_sup).

-behaviour(supervisor).

%% API
-export([start_link/0, start_worker/1]).

%% Supervisor callbacks
-export([init/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the supervisor
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    supervisor:start_link({local, ?SERVER}, ?MODULE, []).


start_worker(Task) ->
    Name = list_to_atom(Task),
    SpecProxy = {Name, {task_worker, start_link, [Task]},
                 transient, 2000, worker, [task_worker]},
    case supervisor:start_child(?SERVER, SpecProxy) of
        {ok, Pid} -> {ok, Pid};
        {error, Error} -> {error, Error}
    end.


%%%===================================================================
%%% Supervisor callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Whenever a supervisor is started using supervisor:start_link/[2,3],
%% this function is called by the new process to find out about
%% restart strategy, maximum restart frequency and child
%% specifications.
%%
%% @spec init(Args) -> {ok, {SupFlags, [ChildSpec]}} |
%%                     ignore |
%%                     {error, Reason}
%% @end
%%--------------------------------------------------------------------
init([]) ->
    {ok, {{one_for_one, 3, 1}, []}}.

%%%===================================================================
%%% Internal functions
%%%===================================================================
