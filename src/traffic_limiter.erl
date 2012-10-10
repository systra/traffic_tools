-module(traffic_limiter).
-author('andrzej.trawinski@jtendo.com').

%% This module is a simple leaky bucket implementation
%% for limiting a requests flow to a user defined rate limit.
%% Each request corresponds to one token.
%%
%% The rate of requests is measured over one second interval.
%%

-behaviour(gen_server).

%% API
-export([start_link/2,
         start_link/3]).

-export([check/1,
         stop/1]).

%% gen_server callbacks
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-type timestamp() :: integer().
-type action_type() :: reject | wait.

-record(state, {limit           :: integer(),
                min_interval    :: integer(),
                latest_request  :: timestamp(),
                action          :: action_type()
               }).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

-spec check(atom() |  pid()) -> ok | reject | {error, timeout}.
%%
check(Ref) when is_atom(Ref); is_pid(Ref) ->
    gen_server:call(Ref, check, infinity).

-spec stop(atom() | pid()) -> ok.
%%
stop(Ref) when is_atom(Ref); is_pid(Ref) ->
    gen_server:cast(Ref, stop).

-spec start_link(atom(), pos_integer()) -> {ok, pid()}.
%%
start_link(Name, Limit) when is_atom(Name),
                             is_integer(Limit), Limit > 0 ->
    start_link(Name, Limit, reject).

-spec start_link(atom(), pos_integer(), action_type()) -> {ok, pid()}.
%
start_link(Name, Limit, Action) when is_integer(Limit), Limit > 0,
                                     is_atom(Action),
                                     (Action == reject orelse Action == wait) ->
    gen_server:start_link({local, Name}, ?MODULE, {Limit, Action}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init({integer(), action_type()}) -> {ok, #state{}}.
%
init({Limit, Action}) ->
    State = #state{
                limit = Limit,
                min_interval = trunc(1000000 / Limit),
                action = Action,
                latest_request = {0, 0, 0} % Empty timestamp
            },
    {ok, State}.

%%--------------------------------------------------------------------
%% Handling call messages
%%--------------------------------------------------------------------
handle_call(check, _From, State) ->
    TS = now(),
    Diff = timer:now_diff(TS, State#state.latest_request),
    %io:format("Diff: ~p~n", [Diff]),
    {Reply, NewState} = case {Diff < State#state.min_interval, State#state.action} of
        {true, reject} -> {reject, State};
        {true, wait} ->
            Wait = trunc((State#state.min_interval - Diff) / 1000),
            %io:format("Sleeping for ~p ms~n", [Wait]),
            timer:sleep(Wait),
            {ok, State#state{latest_request = TS}};
        {false, _} -> {ok, State#state{latest_request = TS}}
    end,
    {reply, Reply, NewState};
handle_call(_Request, _From, State) ->
    {reply, ok, State}.

%%--------------------------------------------------------------------
%% Handling cast messages
%%--------------------------------------------------------------------
handle_cast(stop, State) ->
    {stop, normal, State};
handle_cast(_Msg, State) ->
    {noreply, State}.

%%--------------------------------------------------------------------
%% Handling all non call/cast messages
%%--------------------------------------------------------------------
handle_info(_Info, State) ->
    {noreply, State}.

terminate(_Reason, _State) ->
    ok.

code_change(_OldVsn, State, _Extra) ->
    {ok, State}.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% Eunit tests
%%--------------------------------------------------------------------

-ifdef(TEST).

-define(TEST_PROC, test_proc).

-include_lib("eunit/include/eunit.hrl").

make_check(Ref, Sleep) ->
    timer:sleep(Sleep),
    check(Ref).

limit_test_() ->
    { "Verifies that the server can reject request coming too fast",
        {foreach,
            fun setup_reject/0,
            fun teardown/1,
            [
                {"accept only first",
                    fun() ->
                            Pid = whereis(?TEST_PROC),
                            ?assertMatch([ok,reject,reject,reject,reject], [make_check(Pid, 0) || _ <- lists:seq(1, 5)])
                    end
                },
                {"accept some",
                    fun() ->
                            Pid = whereis(?TEST_PROC),
                            ?assertMatch([ok,reject,ok,reject,ok], [make_check(Pid, 100) || _ <- lists:seq(1, 5)])
                    end
                },
                {"accept all",
                    fun() ->
                            Pid = whereis(?TEST_PROC),
                            ?assertMatch([ok,ok,ok,ok,ok], [make_check(Pid, 200) || _ <- lists:seq(1, 5)])
                    end
                }
            ]
        }}.

wait_test_() ->
    { "Verifies that the server can slow down request coming too fast",
        {foreach,
            fun setup_wait/0,
            fun teardown/1,
            [
                {"slow down all",
                    fun() ->
                            Pid = whereis(?TEST_PROC),
                            ?_assertMatch([ok,ok,ok,ok,ok], [check(Pid) || _ <- lists:seq(1, 5)])
                    end
                }
            ]
        }}.

setup_reject() ->
    setup(reject).

setup_wait() ->
    setup(wait).

setup(Type) ->
    {ok, Pid} = start_link(?TEST_PROC, 5, Type),
    Pid.

teardown(_) ->
    case whereis(?TEST_PROC) of
        undefined -> ok;
        Pid -> exit_and_wait(Pid)
    end.

exit_and_wait(Pid) ->
    MRef = erlang:monitor(process, Pid),
    stop(Pid),
    receive
        {'DOWN', MRef, _, _, _} -> ok
    end.

-endif.

