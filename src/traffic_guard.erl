-module(traffic_guard).
-author('andrzej.trawinski@jtendo.com').

%% This module is a simple token bucket implementation
%% for limiting a requests flow to a user defined rate limit.
%% Each request corresponds to one token.
%%
%% The rate of requests is measured over given interval.
%%
%% There is a total amount of requests allowed to pass for the flow during
%% each interval. When all tokens allowed per interval are consumed the request
%% is rejected.

-behaviour(gen_server).

%% API
-export([start_link/3]).

-export([check/1,
         reset/1,
         info/1,
         stop/1]).

%% gen_server callbacks
-export([init/1,
        handle_call/3,
        handle_cast/2,
        handle_info/2,
        terminate/2,
        code_change/3]).

-type timestamp() :: integer().

-record(state, {tokens       :: integer(),  % number tokens in bucket (at last update)
                                            % this variable should never
                                            % be used without previous
                                            % call to update_tokens()
                interval     :: integer(),  % time interval for specified limit
                limit        :: integer(),  % max number of tokens
                last_update  :: timestamp() % last time number of tokens was updated
               }).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

-spec check(atom() |  pid()) -> ok | reject | {error, timeout}.
%%
check(Ref) when is_atom(Ref); is_pid(Ref) ->
    gen_server:call(Ref, check, infinity).

%% Reset the token counter
-spec reset(atom() |  pid()) -> ok | {error, timeout}.
%%
reset(Ref) when is_atom(Ref); is_pid(Ref) ->
    gen_server:call(Ref, reset, infinity).

-spec info(atom() |  pid()) -> {ok, non_neg_integer(), pos_integer(), pos_integer()} | {error, timeout}.
%%
info(Ref) when is_atom(Ref); is_pid(Ref) ->
    gen_server:call(Ref, info, infinity).

-spec stop(atom() | pid()) -> ok.
%%
stop(Ref) when is_atom(Ref); is_pid(Ref) ->
    gen_server:cast(Ref, stop).

%-spec start_link(atom(), pos_integer() | infinity, non_neg_integer()) -> ok.
-spec start_link(atom(), pos_integer(), pos_integer()) -> {ok, pid()}.
%%
start_link(Name, Limit, Interval) when is_atom(Name),
                             is_integer(Limit), Limit > 0,
                             is_integer(Interval), Interval > 0 ->
    gen_server:start_link({local, Name}, ?MODULE, {Limit, Interval}, []).

%%%===================================================================
%%% gen_server callbacks
%%%===================================================================

-spec init({pos_integer(), pos_integer()}) -> {ok, #state{}}.
%
init({Limit, Interval}) ->
    State = #state{
        tokens = Limit,
        limit = Limit,
        interval = Interval,
        last_update = erlang:now()
    },
    {ok, State}.

%%--------------------------------------------------------------------
%% Handling call messages
%%--------------------------------------------------------------------
handle_call(check, _From, State) ->
    {ok, TS, Tokens} = update_tokens(State),
    case Tokens > 0 of
        true -> {reply, ok, State#state{last_update = TS, tokens = Tokens - 1}};
        false -> {reply, reject, State#state{last_update = TS, tokens = Tokens}}
    end;
handle_call(reset, _From, State) ->
    NewState = State#state{
        tokens = State#state.limit,
        last_update = erlang:now()
    },
    {reply, ok, NewState};
handle_call(info, _From, State) ->
    {ok, TS, Tokens} = update_tokens(State),
    NewState = State#state{
        tokens = Tokens,
        last_update = TS
    },
    {reply, {ok, Tokens, NewState#state.limit, NewState#state.interval}, NewState};
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

-spec update_tokens(term()) -> {ok, timestamp(), integer()}.
%
update_tokens(State) ->
    %io:format("Current tokens: ~p~n", [State#state.tokens]),
    TS = erlang:now(),
    Diff = timer:now_diff(TS, State#state.last_update),
    NewTokens = trunc((Diff * State#state.limit) / (1000000 * State#state.interval)),
    Tokens = erlang:min(State#state.limit, NewTokens + State#state.tokens),
    %io:format("Updated tokens: ~p~n", [Tokens]),
    {ok, TS, Tokens}.

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
    { "Verifies that the server can reject too many requests",
        {foreach,
            fun setup/0,
            fun teardown/1,
            [
                {"accept only 4 reqs on 3 req/s limit",
                    fun() ->
                            Pid = whereis(?TEST_PROC),
                            ?assertMatch([ok,ok,ok,ok,reject],
                                [make_check(Pid, 0) || _ <- lists:seq(1, 5)])
                    end
                },
                {"accept all 3 reqs on 3 req/s limit",
                    fun() ->
                            Pid = whereis(?TEST_PROC),
                            ?assertMatch([ok,ok,ok,ok,ok],
                                [make_check(Pid, 350) || _ <- lists:seq(1, 5)])
                    end
                }
            ]
        }}.

setup() ->
    {ok, Pid} = start_link(?TEST_PROC, 4, 1),
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

