-module(loadtest).

-behaviour(gen_server).

%% API functions
-export([start_link/0,
         stop/0,
         set_rate/1,
         func_to_be_traced/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {rate, tref, tracer, formatter, stable_c,
            report_fd}).

-define(max_allowed_mq_len, 8000).
-define(stable_mq_len, 100).
-define(fine_tuning_var, 0.1).
-define(quick_ramp_var, 1).
-define(invoke_interval, 10). % in milliseconds
-define(report_path, "/tmp/loadtest.report"). % in milliseconds

%%%===================================================================
%%% API functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% Starts the server
%%
%% @spec start_link() -> {ok, Pid} | ignore | {error, Error}
%% @end
%%--------------------------------------------------------------------
start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

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
    % initalize the performance report
    {ok, Fd} = file:open(?report_path, [write]),
    io:fwrite(Fd, "##-----------Load Test Report For Erlang Tracing Tool------------~n", []),
    io:fwrite(Fd, "~-30.25s~-30.25s~-30.25s~n", ["InvokingRate (times/m)", "MQLenTracer", "MQLenFormatter"]),
    % measure over-loaded process every 10 ms
    {ok, Ref} = timer:send_interval(?invoke_interval, ?MODULE, invoke),
    PidTracer = whereis(recon_trace_tracer),
    PidFormatter = whereis(recon_trace_formatter),
    % rate = times of invoke per millisecond
    % initial rate is 1 invoke per ms
    {ok, #state{rate = 1, tref = Ref, tracer = PidTracer,
                formatter = PidFormatter, stable_c = 0,
                report_fd = Fd
                }}.

%% TimesPerMs is the invoke rate
%% How many times we will invoke the function per millisecond
set_rate(TimesPerMs) ->
    gen_server:call(?MODULE, {set_rate, TimesPerMs}).

stop() ->
    gen_server:cast(?MODULE, stop).

func_to_be_traced(_Arg) ->
    764543 + 676887.
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
handle_call({set_rate, 0}, _From, #state{tref = TRef} = State) ->
    timer:cancel(TRef),
    {reply, ok, State#state{rate = 0}};

handle_call({set_rate, TimesPerMs}, _From, State) ->
    {reply, ok, State#state{rate = TimesPerMs, stable_c = 0}};

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
handle_cast(stop, State) ->
    error_logger:info_msg("stop loadtest service~n", []),
    {stop, normal, State};
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
handle_info(invoke, #state{rate = 0} = State) ->
    {stop, normal, State};

handle_info(invoke, #state{rate = CurrentRate,
                stable_c = CurrentStableCounter,
                tracer = Tracer, formatter = Formatter,
                report_fd = Fd
                } = State) ->

    [{message_queue_len, TracerMQLen}] =
        erlang:process_info(Tracer, [message_queue_len]),
    [{message_queue_len, FormatterMQLen}] =
        erlang:process_info(Formatter, [message_queue_len]),

    io:fwrite(Fd, "~-30.25w~-30.25w~-30.25w~n", [trunc(CurrentRate*1000), TracerMQLen, FormatterMQLen]),

    [loadtest:func_to_be_traced([]) || _ <- lists:seq(1, trunc(CurrentRate*10)) ],

    NextState =

    if
        TracerMQLen >= ?max_allowed_mq_len orelse FormatterMQLen >= ?max_allowed_mq_len ->
            error_logger:error_msg("Reached Max Allowed Message Queue Len:~w~n"
                    "Rate Now:~w invoke/s~n,"
                    "TracerMQLen:~w, FromatterMQLen:~w~n"
                    , [?max_allowed_mq_len, CurrentRate*1000, TracerMQLen, FormatterMQLen]),
            State#state{rate = 0, stable_c = 0};

        TracerMQLen < ?max_allowed_mq_len andalso FormatterMQLen < ?max_allowed_mq_len->
            {NextRate, NextStableCounter} =
                case CurrentStableCounter of
                    25 ->
                        {CurrentRate + ?fine_tuning_var, 0};
                    _ ->
                        {CurrentRate, CurrentStableCounter + 1}
                end,

            State#state{rate = NextRate, stable_c = NextStableCounter}
    end,


    {noreply, NextState};

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
terminate(_Reason, #state{report_fd = Fd, tref = TRef}=_State) ->
    io:fwrite(Fd, "##-----------End of Load Test Report ------------~n", []),
    error_logger:info_msg("generating loadtest report...~n", []),
    os:cmd("./ebin/plot.pg > ./loadtest.png"),
    timer:cancel(TRef),
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
