-module(loadtest).

-behaviour(gen_server).

%% API functions
-export([start_link/0,
         set_rate/1,
         func_to_be_traced/1]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {rate, tref, tracer, formatter, stable_c}).

-define(max_allowed_mq_len, 8000).

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
    {ok, Ref} = timer:send_interval(10, ?MODULE, invoke),
    PidTracer = whereis(recon_trace_tracer),
    PidFormatter = whereis(recon_trace_formatter),
    {ok, #state{rate = 1, tref = Ref, tracer = PidTracer,
                formatter = PidFormatter, stable_c = 0}}.

%% TimesPerMs is the invoke rate
%% How many times we will invoke the function per millisecond
set_rate(TimesPerMs) ->
    gen_server:call(?MODULE, {set_rate, TimesPerMs}).

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
handle_info(invoke, #state{rate = TimesPerMs, tracer = Tracer,
        formatter = Formatter} = State) ->
    [{message_queue_len, TracerMQLen}] =
        erlang:process_info(Tracer, [message_queue_len]),
    [{message_queue_len, FormatterMQLen}] =
        erlang:process_info(Formatter, [message_queue_len]),
    if TracerMQLen >= ?max_allowed_mq_len orelse FormatterMQLen >= ?max_allowed_mq_len ->
        error_logger:error_msg("Reached Max Allowed Message Queue Len:~w~n"
                               "Rate Now:~w invoke/ms~n", [?max_allowed_mq_len, TimesPerMs]),
        {stop, normal, State};
    true ->
        CurrentRate = State#state.rate,
        CurrentStableCounter = State#state.stable_c,
        {NextRate, NextStableCounter} =
            case CurrentStableCounter of
                25 ->
                    {CurrentRate + 1, 0};
                _ ->
                    {CurrentRate, CurrentStableCounter + 1}
            end,

        [loadtest:func_to_be_traced([]) || _ <- lists:seq(1, TimesPerMs*10)],
        {noreply, State#state{rate = NextRate, stable_c = NextStableCounter} }
    end;

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
terminate(_Reason, #state{tref = TRef}=_State) ->
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
