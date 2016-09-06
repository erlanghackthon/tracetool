%% @author zegang.luo@qq.com
%% @doc @todo Add description to tracetool statistics.


-module(tt_statistics).

-define(dbname, tt_statistics).

-behaviour(gen_server).

-export([start_link/0,
         start/0,
         update_amount/1
         ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {amount, lastamount, lasttime, rate}).

%% ====================================================================
%% API functions
%% ====================================================================
start() ->
    case whereis(?MODULE) of
        undefined ->
            gen_server:start({local, ?MODULE}, ?MODULE, [], []);
        _ ->
            ok
    end.

start_link() ->
    gen_server:start_link({local, ?MODULE}, ?MODULE, [], []).

update_amount({Acc, RecordTime}) ->
    gen_server:call(?MODULE, {update_amount, {Acc, RecordTime}}).

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
    trace_mgr:create_db(?dbname),
    trace_mgr:put_value(?dbname, amount, 0),
    {ok, #state{amount=0, lastamount=0, lasttime=erlang:timestamp(), rate=0}}.

handle_call({update_amount, {Acc, RecordTime}}, _From, State) ->
    #state{ amount = Amount, lastamount = LastAmount, lasttime = LastTime} = State,
    NewAmount = Amount + Acc,
    RtnVar = trace_mgr:put_value(?dbname, amount, NewAmount),
    TimeDiff = timer:now_diff(RecordTime, LastTime) / 1000000,
    if
        TimeDiff >= 1 ->
            NewRate = (NewAmount - LastAmount) / TimeDiff,
            trace_mgr:put_value(?dbname, {rate, RecordTime}, NewRate),
            {reply, RtnVar, #state{ amount = NewAmount, lastamount = NewAmount, lasttime = RecordTime, rate = NewRate}};
        true ->
            {reply, RtnVar, State#state{amount = NewAmount}}
    end.

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