-module(trace_mgr).

-behaviour(gen_server).

%% API functions
-export([start_link/0,
         start/0,
         create_db/1,
         create_db/2,
         destroy_db/1,
         open_report/1,
         close_report/0,
         get_value/2,
         put_value/3,
         append_value/3
         ]).

%% gen_server callbacks
-export([init/1,
         handle_call/3,
         handle_cast/2,
         handle_info/2,
         terminate/2,
         code_change/3]).

-record(state, {fd}).

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
start() ->
    case whereis(?MODULE) of
        undefined ->
            gen_server:start({local, ?MODULE}, ?MODULE, [], []);
        _ ->
            ok
    end.

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
    {ok, #state{}}.

create_db(DBName) ->
    gen_server:call(?MODULE, {create_db, DBName}).
create_db(DBName, Options) when is_list(Options)->
    gen_server:call(?MODULE, {create_db, DBName, Options}).
destroy_db(DBName) ->
    gen_server:call(?MODULE, {destroy_db, DBName}).

open_report(ReportName) ->
    gen_server:call(?MODULE, {open_report, ReportName}).
close_report() ->
    gen_server:call(?MODULE, close_report).

get_value(DBName, Key) ->
    gen_server:call(?MODULE, {get_value, DBName, Key}).
put_value(DBName, Key, Value) ->
    gen_server:call(?MODULE, {put_value, DBName, Key, Value}).
append_value(DBName, Key, Value) ->
    gen_server:call(?MODULE, {append_value, DBName, Key, Value}).
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
handle_call({create_db, DBName}, _From, State) ->
    case ets:info(DBName) of
        undefined ->
            TableName = ets:new(DBName, [public, named_table, compressed]);
        _ ->
            TableName = DBName
    end,    
    {reply, TableName, State};
handle_call({create_db, DBName, Options}, _From, State) ->
    case ets:info(DBName) of
        undefined ->
            TableName = ets:new(DBName, Options);
        _ ->
            TableName = DBName
    end,    
    {reply, TableName, State};
handle_call({destroy_db, DBName}, _From, State) ->
    RtnVar = ets:delete(DBName),
    {reply, RtnVar, State};

handle_call({open_report, ReportName}, _From, State) ->
    filelib:ensure_dir(ReportName),
    {ok, Fd} = file:open(ReportName, [write]),
    file:write(Fd, "-----------Tracing Started---------------"),
    {reply, Fd, State#state{fd = Fd}};
handle_call(close_report, _From, State) ->
    file:close(State#state.fd),
    {reply, ok, State};

handle_call({get_value, DBName, Key}, _From, State) ->
    Reply =
    case ets:info(DBName) of
        undefined ->
            {error, enotable};
        _Other ->
            case ets:lookup(DBName, Key) of
                [] ->
                    {error, enoentry};
                [{Key, Value}] ->
                    {ok, Value}
            end
    end,
    {reply, Reply, State};

handle_call({append_value, DBName, Key, Value}, _From, State) ->
    Reply =
    case ets:info(DBName) of
        undefined ->
            {error, enotable};
        _Other ->
            case ets:lookup(DBName, Key) of
                [] ->
                    ets:insert(DBName, {Key, [Value]});
                [{Key, Values}] when is_list(Values)->
                    NewValue = [Value| Values],
                    case ets:update_element(DBName, Key, {2, NewValue}) of
                        true ->
                            ok;
                        false ->
                            {error, enoentry}
                    end
            end
            
    end,
    {reply, Reply, State};

handle_call({put_value, DBName, Key, Value}, _From, State) ->
    Reply =
    case ets:info(DBName) of
        undefined ->
            {error, enotable};
        _Other ->
            case ets:lookup(DBName, Key) of
                [] ->
                    ets:insert(DBName, {Key, Value});
                [{Key, _}] ->
                    case ets:update_element(DBName, Key, {2, Value}) of
                        true ->
                          ok;
                        false ->
                          {error, enoentry}
                    end
            end
     end,
    {reply, Reply, State};
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

