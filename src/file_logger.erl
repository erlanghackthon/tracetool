%% @author ezijixu
%% @doc @todo Add description to file_logger.


-module(file_logger).
-behaviour(gen_event).

-include_lib("kernel/include/file.hrl").

-define(FILE_OPTIONS,[write, delayed_write, raw]).

-export([init/1, handle_event/2, handle_call/2, handle_info/2, terminate/2, code_change/3]).

%% ====================================================================
%% API functions
%% ====================================================================
-export([start/0, add_handler/1, send_event/1]).

start() ->
	gen_event:start_link({local, ?MODULE}).

add_handler(Args) ->
	gen_event:add_handler(?MODULE, ?MODULE, Args).

send_event(Msg) ->
	gen_event:notify(?MODULE, Msg).

%% ====================================================================
%% Behavioural functions
%% ====================================================================
-record(state, { 
			dir,
			file_name,
			max_files, 
			max_size, 
			fd
  		}).

%% init/1
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:init-1">gen_event:init/1</a>
-spec init(InitArgs) -> Result when
	InitArgs :: Args | {Args, Term :: term()},
	Args :: term(),
	Result :: {ok, State}
			| {ok, State, hibernate}
			| {error, Reason :: term()},
	State :: term().
%% ====================================================================
init(Args) ->
	Dir = proplists:get_value(dir, Args, "."),
	MaxFiles = proplists:get_value(max_files, Args, 5),
	MaxSize = proplists:get_value(max_size, Args, 100*1024*1024),
	FileName = "tracetool_" ++ atom_to_list(node()) ++ ".log",
	File = filename:join(Dir, FileName),
	filelib:ensure_dir(File),
    Fd = file:open(File, ?FILE_OPTIONS),
	State = #state{
			dir = Dir,
			file_name = FileName,
			max_files = MaxFiles,
			max_size = MaxSize,
			fd = Fd},
    {ok, State}.


%% handle_event/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:handle_event-2">gen_event:handle_event/2</a>
-spec handle_event(Event :: term(), State :: term()) -> Result when
	Result :: {ok, NewState}
			| {ok, NewState, hibernate}
			| {swap_handlers, Args1, NewState, Handler2, Args2}
			| remove_handler,
	NewState :: term(), Args1 :: term(), Args2 :: term(),
	Handler2 :: Module2 | {Module2, Id :: term()},
	Module2 :: atom().
%% ====================================================================
handle_event(Msg, State) ->
	Fd = State#state.fd,
	file:write(Fd, Msg),
	check_rotation(State),
    {ok, State}.


%% handle_call/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:handle_call-2">gen_event:handle_call/2</a>
-spec handle_call(Request :: term(), State :: term()) -> Result when
	Result :: {ok, Reply, NewState}
			| {ok, Reply, NewState, hibernate}
			| {swap_handler, Reply, Args1, NewState, Handler2, Args2}
			| {remove_handler, Reply},
	Reply :: term(),
	NewState :: term(), Args1 :: term(), Args2 :: term(),
	Handler2 :: Module2 | {Module2, Id :: term()},
	Module2 :: atom().
%% ====================================================================
handle_call(_Request, State) ->
    Reply = ok,
    {ok, Reply, State}.


%% handle_info/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:handle_info-2">gen_event:handle_info/2</a>
-spec handle_info(Info :: term(), State :: term()) -> Result when
	Result :: {ok, NewState}
			| {ok, NewState, hibernate}
			| {swap_handler, Args1, NewState, Handler2, Args2}
			| remove_handler,
	NewState :: term(), Args1 :: term(), Args2 :: term(),
	Handler2 :: Module2 | {Module2, Id :: term()},
	Module2 :: atom().
%% ====================================================================
handle_info(_Info, State) ->
    {ok, State}.


%% terminate/2
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:terminate-2">gen_event:terminate/2</a>
-spec terminate(Arg, State :: term()) -> term() when
	Arg :: Args
		| {stop, Reason}
		| stop
		| remove_handler
		| {error, {'EXIT', Reason}}
		| {error, Term :: term()},
	Args :: term(), Reason :: term().
%% ====================================================================
terminate(_Arg, _State) ->
    ok.


%% code_change/3
%% ====================================================================
%% @doc <a href="http://www.erlang.org/doc/man/gen_event.html#Module:code_change-3">gen_event:code_change/3</a>
-spec code_change(OldVsn, State :: term(), Extra :: term()) -> {ok, NewState :: term()} when
	OldVsn :: Vsn | {down, Vsn},
	Vsn :: term().
%% ====================================================================
code_change(_OldVsn, State, _Extra) ->
    {ok, State}.


%% ====================================================================
%% Internal functions
%% ====================================================================
check_rotation(State) ->
	#state{dir=Dir, file_name=FileName, max_size=MaxSize} = State,
	FilePath = filename:join(Dir, FileName ++ ".log"),
	{ok, FileInfo} = file:read_file_info(FilePath),
	Size = FileInfo#file_info.size,
    if 
		Size > MaxSize ->
			NewFd = rotate_file(State),
			NewState = State#state{fd = NewFd},
			NewState;
		true ->
			State
	end.

rotate_file(State) ->
	#state{dir=Dir, file_name=FileName, fd=Fd, max_files=RotNum} = State,
	file:close(Fd),
	FileName = filename:join(Dir, FileName),
	rotate_file_name(FileName, RotNum-1),
	{ok, Fd2} = file:open(FileName, ?FILE_OPTIONS),
	Fd2.

rotate_file_name(FileName, 0) ->
	file:rename(FileName, FileName ++ ".1");

rotate_file_name(FileName, Index) when index > 0 ->
	file:rename(FileName ++ "." ++ integer_to_list(Index), 
		FileName ++ "." ++ integer_to_list(Index+1)),
	rotate_file_name(FileName, Index-1).
	

