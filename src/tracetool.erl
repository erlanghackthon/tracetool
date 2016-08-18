%% @author ezegluo
%% @doc @todo Add description to tracetool.


-module(tracetool).

-define(dbname, tracetool_db).

%% ====================================================================
%% API functions
%% ====================================================================
-export([stop/0, start/1, trace/3, clear/0]).

start(ConfFilePath) ->
    {ok, ConfigList} = file:consult(ConfFilePath),
	ets:new(?dbname, [public, named_table, compressed]),
	lists:foreach(
	  fun({trace, Node, Specs, Max, Options})->
			  rpc:call(Node, tracetool, trace, [Specs, Max, Options]),
			  record_node([Node])
	  end,
	  ConfigList).

trace(Specs, Max, Options) ->
	case ets:info(?dbname) of
		undefined ->
			ets:new(?dbname, [public, named_table, compressed])
	end,
	NewOpts = modify_options_for_recon(Options),
	recon_trace:calls(Specs, Max, NewOpts).

stop() -> 
   %% Nodes = ets:lookup(?dbname, enodes),
	case ets:lookup(?dbname, enodes) of
		[] ->
            ok;
        [{enodes, Nodes}] ->
			lists:foreach(
	            fun(Node)->
			        rpc:call(Node, tracetool, clear, []),
			        remove_node(Node)
	                end,
	            Nodes)
	end.



clear() ->
	close_openresource(),
        ets:delete(?dbname),
	recon_trace:clear().
%% ====================================================================
%% Internal functions
%% ====================================================================
close_openresource() ->
    case ets:lookup(?dbname, handlers) of
		[] ->
			ok;
		[{handlers, Handlers}] - >
			lists:foreach(
		        fun(Handler) -> 
						case Handler of
							{logfile, Dev} ->
								file:close(Dev);
							true ->
								ok
						end
				end,
			Handlers)
    end.

record_node(Node) ->
 %%   Nodes = ets:lookup(?dbname, enodes),
	case ets:lookup(?dbname, enodes) of
		[] ->
            ets:insert(?dbname, {enodes, [Node]});
        [{enodes, Nodes}] ->
			NewNodes = Nodes ++ Node,
			ets:insert(?dbname, {enodes, NewNodes})
	end.

remove_node(Node) ->
  %%  Nodes = ets:lookup(?dbname, enodes),
	case ets:lookup(?dbname, enodes) of
		[] ->
            ok;
        [{enodes, Nodes}] ->
			NewNodes = Nodes -- Node,
			ets:insert(?dbname, {enodes, NewNodes})
	end.

%%% Store handler of opened resources in DB
record_handler(Handler) ->
%%	Handlers = ets:lookup(?dbname, handlers),
	case ets:lookup(?dbname, handlers) of
		[] ->
            ets:insert(?dbname, {handlers, [Handler]});
        [{handlers, Handlers}] ->
			NewHandlers = Handlers ++ Handler,
			ets:insert(?dbname, {handlers, NewHandlers})
	end.

remove_handler(Handler) ->
%%    Handlers = ets:lookup(?dbname, handlers),
	case ets:lookup(?dbname, handlers) of
		[] ->
            ok;
        [{handlers, Handlers}] ->
			NewHandlers = Handlers -- Handler,
			ets:insert(?dbname, {handlers, NewHandlers})
	end.

modify_options_for_recon(Options) -> 
	lists:foldl(
	  fun(Opt, NewOptions) ->
			  case Opt of
				  {logfilepath, LogfilePath} ->
					  {ok, Dev} = file:open(LogfilePath ++ "tracetool_" ++ atom_to_list(node()) ++ ".log",[write]),
                                          file:write(Dev, "-----------Trace toll started---------------"),
					  NewOptions ++ [{io_server, Dev}],
					  record_handler([{logfile, Dev}]);
				  Opt ->
					  NewOptions ++ [Opt]
			  end
	  end, [], Options).
