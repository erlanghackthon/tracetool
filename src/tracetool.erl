%% @author zegang.luo@qq.com
%% @doc @todo Add description to tracetool.


-module(tracetool).

-define(dbname, tracetool_db).

%% ====================================================================
%% API functions
%% ====================================================================
-export([stop/0, start/1, trace/3, clear/0]).

start(ConfFilePath) ->
    trace_mgr:start(),
    error_logger:info_msg("Config Path: ~p~n", [ConfFilePath]),
    {ok, ConfigList} = file:script(ConfFilePath),
    trace_mgr:create_db(?dbname),
    lists:foreach(
        fun({trace, Node, Specs, Max, Options})->
            case Node of
                all ->
                    Nodes = [node()] ++ nodes(),
                    lists:foreach( fun(Node1) -> rpc:call(Node1, tracetool, trace, [Specs, Max, Options]),record_node(Node1) end, Nodes);
                _ ->
                    rpc:call(Node, tracetool, trace, [Specs, Max, Options]),
                    record_node(Node)
            end
        end,
        ConfigList).

trace(Specs, Max, Options) ->
    file_logger:start(),
    trace_mgr:start(),
    case ets:info(?dbname) of
        undefined ->
               trace_mgr:create_db(?dbname);
        _Others ->
                error_logger:info_msg("Node:~p, dbname exists~n", [node()])
    end,
    NewOpts = modify_options_for_recon(Options),
    MatchCount = recon_trace:calls(Specs, Max, NewOpts),
    error_logger:info_msg("Node:~p, ~w lines matched~n", [node(), MatchCount]).

stop() -> 
    case trace_mgr:get_value(?dbname, enodes) of
        {error, _Reason} ->
            ok;
        {ok, Nodes} ->
        lists:foreach(
                fun(Node)->
                    rpc:call(Node, tracetool, clear, [])
                    end,
                Nodes)
    end.

clear() ->
    close_openresource(),
    trace_mgr:destroy_db(?dbname),
    recon_trace:clear().
%% ====================================================================
%% Internal functions
%% ====================================================================
close_openresource() ->
    trace_mgr:close_report().

record_node(Node) ->
     trace_mgr:append_value(?dbname, enodes, Node).

modify_options_for_recon(Options) -> 
    lists:foldl(
      fun(Opt, NewOptions) ->
              case Opt of
                  {logfilepath, LogfilePath} ->
                      file_logger:add_handler([{dir, LogfilePath}, {max_file, 5}]),
                      NewOptions;
                  Opt ->
                      [Opt | NewOptions]
              end
              end, [], Options).
