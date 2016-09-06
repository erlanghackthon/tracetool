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
    msg_trace:info(tracetool,"Config Path: ~w~n", [ConfFilePath]),
    {ok, ConfigList} = file:script(ConfFilePath),
    trace_mgr:create_db(?dbname),
    tt_statistics:start(),
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
    trace_mgr:start(),
    msg_trace:info(tracetool,"strat trace on node: ~w~n", [node()]),
    case ets:info(?dbname) of
        undefined ->
               trace_mgr:create_db(?dbname);
        _Others ->
                msg_trace:info(tracetool,"Node:~p, dbname exists~n", [node()])
    end,
    NewOpts = modify_options_for_recon(Options),
    MatchCount = recon_trace:calls(Specs, Max, NewOpts),
        msg_trace:info(tracetool,"recon_trace:calls(~w, ~w, ~w), MatchCount:~w~n", [Specs, Max, NewOpts, MatchCount]).

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
    msg_trace:info(tracetool,"modify_options_for_recon ~p on ~w ~n", [Options, node()]),
    lists:foldl(
      fun(Opt, NewOptions) ->
              msg_trace:info(tracetool,"Deal with Opt: ~p~n", [Opt]),
              case Opt of
                  {logfilepath, LogfilePath} ->
                      Dev = trace_mgr:open_report(LogfilePath ++ "tracetool_" ++ atom_to_list(node()) ++ ".log"),
                      [{io_server, Dev} | NewOptions];
                  Opt ->
                      [Opt | NewOptions]
              end
              end, [], Options).
