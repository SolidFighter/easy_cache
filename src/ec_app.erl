%%%-------------------------------------------------------------------
%%% @author myang
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 21. Apr 2015 8:32 PM
%%%-------------------------------------------------------------------
-module(ec_app).
-author("myang").

-behaviour(application).

%% Application callbacks
-export([start/2,
  stop/1]).

%%%===================================================================
%%% Application callbacks
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application is started using
%% application:start/[1,2], and should start the processes of the
%% application. If the application is structured according to the OTP
%% design principles as a supervision tree, this means starting the
%% top supervisor of the tree.
%%
%% @end
%%--------------------------------------------------------------------
-spec(start(StartType :: normal | {takeover, node()} | {failover, node()},
    StartArgs :: term()) ->
  {ok, pid()} |
  {ok, pid(), State :: term()} |
  {error, Reason :: term()}).
start(_StartType, _StartArgs) ->
  ok = ensure_contact(),
  ec_route:init(),
  case ec_sup:start_link() of
    {ok, Pid} ->
      ec_event_logger:add_handler(),
      {ok, Pid};
    Error ->
      Error
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% This function is called whenever an application has stopped. It
%% is intended to be the opposite of Module:start/2 and should do
%% any necessary cleaning up. The return value is ignored.
%%
%% @end
%%--------------------------------------------------------------------
-spec(stop(State :: term()) -> term()).
stop(_State) ->
  ok.

%%%===================================================================
%%% Internal functions
%%%===================================================================

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Query contact nodes
%%
%% @end
%%--------------------------------------------------------------------
-spec ensure_contact() -> any().
ensure_contact() ->
  DefaultNodes = ['contact1@localhost', 'contact2@localhost'],
  case get_env(easy_cache, contact_nodes, DefaultNodes) of
    [] ->
      {error, no_contact_nodes};
    ContactNodes ->
      ensure_contact(ContactNodes)
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Ping contact nodes
%%
%% @end
%%--------------------------------------------------------------------
-spec ensure_contact(ContactNodes) -> any() when
  ContactNodes :: list().
ensure_contact(ContactNodes) ->
  Answering = [N || N <- ContactNodes, net_adm:ping(N) =:= pong],
  case Answering of
    [] ->
      {error, no_contact_nodes_reachable};
    _ ->
      DefaultTime = 6000,
      WaitTime = get_env(easy_cache, wait_time, DefaultTime),
      wait_for_nodes(length(Answering), WaitTime)
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Wait for nodes.
%%
%% @end
%%--------------------------------------------------------------------
wait_for_nodes(MinNodes, WaitTime) ->
  Slices = 10,
  SliceTime = round(WaitTime/Slices),
  wait_for_nodes(MinNodes, SliceTime, Slices).

wait_for_nodes(_MinNodes, _SliceTime, 0) ->
  ok;
wait_for_nodes(MinNodes, SliceTime, Iterations) ->
  case length(nodes()) > MinNodes of
    true ->
      ok;
    false ->
      timer:sleep(SliceTime),
      wait_for_nodes(MinNodes, SliceTime, Iterations - 1)
  end.

%%--------------------------------------------------------------------
%% @private
%% @doc
%% Get config information.
%%
%% @end
%%--------------------------------------------------------------------
get_env(AppName, Key, Default) ->
  case application:get_env(AppName, Key) of
    undefined -> Default;
    {ok, Value} -> Value
  end.
