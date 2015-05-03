%%%-------------------------------------------------------------------
%%% @author myang
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. May 2015 6:17 PM
%%%-------------------------------------------------------------------
-module(ec_route).
-author("myang").

%% API
-export([init/0, create/2, read/1, delete/1]).

-define(TABLE_ID, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% init ets to store route data
%%
%% @end
%%--------------------------------------------------------------------
-spec init() -> atom().
init() ->
  ets:new(?TABLE_ID, [public, named_table]),
  ok.

%%--------------------------------------------------------------------
%% @doc
%% insert or update route data
%%
%% @end
%%--------------------------------------------------------------------
-spec create(Key, Pid) -> any() when
  Key :: any(),
  Pid :: pid().
create(Key, Pid) ->
  ets:insert(?TABLE_ID, {Key, Pid}).


%%--------------------------------------------------------------------
%% @doc
%% lookup route data
%%
%% @end
%%--------------------------------------------------------------------
-spec read(Key) -> any() when
  Key :: any().
read(Key) ->
  case ets:lookup(?TABLE_ID, Key) of
    [{Key, Pid}] ->
      {ok, Pid};
    [] ->
      {error, not_found}
  end.

%%--------------------------------------------------------------------
%% @doc
%% delete route data
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(Pid) -> any() when
  Pid :: pid().
delete(Pid) ->
  ets:match_delete(?TABLE_ID, {'_', Pid}).
