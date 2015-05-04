%%%-------------------------------------------------------------------
%%% @author myang
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 03. May 2015 6:43 PM
%%%-------------------------------------------------------------------
-module(easy_cache).
-author("myang").

%% API
-export([create/2, read/1, delete/1]).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% insert or update data
%%
%% @end
%%--------------------------------------------------------------------
-spec create(Key, Value) -> any() when
  Key :: any(),
  Value :: any().
create(Key, Value) ->
  case ec_route:read(Key) of
    {ok, Pid} ->
      ec_event:update(Key, Value),
      ec_element:update(Pid, Value);
    {error, _Reason} ->
      ec_event:create(Key, Value),
      {ok, Pid} = ec_element:create(Value),
      ec_route:create(Key, Pid)
    end.

%%--------------------------------------------------------------------
%% @doc
%% lookup data
%%
%% @end
%%--------------------------------------------------------------------
-spec read(Key) -> any() when
  Key :: any().
read(Key) ->
  ec_event:read(Key),
  try
    {ok, Pid} = ec_route:read(Key),
    {ok, Value} = ec_element:read(Pid),
    {ok, Value}
  catch
    _Class:_Exception ->
      {error, not_found}
  end.

%%--------------------------------------------------------------------
%% @doc
%% delete data
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(Key) -> any() when
  Key :: any().
delete(Key) ->
  ec_event:delete(Key),
  case ec_route:read(Key) of
    {ok, Pid} ->
      ec_element:delete(Pid);
    {error, _Reason} ->
      ok
  end.


