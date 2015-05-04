%%%-------------------------------------------------------------------
%%% @author myang
%%% @copyright (C) 2015, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 04. May 2015 8:01 PM
%%%-------------------------------------------------------------------
-module(ec_event).
-author("myang").

%% API
-export([start_link/0, add_handler/2, delete_handler/2, read/1, create/2, update/2, delete/1]).

-define(SERVER, ?MODULE).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% start gen_event process
%%
%% @end
%%--------------------------------------------------------------------
-spec start_link() -> any().
start_link() ->
  gen_event:start_link({local, ?SERVER}).

%%--------------------------------------------------------------------
%% @doc
%% add handler
%%
%% @end
%%--------------------------------------------------------------------
-spec add_handler(Handler, Args) -> any() when
  Handler :: any(),
  Args :: any().
add_handler(Handler, Args) ->
  gen_event:add_handler(?SERVER, Handler, Args).

%%--------------------------------------------------------------------
%% @doc
%% delete handler
%%
%% @end
%%--------------------------------------------------------------------
-spec delete_handler(Handler, Args) -> any() when
  Handler :: any(),
  Args :: any().
delete_handler(Handler, Args) ->
  gen_event:delete_handler(?SERVER, Handler, Args).

%%--------------------------------------------------------------------
%% @doc
%% notify handler read event
%%
%% @end
%%--------------------------------------------------------------------
-spec read(Key) -> any() when
  Key :: any().
read(Key) ->
  gen_event:notify(?SERVER, {read, Key}).

%%--------------------------------------------------------------------
%% @doc
%% notify handler create event
%%
%% @end
%%--------------------------------------------------------------------
-spec create(Key, Value) -> any() when
  Key :: any(),
  Value :: any().
create(Key, Value) ->
  gen_event:notify(?SERVER, {create, {Key, Value}}).

%%--------------------------------------------------------------------
%% @doc
%% notify handler update event
%%
%% @end
%%--------------------------------------------------------------------
-spec update(Key, Value) -> any() when
  Key :: any(),
  Value :: any().
update(Key, Value) ->
  gen_event:notify(?SERVER, {update, {Key, Value}}).

%%--------------------------------------------------------------------
%% @doc
%% notify handler delete event
%%
%% @end
%%--------------------------------------------------------------------
-spec delete(Key) -> any() when
  Key :: any().
delete(Key) ->
  gen_event:notify(?SERVER, {delete, Key}).