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

-record(key_to_pid, {key, pid}).

%%%===================================================================
%%% API
%%%===================================================================

%%--------------------------------------------------------------------
%% @doc
%% init schema to store route data
%%
%% @end
%%--------------------------------------------------------------------
-spec init() -> atom().
init() ->
  mnesia:start(),
  mnesia:create_table(key_to_pid,
    [{index, [pid]}, {attributes, record_info(fields, key_to_pid)}]),
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
  mnesia:dirty_write(#key_to_pid{key = Key, pid = Pid}).

%%--------------------------------------------------------------------
%% @doc
%% lookup route data
%%
%% @end
%%--------------------------------------------------------------------
-spec read(Key) -> any() when
  Key :: any().
read(Key) ->
  case mnesia:dirty_read(key_to_pid, Key) of
    [{key_to_pid, Key, Pid}] ->
      case is_pid_alive(Pid) of
        true -> {ok, Pid};
        false -> {error, not_found}
      end;
    [] -> {error, not_found}
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
  case mnesia:dirty_index_read(key_to_pid, Pid, #key_to_pid.pid) of
    [#key_to_pid{} = Record] ->
      mnesia:dirty_delete_object(Record);
    _ ->
      ok
  end.


%%%===================================================================
%%% Internal functions
%%%===================================================================


%%--------------------------------------------------------------------
%% @private
%% @doc
%% Is the process alive
%%
%% @spec is_pid_alive(Pid) -> boolean() when
%%                        Pid :: pid()
%% @end
%%--------------------------------------------------------------------
-spec is_pid_alive(Pid) -> boolean() when
  Pid :: pid().
is_pid_alive(Pid) when node(Pid) =:= node() ->
  is_process_alive(Pid);
is_pid_alive(Pid) ->
  lists:member(node(Pid), nodes()) andalso
    (rpc:call(node(Pid), erlang, is_process_alive, [Pid]) =:= true).