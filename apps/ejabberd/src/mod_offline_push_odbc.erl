%%%-------------------------------------------------------------------
%%% @author jaspreet
%%% @copyright (C) 2017, Scramble Apps PVT LTD
%%% @doc
%%%
%%% @end
%%% Created : 17. Apr 2017 8:52 PM
%%%-------------------------------------------------------------------
-module(mod_offline_push_odbc).
-behaviour(mod_offline_push).
-author("jaspreet").



-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/mod_offline_push.hrl").
% API
-export([init/2,set_availability/3,get_availability/2,get_members/2,record_to_offline_message/3,get_group_name/2]).

init(_Host, _Opts) ->
  ok.

set_availability(User, VHost,Availability) ->
  U = ejabberd_odbc:escape(User),
  S = ejabberd_odbc:escape(VHost),
  odbc_queries:set_availability(U, S,Availability),
  ok.

get_availability(User, VHost) ->
  U = ejabberd_odbc:escape(User),
  S = ejabberd_odbc:escape(VHost),

  case odbc_queries:get_availability(S, U) of
    {selected, [<<"availability">>], []} ->
      none;
    {selected, [<<"availability">>], [{AVAILABILITY}]} ->
      {ok};
    _ ->
      none
  end.

get_group_name(VHost,GroupId) ->
  S = ejabberd_odbc:escape(VHost),

  case odbc_queries:get_group_name(S,GroupId) of
    {selected, [<<"val">>], [{GROUP_NAME}]} ->
      {GROUP_NAME};
    _ ->
      none
  end.


get_members(User, VHost) ->
  U = ejabberd_odbc:escape(User),
  S = ejabberd_odbc:escape(VHost),

  case odbc_queries:get_members(S, U) of
    {selected, [<<"username">>], List} ->
      {ok, List};
    _ ->
      {error,[]}
  end.

record_to_offline_message(SUser, SServer, Message) ->
  Rows = [get_raw(SUser, SServer, Message)],
  case catch odbc_queries:push_offline_messages(SServer, Rows) of
    {updated, _} ->
      ok;
    {aborted, Reason} ->
      {error, {aborted, Reason}};
    {error, Reason} ->
      {error, Reason}
  end.

get_raw(SUser, SServer, Message) ->
  SFrom = ejabberd_odbc:escape(Message#message.group_id),
  SPacket = ejabberd_odbc:escape(exml:to_binary(Message#message.packet)),
  STimeStamp = encode_timestamp(Message#message.timestamp),
  SExpire = "null",
  odbc_queries:prepare_offline_message(SUser, SServer, STimeStamp, SExpire, SFrom, SPacket).

encode_timestamp(TimeStamp) ->
  integer_to_list(usec:from_now(TimeStamp)).
maybe_encode_timestamp(never) ->
  "null";
maybe_encode_timestamp(TimeStamp) ->
  encode_timestamp(TimeStamp).