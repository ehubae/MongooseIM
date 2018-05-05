%%%----------------------------------------------------------------------
%%% File    : mod_offline_odbc.erl
%%% Author  : Alexey Shchepin <alexey@process-one.net>
%%% Purpose : Store and manage offline messages in relational database.
%%% Created :  5 Jan 2003 by Alexey Shchepin <alexey@process-one.net>
%%%
%%%
%%% ejabberd, Copyright (C) 2002-2011   ProcessOne
%%%
%%% This program is free software; you can redistribute it and/or
%%% modify it under the terms of the GNU General Public License as
%%% published by the Free Software Foundation; either version 2 of the
%%% License, or (at your option) any later version.
%%%
%%% This program is distributed in the hope that it will be useful,
%%% but WITHOUT ANY WARRANTY; without even the implied warranty of
%%% MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
%%% General Public License for more details.
%%%
%%% You should have received a copy of the GNU General Public License
%%% along with this program; if not, write to the Free Software
%%% Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
%%% 02111-1307 USA
%%%
%%%----------------------------------------------------------------------

-module(mod_flexible_offline_odbc).
-behaviour(mod_offline).
-export([init/2,
         fetch_messages/3,
         remove_messages/3,
         count_offline_messages/3]).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/mod_offline.hrl").
-define(OFFLINE_TABLE_LOCK_THRESHOLD, 1000).

init(_Host, _Opts) ->
    ok.

fetch_messages(LUser, LServer,SFrom) ->
    US = {LUser, LServer},
    To = jid:make(LUser, LServer, <<>>),
    SUser = ejabberd_odbc:escape(LUser),
    SServer = ejabberd_odbc:escape(LServer),
    TimeStamp = now(),
    STimeStamp = encode_timestamp(TimeStamp),
    case odbc_queries:fetch_offline_messages(LServer, SUser, SServer, STimeStamp,SFrom) of
        {selected, Rows} ->
            {ok, rows_to_records(US, To, Rows)};
        {aborted, Reason} ->
            {error, Reason};
        {error, Reason} ->
            {error, Reason};
        _ ->
            {error, <<"">>}
    end.


remove_messages(LUser, LServer,SFrom) ->
    SUser = ejabberd_odbc:escape(LUser),
    SServer = ejabberd_odbc:escape(LServer),
    odbc_queries:remove_offline_messages(LServer, SUser, SServer,SFrom).

rows_to_records(US, To, Rows) ->
    [row_to_record(US, To, Row) || Row <- Rows].

row_to_record(US, To, {STimeStamp, SFrom, SPacket}) ->
    {ok, Packet} = exml:parse(SPacket),
    TimeStamp = usec:to_now(mongoose_rdbms:result_to_integer(STimeStamp)),
    From = jid:from_binary(SFrom),
    #offline_msg{us = US,
             timestamp = TimeStamp,
             expire = never,
             from = From,
             to = To,
             packet = Packet}.


count_offline_messages(LUser, LServer, MaxArchivedMsgs) ->
    SUser = ejabberd_odbc:escape(LUser),
    SServer = ejabberd_odbc:escape(LServer),
    count_offline_messages(LServer, SUser, SServer, MaxArchivedMsgs + 1).

count_offline_messages(LServer, SUser, SServer, Limit) ->
    case odbc_queries:count_offline_messages(LServer, SUser, SServer, Limit) of
        {selected, [{Count}]} ->
            ejabberd_odbc:result_to_integer(Count);
        Error ->
            ?ERROR_MSG("count_offline_messages failed ~p", [Error]),
            0
    end.

encode_timestamp(TimeStamp) ->
    integer_to_list(usec:from_now(TimeStamp)).

maybe_encode_timestamp(never) ->
    "null";
maybe_encode_timestamp(TimeStamp) ->
    encode_timestamp(TimeStamp).
