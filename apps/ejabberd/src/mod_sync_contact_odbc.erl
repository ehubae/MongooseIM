%%%-------------------------------------------------------------------
%%% @author jaspreet
%%% @copyright (C) 2016, safe apps pvt ltd
%%% @doc
%%%
%%% @end
%%% Created : 18. Dec 2016 10:01 AM
%%%-------------------------------------------------------------------
-module(mod_sync_contact_odbc).
-behaviour(mod_sync_contact).
-author("jaspret").

-include("ejabberd.hrl").
-include("jlib.hrl").

-export([init/2,get_registered_contacts/3]).


-record(users, {username}).

init(_Host, _Opts) ->
  ok.

get_registered_contacts(User, VHost ,Contacts)->

  U = ejabberd_odbc:escape(User),
  S = ejabberd_odbc:escape(VHost),
  case odbc_queries:get_registered_contacts(U, S,Contacts) of

    {selected, [<<"username">>], [{ID}]} ->
      ID;
    {selected, [<<"username">>], List} ->
      fill_list(List);
    _ ->
      <<"">>
  end.


fill_list( [],Res) ->
  lists:flatten(lists:join(",",Res));
fill_list( [{ID} = I | Is],Res) ->
  fill_list(Is,[ID|Res]).
fill_list(List) ->
  fill_list(List, []).
