%%%-------------------------------------------------------------------
%%% @author jaspreet
%%% @copyright (C) 2016, safe apps pvt ltd
%%% @doc
%%%
%%% @end
%%% Created : 27. Jun 2018 11:49 PM
%%%-------------------------------------------------------------------
-module(mod_white_list_odbc).
-behaviour(mod_white_list).
-author("jaspret").

-include("../include/ejabberd.hrl").
-include("../include/jlib.hrl").

-export([init/2,get_white_list_users/1,set_white_list_users/2]).


-record(users, {username}).

init(_Host, _Opts) ->
  ok.

get_white_list_users(VHost) ->
  S = ejabberd_odbc:escape(VHost),
  case odbc_queries:get_white_list(S) of
    {selected, [<<"username">>], List} ->
      fill_list(List);
    _ ->
      <<"">>
  end.

set_white_list_users(User,VHost) ->
  U = ejabberd_odbc:escape(User),
  S = ejabberd_odbc:escape(VHost),
  odbc_queries:set_white_list(U,S),
 ok.



fill_list( [],Res) ->
  lists:flatten(lists:join(",",Res));
fill_list( [{ID} = I | Is],Res) ->
  fill_list(Is,[ID|Res]).
fill_list(List) ->
  fill_list(List, []).