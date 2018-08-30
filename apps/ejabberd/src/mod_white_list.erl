%%%-------------------------------------------------------------------
%%% @author jaspreet
%%% @copyright (C) 2018, <Scramble Apps pvt ltd>
%%% @doc
%%%
%%% @end
%%% Created : 27. Jun 2018 11:49 PM
%%%-------------------------------------------------------------------
-module(mod_white_list).
-author("jaspreet").



-export([start/2, stop/1, process_sm_iq/3]).

-include("../include/ejabberd.hrl").
-include("../include/jlib.hrl").

-define(BACKEND, mod_white_list_backend).

%%--------------------------------------------------------------------
%% backend callbacks
%%--------------------------------------------------------------------
-callback init(Host, Opts) -> ok when
  Host :: binary(),
  Opts :: list().

-callback get_white_list_users(VHost) -> any() when
  VHost :: binary().

-callback set_white_list_users(User,WhiteListId,VHost) -> any() when
  User :: binary(),
  WhiteListId :: binary(),
  VHost :: binary().

-callback rm_white_list_users(User,WhiteListId,VHost) -> any() when
  User :: binary(),
  WhiteListId :: binary(),
  VHost :: binary().

start(Host, Opt) ->
  gen_mod:start_backend_module(?MODULE, Opt, [get_white_list_users,set_white_list_users,rm_white_list_users]),
  IQDisc = gen_mod:get_opt(iqdisc, Opt, one_queue),
  ?BACKEND:init(Host, Opt),
  gen_iq_handler:add_iq_handler(ejabberd_sm, Host,?NS_WHITE_LIST, ?MODULE, process_sm_iq, IQDisc),
  gen_iq_handler:add_iq_handler(ejabberd_sm, Host,?NS_WHITE_LIST_DELETE, ?MODULE, process_sm_iq_del, IQDisc),
  ejabberd_hooks:add(get_registered_contacts, Host, ?MODULE, get_registered_contacts, 50).

stop(Host) ->
  gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_WHITE_LIST).


-spec process_sm_iq(ejabberd:jid(), ejabberd:jid(), ejabberd:iq()) -> ejabberd:iq().
process_sm_iq(From, To, #iq{type = Type, sub_el = Sub_Ele} = IQ) ->
  case Type of
    set ->
      #jid{user = FromUser, lserver = FromVHost} = From,
      User = xml:get_path_s(Sub_Ele, [{elem, <<"username">>}, cdata]),
      WhiteListId = xml:get_path_s(Sub_Ele, [{elem, <<"white_list_id">>}, cdata]),
      ?BACKEND:set_white_list_users(User,WhiteListId, FromVHost),
      IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [{<<"xmlns">>, ?NS_WHITE_LIST}], children = []}]};
    get ->
      #jid{user = FromUser, lserver = FromVHost} = From,
      List=?BACKEND:get_white_list_users(FromVHost),
      IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [{<<"xmlns">>, ?NS_WHITE_LIST}], children = List }]}
  end.


-spec process_sm_iq_del(ejabberd:jid(), ejabberd:jid(), ejabberd:iq()) -> ejabberd:iq().
process_sm_iq_del(From, To, #iq{type = Type, sub_el = Sub_Ele} = IQ) ->
  case Type of
    set ->
      #jid{user = FromUser, lserver = FromVHost} = From,
      User = xml:get_path_s(Sub_Ele, [{elem, <<"username">>}, cdata]),
      WhiteListId = xml:get_path_s(Sub_Ele, [{elem, <<"white_list_id">>}, cdata]),
      ?BACKEND:rm_white_list_users(User,WhiteListId, FromVHost),
      IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [{<<"xmlns">>, ?NS_WHITE_LIST}], children = []}]};
    get ->
      IQ#iq{type = result, sub_el = [#xmlel{name = <<"query">>, attrs = [{<<"xmlns">>, ?NS_WHITE_LIST}], children = [] }]}
  end.