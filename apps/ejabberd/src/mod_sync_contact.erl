%%%-------------------------------------------------------------------
%%% @author jaspreet
%%% @copyright (C) 2016, <COMPANY>
%%% @doc
%%%
%%% @end
%%% Created : 14. Dec 2016 7:22 PM
%%%-------------------------------------------------------------------
-module(mod_sync_contact).
-behaviour(gen_mod).

-author("jaspreet").


%% API
-export([start/2, stop/1, process_local_iq/3, process_sm_iq/3]).

-include("ejabberd.hrl").
-include("jlib.hrl").


-define(BACKEND, mod_sync_contact_backend).


%%--------------------------------------------------------------------
%% backend callbacks
%%--------------------------------------------------------------------
-callback init(Host, Opts) -> ok when
  Host :: binary(),
  Opts :: list().

-callback get_registered_contacts(User, VHost,Contacts) -> any() when
  User :: binary(),
  VHost :: binary(),
  Contacts :: binary().

start(Host, Opt) ->
  gen_mod:start_backend_module(?MODULE, Opt, [get_registered_contacts]),
  IQDisc = gen_mod:get_opt(iqdisc, Opt, one_queue),
  ?BACKEND:init(Host, Opt),
  gen_iq_handler:add_iq_handler(ejabberd_sm, Host,?NS_CONTACT, ?MODULE, process_sm_iq, IQDisc),
  gen_iq_handler:add_iq_handler(ejabberd_local, Host, ?NS_CONTACT, ?MODULE, process_local_iq, IQDisc),
  ejabberd_hooks:add(get_registered_contacts, Host, ?MODULE, get_registered_contacts, 50),
  ?INFO_MSG("Loading module 'mod_sync_contact' v.01", []).

stop(Host) ->
  gen_iq_handler:remove_iq_handler(ejabberd_local, Host, ?NS_CONTACT),
  gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_CONTACT).


-spec process_sm_iq(ejabberd:jid(), ejabberd:jid(), ejabberd:iq()) -> ejabberd:iq().
process_sm_iq(From, To, #iq{type = Type, sub_el = CONTACTS} = IQ) ->
  case Type of
    set ->
      IQ#iq{type = error, sub_el = [CONTACTS, ?ERR_NOT_ALLOWED]};
    get ->
      #jid{user = FromUser, lserver = FromVHost} = From,
      Contacts = xml:get_path_s(CONTACTS, [{elem, <<"contacts">>}, cdata]),
      LIST=?BACKEND:get_registered_contacts(FromUser, FromVHost,Contacts),
      IQ#iq{type = result,
        sub_el =
        [#xmlel{name = <<"query">>,
          attrs =
          [{<<"xmlns">>, ?NS_CONTACT},
            {<<"contacts">>,
              ejabberd_binary:string_to_binary(LIST)}],
          children = []}]}
  end.







-spec process_local_iq(ejabberd:jid(), ejabberd:jid(), ejabberd:iq())
      -> ejabberd:iq().
process_local_iq(_From, _To, #iq{type = Type, sub_el = CONTACTS} = IQ) ->
  case Type of
    set ->
      IQ#iq{type = error, sub_el = [CONTACTS, ?ERR_NOT_ALLOWED]};
    get ->
      CONTACTS_FROM = xml:get_path_s(CONTACTS, [{elem, <<"CONTACTS">>}, cdata]),
      IQ#iq{type = result,
        sub_el =
        [#xmlel{name = <<"query">>,
          attrs =
          [{<<"xmlns">>, ?NS_CONTACT},
            {<<"CONTACTS">>,
              ejabberd_binary:string_to_binary(CONTACTS_FROM)}],
          children = []}]}
  end.