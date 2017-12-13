%%%-------------------------------------------------------------------
%%% @author jaspreet
%%% @copyright (C) 2017, Scramble Apps PVT LTD
%%% @doc
%%%
%%% @end
%%% Created : 14. Apr 2017 10:03 PM
%%%-------------------------------------------------------------------
-module(mod_offline_push).
-author("jaspreet").

-behavior(gen_mod).

-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").
-include_lib("ejabberd/include/mod_offline_push.hrl").

-define(BACKEND, mod_offline_push_backend).
-define(LOG_URL, "http://google.com").

-define(MUC_LIGHT_DEFAULT_HOST,<<"muclight.chat.devinprocess.com">>).
-define(NS_FILE,       <<"jabber:x:file">>).
-define(NS_DOMAIN,       "@chat.devinprocess.com").

-type packet() :: any().

%% API
%% MIM module callbacks
-export([start/2, stop/1]).

%% Hooks
-export([user_send_packet/3, user_present/1, user_not_present/4, filter_room_packet/2,log_msg/8,log_msg/2,log_msg/1]).


-callback init(Host, Opts) -> ok when
  Host :: binary(),
  Opts :: list().

-callback set_availability(LUser, LServer,Availability) -> ok when
  LUser :: binary(),
  LServer :: binary(),
  Availability :: binary().

-callback get_availability(LUser, LServer) -> ok when
  LUser :: binary(),
  LServer :: binary().


-callback get_group_name( LServer,GroupId) -> ok when
  LServer :: binary(),
  GroupId :: binary().

-callback get_members(LUser, LServer) -> ok when
  LUser :: binary(),
  LServer :: binary().

-callback record_to_offline_message(LUser, LServer, Message) -> ok when
  LUser :: binary(),
  LServer :: binary(),
  Message :: term().


%%%===================================================================
%%% API functions
%%%===================================================================

-spec start(Host :: ejabberd:server(), Opts :: proplists:proplist()) -> ok.
start(Host, Opts) ->
  gen_mod:start_backend_module(?MODULE, Opts, [set_availability,get_availability,get_group_name,get_members,record_to_offline_message]),
  MUCHost =  gen_mod:get_module_opt_host(Host, ?MODULE, ?MUC_LIGHT_DEFAULT_HOST),
  ?BACKEND:init(Host, Opts),
  ejabberd_hooks:add(filter_room_packet, MUCHost, ?MODULE, filter_room_packet, 90),
  ejabberd_hooks:add(rest_user_send_packet, Host, ?MODULE, user_send_packet, 90),
  ejabberd_hooks:add(user_send_packet, Host, ?MODULE, user_send_packet, 90),
  ejabberd_hooks:add(user_available_hook, Host, ?MODULE, user_present, 90),
  ejabberd_hooks:add(unset_presence_hook, Host, ?MODULE, user_not_present, 90),
  ok.

-spec stop(Host :: ejabberd:server()) -> ok.
stop(Host) ->
  MUCHost = gen_mod:get_module_opt_host(Host, ?MODULE, ?MUC_LIGHT_DEFAULT_HOST),
  ejabberd_hooks:delete(unset_presence_hook, Host, ?MODULE, user_not_present, 90),
  ejabberd_hooks:delete(user_available_hook, Host, ?MODULE, user_present, 90),
  ejabberd_hooks:delete(user_send_packet, Host, ?MODULE, user_send_packet, 90),
  ejabberd_hooks:delete(rest_user_send_packet, Host, ?MODULE, user_send_packet, 90),
  ejabberd_hooks:delete(filter_room_packet, MUCHost, ?MODULE, filter_room_packet, 90),
  ok.



%% @doc Handle an outgoing message.
%%
%% Note: for outgoing messages, the server MUST use the value of the 'to'
%%       attribute as the target JID.
-spec user_send_packet(From :: ejabberd:jid(), To :: ejabberd:jid(),
    Packet :: jlib:xmlel()) -> 'ok'.
user_send_packet(From, To, Packet) ->
%%  ?DEBUG("Send packet~n    from ~p ~n    to ~p~n    packet ~p.", [From, To, Packet]),
  handle_packet(From,To,Packet),
  ok.

%% ----------------------------------------------------------------------
%% hooks and handlers for MUC

%% @doc Handle public MUC-message.
-spec filter_room_packet(Packet :: packet(), EventData :: list()) -> packet().
filter_room_packet(Packet, EventData) ->
%%  ?DEBUG("Incoming room packet.", []),
      {_, FromNick}    = lists:keyfind(from_nick, 1, EventData),
      {_, FromJID}     = lists:keyfind(from_jid, 1, EventData),
      {_, RoomJID}     = lists:keyfind(room_jid, 1, EventData),
      {_, Role}        = lists:keyfind(role, 1, EventData),
      {_, Affiliation} = lists:keyfind(affiliation, 1, EventData),
      Type = xml:get_tag_attr_s(list_to_binary("type"), Packet),
      Id = xml:get_tag_attr_s(list_to_binary("id"), Packet),
      FROM_FULL = xml:get_tag_attr_s(list_to_binary("from"), Packet),
      Body = xml:get_path_s(Packet, [{elem, list_to_binary("body")}, cdata]),
      #jid{luser = LUser, lserver = LServer} = RoomJID,
      FromUser=FromJID#jid.luser,
      FromResource=FromNick,
      FromServer=FromJID#jid.lserver,

  if((LUser /= <<"">>) and (Type == <<"groupchat">>))->
    Sent = jlib:make_sent_reply(Packet, ?MESSAGE_SENT),
    ejabberd_router:route(RoomJID, FromJID, Sent),
      if (Body == <<"">>) ->
        X = xml:get_subtag(Packet, <<"x">>),

                if (X =/= false ) ->

                 XMLNS= xml:get_tag_attr_s(<<"xmlns">>, X),

                        if(XMLNS == ?NS_FILE) ->

                                case catch ?BACKEND:get_group_name( FromServer,LUser) of
                                    {Group_Name} ->
                                      Mime = xml:get_path_s(X , [{elem, list_to_binary("mime")}, cdata]),
%%                                      ?DEBUG(" Mime in SubTag = ~s", [Mime]),
                                      Message=#message{mime=Mime,body = Body,from = FromUser,from_jid=FromJID,type = Type,id = Id,group_id = LUser,resource = FromResource,timestamp = os:timestamp(),packet = Packet,server = FromServer,group_name = Group_Name},

                                            case catch ?BACKEND:get_members(LUser, FromServer) of
                                              {ok, List} ->
                                                iterate_list(List,Message),
                                                Packet;
                                              {error,[]} ->
                                                Packet;
                                              Else ->
                                                Packet
                                            end;
                                      none ->
                                        Packet
                                end;

                          true ->
                            Packet
                          end;
                  true ->
                    Packet
                end;

        true ->
          case catch ?BACKEND:get_group_name(FromServer,LUser) of
            {Group_Name} ->
              Message=#message{mime= <<"">>,body = Body,from = FromUser,from_jid=FromJID,type = Type,id = Id,group_id = LUser,resource = FromResource,timestamp = os:timestamp(),packet = Packet,server = FromServer,group_name = Group_Name},
              case catch ?BACKEND:get_members(LUser, FromServer) of
                {ok, List} ->
                  iterate_list(List,Message),
                  Packet;
                {error,[]} ->
                  Packet;
                Else ->
                  Packet
              end;
            none ->
              Packet
          end
      end;
    true ->
      ok
  end.


iterate_list( [],Message,Res) ->
  Res;
iterate_list( [I | Is],Message,Res) ->
  log_msg(Message,I),
  iterate_list(Is,Message,[I|Res]).
iterate_list(List,Message) ->
  iterate_list(List,Message, []).

log_msg(Message,{User})->
  log_msg(Message#message.mime,Message#message.body,Message#message.from,User,Message#message.type,Message#message.id,Message#message.group_id,Message#message.group_name),
  ?BACKEND:record_to_offline_message(User,Message#message.server,Message).

log_msg(Message)->
  log_msg(Message#message.mime,Message#message.body,Message#message.from,Message#message.to,Message#message.type,Message#message.id,Message#message.group_id,Message#message.group_name).
%%  ?BACKEND:record_to_offline_message(Message#message.from,Message#message.server,Message).

%% Handle user_available_hook
-spec user_present(UserJID :: ejabberd:jid()) -> ok.
user_present( #jid{} = UserJID) ->
  #jid{luser = LUser, lserver = LServer} = UserJID,
  ?BACKEND:set_availability(LUser, LServer,"Available"),
  user_presence_changed(UserJID, true).

%% Handle unset_presence_hook
-spec user_not_present(
    User     :: ejabberd:luser(),
    Server   :: ejabberd:lserver(),
    Resource :: ejabberd:lresource(),
    _Status :: any()) -> ok.
user_not_present(User, Host, Resource, _Status) ->
%%  ?DEBUG("Packet User~s    Host ~s    Resource ~s   _Status ~s.", [User, Host, Resource, _Status]),
  ?BACKEND:set_availability(User, Host,"Offline"),
  ok.

-spec user_presence_changed(UserJID :: ejabberd:jid(), IsOnline :: boolean()) -> ok.
user_presence_changed(#jid{lserver = Host} = UserJID, IsOnline) ->
  ok.


%%%===================================================================
%%% Internal functions
%%%===================================================================

%% @doc Handles packet and if needed publishes SNS notification
-spec handle_packet(From :: ejabberd:jid(), To :: ejabberd:jid(),
    Packet :: jlib:xmlel()) -> ok | skip.
handle_packet(From = #jid{lserver = Host}, To, Packet) ->
%%  ?DEBUG("Offline push Packet handle~n    from ~p ~n    to ~p~n    packet ~p.", [From, To, Packet]),
  Type = xml:get_tag_attr_s(list_to_binary("type"), Packet),
  Body = xml:get_path_s(Packet, [{elem, list_to_binary("body")}, cdata]),
  Id = xml:get_tag_attr_s(list_to_binary("id"), Packet),
  #jid{luser = LUser, lserver = LServer} = To,
  FromUser=From#jid.luser,
  Resource=From#jid.lresource,
  if((LUser /= <<"">>) and (Type == <<"chat">>))->
    Sent = jlib:make_sent_reply(Packet, ?MESSAGE_SENT),
    ejabberd_router:route(To, From, Sent),
    case catch ?BACKEND:get_availability(LUser, LServer) of
      {ok} ->
          if (Body == <<"">>) ->

            X = xml:get_subtag(Packet, <<"x">>),

            if (X =/= false ) ->

              XMLNS= xml:get_tag_attr_s(<<"xmlns">>, X),

                   if(XMLNS == ?NS_FILE) ->

                        Mime = xml:get_path_s(X, [{elem, list_to_binary("mime")}, cdata]),
%%                        ?DEBUG(" Mime in SubTag = ~s", [Mime]),
                        Message=#message{mime=Mime,body = Body,from = FromUser,type = Type,id = Id,group_id = From,resource = Resource,to = LUser ,packet = Packet,group_name = <<"">>},
                        log_msg(Message);

                     true ->
                      ok
                   end;

              true ->
                ok
            end;
            true ->
              Message=#message{mime= <<"">>,body = Body,from = FromUser,type = Type,id = Id,group_id = From,resource = Resource,to = LUser ,packet = Packet,group_name = <<"">>},
              log_msg(Message)
          end;
      none ->
        ok

    end;

    true ->
   ok
 end.


log_msg(MIME,BODY,FROM,TO,TYPE,ID,GROUP_ID,GROUP_NAME) ->
  log_msg(BODY,FROM,TO,TYPE,GROUP_ID,MIME,GROUP_NAME).
%%

log_msg(BODY,FROM,TO,TYPE,GROUP_ID,MIME,GROUP_NAME) ->
  Body= "{\"alert\":\"" ++ binary:bin_to_list(BODY) ++ "\",\"MimeType\":\"" ++ binary:bin_to_list(MIME) ++ "\",\"channel\":\"" ++ binary:bin_to_list(TO) ++ "\",\"ChatType\":\"" ++ binary:bin_to_list(TYPE) ++ "\",\"Domain\":\"" ++ ?NS_DOMAIN ++ "\",\"GroupName\":\"" ++ binary:bin_to_list(GROUP_NAME) ++ "\",\"from\":\"" ++ binary:bin_to_list(FROM) ++ "\"}",
%%  ?DEBUG("+++++++ encoded json: ~s", [Body]),
  Method = post,
  URL = "http://pushinstablocker.shared.svc/api/BooqChat/SendIOSPushNotification",
  Header = [],
  Type = "application/json",
  HTTPOptions = [],
  Options = [],
  R = httpc:request(Method, {URL, Header, Type, Body}, HTTPOptions, Options),
  {ok, {{"HTTP/1.1",ReturnCode, State}, Head, Res}} = R,
  ?DEBUG("+++++++ SendIOSPushNotification response code : ~p ",[ReturnCode]),
  ?DEBUG("+++++++ SendIOSPushNotification response body : ~p ",[Res]).



