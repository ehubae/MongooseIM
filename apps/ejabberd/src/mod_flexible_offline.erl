%%%-------------------------------------------------------------------
%%% @author Jaspreet Singh
%%% @copyright (C) 2018, Scramble apps
%%% @doc XEP-0013: Flexible Offline Message Retrieval
%%% @end
%%%-------------------------------------------------------------------
-module(mod_flexible_offline).
-author('jaspreet@scrambleapps.com').
-behaviour(gen_mod).
-export([start/2, stop/1, process_sm_iq/4,send_message/3]).
-include_lib("ejabberd/include/ejabberd.hrl").
-include_lib("ejabberd/include/jlib.hrl").

-define(BACKEND, mod_flexible_offline_backend).


-callback init(Host, Opts) -> ok when
    Host :: binary(),
    Opts :: list().


-callback fetch_messages(LUser, LServer,SFrom) -> ok when
    LUser :: binary(),
    LServer :: binary(),
    SFrom :: binary().

-callback remove_messages(LUser, LServer,SFrom) -> ok when
    LUser :: binary(),
    LServer :: binary(),
    SFrom :: binary().

start(Host, Opts) ->
    gen_mod:start_backend_module(?MODULE, Opts, [fetch_messages,remove_messages]),
    mod_disco:register_feature(Host, ?NS_FLEXIBLE_OFFLINE),
    IQDisc = gen_mod:get_opt(iqdisc, Opts, one_queue),
    ?BACKEND:init(Host, Opts),
    gen_iq_handler:add_iq_handler(ejabberd_sm, Host, ?NS_FLEXIBLE_OFFLINE, ?MODULE, process_sm_iq, IQDisc).


stop(Host) ->
    mod_disco:unregister_feature(Host, ?NS_FLEXIBLE_OFFLINE),
    gen_iq_handler:remove_iq_handler(ejabberd_sm, Host, ?NS_FLEXIBLE_OFFLINE).


process_sm_iq(_From, _To, Acc, #iq{type = set, sub_el = SubEl} = IQ) ->
    IQ#iq{type = error, sub_el = [SubEl, ?ERR_NOT_ALLOWED]};

process_sm_iq(_From, _To, Acc, #iq{type = get,sub_el = SubEl} = IQ) ->
    #jid{user = FromUser, lserver = FromVHost} = _From,
    ?DEBUG("Item =  ~p.", [SubEl]),
    Item = xml:get_subtag(SubEl, <<"item">>),
    ?DEBUG("Item =  ~p.", [Item]),
    Action = xml:get_tag_attr_s(<<"action">>, Item),
    Node = xml:get_tag_attr_s(<<"node">>, Item),
    case(Action) of
        <<"view">> ->
        case ?BACKEND:fetch_messages(FromUser, FromVHost,Node) of
            {ok, Rs} ->
                ?DEBUG("Rs =  ~p", [Rs]),
                lists:map(fun(R) ->
                    Packet = mod_offline:resend_offline_message_packet(FromVHost, R),
                    ?DEBUG("Packet =  ~p", [Packet]),
                    spawn(?MODULE, send_message, [Packet,_To,_From])
                          end, Rs);
            {error, Reason} ->
                ?DEBUG("~ts@~ts: fetch_messages failed with ~p.", [FromUser, FromVHost, Reason]),
                []
        end;
        <<"remove">> ->
            ?BACKEND:remove_messages(FromUser, FromVHost,Node)
    end,

   IQ#iq{type = result,
        sub_el =
        [#xmlel{name = <<"offline">>,
            attrs =
            [{<<"xmlns">>, ?NS_FLEXIBLE_OFFLINE}],
            children = []}]}.


send_message(Packet,From,To)->
    ejabberd_router:route(From, To, Packet).