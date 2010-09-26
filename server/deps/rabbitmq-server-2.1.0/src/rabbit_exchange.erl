%%   The contents of this file are subject to the Mozilla Public License
%%   Version 1.1 (the "License"); you may not use this file except in
%%   compliance with the License. You may obtain a copy of the License at
%%   http://www.mozilla.org/MPL/
%%
%%   Software distributed under the License is distributed on an "AS IS"
%%   basis, WITHOUT WARRANTY OF ANY KIND, either express or implied. See the
%%   License for the specific language governing rights and limitations
%%   under the License.
%%
%%   The Original Code is RabbitMQ.
%%
%%   The Initial Developers of the Original Code are LShift Ltd,
%%   Cohesive Financial Technologies LLC, and Rabbit Technologies Ltd.
%%
%%   Portions created before 22-Nov-2008 00:00:00 GMT by LShift Ltd,
%%   Cohesive Financial Technologies LLC, or Rabbit Technologies Ltd
%%   are Copyright (C) 2007-2008 LShift Ltd, Cohesive Financial
%%   Technologies LLC, and Rabbit Technologies Ltd.
%%
%%   Portions created by LShift Ltd are Copyright (C) 2007-2010 LShift
%%   Ltd. Portions created by Cohesive Financial Technologies LLC are
%%   Copyright (C) 2007-2010 Cohesive Financial Technologies
%%   LLC. Portions created by Rabbit Technologies Ltd are Copyright
%%   (C) 2007-2010 Rabbit Technologies Ltd.
%%
%%   All Rights Reserved.
%%
%%   Contributor(s): ______________________________________.
%%

-module(rabbit_exchange).
-include("rabbit.hrl").
-include("rabbit_framing.hrl").

-export([recover/0, declare/5, lookup/1, lookup_or_die/1, list/1, info_keys/0,
         info/1, info/2, info_all/1, info_all/2, publish/2, delete/2]).
%% this must be run inside a mnesia tx
-export([maybe_auto_delete/1]).
-export([assert_equivalence/5, assert_args_equivalence/2, check_type/1]).

%%----------------------------------------------------------------------------

-ifdef(use_specs).

-export_type([name/0, type/0]).

-type(name() :: rabbit_types:r('exchange')).
-type(type() :: atom()).

-spec(recover/0 :: () -> 'ok').
-spec(declare/5 ::
        (name(), type(), boolean(), boolean(), rabbit_framing:amqp_table())
        -> rabbit_types:exchange()).
-spec(check_type/1 ::
        (binary()) -> atom() | rabbit_types:connection_exit()).
-spec(assert_equivalence/5 ::
        (rabbit_types:exchange(), atom(), boolean(), boolean(),
         rabbit_framing:amqp_table())
        -> 'ok' | rabbit_types:connection_exit()).
-spec(assert_args_equivalence/2 ::
        (rabbit_types:exchange(), rabbit_framing:amqp_table())
        -> 'ok' | rabbit_types:connection_exit()).
-spec(lookup/1 ::
        (name()) -> rabbit_types:ok(rabbit_types:exchange()) |
                    rabbit_types:error('not_found')).
-spec(lookup_or_die/1 ::
        (name()) -> rabbit_types:exchange() |
                    rabbit_types:channel_exit()).
-spec(list/1 :: (rabbit_types:vhost()) -> [rabbit_types:exchange()]).
-spec(info_keys/0 :: () -> [rabbit_types:info_key()]).
-spec(info/1 :: (rabbit_types:exchange()) -> [rabbit_types:info()]).
-spec(info/2 ::
        (rabbit_types:exchange(), [rabbit_types:info_key()])
        -> [rabbit_types:info()]).
-spec(info_all/1 :: (rabbit_types:vhost()) -> [[rabbit_types:info()]]).
-spec(info_all/2 ::(rabbit_types:vhost(), [rabbit_types:info_key()])
                    -> [[rabbit_types:info()]]).
-spec(publish/2 :: (rabbit_types:exchange(), rabbit_types:delivery())
                   -> {rabbit_router:routing_result(), [pid()]}).
-spec(delete/2 ::
        (name(), boolean())-> 'ok' |
                              rabbit_types:error('not_found') |
                              rabbit_types:error('in_use')).
-spec(maybe_auto_delete/1:: (rabbit_types:exchange()) ->
                                 'not_deleted' | 'auto_deleted').

-endif.

%%----------------------------------------------------------------------------

-define(INFO_KEYS, [name, type, durable, auto_delete, arguments]).

recover() ->
    Xs = rabbit_misc:table_fold(
           fun (X, Acc) ->
                   ok = mnesia:write(rabbit_exchange, X, write),
                   [X | Acc]
           end, [], rabbit_durable_exchange),
    Bs = rabbit_binding:recover(),
    recover_with_bindings(
      lists:keysort(#binding.exchange_name, Bs),
      lists:keysort(#exchange.name, Xs), []).

recover_with_bindings([B = #binding{exchange_name = Name} | Rest],
                      Xs = [#exchange{name = Name} | _],
                      Bindings) ->
    recover_with_bindings(Rest, Xs, [B | Bindings]);
recover_with_bindings(Bs, [X = #exchange{type = Type} | Xs], Bindings) ->
    (type_to_module(Type)):recover(X, Bindings),
    recover_with_bindings(Bs, Xs, []);
recover_with_bindings([], [], []) ->
    ok.

declare(XName, Type, Durable, AutoDelete, Args) ->
    X = #exchange{name        = XName,
                  type        = Type,
                  durable     = Durable,
                  auto_delete = AutoDelete,
                  arguments   = Args},
    %% We want to upset things if it isn't ok; this is different from
    %% the other hooks invocations, where we tend to ignore the return
    %% value.
    TypeModule = type_to_module(Type),
    ok = TypeModule:validate(X),
    case rabbit_misc:execute_mnesia_transaction(
           fun () ->
                   case mnesia:wread({rabbit_exchange, XName}) of
                       [] ->
                           ok = mnesia:write(rabbit_exchange, X, write),
                           ok = case Durable of
                                    true ->
                                        mnesia:write(rabbit_durable_exchange,
                                                     X, write);
                                    false ->
                                        ok
                           end,
                           {new, X};
                       [ExistingX] ->
                           {existing, ExistingX}
                   end
           end) of
        {new, X}      -> TypeModule:create(X),
                         rabbit_event:notify(exchange_created, info(X)),
                         X;
        {existing, X} -> X;
        Err           -> Err
    end.

%% Used with atoms from records; e.g., the type is expected to exist.
type_to_module(T) ->
    {ok, Module} = rabbit_exchange_type_registry:lookup_module(T),
    Module.

%% Used with binaries sent over the wire; the type may not exist.
check_type(TypeBin) ->
    case rabbit_exchange_type_registry:binary_to_type(TypeBin) of
        {error, not_found} ->
            rabbit_misc:protocol_error(
              command_invalid, "unknown exchange type '~s'", [TypeBin]);
        T ->
            case rabbit_exchange_type_registry:lookup_module(T) of
                {error, not_found} -> rabbit_misc:protocol_error(
                                        command_invalid,
                                        "invalid exchange type '~s'", [T]);
                {ok, _Module}      -> T
            end
    end.

assert_equivalence(X = #exchange{ durable     = Durable,
                                  auto_delete = AutoDelete,
                                  type        = Type},
                   Type, Durable, AutoDelete, RequiredArgs) ->
    (type_to_module(Type)):assert_args_equivalence(X, RequiredArgs);
assert_equivalence(#exchange{ name = Name }, _Type, _Durable, _AutoDelete,
                   _Args) ->
    rabbit_misc:protocol_error(
      not_allowed,
      "cannot redeclare ~s with different type, durable or autodelete value",
      [rabbit_misc:rs(Name)]).

assert_args_equivalence(#exchange{ name = Name, arguments = Args },
                        RequiredArgs) ->
    %% The spec says "Arguments are compared for semantic
    %% equivalence".  The only arg we care about is
    %% "alternate-exchange".
    rabbit_misc:assert_args_equivalence(Args, RequiredArgs, Name,
                                        [<<"alternate-exchange">>]).

lookup(Name) ->
    rabbit_misc:dirty_read({rabbit_exchange, Name}).

lookup_or_die(Name) ->
    case lookup(Name) of
        {ok, X}            -> X;
        {error, not_found} -> rabbit_misc:not_found(Name)
    end.

list(VHostPath) ->
    mnesia:dirty_match_object(
      rabbit_exchange,
      #exchange{name = rabbit_misc:r(VHostPath, exchange), _ = '_'}).

info_keys() -> ?INFO_KEYS.

map(VHostPath, F) ->
    %% TODO: there is scope for optimisation here, e.g. using a
    %% cursor, parallelising the function invocation
    lists:map(F, list(VHostPath)).

infos(Items, X) -> [{Item, i(Item, X)} || Item <- Items].

i(name,        #exchange{name        = Name})       -> Name;
i(type,        #exchange{type        = Type})       -> Type;
i(durable,     #exchange{durable     = Durable})    -> Durable;
i(auto_delete, #exchange{auto_delete = AutoDelete}) -> AutoDelete;
i(arguments,   #exchange{arguments   = Arguments})  -> Arguments;
i(Item, _) -> throw({bad_argument, Item}).

info(X = #exchange{}) -> infos(?INFO_KEYS, X).

info(X = #exchange{}, Items) -> infos(Items, X).

info_all(VHostPath) -> map(VHostPath, fun (X) -> info(X) end).

info_all(VHostPath, Items) -> map(VHostPath, fun (X) -> info(X, Items) end).

publish(X, Delivery) ->
    publish(X, [], Delivery).

publish(X = #exchange{type = Type}, Seen, Delivery) ->
    case (type_to_module(Type)):publish(X, Delivery) of
        {_, []} = R ->
            #exchange{name = XName, arguments = Args} = X,
            case rabbit_misc:r_arg(XName, exchange, Args,
                                   <<"alternate-exchange">>) of
                undefined ->
                    R;
                AName ->
                    NewSeen = [XName | Seen],
                    case lists:member(AName, NewSeen) of
                        true  -> R;
                        false -> case lookup(AName) of
                                     {ok, AX} ->
                                         publish(AX, NewSeen, Delivery);
                                     {error, not_found} ->
                                         rabbit_log:warning(
                                           "alternate exchange for ~s "
                                           "does not exist: ~s",
                                           [rabbit_misc:rs(XName),
                                            rabbit_misc:rs(AName)]),
                                         R
                                 end
                    end
            end;
        R ->
            R
    end.

call_with_exchange(XName, Fun) ->
    rabbit_misc:execute_mnesia_transaction(
      fun () -> case mnesia:read({rabbit_exchange, XName}) of
                   []  -> {error, not_found};
                   [X] -> Fun(X)
               end
      end).

delete(XName, IfUnused) ->
    Fun = case IfUnused of
              true  -> fun conditional_delete/1;
              false -> fun unconditional_delete/1
          end,
    case call_with_exchange(XName, Fun) of
        {deleted, X = #exchange{type = Type}, Bs} ->
            (type_to_module(Type)):delete(X, Bs),
            ok;
        Error = {error, _InUseOrNotFound} ->
            Error
    end.

maybe_auto_delete(#exchange{auto_delete = false}) ->
    not_deleted;
maybe_auto_delete(#exchange{auto_delete = true} = X) ->
    case conditional_delete(X) of
        {error, in_use}  -> not_deleted;
        {deleted, X, []} -> auto_deleted
    end.

conditional_delete(X = #exchange{name = XName}) ->
    case rabbit_binding:has_for_exchange(XName) of
        false  -> unconditional_delete(X);
        true   -> {error, in_use}
    end.

unconditional_delete(X = #exchange{name = XName}) ->
    Bindings = rabbit_binding:remove_for_exchange(XName),
    ok = mnesia:delete({rabbit_durable_exchange, XName}),
    ok = mnesia:delete({rabbit_exchange, XName}),
    rabbit_event:notify(exchange_deleted, [{name, XName}]),
    {deleted, X, Bindings}.
