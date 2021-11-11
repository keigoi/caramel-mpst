% Source code generated with Caramel.
-module(transport).
-export_type([handle/1]).
-export_type([label_tag/0]).
-export_type([mpchan/0]).
-export_type([mpst_msg/0]).
-export_type([payload/0]).
-export_type([role_tag/0]).

-export([raw_receive/1]).
-export([raw_send/4]).

-type role_tag() :: polyvar:tag().

-type label_tag() :: polyvar:tag().

-opaque payload() :: reference().

-type mpst_msg() :: {role_tag(), label_tag(), payload()}.

-type handle(Msg) :: #{ pid => erlang:pid() }.

-type mpchan() :: #{ self => role_tag()
                   , channels => maps:t(role_tag(), handle(mpst_msg()))
                   }.

-spec raw_send(mpchan(), role_tag(), label_tag(), _) -> ok.
raw_send(Mpchan, Role, Label, V) ->
  Ch = maps:get(Role, maps:get(channels, Mpchan), raw:dontknow()),
  process:send(maps:get(pid, Ch), {maps:get(self, Mpchan), Label, payload_cast(V)}).

-spec raw_receive(role_tag()) -> {label_tag(), _}.
raw_receive(From) ->
  {_, Label, V} = raw:guarded_receive(fun
  ({Role, _, _}) -> erlang:'=:='(Role, From)
end),
  {Label, V}.


