% Source code generated with Caramel.
-module(process).
-export_type([after_time/0]).
-export_type([recv/1]).

-export([contramap/2]).
-export([make/1]).
-export([recv/1]).
-export([send/2]).

-type after_time() :: infinity
                    | {bounded, integer()}
                    .

-type recv(M) :: fun((after_time()) -> option:t(M)).

-spec recv(after_time()) -> _.
recv(Timeout) ->
  case Timeout of
    infinity -> caramel_runtime:recv();
    {bounded, T} -> caramel_runtime:recv(T)
  end.

-spec make(fun((erlang:pid(), recv(_)) -> _)) -> erlang:pid().
make(F) -> erlang:spawn(fun
  () ->
  Pid = erlang:self(),
  F(Pid, fun recv/1)
end).

-spec send(erlang:pid(), _) -> ok.
send(Proc, Msg) -> erlang:send(Proc, Msg).

-spec contramap(fun((_) -> _), erlang:pid()) -> erlang:pid().
contramap(F, Pid) -> make(fun
  (_self, Recv) ->
  case Recv(infinity) of
    {some, A} -> send(Pid, F(A));
    none -> ok
  end
end).


