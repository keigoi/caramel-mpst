% Source code generated with Caramel.
-module(caramel_mpst).
-export_type([closed_variant/2]).
-export_type([disj/3]).
-export_type([global/3]).
-export_type([inp/1]).
-export_type([label/4]).
-export_type([lens/4]).
-export_type([open_variant/2]).
-export_type([out/1]).
-export_type([role/6]).
-export_type([session/1]).

-export(['-->'/4]).
-export([a/1]).
-export([alice/0]).
-export([b/1]).
-export([bob/0]).
-export([c/1]).
-export([carol/0]).
-export([choice_at/4]).
-export([close/1]).
-export([extract/2]).
-export([finish/0]).
-export([from_some/1]).
-export([g/1]).
-export([goodbye/0]).
-export([hello/0]).
-export([hello_or_goodbye/0]).
-export([lens_a/0]).
-export([lens_b/0]).
-export([lens_c/0]).
-export([list_match/2]).
-export([open_variant_to_tag/1]).
-export([receive/2]).
-export([send/4]).
-export([start/4]).
-export([to_bob/1]).

-type session(A) :: #{ mpchan => transport:mpchan()
                     , dummy_witness => A
                     }.

-type global(A, B, C) :: {session(A), session(B), session(C)}.

-type lens(A, B, S, T) :: #{ get => fun((S) -> session(A))
                           , put => fun((S, session(B)) -> T)
                           }.

-type open_variant(Var, V) :: fun((V) -> Var).

-type closed_variant(Var, V) :: fun((Var) -> V).

-type disj(Lr, L, R) :: #{ concat => fun((list(L), list(R)) -> list(Lr))
                         , split => fun((list(Lr)) -> {list(L), list(R)})
                         }.

-type role(A, B, S, T, Obj, V) :: #{ role_label => closed_variant(Obj, V)
                                   , role_lens => lens(A, B, S, T)
                                   }.

-type label(Obj, T, Var, U) :: #{ label_closed => closed_variant(Obj, T)
                                , label_open => open_variant(Var, U)
                                }.

-type out(Lab) :: #{ '__out_witness' => Lab }.

-type inp(Lab) :: #{ '__inp_witness' => Lab }.

-spec lens_a() -> lens(A, B, {session(A), C, D}, {session(B), C, D}).
lens_a() ->
  #{ get => fun
  ({A, _, _}) -> A
end
   , put => fun
  ({_, B, C}, A) -> {A, B, C}
end
   }.

-spec lens_b() -> lens(A, B, {C, session(A), D}, {C, session(B), D}).
lens_b() ->
  #{ get => fun
  ({_, B, _}) -> B
end
   , put => fun
  ({A, _, C}, B) -> {A, B, C}
end
   }.

-spec lens_c() -> lens(A, B, {C, D, session(A)}, {C, D, session(B)}).
lens_c() ->
  #{ get => fun
  ({_, _, C}) -> C
end
   , put => fun
  ({A, B, _}, C) -> {A, B, C}
end
   }.

-spec alice() -> role(A, B, {session(A), C, D}, {session(B), C, D}, {alice, E}
   , E).
alice() ->
  #{ role_label => fun
  ({alice, V}) -> V
end
   , role_lens => lens_a()
   }.

-spec bob() -> role(A, B, {C, session(A), D}, {C, session(B), D}, {bob, E}
 , E).
bob() ->
  #{ role_label => fun
  ({bob, V}) -> V
end
   , role_lens => lens_b()
   }.

-spec carol() -> role(A, B, {C, D, session(A)}, {C, D, session(B)}, {carol, E}
   , E).
carol() ->
  #{ role_label => fun
  ({carol, V}) -> V
end
   , role_lens => lens_c()
   }.

-spec hello() -> label({hello, A}
   , A, {hello, B}
   , B).
hello() ->
  #{ label_closed => fun
  ({hello, V}) -> V
end
   , label_open => fun
  (V) -> {hello, V}
end
   }.

-spec goodbye() -> label({goodbye, A}
     , A, {goodbye, B}
     , B).
goodbye() ->
  #{ label_closed => fun
  ({goodbye, V}) -> V
end
   , label_open => fun
  (V) -> {goodbye, V}
end
   }.

-spec list_match(fun((_a) -> _b), list(_a)) -> _b.
list_match(_, _) -> raw:assertfalse().

-spec hello_or_goodbye() -> disj({goodbye, A}
              | {hello, B}
              , {hello, B}
              , {goodbye, A}
              ).
hello_or_goodbye() ->
  #{ concat => fun
  (L, R) -> [{hello, list_match(fun
  ({hello, V}) -> V
end, L)} | [{goodbye, list_match(fun
  ({goodbye, V}) -> V
end, R)} | []]]
end
   , split => fun
  (Lr) -> {[{hello, list_match(fun
  ({hello, V}) -> V;
  ({goodbye, _}) -> raw:dontknow()
end, Lr)} | []], [{goodbye, list_match(fun
  ({goodbye, V}) -> V;
  ({hello, _}) -> raw:dontknow()
end, Lr)} | []]}
end
   }.

-spec to_bob(fun(() -> disj(A, B, C))) -> disj({bob, out(A)}
    , {bob, out(B)}
    , {bob, out(C)}
    ).
to_bob(Disj) ->
  #{ concat => fun
  (L, R) -> lists:map(fun
  (V) -> {bob, #{ '__out_witness' => V }}
end, maps:get(concat, Disj())(lists:map(fun
  ({bob, V}) -> maps:get('__out_witness', V)
end, L), lists:map(fun
  ({bob, V}) -> maps:get('__out_witness', V)
end, R)))
end
   , split => fun
  (Lr) ->
  {L, R} = maps:get(split, Disj())(lists:map(fun
  ({bob, V}) -> maps:get('__out_witness', V)
end, Lr)),
  {lists:map(fun
  (V) -> {bob, #{ '__out_witness' => V }}
end, L), lists:map(fun
  (V) -> {bob, #{ '__out_witness' => V }}
end, R)}
end
   }.

-spec open_variant_to_tag(open_variant(_, _)) -> polyvar:tag().
open_variant_to_tag(Var) ->
  {Roletag, _} = raw:destruct_polyvar(Var(raw:dontknow())),
  Roletag.

-spec send(session(_var), open_variant(_var, out(_lab)), open_variant(_lab, {_v, session(_c)}), _v) -> session(_c).
send(Sess, Role, Label, V) ->
  Roletag = open_variant_to_tag(Role),
  Labeltag = open_variant_to_tag(Label),
  begin
    transport:raw_send(maps:get(mpchan, Sess), Roletag, Labeltag, V),
    #{ mpchan => maps:get(mpchan, Sess)
     , dummy_witness => raw:dontknow()
     }
  end.

-spec receive(session(_var), open_variant(_var, inp(_lab))) -> _lab.
receive(Sess, Role) ->
  Roletag = open_variant_to_tag(Role),
  {Labeltag, V} = transport:raw_receive(Roletag),
  Cont = #{ mpchan => maps:get(mpchan, Sess)
   , dummy_witness => raw:dontknow()
   },
  raw:make_polyvar(Labeltag, {V, Cont}).

-spec close(session(ok)) -> ok.
close(_) -> ok.

-spec '-->'(fun(() -> role(_s, _to_, _mid, _cur, _from, inp(_inplab))), fun(() -> role(_t, _from, _next, _mid, _to_, out(_outlab))), fun(() -> label(_outlab, {_v, session(_s)}, _inplab, {_v, session(_t)})), fun(() -> _next), ok) -> _cur.
-->(_from, _to, _label, _next) -> raw:dontknow().

-spec finish() -> global(ok, ok, ok).
finish() -> raw:dontknow().

-spec choice_at(fun(() -> role(ok, _lr, global(_a, _b, _c), _cur, _x, _)), disj(_lr, _l, _r), {fun(() -> role(_l, ok, _left, global(_a, _b, _c), _x, _)), fun(() -> _left)}, {fun(() -> role(_r, ok, _right, global(_a, _b, _c), _x, _)), fun(() -> _right)}, ok) -> _cur.
choice_at(_alice, _disj, {_alice1, _left}, {_alice2, _right}) -> raw:dontknow().

-spec extract(fun(() -> global(_a, _b, _c)), fun(() -> role(_t, _, global(_a, _b, _c), _, _, _))) -> session(_t).
extract(_g, _role) -> raw:todo().

-spec g(ok, ok) -> {session({bob, out({goodbye, {A, session(ok)}}
| {hello, {B, session({carol, inp({hello, {C, session(ok)}}
 )}
 )}}
)}
), session({alice, inp({hello, {B, session({carol, out({hello, {D, session(ok)}}
)}
)}}
), inp({goodbye, {A, session({carol, out({goodbye, {E, session(ok)}}
)}
)}}
)}
), session({bob, inp({hello, {D, session({alice, out({hello, {C, session(ok)}}
)}
)}}
), inp({goodbye, {E, session(ok)}}
)}
)}.
g() -> choice_at(fun alice/0, to_bob(fun hello_or_goodbye/0), {fun alice/0, '-->'(fun alice/0, fun bob/0)(fun hello/0, '-->'(fun bob/0, fun carol/0)(fun hello/0, '-->'(fun carol/0, fun alice/0)(fun hello/0, fun finish/0)))}, {fun alice/0, '-->'(fun alice/0, fun bob/0)(fun goodbye/0, '-->'(fun bob/0, fun carol/0)(fun goodbye/0, fun finish/0))}).

-spec a(session({bob, out({goodbye, {integer(), session(ok)}}
| {hello, {integer(), session({carol, inp({hello, {_, session(ok)}}
 )}
 )}}
)}
)) -> ok.
a(Ch) ->
  case true of
    true -> Ch1 = send(Ch, fun
  (X) -> {bob, X}
end, fun
  (X) -> {hello, X}
end, 123),
case receive(Ch1, fun
  (X) -> {carol, X}
end) of
  {hello, {_v, Ch2}} -> close(Ch2)
end;
    false -> Ch1 = send(Ch, fun
  (X) -> {bob, X}
end, fun
  (X) -> {goodbye, X}
end, 123),
close(Ch1)
  end.

-spec b(session({alice, inp({goodbye, {_, session({carol, out({goodbye, {binary(), session(ok)}}
)}
)}}
| {hello, {integer(), session({carol, out({hello, {integer(), session(ok)}}
 )}
 )}}
)}
)) -> ok.
b(Ch) ->
  Ch3 = case receive(Ch, fun
  (X) -> {alice, X}
end) of
    {hello, {V, Ch2}} -> send(Ch2, fun
  (X) -> {carol, X}
end, fun
  (X) -> {hello, X}
end, erlang:'+'(V, 123));
    {goodbye, {_v, Ch2}} -> send(Ch2, fun
  (X) -> {carol, X}
end, fun
  (X) -> {goodbye, X}
end, <<"foo">>)
  end,
  close(Ch3).

-spec c(session({bob, inp({goodbye, {_, session(ok)}}
| {hello, {_, session({alice, out({hello, {integer(), session(ok)}}
 )}
 )}}
)}
)) -> ok.
c(Ch) ->
  Ch3 = case receive(Ch, fun
  (X) -> {bob, X}
end) of
    {hello, {_v, Ch2}} -> send(Ch2, fun
  (X) -> {alice, X}
end, fun
  (X) -> {hello, X}
end, 123);
    {goodbye, {_v, Ch2}} -> Ch2
  end,
  close(Ch3).

-spec from_some(option:t(A)) -> A.
from_some(Opt) ->
  case Opt of
    {some, V} -> V;
    none -> raw:fail()
  end.

-spec start(global(_, _, _), fun((session(_)) -> ok), fun((session(_)) -> ok), fun((session(_)) -> ok)) -> ok.
start(_g, A, B, C) ->
  Pid_a = process:make(fun
  (_, Recv) ->
  Ch_a = from_some(Recv(infinity)),
  A(Ch_a)
end),
  Pid_b = process:make(fun
  (_, Recv) ->
  Ch_b = from_some(Recv(infinity)),
  B(Ch_b)
end),
  Pid_c = process:make(fun
  (_, Recv) ->
  Ch_c = from_some(Recv(infinity)),
  C(Ch_c)
end),
  Ch_a = raw:dontknow(),
  Ch_b = raw:dontknow(),
  Ch_c = raw:dontknow(),
  begin
    process:send(Pid_a, Ch_a),
    process:send(Pid_b, Ch_b),
    process:send(Pid_c, Ch_c),
    ok
  end.


