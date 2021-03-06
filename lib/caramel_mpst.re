type session('a) = {
  dummy_witness: 'a,
  mpchan: Transport.mpchan,
};

// 多相ヴァリアントのコンストラクタ. open_variant([> `Bob('v)], 'v)
type open_variant('var, 'v) = 'v => 'var;

type closed_variant('var, 'v) = 'var => 'v;

type disj('lr, 'l, 'r) = {
  concat: (list('l), list('r)) => list('lr),
  split: list('lr) => (list('l), list('r)),
};

type global('a, 'b, 'c) = (session('a), session('b), session('c));

type lens('a, 'b, 's, 't) = {
  get: 's => session('a),
  put: ('s, session('b)) => 't,
};

let lens_a = () => {
  get: ((a, _, _)) => a,
  put: ((_, b, c), a) => (a, b, c),
};
let lens_b = () => {
  get: ((_, b, _)) => b,
  put: ((a, _, c), b) => (a, b, c),
};
let lens_c = () => {
  get: ((_, _, c)) => c,
  put: ((a, b, _), c) => (a, b, c),
};

type role('a, 'b, 's, 't, 'obj, 'v) = {
  role_label: closed_variant('obj, 'v),
  role_lens: lens('a, 'b, 's, 't),
};

let alice = () => {role_label: (`Alice(v)) => v, role_lens: lens_a()};

let bob = () => {role_label: (`Bob(v)) => v, role_lens: lens_b()};

let carol = () => {role_label: (`Carol(v)) => v, role_lens: lens_c()};

type label('obj, 't, 'var, 'u) = {
  label_closed: closed_variant('obj, 't),
  label_open: open_variant('var, 'u),
};

let hello = () => {
  label_closed: (`hello(v)) => v,
  label_open: v => `hello(v),
};

let goodbye = () => {
  label_closed: (`goodbye(v)) => v,
  label_open: v => `goodbye(v),
};

type out('lab) = {__out_witness: 'lab};

type inp('lab) = {__inp_witness: 'lab};

let list_match: ('a => 'b, list('a)) => 'b = (_, _) => Raw.assertfalse();

let hello_or_goodbye = () => {
  split: lr => (
    [
      `hello(
        list_match(
          fun
          | `hello(v) => v
          | `goodbye(_) => Raw.dontknow(),
          lr,
        ),
      ),
    ],
    [
      `goodbye(
        list_match(
          fun
          | `goodbye(v) => v
          | `hello(_) => Raw.dontknow(),
          lr,
        ),
      ),
    ],
  ),
  concat: (l, r) => [
    `hello(list_match((`hello(v)) => v, l)),
    `goodbye(list_match((`goodbye(v)) => v, r)),
  ],
};

let to_bob = disj => {
  concat: (l, r) =>
    Lists.map(
      v => `Bob({__out_witness: v}),
      disj().concat(
        Lists.map((`Bob(v)) => v.__out_witness, l),
        Lists.map((`Bob(v)) => v.__out_witness, r),
      ),
    ),

  split: lr => {
    let (l, r) = disj().split(Lists.map((`Bob(v)) => v.__out_witness, lr));
    (
      Lists.map(v => `Bob({__out_witness: v}), l),
      Lists.map(v => `Bob({__out_witness: v}), r),
    );
  },
};

let open_variant_to_tag: 'var. open_variant('var, _) => Polyvar.tag =
  var => {
    let (roletag, _) = Raw.destruct_polyvar(var(Raw.dontknow()));
    roletag;
  };

let send:
  'var 'lab 'v 'c.
  (
    session('var),
    open_variant('var, out('lab)),
    open_variant('lab, ('v, session('c))),
    'v
  ) =>
  session('c)
 =
  (sess, role, label, v) => {
    let roletag = open_variant_to_tag(role);
    let labeltag = open_variant_to_tag(label);
    Transport.raw_send(sess.mpchan, roletag, labeltag, v);
    {mpchan: sess.mpchan, dummy_witness: Raw.dontknow()};
  };

let receive:
  'var 'lab.
  (session('var), open_variant('var, inp('lab))) => 'lab
 =
  (sess, role) => {
    let roletag = open_variant_to_tag(role);
    let (labeltag, v) = Transport.raw_receive(~from=roletag);
    let cont = {mpchan: sess.mpchan, dummy_witness: Raw.dontknow()};
    Raw.make_polyvar(labeltag, (v, cont));
  };

let close: session(unit) => unit = _ => ();

let (-->):
  'from 'to_ 'outlab 'inplab 's 't 'v 'next 'mid 'cur.
  (
    unit => role('s, 'to_, 'mid, 'cur, 'from, inp('inplab)),
    unit => role('t, 'from, 'next, 'mid, 'to_, out('outlab)),
    unit => label('outlab, ('v, session('s)), 'inplab, ('v, session('t))),
    unit => 'next,
    unit
  ) =>
  'cur
 =
  (_from, _to, _label, _next) => Raw.dontknow();

let finish: unit => global(unit, unit, unit) = () => Raw.dontknow();

let choice_at:
  'cur 'a 'b 'c 'left 'right 'lr 'l 'r 'x.
  (
    unit => role(unit, 'lr, global('a, 'b, 'c), 'cur, 'x, _),
    disj('lr, 'l, 'r),
    (
      unit => role('l, unit, 'left, global('a, 'b, 'c), 'x, _),
      unit => 'left,
    ),
    (
      unit => role('r, unit, 'right, global('a, 'b, 'c), 'x, _),
      unit => 'right,
    ),
    unit
  ) =>
  'cur
 =
  (_alice, _disj, (_alice1, _left), (_alice2, _right)) => Raw.dontknow();

let extract:
  'a 'b 'c.
  (
    unit => global('a, 'b, 'c),
    unit => role('t, _, global('a, 'b, 'c), _, _, _)
  ) =>
  session('t)
 =
  (_g, _role) => Raw.todo();

// Example

let g = () =>
  choice_at(
    alice,
    to_bob(hello_or_goodbye),
    (
      alice,
      (alice --> bob)(
        hello,
        (bob --> carol)(hello, (carol --> alice)(hello, finish)),
      ),
    ),
    (alice, (alice --> bob)(goodbye, (bob --> carol)(goodbye, finish))),
  );

let a = () => {
  let ch = extract(g(), alice);
  let ch1 = send(ch, x => `Bob(x), x => `hello(x), 123);
  switch (receive(ch1, x => `Carol(x))) {
  | `hello(_v, ch2) => close(ch2)
  };
  ();
};

let b = () => {
  let ch = extract(g(), bob);
  let ch3 =
    switch (receive(ch, x => `Alice(x))) {
    | `hello(_v, ch2) => send(ch2, x => `Carol(x), x => `hello(x), 123)
    | `goodbye(_v, ch2) =>
      send(ch2, x => `Carol(x), x => `goodbye(x), "foo")
    };
  close(ch3);
};

let c = () => {
  let ch = extract(g(), carol);
  let ch3 =
    switch (receive(ch, x => `Bob(x))) {
    | `hello(_v, ch2) => send(ch2, x => `Alice(x), x => `hello(x), 123)
    | `goodbye(_v, ch2) => ch2
    };
  close(ch3);
};
