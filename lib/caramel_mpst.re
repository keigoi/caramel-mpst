type session('a) = {
  mpchan: Transport.mpchan,
  dummy_witness: 'a,
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

// 多相ヴァリアントのコンストラクタ. open_variant([> `Bob('v)], 'v)
// [`Bob; `Carol; `Alice] <<-- list([> `Alice | `Bob | `Carol])
type open_variant('var, 'v) = 'v => 'var;

// (fun | `Bob(v) => v | `Alice(v) => v) : [< `Bob('v) | `Alice('v)] => 'v
type closed_variant('var, 'v) = 'var => 'v;

type disj('lr, 'l, 'r) = {
  concat: (list('l), list('r)) => list('lr),
  split: list('lr) => (list('l), list('r)),
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

// (x => `Bob(x))  を Erlang のアトム bob に変換する
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
  // payload
  // x => `hello(x)
  // x => `Bob(x)
  // mpchan のところに pid の一覧が入っている
  (sess, role, label, v) => {
    let roletag /* アトムbob */ = open_variant_to_tag(role);
    let labeltag /* hello */ = open_variant_to_tag(label);
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
  (_from, _to, _label, _next) =>
    /*
     (alice --> bob)(hello, next)
     next : global('a, 'b, 'c)
     これがほしい: global([`Bob(out([`hello(session('a))]))], [`Alice(inp([`hello(session('b))]))], 'c)
     let bob_next = bob.role_lens.get(next);
     let bob_now = alice.role_label(hello.label_open(bob_next));
     let next = bob.role_lens.put(next, bob_now);
     let alice_next = alice.role_lens.get(next);
     let alice_now = bob.role_label(hello.label_close(alice_next));
     alice.role_lens.put(next,alice_now)
      */
    Raw.dontknow();

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
  (_alice, _disj, (_alice1, _left), (_alice2, _right)) =>
    /*
      choice(alice)(disj)(alice, (alice --> bob)(hello, left),
                          alice, (alice --> bob)(goodbye, right))
      let

     */
    Raw.dontknow();

let extract:
  'a 'b 'c.
  (
    unit => global('a, 'b, 'c),
    unit => role('t, _, global('a, 'b, 'c), _, _, _)
  ) =>
  session('t)
 =
  (_g, _role) => {
    Raw.todo();
            // {mpchan: /* ここに入れる */, dummy_witness: Raw.dontknow()}
  };

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

let a = ch =>
  if (true) {
    let ch1 = send(ch, x => `Bob(x), x => `hello(x), 123);
    switch (receive(ch1, x => `Carol(x))) {
    | `hello(_v, ch2) => close(ch2)
    };
  } else {
    let ch1 = send(ch, x => `Bob(x), x => `goodbye(x), 123);
    close(ch1);
  };

let b = ch => {
  let ch3 =
    switch (receive(ch, x => `Alice(x))) {
    | `hello(v, ch2) => send(ch2, x => `Carol(x), x => `hello(x), v + 123)
    | `goodbye(_v, ch2) =>
      send(ch2, x => `Carol(x), x => `goodbye(x), "foo")
    };
  close(ch3);
};

let c = ch => {
  let ch3 =
    switch (receive(ch, x => `Bob(x))) {
    | `hello(_v, ch2) => send(ch2, x => `Alice(x), x => `hello(x), 123)
    | `goodbye(_v, ch2) => ch2
    };
  close(ch3);
};

let from_some = opt => {
  switch (opt) {
  | Some(v) => v
  | None => Raw.fail()
  };
};

let start = (_g: global(_, _, _), fa, fb, fc) => {
  let pid_a =
    Process.make((_, recv) => {
      let ch_a = from_some(recv(~timeout=Process.Infinity));
      (fa(ch_a): unit);
    });
  let pid_b =
    Process.make((_, recv) => {
      let ch_b = from_some(recv(~timeout=Process.Infinity));
      (fb(ch_b): unit);
    });
  let pid_c =
    Process.make((_, recv) => {
      let ch_c = from_some(recv(~timeout=Process.Infinity));
      (fc(ch_c): unit);
    });
  let ch_a: session(_) =
    /* ここで `alice->pid_a, `bob->pid_b, `carol->pid_c の Map を作る */ Raw.dontknow();
  let ch_b: session(_) =
    /* ここで `alice->pid_a, `bob->pid_b, `carol->pid_c の Map を作る */ Raw.dontknow();
  let ch_c: session(_) =
    /* ここで `alice->pid_a, `bob->pid_b, `carol->pid_c の Map を作る */ Raw.dontknow();
  Process.send(pid_a, ch_a);
  Process.send(pid_b, ch_b);
  Process.send(pid_c, ch_c);
  ();
};

let () = {
  start(
    (alice --> bob)(hello, finish, ()),
    ch => {
      // Alice
      let ch' = send(ch, x => `Bob(x), x => `hello(x), 123);
      close(ch');
    },
    ch => {
      // Bob
      let `hello(_v, ch') = receive(ch, x => `Alice(x));
      close(ch');
    },
    ch => {
      // Carol
      close(ch)
    },
  );
};

// let f = () => {
//   let x = 1;
//   let x = 2;
//   ()
// }
