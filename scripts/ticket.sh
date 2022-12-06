#!/usr/bin/env bash

## Generate ticket.pyv and then apply a whole bunch of manual fixes to the
## file.
## 
## Outputs on stdout.

# operate in this temp file in-place, then cat the output at the end (just to
# avoid writing a very long pipe)
file=$(mktemp "ticket-XXXXXXXXX.pyv")

SED() {
  sed -i "$@" "$file"
}

cargo run -q -- tests/ticket.l2s.out > "$file"

SED 's/m(?, ?)/m(thread,ticket)/'
SED 's/next_ticket(?)/next_ticket(ticket)/'
SED 's/service(?)/service(ticket)/'
SED 's/scheduled(?)/scheduled(thread)/'

if grep '?' ticket.pyv >/dev/null 2>&1; then
  echo "not all types specified" 1>&2
  exit 1
fi

SED '/^mutable relation zero$/d'
SED '3aimmutable constant zero: ticket'
SED '4aaxiom forall X. le(zero, X)'
SED '5aimmutable constant t0: thread'
SED '6G'
# total_order(le)
SED '7aimmutable relation le(ticket, ticket)'
SED '8aaxiom le(X,X)'
SED '9aaxiom le(X, Y) & le(Y, Z) -> le(X, Z)'
SED '10aaxiom le(X, Y) & le(Y, X) -> X = Y'
SED '11aaxiom le(X, Y) | le(Y, X)'
SED '12G'

SED 's/^mutable relation l2s_a.*/mutable relation l2s_a_t(thread)\nmutable relation l2s_a_k(ticket)/'
SED 's/^mutable relation l2s_d.*/mutable relation l2s_d_t(thread)\nmutable relation l2s_d_k(ticket)/'
SED '/modifies/s/l2s_a/l2s_a_t,l2s_a_k/'
SED '/modifies/s/l2s_d/l2s_d_t,l2s_d_k/'
SED 's/(new(l2s_a(X)) <-> l2s_d(X))/(new(l2s_a_t(T)) <-> l2s_d_t(T)) \& (new(l2s_a_k(K)) <-> l2s_d_k(K))/'
SED 's/l2s_d(t0)/l2s_d_t(t0)/'
SED 's/l2s_a(t0)/l2s_a_t(t0)/'

# heuristic based on name
SED 's/\(l2s_d\|l2s_a\)(T/\1_t(T/g'
SED 's/\(l2s_d\|l2s_a\)(K/\1_k(K/g'
# heuristics based on observation
SED 's/l2s_a(V0) \& l2s_a(V1)/l2s_a_t(V0) \& l2s_a_k(V1)/g'
SED 's/l2s_d(V0) \& l2s_d(V1)/l2s_d_t(V0) \& l2s_d_k(V1)/g'

# not exactly heuristics
SED 's/l2s_\(a\|d\)(V0) -> \((\(pc\|l2s_s_0\|l2s_s_1\|l2s_s_2\)\)/l2s_\1_t(V0) -> \2/g'
SED 's/l2s_\(a\|d\)(V0) -> \((\(service\|next_ticket\|l2s_s_4\|l2s_s_5\)\)/l2s_\1_k(V0) -> \2/g'
SED '/init/s/l2s_d(V0)/l2s_d_t(V0)/'

# warning: depends on previous substitutions
SED 's/l2s_d(V0)/l2s_d_t(V0)/g'

SED 's/l2s_d(zero)/l2s_d_k(zero)/'

pat="(new(l2s_d_t(T0)) <-> if T0 = t0 then true else (if T0 = zero then true else (if T0 = s then true else (if T0 = next_ticket0 then true else (if T0 = t then true else l2s_d_t(T0))))))"
repl="(new(l2s_d_t(T0)) <-> T0=t0 | T0=t | l2s_d_t(T0))"
repl="$repl \\& (new(l2s_d_k(K0)) <-> K0=zero | K0=s | K0=next_ticket0 | l2s_d_k(K0))"
SED "s/$pat/$repl/"

pat="(new(l2s_d_t(T0)) <-> if T0 = t0 then true else (if T0 = zero then true else (if T0 = service0 then true else (if T0 = k then true else (if T0 = t then true else l2s_d_t(T0))))))"
repl="(new(l2s_d_t(T0)) <-> T0=t0 | T0=t | l2s_d_t(T0))"
repl="$repl \\& (new(l2s_d_k(K0)) <-> K0=k | K0=zero | K0=service0 | l2s_d_k(K0))"
SED "s/$pat/$repl/"

pat="(new(l2s_d_t(T0)) <-> if T0 = t0 then true else (if T0 = zero then true else (if T0 = service0 then true else (if T0 = s then true else (if T0 = t then true else l2s_d_t(T0))))))"
repl="(new(l2s_d_t(T0)) <-> T0=t0 | T0=t | l2s_d_t(T0))"
repl="$repl \\& (new(l2s_d_k(K0)) <-> K0=zero | K0=service0 | K0=s | l2s_d_k(K0))"
SED "s/$pat/$repl/"

pat="(new(l2s_d_t(T0)) <-> if T0 = t0 then true else (if T0 = zero then true else l2s_d_t(T0)))"
repl="(new(l2s_d_t(T0)) <-> T0=t0 | l2s_d_t(T0))"
repl="$repl \\& (new(l2s_d_k(K0)) <-> K0=zero | l2s_d_k(K0))"
SED "s/$pat/$repl/"

SED 's/new(l2s_s_4(V0)) <-> next_ticket(V0)/forall V0. \0/'
SED 's/new(l2s_s_5(V0)) <-> service(V0)/forall V0. \0/'

cat "$file"
rm "$file"
