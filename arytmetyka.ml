(** Project: Arithmetics of approximated values **)
(** Author: Antoni Koszowski  **)

(** type representing approximated values **)
(** [Pw] - proper interval (e.x. [1, 10]); [Pp] - opposite interval (e.x. [-inf, -5] U [10, inf]); [Pn] -  incorrect interval (containing nan) **)
type wartosc =
    Pw of float * float | Pp of float * float
  | Pn of float * float

(** all arguments of type float are real numbers, different from -inf, inf, nan **)
(** wartosc_dokladnosc [x] [p] = x +/- p% **)
let wartosc_dokladnosc x p =
  if x >= 0. then
    Pw (x *. (1. -. (p /. 100.)), x *. (1. +. (p /. 100.)))
  else
    Pw (x *. (1. +. (p /. 100.)), x *. (1. -. (p /. 100.)))

(** wartosc_od_do [x] [y] = [x;y] **)
let wartosc_od_do l p =
  Pw (l, p)

(** wartosc_dokladna [x] = [x;x] **)
let wartosc_dokladna x =
  Pw (x, x)


(** returns the smallest possible end of interval for given operations **)
let minir l1 p1 l2 p2 =
  min (min (l1 *. l2) (l1 *. p2)) (min (p1 *. l2) (p1 *. p2))

let minip l1 p1 l2 p2 =
  min (min (l1 /. l2) (l1 /. p2)) (min (p1 /. l2) (p1 /. p2))

(** returns the biggest possible end of interval for given operations **)
let maksr l1 p1 l2 p2 =
  max (max (l1 *. l2) (l1 *. p2)) (max (p1 *. l2) (p1 *. p2))

let maksp l1 p1 l2 p2 =
  max (max (l1 /. l2) (l1 /. p2)) (max (p1 /. l2) (p1 /. p2))

(** checks whether opposite interval after transformation changes into R **)
let fix l p =
  if l >= p then Pw (neg_infinity, infinity)
  else Pp (l, p)

(** swaps ends of interval **)
let swap l p =
  if l > p then Pp (p, l)
  else Pp (l, p)


(** checks whether value [y] is in the given interval [x] **)
let in_wartosc x y =
  match x with
    Pw (l, p) -> if y >= l && y <= p then true else false
  | Pp (l, p) -> if y <= l || y >= p then true else false
  | _ -> false

(** returns infimum value from interval [x] **)
let min_wartosc x =
  match x with
    Pw (l, p) -> l
  | Pp (l, p)-> neg_infinity
  | _ -> nan

(** returns supremum value from interval [x] **)
let max_wartosc x =
  match x with
    Pw (l, p) -> p
  | Pp (l, p) -> infinity
  | _ -> nan

(** returns average value of infimum and supremum of interval [x]; nan if both these values are infinite **)
let sr_wartosc x =
  match x with
    Pw (l, p) -> (l +. p) /. 2.
  | _ -> nan


(** whatever operation with incorrect inerval returns incorrect interval **)
(** returns a set of elements, that can be found in interval made of by sum of elements from intervals [a], [b] **)
let plus a b =
  match a, b with
    Pn (_, _), _ -> Pn (nan, nan)
  | _, Pn (_, _ ) -> Pn (nan,nan)
  | Pw (l1, p1), Pw (l2, p2) -> Pw (l1 +. l2, p1 +. p2)
  | Pw (l1, p1), Pp (l2, p2) -> fix (l2 +. p1) (p2 +. l1)
  | Pp (l1, p1), Pw (l2, p2) -> fix (l1 +. p2) (p1 +. l2)
  | Pp (l1, p1), Pp (l2, p2) -> Pw (neg_infinity, infinity)

(** returns a set of elements, that can be found in interval made of by substraction of elements from intervals [a], [b] **)
let minus a b =
  match a, b with
    Pn (_, _), _ -> Pn (nan, nan)
  | _, Pn (_, _) -> Pn (nan, nan)
  | Pw (l1, p1), Pw (l2, p2) -> Pw (l1 -. p2, p1 -. l2)
  | Pw (l1, p1), Pp (l2, p2) -> fix (p1 -. p2) (l1 -. l2)
  | Pp (l1, p1), Pw (l2, p2) -> fix (l1 -. l2) (p1 -. p2)
  | Pp (l1, p1), Pp (l2, p2) -> Pw (neg_infinity, infinity)

(** returns a set of elements, that can be found in interval made of by multiplication of elements from intervals [a], [b] **)
let razy a b =
  match a, b with
    Pn (_, _), _ -> Pn (nan, nan)
  | _, Pn (_, _) -> Pn (nan, nan)
  | Pw (l1, p1), Pw (l2, p2) ->
    if a = wartosc_dokladna 0. || b = wartosc_dokladna 0. then wartosc_dokladna 0.
    else
    if in_wartosc b 0. then
      if l2 = 0. then
        if l1 >= 0. then Pw (0., p1 *. p2)
        else
        if p1 <= 0. then Pw (l1 *. p2, 0.)
        else
          Pw (l1 *. p2, p1 *. p2)
      else
      if p2 = 0. then
        if l1 >= 0. then Pw (p1 *. l2, 0.)
        else
        if p1 <= 0. then Pw (0., l1 *. l2 )
        else
          Pw (p1 *. l2, l1 *. l2)
      else
        Pw (minir l1 p1 l2 p2, maksr l1 p1 l2 p2)
    else
      Pw (minir l1 p1 l2 p2, maksr l1 p1 l2 p2)
  | Pw (l1, p1), Pp (l2, p2) ->
    if in_wartosc a 0. then
      if a = wartosc_dokladna 0. then Pw (0., 0.)
      else
        Pw (neg_infinity, infinity)
    else
    if l1 > 0. then fix (max (l1 *. l2) (p1 *. p2)) (min (p2 *. l1) (p2 *. p1))
    else
      fix (min (p2 *. l1) (p2 *. p1)) (max (l2 *. l1) (l2 *. p1))

  | Pp (l1, p1), Pw (l2, p2) ->
    if in_wartosc b 0. then
      if b = wartosc_dokladna 0. then Pw (0., 0.)
      else
        Pw (neg_infinity, infinity)
    else
    if l2 > 0. then fix (max (l1 *. l2) (l1 *. p2)) (min (p1 *. l2) (p1 *. p2))
    else
      fix (max (p1 *. l2) (p1 *. p2)) (min (l1 *. l2) (l1 *. p2))
  | Pp (l1, p1), Pp(l2, p2) ->
    if (l1 < 0. && p1 > 0.) && (l2 < 0. && p2 > 0.) then swap (minir l1 p1 l2 p2) (maksr l1 p1 l2 p2)
    else
      Pw (neg_infinity, infinity)

(** returns a set of elements, that can be found in interval made of by division of elements from intervals [a], [b] **)
let podzielic a b =
  match a, b with
    Pn (_, _), _ -> Pn (nan, nan)
  | _, Pn (_, _) -> Pn (nan, nan)
  | Pw (l1, p1), Pw (l2, p2) ->
    if b = wartosc_dokladna 0. then Pn (nan, nan)
    else
    if a = wartosc_dokladna 0. then Pw (0., 0.)
    else
    if in_wartosc b 0. then
      if l2 = 0. then
        if p1 < 0. then Pw (neg_infinity, p1 /. p2)
        else
        if l1 > 0. then Pw (l1 /. p2, infinity)
        else
        if p1 = 0. then Pw (neg_infinity, 0.)
        else
        if l1 = 0. then Pw (0., infinity)
        else
          Pw (neg_infinity, infinity)
      else
      if p2 = 0. then
        if p1 < 0. then Pw (p1 /. l2, infinity)
        else
        if l1 > 0. then Pw (neg_infinity, l1 /. l2)
        else
        if p1 = 0. then Pw (0., infinity)
        else
        if l1 = 0. then Pw (neg_infinity, 0.)
        else
          Pw (neg_infinity, infinity)
      else
      if in_wartosc a 0. then Pw (neg_infinity, infinity)
      else
        fix (max (min (l1 /. l2) (l1 /. p2)) (min (p1 /. l2) (p1 /. p2))) (min (max (l1 /. l2) (l1 /. p2)) (max (p1 /. l2) (p1 /. p2)))
    else
      Pw (minip l1 p1 l2 p2, maksp l1 p1 l2 p2)

  | Pw (l1, p1), Pp (l2, p2) ->
    if a = wartosc_dokladna 0. then Pw (0., 0.)
    else
    if l2 >= 0. || p2 <= 0. then
      if l2 = 0. then
        if l1 >= 0. then Pw (neg_infinity, p1 /. p2)
        else
        if p1 <= 0. then Pw (l1 /. p2, infinity)
        else
          Pw (neg_infinity, infinity)
      else
      if p2 = 0. then
        if l1 >= 0. then Pw (p1 /. l2, infinity)
        else
        if p1 <= 0. then Pw (neg_infinity, l1 /. l2)
        else
          Pw (neg_infinity, infinity)
      else
      if l2 > 0. then
        if l1 >= 0. then fix (p1 /. p2) (l1 /. l2)
        else
        if p1 <= 0. then fix (p1 /. l2) (l1 /. p2)
        else
          Pw (neg_infinity, infinity)
      else
        if l1 >= 0. then fix (l1 /. p2) (p1 /. l2)
        else
        if p1 <= 0. then fix (p1 /. p2) (l1 /. l2)
        else
          Pw (neg_infinity, infinity)
    else
      Pw (minip l1 p1 l2 p2, maksp l1 p1 l2 p2)
  | Pp (l1, p1), Pw (l2, p2) ->
    if b = wartosc_dokladna 0. then Pn (nan, nan)
    else
    if in_wartosc b 0. then
      if l2 = 0. then Pp (l1 /. p2, p1 /. p2)
      else
      if p2 = 0. then Pp (p1 /. l2, l1 /. l2)
      else
        Pp (max (l1 /. p2) (p1 /. l2), min (p1 /. p2) (l1 /. l2))
    else
    if l2 > 0. then Pp (max (l1 /. l2) (l1 /. p2), min (p1 /. l2) (p1 /. p2))
    else
      Pp (max (p1 /. l2) (p1 /. p2), min (l1 /. l2) (l1 /. p2))

  | Pp (l1, p1), Pp (l2, p2) -> Pw (neg_infinity, infinity)
