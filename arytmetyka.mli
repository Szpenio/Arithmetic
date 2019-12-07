(************************************************)
(*      Arithmetics of approximated values.      *)
(************************************************)

(** type representing approximated values **)
type wartosc

(** Implicite we are assuming that all floar arguments are real numbers **)
(** different from -inf, inf, nan **)


(** wartosc_dokladnosc [x] [p] = x +/- p%; p > 0 **)
val wartosc_dokladnosc: float -> float -> wartosc

(* ** wartosc_od_do [x] [y] = [x;y]; x <= y **)
val wartosc_od_do: float -> float -> wartosc

(** wartosc_dokladna [x] = [x;x] **)
val wartosc_dokladna: float -> wartosc

(** in wartosc x y - checks whether value [y] is in the given interval [x] **)
val in_wartosc: wartosc -> float -> bool

(** min_wartosc x returns infimum value from interval [x], or -inf if there is no infimum **)
val min_wartosc: wartosc -> float

(** max_wartosc x returns supremum value from interval [x], or inf if there is no supremum **)
val max_wartosc: wartosc -> float

(** returns average value of infimum and supremum of interval [x]; nan if both these values are infinite **)
val sr_wartosc:  wartosc -> float

    (** Arithmetic operations on approximated values **)
val plus:      wartosc -> wartosc -> wartosc
val minus:     wartosc -> wartosc -> wartosc
val razy:      wartosc -> wartosc -> wartosc
val podzielic: wartosc -> wartosc -> wartosc
