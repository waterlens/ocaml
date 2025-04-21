(* TEST

flags = "-w +A-70"

* setup-ocamlc.byte-build-env
** ocamlc.byte
compile_only = "true"
*** check-ocamlc.byte-output

*)

type 'a mut_cell =
  | MEmpty of int
  | MFull of { mutable data: 'a }

type 'a cell =
  | Empty of int
  | Full of 'a

let non_op = function
  | Empty dummy -> Empty dummy [@share] (* ok, shared *)
  | Full data -> Full data [@share] (* ok, shared *)

let to_immut = function
  | MEmpty dummy -> Empty dummy [@share] (* ok, shared *)
  | MFull data -> Full data.data [@share] (* not ok, not shared *)

let to_mut = function
  | Empty dummy -> MEmpty dummy [@share] (* ok, shared *)
  | Full data -> MFull ({ data } [@share]) (* not ok, warned *)

let to_mut_wrongly_annotated = function
  | Empty dummy -> MEmpty dummy [@share] (* ok, shared *)
  | Full data -> MFull { data } [@share] (* [share] on `Texp_construct` but not `Texp_record`, silent *)

let to_immut_with_hint = function
  | MEmpty dummy -> Empty dummy [@share hint] (* ok, shared *)
  | MFull data -> Full data.data [@share hint] (* ok, not shared *)

let non_op_with_never = function
  | Empty dummy -> Empty dummy [@share never] (* ok, not shared *)
  | Full data -> Full data [@share never] (* ok, not shared *)

let non_op_with_always = function
  | Empty dummy -> Empty dummy [@share always] (* ok, shared *)
  | Full data -> Full data [@share always] (* ok, shared *)
