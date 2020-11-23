(** * Formalization of CPS-Conversion *)

(** We'll start out by defining the syntax of a direct-style program,
followed by defining what evaluating said program means. Once we've
done that, we'll define a CPS-conversion function, and then prove that
the resulting expression is equivalent to the original. *)

From Coq Require Import Bool.Bool.
From Coq Require Import Arith.Arith.
From Coq Require Import Arith.EqNat.
From Coq Require Import Arith.PeanoNat. Import Nat.
From Coq Require Import Lia.
From Coq Require Import Lists.List.
From Coq Require Import Strings.String.

(* This "CpsFormalization" comes from the _CoqProject file *)
From CpsFormalization Require Import Env.

(* ################################################################# *)
(** * Syntax Definition *)

(** We're going to be modeling an untyped lambda calculus for
    simplicity's stake. What follows below are the formal
    definitions of the syntax. *)

(* Primitive binary operators *)
Inductive prim_binop : Type :=
  | PrimPlus
  | PrimMinus
  | PrimMult
  | PrimEq.

Inductive sexpr : Type :=
  | SId (x : string)
  | SNum (n : nat)
  (* TODO: include a closure case maybe *)
  | SLambda (args : list string) (body : sexpr)
  (* I'd like to add a constraint that fexpr reduces to an instance of SLambda. *)
  | SApp (fexpr : sexpr) (args : list sexpr)
  (* Might be nice to constrain the set of strings *)
  | SPrimBinary (op : prim_binop) (a1 a2 : sexpr)
  | SIf (cond : sexpr) (tcase : sexpr) (fcase : sexpr).

Definition environment := total_map sexpr.

(** ** Notations *)

(** Now we'll define some convenience notations so we don't have to
    write awkward Coq and can write something a little closer to the
    Lisp we all know and love. *)

Coercion SId : string >-> sexpr.
Coercion SNum : nat >-> sexpr.
(* TODO: fill in here *)

(* ================================================================= *)
(** * Evaluation *)

(** Let's write a little interpreter for our language. *)

Fixpoint zip_map {X Y Z : Type} (f : X -> Y -> Z) (l1 : list X) (l2 : list Y) : list Z :=
  match l1, l2 with
  | nil, _ => nil
  | _, nil => nil
  | x :: xs, y :: ys => (f x y) :: (zip_map f xs ys)
  end.

Fixpoint extend_env (env : environment) (params : list string) (args : list sexpr) : environment :=
  match params, args with
  | nil, _ => env
  | _, nil => env
  | x :: rest_params, arg :: rest_args => (x !-> arg ; (extend_env env rest_params rest_args))
  end.

Fixpoint seval (env : environment) (expr : sexpr) : sexpr :=
  match expr with
  | SId x => env x
  | SNum n => SNum n
  | SLambda args body => SLambda args body
  (* Now the fun begins *)
  | SApp func args =>
    (* TODO: evaluate the function *)
    (* evaulate the arguments *)
    let xs := map (seval env) args in
    seval (extend_env env )

  | SPrimBinary op a1 a2 => SNum 42
  | SIf cond tcase fcase => SNum 42
  end.

Definition empty_env := t_empty (SId "null").
Check (seval empty_env).
         
