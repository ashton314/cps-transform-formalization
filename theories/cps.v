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

(* ################################################################# *)
(** * Syntax Definition *)

Inductive sexpr : Type :=
  | SId (x : string)
  | SLambda (args : list string) (body : sexpr)
  (* I'd like to add a constraint that fexpr reduces to an instance of SLambda. *)
  | SApp (fexpr : sexpr) (args : list sexpr).
