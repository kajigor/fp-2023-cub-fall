
From hahn Require Import Hahn.

Require Import Lia.
Require Import List.
Export ListNotations.
Require Import Arith Arith.EqNat.
Require Extraction.

Print bool.
Print nat.

Definition is_zero n :=
  match n with
  | O => true
  | S n' => false
  end.

Compute (is_zero 0).
Compute (is_zero 1).

Fixpoint is_odd n :=
  match n with
  | 0 => false
  | S n' => negb (is_odd n')
  end.

Print is_odd.

Definition is_odd' :=
  fix is_odd (n : nat) : bool := match n with
                               | 0 => false
                               | S n' => negb (is_odd n')
                                 end.

Print list.

(* Fixpoint length (A : Type) (l : list A) := *)
(*   match l with *)
(*   | nil => 0 *)
(*   | cons a l' => 1 + length A l' *)
(*   end. *)

(* Compute (length nat [1; 2; 3]). *)

Inductive tree A :=
| Empty
| Leaf (a : A)
| Node (a : A) (l : tree A) (r : tree A).

Fixpoint tree2list {A} (t : tree A) : list A :=
  match t with
  | Empty _ => nil
  | Leaf _ a => [a]
  | Node _ a l r => tree2list l ++ [a] ++ tree2list r
  end.

Theorem the_main_th1 : 2 * 2 = 4.
Proof.
  cbn. 
  reflexivity.
Qed.
Print the_main_th1.
Definition the_main_th1' : 2*2=4 := eq_refl.
Print the_main_th1'.

Theorem the_main_th2 : exists x, x * 2 = 4.
Proof.
  exists 2.
  (* auto. *)
  apply the_main_th1'.
Qed.

Print the_main_th2.

Theorem list_123 : list nat.
Proof.
  apply nil.
Qed.
Print list_123.

Theorem list_123' : list nat.
Proof.
  apply (cons 1 (cons 2 (cons 3 nil))).
Qed.
Print list_123'.

Theorem list_123'' : list nat.
Proof.
  apply cons.
  { apply 1. }
  apply cons.
  { apply 2. }
  apply cons.
  { apply 3. }
  apply nil.
Qed.
Print list_123''.

Print False.

Theorem ex_false (AAA : False) : 5=6.
Proof.
  destruct AAA.  
Qed.
Print ex_false.


Theorem th3 (AA : 2*2=5) : False.
Proof.
  inversion AA.
Qed.

Theorem th4 {A} (l : list A) (Nonempty : l <> nil) :
  length l > 0.
Proof.
  destruct l.
  + simpl. exfalso. apply Nonempty. reflexivity.
  + simpl.
    (* Locate ">". *)
    (* Print gt. *)
    (* Locate "<". *)
    (* Print lt. *)
    (* Print le. *)
    (* Search le. *)
    (* Print le_n_S. *)
    apply le_n_S.
    apply le_0_n.
    (* lia. *)
Qed.
Print th4.

Inductive Vector (A : Type) : nat -> Type :=
| Nil : Vector A 0
| Cons n : A -> Vector A n -> Vector A (S n).

Eval compute in Nil nat.
Eval compute in Cons _ _ 1 (Nil nat).
Eval compute in Cons _ _ 2 (Cons _ _ 1 (Nil nat)).

Set Implicit Arguments.
Arguments Nil {A}.
Arguments Cons {A n}.

Eval compute in Cons 2 (Cons 1 Nil).

Fixpoint vec_append {A : Type} {m n : nat}
  (v1 : Vector A m) (v2 : Vector A n)
  : Vector A (m+n) :=
  match v1 with
  | Nil => v2
  | Cons h t => Cons h (vec_append t v2)
  end.
Print vec_append.

Fixpoint vec_append' {A : Type} {m n : nat}
  (v1 : Vector A m) (v2 : Vector A n)
  : Vector A (m+n).
Proof.
  refine (
      match v1 with
      | Nil => _
      | Cons h t => _
      end
    ).
  apply v2.
  apply (Cons h (vec_append' _ _ _ t v2)).
Qed.


Fail Theorem sort l : {l' | Permutation l l' & is_sorted l'}.

Print Permutation.

Inductive is_smallest : nat -> list nat -> Prop :=
  smallest_unit : forall n, is_smallest n [n]
| smallest_head : forall n m tl,
    n <= m -> is_smallest m tl -> is_smallest n (n::tl)
| smallest_tail : forall n m tl,
    m < n -> is_smallest m tl -> is_smallest m (n::tl).

Inductive is_sorted : list nat -> Prop :=
  sorted_nil : is_sorted []
| sorted_one : forall n, is_sorted [n]
| sorted_cons : forall n tl
                       (STL: is_sorted tl)
                       (SST: is_smallest n (n::tl)),
    is_sorted (n::tl).
  
#[local]
Hint Constructors is_smallest : myconstr.
#[local]
Hint Constructors is_sorted : myconstr.

Inductive is_inserted : nat -> list nat -> list nat -> Prop :=
  ins_head : forall n tl, is_inserted n tl (n::tl)
| ins_tail : forall m n tl tl' (INS: is_inserted n tl tl'),
    is_inserted n (m::tl) (m::tl').

#[local]
Hint Constructors is_inserted : myconstr.

Lemma instert_sorted a l (SORT: is_sorted l):
  {l' | is_inserted a l l' & is_sorted l'}.
Proof.
  induction l; eauto with myconstr.
  edestruct IHl as [l'].
  { clear -SORT.
    inv SORT. constructor.
  }
  destruct (le_gt_dec a a0).
  { exists (a::a0::l); eauto with myconstr.
    eapply sorted_cons; auto.
    eapply smallest_head; eauto.
    inv SORT. auto with myconstr.
  }
  exists (a0::l'); eauto. constructor; auto.
  
  clear -SORT i i0 g.
  induction i; auto.
  { constructor; auto.
    apply smallest_head with (m:=n).
    { lia. }
    inv i0. constructor.
  }
  constructor; auto.
  apply smallest_head with (m:=m).
  2 : { inv i0. constructor.  }

  clear -SORT.
  inv SORT.
  inv STL.
  { inv SST.
    { inv H2. inv H5. }
    lia.
  }
  inv SST.
  inv H2. lia.
  lia.
Qed.

Lemma is_ins_perm a l l' (INS : is_inserted a l l') : Permutation (a::l) l'.
Proof.
  generalize dependent l'.
  generalize dependent a.
  induction l; ins; inv INS.
  apply IHl in INS0.
  etransitivity.
  {
    by apply perm_swap.
  }
  by constructor.
Qed.
  
Theorem sort l : {l' | Permutation l l' & is_sorted l'}.
Proof.
  induction l.
  + exists [].
    Print Permutation.
    apply perm_nil.
    apply sorted_nil.
  + inv IHl.
    apply (instert_sorted a) in H0. inv H0.
    exists x0; auto.
    etransitivity.

    2: { apply is_ins_perm; eassumption. }
    by constructor.
Qed.

Print sort.

Extraction Language OCaml.
Recursive Extraction sort.
