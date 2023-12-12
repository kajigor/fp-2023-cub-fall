(* These exercises require inductive proofs. *)
(* Now that you have some experience with Coq, 
   you can use 'auto' tactic to avoid solving simple subgoals manually. *)

Require Import b1.
Require Import b2.
Require Import List.
Import ListNotations. 

Section NatDict'Proofs.
  Context {V: Type}.

  (* Prove that 'remove' operation actually removes a key. *)
  (* The list inside nat_dict_list consists of pairs.
     To extract components of such list's head, 
     either perform pattern matching in 'induction' tactics
     or directly 'destruct' the element of nat*V type. *)

  Lemma remove_head (d: @nat_dict_list V) (n: nat) (v: V):
    remove'' ((n, v) :: d) n = remove'' d n.
  Proof.
    unfold remove''.
    rewrite nat_eq_refl.
    reflexivity.
  Qed.

  Lemma skip_head (d: @nat_dict_list V) (n n': nat) (v: V) (NEQ : n' <> n):
    remove'' ((n, v) :: d) n' = (n, v) :: remove'' d n'.
  Proof.
  Admitted.

  Lemma removed_not_contained (d: @nat_dict_list V) (n: nat):
    contains'' (remove'' d n) n = false.
  Proof.
    induction d.
    reflexivity.
    {
      destruct a.
      destruct (nat_eq n n0) eqn:EQ.
      {
        apply nat_eq_implies_eq in EQ.
        rewrite EQ in *.
        rewrite remove_head.
        exact IHd.
      }
      {
        rewrite skip_head.
        {
          unfold contains''.
          rewrite EQ.
          exact IHd.
        }
        {
          rewrite <- nat_eq_neg_spec.
          apply EQ.
        }
      }
    }
  Qed.

  (* Define a mapping function similar to one defined for regular lists.
     It should replace values stored in dict but keep them under the same keys. *)
  Fixpoint map'' {W: Type} (f: V -> W) (d: @nat_dict_list V): @nat_dict_list W :=
    match d with 
    | [] => []
    | (k, v) :: d => (k, f v) :: map'' f d
    end.
  
  Lemma map_head {W : Type} (f : V -> W) (d: @nat_dict_list V) (k: nat) (v: V) :
    map'' f ((k, v) :: d) = (k, f v) :: map'' f d.
  Proof.
    destruct d; auto.
  Qed.

  Lemma get_skip_head {W : Type} (d : @nat_dict_list W) (n n': nat) (v: W) (NEQ: n <> n'):
    get'' ((n', v) :: d) n = get'' d n.
  Proof.
    unfold get''.
    rewrite <- nat_eq_neg_spec in NEQ.
    rewrite NEQ.
    reflexivity.
  Qed.

  (* Prove that a value stored in a mapped dict 
     requires a corresponding value stored in an original dict. *)
  Lemma dict_map_get {W: Type} (m: V -> W) (d: @nat_dict_list V):
    forall n w,
      (get'' (map'' m d) n = Some w) <->
      (exists v, get'' d n = Some v /\ m v = w).
  Proof.
    intros n w.
    split; intros H.
    {
      induction d.
      {
        unfold map'', get'' in H.
        inversion H.
      }
      {
        destruct a.
        rewrite map_head in H.
        destruct (nat_eq n n0) eqn:EQ.
        {
          clear IHd.
          exists v.
          unfold get'' in *.
          rewrite EQ in *.
          inversion H.
          auto.
        }
        {
          rewrite get_skip_head.
          {
            apply IHd.
            rewrite get_skip_head in H.
            - exact H.
            - {
              apply nat_eq_neg_spec.
              exact EQ.
            }
          }
          {
              apply nat_eq_neg_spec.
              exact EQ.
          }
        }
      }
    }
    {
      destruct H.
      destruct H.
      induction d.
      - inversion H.
      - {
        destruct a.
        rewrite map_head.
        destruct (nat_eq n n0) eqn:EQ.
        {
          clear IHd.
          unfold get'' in *.
          rewrite EQ in *.
          inversion H.
          rewrite H2 in *.
          rewrite H0.
          reflexivity.
        }
        {
          rewrite get_skip_head.
          {
            apply IHd.
            rewrite get_skip_head in H.
            - exact H.
            - {
              apply nat_eq_neg_spec.
              exact EQ.
            }
          }
          {
            apply nat_eq_neg_spec.
            exact EQ.
          }
        }
      }
    }
  Qed.
  (* Implement a filtering function. 
     The result should contain only those keys whose values satisfy the predicate;
     in this case they remain unchanged. *)
  Fixpoint filter'' {U: Type} (f: U -> bool) (d: @nat_dict_list U): @nat_dict_list U :=
    match d with
    | [] => []
    | (k, v) :: d => let d' := remove'' (filter'' f d) k in if f v then (k, v) :: d' else d'
    end.

(*...*)

  (* Prove that the result of filtering is actually filtered *)
  Lemma filter_elem (f: V -> bool) (d: @nat_dict_list V):
    forall n,
      (contains'' (filter'' f d) n = true) <->
      (exists v, get'' d n = Some v /\ f v = true).
  Proof.
  Admitted.


  (* You (most probably) implemented list-based dictionary in a way
     that doesn't distinguish, say, [(1, 2), (3, 4)] and [(3, 4), (1, 2)] dicts. *)
  (* That is, the results of 'insert', 'contains' and other interface operations
     should be the same for them. *)
  (* Such lists are not-equal, though, 
     since the only list equal to [(1, 2), (3, 4)] is exactly [(1, 2), (3, 4)]. *)
  (* We can formalize the specific notion of equivalence for dictionaries 
     to prove their more complicated properties. *)
  (* Note that this equ ivalence only deals with dict interface 
     and not the particular implementation. *)
  Definition sim_dicts (d1 d2: @nat_dict_list V) :=
    forall n, get'' d1 n = get'' d2 n.

(*...*)

  (* Prove that an insertion makes a preceding removal pointless. 
     To ease the proof, you may want to prove separately that:
     - sim_dicts relation is transitive
     - an insertion of the same key-value pair preserves sim_dicts
     - a double insertion of the same key-value pair
       is similar (in terms of sim_dicts) to a single insertion
     - insertions of separate keys commute
       (that is, their results are related by sim_dicts).
     Also, it can be easier to operate on level of a higher level of 'insert's 
     instead of a lower level of list 'cons'es. 
     To replace 'cons' with 'insert', use 'fold' tactic. 
*)
  Lemma insert_remove_simpl (d: @nat_dict_list V) (n: nat) (v: V):
    sim_dicts (insert'' (remove'' d n) n v) (insert'' d n v).
  Proof.
Admitted.
  
End NatDict'Proofs.   
