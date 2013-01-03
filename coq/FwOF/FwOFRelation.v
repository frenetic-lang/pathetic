Set Implicit Arguments.

Require Import Coq.Lists.List.
Require Import Coq.Classes.Equivalence.
Require Import Coq.Structures.Equalities.
Require Import Coq.Classes.Morphisms.
Require Import Coq.Classes.EquivDec.
Require Import Coq.Setoids.Setoid.
Require Import Common.Types.
Require Import Common.Bisimulation.
Require Import Bag.Bag.
Require Import FwOF.FwOF.

Local Open Scope list_scope.
Local Open Scope equiv_scope.
Local Open Scope bag_scope.

  Section BagAxioms.

    Import Bag.

    Axiom bag_unions_app : forall (A : Type) (E : Eq A)
      (lst lst0 : list (Bag.Bag A)),
      bag_unions (lst ++ lst0) === union (bag_unions lst) (bag_unions lst0).

    Axiom bag_unions_cons : forall (A : Type) (E : Eq A)
      (elt : Bag.Bag A)
      (lst : list (Bag.Bag A)),
      bag_unions (elt :: lst) === union elt (bag_unions lst).

    Axiom bag_map_union : forall {A B : Type} {EA : Eq A} {EB : Eq B}
      (f : A -> B) (bag1 bag2 : Bag.Bag A),
      (from_list (List.map f (to_list (union bag1 bag2)))) ===
      (union (from_list (List.map f (to_list bag1))) 
             (from_list (List.map f (to_list bag2)))).

    Axiom bag_from_list_singleton : forall (A : Type) (E : Eq A)
      (elt : A),
      from_list [elt] = singleton elt.
    
    Axiom bag_unions_unlist : forall (A B : Type) (EA : Eq A) (EB : Eq B)
      (f : A -> Bag.Bag B) (lst : list A) (bag : Bag A),
      bag_unions (List.map f (to_list (from_list lst <+> bag))) = 
      bag_unions (List.map f lst) <+> 
      bag_unions (List.map f (to_list bag)).

    Lemma bag_unions_pop : forall (A : Type) (E : Eq A)
      (b : Bag.Bag A) (lst lst0 : list (Bag.Bag A)),
      bag_unions lst === bag_unions lst0 ->
      bag_unions (b :: lst) === bag_unions (b :: lst0).
    Proof.
      intros.
      simpl.
      rewrite -> H.
      apply reflexivity.
    Qed.

    Lemma bag_unions_rotate : forall (A : Type) (E : Eq A)
      (b : Bag.Bag A) (lst lst0 : list (Bag.Bag A)),
      bag_unions (lst ++ [b]) === bag_unions lst0 ->
      bag_unions (b :: lst) === bag_unions lst0.
    Proof with auto.
      intros.
      generalize dependent lst0.
      induction lst; intros...
      simpl in H.
      rewrite <- (IHlst (lst ++ [b])) in H.
      simpl in H.
      simpl.
      rewrite <- Bag.union_assoc in H.
      rewrite -> (Bag.union_comm E a b) in H.
      rewrite -> Bag.union_assoc in H.
      exact H.
      apply reflexivity.
    Qed.

    Lemma make_bag_unions : forall (A : Type) (E : Eq A)
      (b b0 : Bag.Bag A),
      b <+> b0 === bag_unions [b; b0].
    Proof.
      intros.
      simpl.
      rewrite -> union_empty_r.
      apply reflexivity.
    Qed.

    Lemma flatten_bag_unions_r : forall (A : Type) (E : Eq A)
      (b0 b1 b2 : Bag.Bag A),
      bag_unions [ b0; bag_unions [ b1; b2 ] ] =
      bag_unions [ b0; b1; b2 ].
    Proof.
      intros.
      simpl.
    Admitted.
        


  End BagAxioms.

Ltac bag_perm n :=
  match goal with
    | |- (bag_unions ?lst) === (bag_unions ?lst) => 
      apply reflexivity
    | |- bag_unions (?b :: ?lst) === bag_unions (?b :: ?lst0) =>
      let newn := eval compute in (length lst) in
        apply bag_unions_pop;
          bag_perm newn
    | |- bag_unions (?b :: ?lst1)  === ?lst2 =>
      match eval compute in n with
        | 1 => fail
        | _ =>
          apply bag_unions_rotate; 
            repeat rewrite <- app_comm_cons;
              rewrite -> app_nil_l;
                bag_perm (pred n)
      end
  end.

Ltac solve_bag_permutation :=
  match goal with
    | |- bag_unions ?lst0 === bag_unions ?lst1 =>
      match eval compute in (length lst0 = length lst1) with
        | (?n = ?n) => bag_perm n
      end
  end.

Ltac make_big_bag_unions bag := 
  match goal with
    | |- context[bag] => 
      match bag with
        | ?lhs <+> ?rhs => 
          make_big_bag_unions rhs;  
          rewrite -> (make_bag_unions _ lhs)
        | _ => idtac
      end
  end.

Ltac flatten_nested_bag_unions cxt :=
  match goal with
    | |- context[cxt] =>
      match cxt with
        | bag_unions _
          (cons ?X (cons (bag_unions _ (cons ?Y (cons ?Z nil))) nil)) =>
            flatten_nested_bag_unions Z;
            rewrite -> (flatten_bag_unions_r X Y Z)
        | _ => idtac
    end
  end.
      

Module Make (Import Atoms : ATOMS).

  Module Concrete := ConcreteSemantics (Atoms).
  Import Concrete.

  Axiom topo : switchId -> portId -> option (switchId * portId).

  Definition affixSwitch (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) => (sw,pt,pk)
    end.

  Definition transfer (sw : switchId) (ptpk : portId * packet) :=
    match ptpk with
      | (pt,pk) =>
        match topo sw pt with
          | Some (sw',pt') => {| (sw',pt',pk) |}
          | None => {| |}
        end
    end.

  Definition select_packet_out (sw : switchId) (msg : fromController) :=
    match msg with
      | PacketOut pt pk => {| (sw,pt,pk) |}
      | _ => {| |}
    end.

  Axiom locate_packet_in : switchId -> portId -> packet -> 
    list (portId * packet).

  Definition select_packet_in (sw : switchId) (msg : fromSwitch) :=
    match msg with
      | PacketIn pt pk => 
        Bag.from_list (map (affixSwitch sw) (locate_packet_in sw pt pk))
      | _ => {| |}
    end.

  Definition abst_state := Bag.Bag (switchId * portId * packet).

  Axiom abst_func : switchId -> portId -> packet -> list (portId * packet).

  Definition flow_table_safe (sw : switchId) (tbl : flowTable) : Prop :=
    forall pt pk forwardedPkts packetIns,
      process_packet tbl pt pk = (forwardedPkts, packetIns) ->
      bag_unions (map (transfer sw) forwardedPkts) <+>
      bag_unions (map (select_packet_in sw) (map (PacketIn pt) packetIns)) ===
      bag_unions (map (transfer sw) (abst_func sw pt pk)).

  Definition state_switches (st : state) := 
    match st with
      | (switches, _, _, _) => switches
    end.

  Record concreteState := ConcreteState {
    concreteState_state : state;
    concreteState_flowTableSafety :
      forall (sw : switch), 
        In sw (state_switches concreteState_state) -> 
        flow_table_safe (switch_swichId sw) (switch_flowTable sw)
  }.

  Definition concreteStep (st : concreteState) (obs : option observation)
    (st0 : concreteState) :=
    step (concreteState_state st) obs (concreteState_state st0).

  Inductive abstractStep : abst_state -> option observation -> abst_state -> 
    Prop := 
  | AbstractStepEquiv : forall st st',
      st === st' ->
      abstractStep st None st'
  | AbstractStep : forall sw pt pk lps,
    abstractStep
      ({| (sw,pt,pk) |} <+> lps)
      (Some (sw,pt,pk))
      (bag_unions (map (transfer sw) (abst_func sw pt pk)) <+> lps).


  Definition relate_switch (sw : switch) : abst_state :=
    match sw with
      | Switch swId _ tbl inp outp ctrlm switchm =>
        Bag.from_list (map (affixSwitch swId) (Bag.to_list inp)) <+>
        bag_unions (map (transfer swId) (Bag.to_list outp)) <+>
        bag_unions (map (select_packet_out swId) (Bag.to_list ctrlm)) <+>
        bag_unions (map (select_packet_in swId) (Bag.to_list switchm))
    end.

  Definition relate_dataLink (link : dataLink) : abst_state :=
    match link with
      | DataLink _ pks (sw,pt) =>
        Bag.from_list (map (fun pk => (sw,pt,pk)) pks)
    end.

  Definition relate_openFlowLink (link : openFlowLink) : abst_state :=
    match link with
      | OpenFlowLink sw switchm ctrlm =>
        bag_unions (map (select_packet_out sw) ctrlm) <+>
        bag_unions (map (select_packet_in sw) switchm)
    end.

  Axiom relate_controller : controller -> abst_state.

  Definition relate (st : state) : abst_state :=
    match st with
      | (switches, links, ofLinks, ctrl) =>
        bag_unions (map relate_switch switches) <+>
        bag_unions (map relate_dataLink links) <+>
        bag_unions (map relate_openFlowLink ofLinks) <+>
        relate_controller ctrl
    end.
        
  Definition bisim_relation : relation concreteState abst_state :=
    fun (st : concreteState) (ast : abst_state) => 
      ast === (relate (concreteState_state st)).



  Theorem weak_sim_1 :
    weak_simulation concreteStep abstractStep bisim_relation.
  Proof with auto with datatypes.
    unfold weak_simulation.
    intros.
    split.
    intros.
    unfold bisim_relation in H.
    unfold concreteStep in H0.
    destruct s. destruct s'. simpl in *.
    inversion H0; subst.
    inversion H1; subst.
    simpl in H.
    rewrite -> map_app in H.
    simpl in H.
    rewrite -> bag_unions_app in H.
    rewrite -> bag_unions_cons in H.
    rewrite -> bag_map_union in H.
    rewrite -> Bag.to_list_singleton in H.
    simpl in H.
    repeat rewrite -> Bag.union_assoc in H.
    rewrite -> Bag.union_comm in H.
    rewrite -> Bag.union_assoc in H.
    rewrite -> bag_from_list_singleton in H.
    match goal with
      | [ H : t === ?t0 <+> ?t1 |- _ ] => remember t1
    end.
    remember(bag_unions (map (transfer swId) (abst_func swId pt pk)) <+> b) as t'.
    exists t'.
    split.
    Focus 2.
    apply multistep_tau with (a0 := ({|(swId, pt, pk)|}) <+> b).
    apply AbstractStepEquiv...
    apply multistep_obs with
      (a0 := (bag_unions (map (transfer swId) (abst_func swId pt pk)) <+> b)).
    apply AbstractStep.
    subst.
    apply multistep_nil.
    (* Showing the states are bisimilar *)
    unfold bisim_relation.
    simpl.
    rewrite -> map_app.
    rewrite -> bag_unions_app.
    simpl.
    clear H0. clear H1.
    rewrite -> bag_unions_unlist.
    rewrite -> bag_unions_unlist.
    subst.
    repeat rewrite -> Bag.union_assoc.
    unfold state_switches in *.
    assert (flow_table_safe swId tbl) as J.
      refine (concreteState_flowTableSafety1
        (Switch swId pts tbl inp (Bag.from_list outp' <+> outp) ctrlm
          (Bag.from_list (map (PacketIn pt) pksToCtrl) <+> switchm)) _)...
      unfold flow_table_safe in J.
      pose (J0 := J pt pk outp' pksToCtrl H3).
      rewrite <- J0.
    repeat rewrite -> Bag.union_assoc.
    match goal with
      | |- ?X === ?Y => make_big_bag_unions X; make_big_bag_unions Y
    end.
    repeat rewrite -> flatten_bag_unions_r.

    match goal with
      | |- ?X === ?Y => flatten_nested_bag_unions X
    end.

    flatten_nested_bag_unions .


    rw (  bag_unions (map (transfer swId) outp') <+>
   bag_unions (map (select_packet_in swId) (map (PacketIn pt) pksToCtrl)) <+>
   Bag.from_list (map (affixSwitch swId) (Bag.to_list inp)) <+>
   bag_unions (map (transfer swId) (Bag.to_list outp)) <+>
   bag_unions (map (select_packet_out swId) (Bag.to_list ctrlm)) <+>
   bag_unions (map (select_packet_in swId) (Bag.to_list switchm)) <+>
   bag_unions (map relate_switch sws0) <+>
   bag_unions (map relate_dataLink links) <+>
   bag_unions (map relate_openFlowLink ofLinks) <+>
   relate_controller ctrl <+> bag_unions (map relate_switch sws)).
    rwlhs (bag_unions (map relate_dataLink links)).
          
     rewrite -> (make_bag_unions _ ).
    Check make_bag_unions.
    Axiom flatten_bag_unions : forall (A : Type) (E : Eq A)
      

    admit. (* gotta bubble sort ... *)
    admit.
    admit.
    admit.
    
    (* No observations *)
    intros.

  Admitted.
    
  Theorem weak_sim_2 :
    weak_simulation abstractStep concreteStep (inverse_relation bisim_relation).
  Proof with auto.

  Admitted.


  Theorem fwof_abst_weak_bisim :
    weak_bisimulation concreteStep abstractStep bisim_relation.
  Proof.
    unfold weak_bisimulation.
    split.
    exact weak_sim_1.
    exact weak_sim_2.
  Qed.

End Make.    
  

  