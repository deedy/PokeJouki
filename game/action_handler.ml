open Definitions
open State
open Constants
open Util
open Netgraphics

let _ = Random.self_init ()

(* Finds a steammon from a list of steammon *)
let find_steammon (steammon_list : steammon list) (name : string) : steammon =
  let helper acc x = if x.species = name then Some x else acc in
  let steammon_option = List.fold_left helper None steammon_list in
  match steammon_option with
  | None -> failwith "Requested steammon was not found"
  | Some x -> x


(* Finds an msteammon from a list of msteammon *)
let find_msteammon (msteammon_list : msteammon list) (name : string) : msteammon =
  let helper acc x = if x.mspecies = name then Some x else acc in
  let steammon_option = List.fold_left helper None msteammon_list in
  match steammon_option with
  | None -> failwith "Requested msteammon was not found"
  | Some x -> x


(* Moves a steammon to the front of the team *)
let switch_steammons (steammons : msteammon list ref) (name : string) : unit =
  let switch_in = find_msteammon !steammons name in
  if switch_in.mcurr_hp = 0 then failwith "Cannot switch in a fainted steammon"
  else begin
    let switch_out = List.hd !steammons in
    (switch_out.mmods).mattack_mod <- 0;
    (switch_out.mmods).mdefense_mod <- 0;
    (switch_out.mmods).mspeed_mod <- 0;
    (switch_out.mmods).maccuracy_mod <- 0;
    switch_out.mstatus <- List.filter (fun x -> x <> Confused) switch_out.mstatus;
    let steammons_after_removal =
      let helper acc x = if x.mspecies = name then acc else x::acc in
      List.fold_left helper [] !steammons in
    steammons := switch_in::steammons_after_removal;
    add_update (SetChosenSteammon (switch_in.mspecies));
    add_update (SetStatusEffects (switch_in.mspecies,switch_in.mstatus)) end

(* Adds an msteammon to the team from a list of steammon *)
let add_steammon (steammons : msteammon list ref) 
      (steammon_list : steammon list ref) (name : string) (color : color) : unit =
  let st = find_steammon !steammon_list name in
  if List.length !steammons < cNUM_PICKS then begin
    steammons := (steammon_mutafication st)::(!steammons); 
    add_update (UpdateSteammon (st.species,st.max_hp,st.max_hp,color));
    steammon_list := List.filter (fun x -> x.species <> name) !steammon_list end

(* Sets the minventory to the given inventory *)
let pick_inventory (minventory : minventory) (inventory : inventory) : unit =
  let helper (acc,c) x =
    let cost = match c with
      | 1 -> cCOST_ETHER
      | 2 -> cCOST_MAXPOTION
      | 3 -> cCOST_REVIVE
      | 4 -> cCOST_FULLHEAL
      | 5 -> cCOST_XATTACK
      | 6 -> cCOST_XDEFEND
      | 7 -> cCOST_XSPEED
      | 8 -> cCOST_XACCURACY 
      | _ -> 0 in
    (acc + (cost * x), c + 1) in
  let sum,_ = List.fold_left helper (0,1) inventory in
  if sum <= cINITIAL_CASH then List.iter2 (fun x y -> x := y) minventory inventory
    

(* Uses an item on the target steammon *)
let use_item (steammons : msteammon list ref) (minventory : minventory)
      (name : string) (item : item) (color : color) : unit =

  let p's = (string_of_color color)^"'s " in

  (* Finds the target msteammon from the team *)
  let msteammon = find_msteammon !steammons name in

  (* Applies Ether to the target msteammon *)
  let apply_ether () =
    let first = msteammon.mfirst_attack and
      second = msteammon.msecond_attack and
      third = msteammon.mthird_attack and
      fourth = msteammon.mfourth_attack in
    let find_new_pp mattack = 
      if mattack.mpp_remaining + 5 >= mattack.mmax_pp then mattack.mmax_pp
      else mattack.mpp_remaining + 5 in
    first.mpp_remaining <- find_new_pp first;
    second.mpp_remaining <- find_new_pp second;
    third.mpp_remaining <- find_new_pp third;
    fourth.mpp_remaining <- find_new_pp fourth in
  
  (* Applies MaxPotion to the target msteammon *)
  let apply_maxpotion () = 
    if msteammon.mcurr_hp = 0 then failwith "Cannot use on a fainted steammon"
    else (let c = msteammon.mcurr_hp in
          msteammon.mcurr_hp <- msteammon.mmax_hp;
          add_update (UpdateSteammon (msteammon.mspecies,
                                      msteammon.mcurr_hp,
                                      msteammon.mmax_hp,
                                      color));
          add_update (PositiveEffect ("HEALED",color,msteammon.mmax_hp - c))) in
  
  (* Applies Revive to the target msteammon *)
  let apply_revive () =
    if msteammon.mcurr_hp <> 0 then failwith "Steammon is not fainted"
    else (msteammon.mcurr_hp <- msteammon.mmax_hp / 2;
          msteammon.mstatus <- [];
          add_update (SetStatusEffects (msteammon.mspecies, []));
          add_update (UpdateSteammon (msteammon.mspecies,
                                      msteammon.mcurr_hp,
                                      msteammon.mmax_hp,
                                      color));
          add_update (Message(p's^msteammon.mspecies^" has been revived!"))) in
  
  (* Applies FullHeal to the target msteammon *)
  let apply_fullheal () = 
    msteammon.mstatus <- [];
    add_update (SetStatusEffects (msteammon.mspecies, []));
    add_update (Message(p's^msteammon.mspecies^"'s statuses have been removed!")) in

  (* Applies XAttack to the current active msteammon *)
  let apply_xattack () = let active = (List.hd !steammons) in
    (active.mmods).mattack_mod <- (active.mmods).mattack_mod + 1 in

  (* Applies XDefense to the current active msteammon *)
  let apply_xdefense () = let active = (List.hd !steammons) in
    (active.mmods).mdefense_mod <- (active.mmods).mdefense_mod + 1 in

  (* Applies XSpeed to the current active msteammon *)
  let apply_xspeed () = let active = (List.hd !steammons) in
    (active.mmods).mspeed_mod <- (active.mmods).mspeed_mod + 1 in

  (* Applies XAccuracy to the current active msteammon *)
  let apply_xaccuracy () = let active = (List.hd !steammons) in
    (active.mmods).maccuracy_mod <- (active.mmods).maccuracy_mod + 1 in

  (* Decrements the item count in postion n of the inventory, then applies f *)
  let decrement n f = 
    if !(List.nth minventory n) < 1 then failwith "No more of that item"
    else ((List.nth minventory n) := !(List.nth minventory n) - 1; f) in
  
  (* Uses the item and decrements the item count in the inventory by 1 *)
  match item with
  | Ether -> decrement 0 (apply_ether ())
  | MaxPotion -> decrement 1 (apply_maxpotion ())
  | Revive -> decrement 2 (apply_revive ())
  | FullHeal -> decrement 3 (apply_fullheal ())
  | XAttack -> decrement 4 (apply_xattack ())
  | XDefense -> decrement 5 (apply_xdefense ())
  | XSpeed -> decrement 6 (apply_xspeed ())
  | XAccuracy -> decrement 7 (apply_xaccuracy ())


(* Uses attack on the opponent active msteammon *)
let use_attack (steammons1 : msteammon list ref) (steammons2 : msteammon list ref)
      (att_name : string) (color : color) : unit =
  
  let opp's = (string_of_color (opposite_color color))^"'s " in

  (* Assigns the attacker as the first steammon in the first team
       and the defender as the first steammon in the second team *)
  let attacker = List.hd !steammons1 and defender = List.hd !steammons2 in

  (* Finds the attack from the attacker's list of attacks *)
  let attack = 
    if (attacker.mfirst_attack).mname = att_name then attacker.mfirst_attack
    else if (attacker.msecond_attack).mname = att_name then attacker.msecond_attack
    else if (attacker.mthird_attack).mname = att_name then attacker.mthird_attack
    else if (attacker.mfourth_attack).mname = att_name then attacker.mfourth_attack
    else failwith "Invalid attack" in
  
  (* Checks the statuses of the attacker *)
  let paralyzed = List.mem Paralyzed attacker.mstatus and
      confused = List.mem Confused attacker.mstatus in
 
  (* Sees if the attacker is paralyzed (unable to attack) *)
  let paralysis = 
    if paralyzed then Random.int 100 < cPARALYSIS_CHANCE else false in

  (* Sees if the attacker will attack himself *)
  let self_attack =
    if confused then Random.int 100 < cSELF_ATTACK_CHANCE else false in
  
  (* Calculates the accuracy modifier for the attacker and
       sees if the attack will hit *)
  let acc_mod = match (attacker.mmods).maccuracy_mod with
                | -3 -> cACCURACY_DOWN3
                | -2 -> cACCURACY_DOWN2
                | -1 -> cACCURACY_DOWN1
                | 1 -> cACCURACY_UP1
                | 2 -> cACCURACY_UP2
                | 3 -> cACCURACY_UP3
                | _ -> 1. in
  let accuracy = int_of_float ((float_of_int attack.maccuracy) *. acc_mod) in
  let attack_hit = Random.int 100 < accuracy in
  
  (* Proceeds to use the attack, checking for paralysis and self_attack,
       and checking if the attack has enough pp left *)
  if attacker.mcurr_hp = 0 then ()
  else if paralysis then 
    add_update (Message(attacker.mspecies^" is paralyzed! It cannot attack!"))
  else if self_attack then begin
      let damage = cSELF_ATTACK_POWER * attacker.mattack / attacker.mdefense in
      let new_hp = if attacker.mcurr_hp - damage < 0 then 0
                   else attacker.mcurr_hp - damage in
      attacker.mcurr_hp <- new_hp;
      add_update (Message(attacker.mspecies^" has hurt itself in confusion!"));
      add_update (NegativeEffect("HIT",color,damage))
    end 
  else if attack.mpp_remaining < 1 then
    add_update (Message(attacker.mspecies^" has no pp remaining for this attack"))
  else if attack_hit then begin
    (* The attacker uses the attack *)

    (* Decrements the mpp_remaining for this attack *)
    attack.mpp_remaining <- attack.mpp_remaining - 1;

    (* Calculates modifiers on attack and defense for the attacker
         and defender, respectively *)
    let att_mod = match (attacker.mmods).mattack_mod with
                  | -3 -> cATTACK_DOWN3
                  | -2 -> cATTACK_DOWN2
                  | -1 -> cATTACK_DOWN1
                  | 1 -> cATTACK_UP1
                  | 2 -> cATTACK_UP2
                  | 3 -> cATTACK_UP3
                  | _ -> 1.
    and def_mod = match (defender.mmods).mdefense_mod with
                  | -3 -> cDEFENSE_DOWN3
                  | -2 -> cDEFENSE_DOWN2
                  | -1 -> cDEFENSE_DOWN1
                  | 1 -> cDEFENSE_UP1
                  | 2 -> cDEFENSE_UP2
                  | 3 -> cDEFENSE_UP3
                  | _ -> 1. in

    (* Decides whether to use spl_attack/spl_defense or attack/defense stats*)
    let (att, def) = 
      match attack.melement with
      | Electric | Psychic | Dark | Dragon | Ice | Fire | Water | Grass -> 
        (float_of_int attacker.mspl_attack, float_of_int defender.mspl_defense)
      | _ -> (float_of_int attacker.mattack *. att_mod, 
              float_of_int defender.mdefense *. def_mod) in

    (* Assigns a critical strike multiplier *)
    let crit = 
      if Random.int 100 < attack.mcrit_chance then cCRIT_MULTIPLIER else 1. in

    (* Assigns a STAB multiplier *)
    let stab = 
      let same type1 type2 = match type2 with None -> false | Some t -> t=type1 in
      if same attack.melement attacker.mfirst_type || 
         same attack.melement attacker.msecond_type then cSTAB_BONUS else 1. in

    (* Assigns a type multiplier *)
    let type_multiplier = let a_type = attack.melement in
      match defender.mfirst_type, defender.msecond_type with
      | Some t1, Some t2 -> (weakness a_type t1) *. (weakness a_type t2)
      | Some t, _ -> weakness a_type t
      | _, Some t -> weakness a_type t
      | _ -> 1. in
  
    (* Calculates and applies damage *)
    let power = float_of_int attack.mpower in
    let damage = power *. att *. crit *. stab *. type_multiplier /. def in
    let new_hp = if defender.mcurr_hp - int_of_float damage < 0 then 0
                 else defender.mcurr_hp - int_of_float damage in
    defender.mcurr_hp <- new_hp;
    add_update (UpdateSteammon (defender.mspecies,
                                defender.mcurr_hp,
                                defender.mmax_hp,
                                opposite_color color));
    if damage > 0. then
      add_update (NegativeEffect ("HIT",opposite_color color,int_of_float damage));
    
    (* Applies the attack's effect (if any) *)
    let (effect,p) = attack.meffect in
    let chance f = if Random.int 100 < p then f else () in
    let statusable = List.length defender.mstatus < 2 in
    let apply f = if statusable then chance f in
    let poison () = 
      if List.mem Poisoned defender.mstatus then
        add_update (Message(opp's^defender.mspecies^" is already poisoned!"))
      else (defender.mstatus <- Poisoned::defender.mstatus;
            add_update (NegativeEffect ("POISONED",opposite_color color,0));
            add_update (Message(opp's^defender.mspecies^" has been poisoned!")))
    and confuse () = 
      if List.mem Confused defender.mstatus then
        add_update (Message(opp's^defender.mspecies^" is already confused!"))
      else (defender.mstatus <- Confused::defender.mstatus;
            add_update (NegativeEffect ("CONFUSED",opposite_color color,0));
            add_update (Message(opp's^defender.mspecies^" has been confused!")))
    and sleep () = 
      if List.mem Asleep defender.mstatus then
        add_update (Message(opp's^defender.mspecies^" is already asleep!"))
      else (defender.mstatus <- Asleep::defender.mstatus;
            add_update (NegativeEffect ("ASLEEP",opposite_color color,0));
            add_update (Message(opp's^defender.mspecies^" has been put to sleep!")))
    and paralyze () = 
      if List.mem Paralyzed defender.mstatus then
        add_update (Message(opp's^defender.mspecies^" is already paralyzed!"))
      else (defender.mstatus <- Paralyzed::defender.mstatus;
            add_update (NegativeEffect ("PARALYZED",opposite_color color,0));
            add_update (Message(opp's^defender.mspecies^" has been paralyzed!")))
    and freeze () = 
      if List.mem Frozen defender.mstatus then
        add_update (Message(opp's^defender.mspecies^" is already frozen!"))
      else (defender.mstatus <- Frozen::defender.mstatus;
            add_update (NegativeEffect ("FROZEN",opposite_color color,0));
            add_update (Message(opp's^defender.mspecies^" has been frozen!")))
    and self_att () = 
      if attacker.mmods.mattack_mod < 3 then
        attacker.mmods.mattack_mod <- attacker.mmods.mattack_mod + 1
    and self_def () = 
      if attacker.mmods.mdefense_mod < 3 then
        attacker.mmods.mdefense_mod <- attacker.mmods.mdefense_mod + 1
    and self_spe () = 
      if attacker.mmods.mspeed_mod < 3 then
        attacker.mmods.mspeed_mod <- attacker.mmods.mspeed_mod + 1
    and self_acc () = 
      if attacker.mmods.maccuracy_mod < 3 then
        attacker.mmods.maccuracy_mod <- attacker.mmods.maccuracy_mod + 1
    and opp_att () = 
      if defender.mmods.mattack_mod > -3 then
        defender.mmods.mattack_mod <- defender.mmods.mattack_mod - 1
    and opp_def () =
      if defender.mmods.mdefense_mod > -3 then
        defender.mmods.mdefense_mod <- defender.mmods.mdefense_mod - 1
    and opp_spe () =
      if defender.mmods.mspeed_mod > -3 then
        defender.mmods.mdefense_mod <- defender.mmods.mdefense_mod - 1
    and opp_acc () =
      if defender.mmods.maccuracy_mod > -3 then
        defender.mmods.maccuracy_mod <- defender.mmods.maccuracy_mod - 1 in
    begin match effect with
    | Poisons -> apply (poison ())
    | Confuses -> apply (confuse ())
    | Sleeps -> apply (sleep ())
    | Paralyzes -> apply (paralyze ())
    | Freezes -> apply (freeze ())
    | SelfAttackUp1 -> chance (self_att ())
    | SelfDefenseUp1 -> chance (self_def ())
    | SelfSpeedUp1 -> chance (self_spe ())
    | SelfAccuracyUp1 -> chance (self_acc ())
    | OpponentAttackDown1 -> chance (opp_att ())
    | OpponentDefenseDown1 -> chance (opp_def ())
    | OpponentSpeedDown1 -> chance (opp_spe ())
    | OpponentAccuracyDown1 -> chance (opp_acc ())
    | _ -> () end;
    add_update (SetStatusEffects (defender.mspecies,defender.mstatus)) end
  
  else add_update (NegativeEffect ("MISS",opposite_color color,0))
