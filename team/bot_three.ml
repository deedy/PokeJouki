open Team
open Definitions
open Constants
open Util

let return_attacks mon = 
  [mon.first_attack;mon.second_attack;mon.third_attack;mon.fourth_attack]

let types = [Fire;Water;Ice;Grass;Poison;Normal;Flying;Psychic;Ghost;
             Dark;Steel;Rock;Ground;Electric;Bug;Dragon;Fighting]

let cmp_tup_floats (x,_) (y,_) = int_of_float (1000.*.y) - int_of_float (1000.*.x)

let highest lst =
  let helper (acc,a) (x,b) = if x > acc then (x,b) else (acc,a) in
  List.fold_left helper (List.hd lst) lst

let special attack = 
  match attack.element with
  | Electric | Psychic | Dark | Dragon | Ice | Fire | Water | Grass -> true
  | _ -> false

let find_att_stat attack mon =
  if special attack then (float_of_int mon.spl_attack)
  else (float_of_int mon.attack)

let find_def_stat attack defender =
  if special attack then (float_of_int defender.spl_defense)
  else (float_of_int defender.defense)

let same type1 type2 = match type2 with None -> false | Some t -> t = type1

let calc_stab attack mon =
  let a_typ = attack.element in
  if same a_typ mon.first_type || same a_typ mon.second_type then cSTAB_BONUS
  else 1.

let calc_mult attack mon = 
  let a_typ = attack.element in
  match (mon.first_type, mon.second_type) with
  | Some t1, Some t2 -> (weakness a_typ t1)*.(weakness a_typ t2)
  | Some t, _ -> weakness a_typ t
  | _, Some t -> weakness a_typ t
  | _ -> 1.


let find_speed mon =
  let find_speed_mod speed_mod = match speed_mod with
                                           | -3 -> cSPEED_DOWN3
                                           | -2 -> cSPEED_DOWN2
                                           | -1 -> cSPEED_DOWN1
                                           | 1 -> cSPEED_UP1
                                           | 2 -> cSPEED_UP2
                                           | 3 -> cSPEED_UP3
                                           | _ -> 1. in
  let speed_mod = find_speed_mod (mon.mods).speed_mod in    
  let speed = float_of_int mon.speed
  and slow = float_of_int cPARALYSIS_SLOW in
  if (List.mem Paralyzed mon.status) then speed *. speed_mod /. slow
  else speed *. speed_mod


(****************PURE_DAMAGE****************)
let find_pure_damage attack mon typ =
  let power = float_of_int attack.power
  and accur = (float_of_int attack.accuracy) /. 100.
  and stab = calc_stab attack mon
  and mult = weakness attack.element typ
  and att = if special attack then (float_of_int mon.spl_attack)
            else (float_of_int mon.attack) in
  power *. accur *. att *. stab *. mult

(*let pure_damage_avg' attack mon =
  let helper acc x = find_pure_damage attack mon x +. acc in
  let sum = List.fold_left helper 0. types in
  sum /. (float_of_int (List.length types))*)

let pure_attacks_list mon typ =
  let helper acc x = (find_pure_damage x mon typ, x)::acc in
  let damages = List.fold_left helper [] (return_attacks mon) in
  List.sort cmp_tup_floats damages

let pure_damage_avgs mon = 
  let helper acc x = fst (List.hd (pure_attacks_list mon x)) +. acc in
  let total = List.fold_left helper 0. types in
  total /. (float_of_int (List.length types))  

(*let max_pure_damage mon = let (dmg,_) = List.hd (pure_attacks_list mon) in dmg*)

let pure_damage_list mons =
  List.fold_left (fun acc x -> (pure_damage_avgs x, x)::acc) [] mons
(*******************************************)


(****************NET_DAMAGE*****************)
let find_damage attack mon1 mon2 =
  let hp = float_of_int mon2.curr_hp
  and power = float_of_int attack.power
  and accur = (float_of_int attack.accuracy) /. 100.
  and (stab,mult) = (calc_stab attack mon1, calc_mult attack mon2)
  and (att,def) = (find_att_stat attack mon1, find_def_stat attack mon2) in
  let dmg = if hp = 0. || def = 0. then 1. 
            else ((power *. accur *. att *. stab *. mult /. def) /. hp) in
  if dmg > 1. then 1. else dmg

let attacks_list mon1 mon2 =
  let helper acc x = (find_damage x mon1 mon2, x)::acc in
  let damages = List.fold_left helper [] (return_attacks mon1) in
  List.sort cmp_tup_floats damages

let max_damage mon1 mon2 =
  let (dmg,_) = List.hd (attacks_list mon1 mon2) in dmg

let net_damage enemies mon = 
  let total_damage = 
    List.fold_left (fun acc x -> (max_damage mon x) +. acc) 0. enemies
  and total_damage_taken =
    List.fold_left (fun acc x -> (max_damage x mon) +. acc) 0. enemies in
  (total_damage) -. (total_damage_taken) +. float_of_int (List.length enemies)

let net_damage_list enemies mons =
  List.fold_left (fun acc x -> (net_damage enemies x, x)::acc) [] mons
(*******************************************)


(**************SPEED_VS_ENEMIES*************)
let speed_vs_enemies enemies mon =
  let helper acc x = if mon.speed > x.speed then acc +. 1. else acc in
  List.fold_left helper 0. enemies

let speed_vs_enemies_list enemies mons =
  List.fold_left (fun acc x -> ((speed_vs_enemies enemies x), x)::acc) [] mons
(*******************************************)


(****************RESISTS********************)
let make_attack typ = {name = "";
                       element = typ;
                       max_pp = 10;
                       pp_remaining = 10;
                       power = 100;
                       accuracy = 100;
                       crit_chance = 0;
                       effect = (Nada,0)}

let avg_resist mon =  
  let helper acc x = if (calc_mult (make_attack x) mon) = 0. then 0.125 *. acc
                     else (calc_mult (make_attack x) mon) *. acc in
  let prod = List.fold_left helper 1. types in
  if prod = 0. then prod else 1. /. prod

let avg_resists_list mons =
  List.fold_left (fun acc x -> (avg_resist x, x)::acc) [] mons
(****************RESISTS********************)


(****************DEF_STATS******************)
let sum_def_stats mon = 
  float_of_int (mon.max_hp + mon.defense + mon.spl_defense)

let def_stats_list mons =
  List.fold_left (fun acc x -> (sum_def_stats x, x)::acc) [] mons
(*******************************************)


(******************SPEED********************)
let speeds_list mons = 
  List.fold_left (fun acc x -> (float_of_int x.speed, x)::acc) [] mons
(*******************************************)

let p f mon high =
  if high = 0. then (print_endline "1";high) else (if mon.species = "Typhlosion" then (print_endline ("2: "^(string_of_float ((f mon) /.high))));(f mon) /. high)

let p_list mons enemies (a,b,c,d,e,f) =
  let highs = [fst (highest (pure_damage_list mons));
               fst (highest (avg_resists_list mons));
               fst (highest (def_stats_list mons));
               fst (highest (speeds_list mons));
               fst (highest (speed_vs_enemies_list enemies mons));
               fst (highest (net_damage_list enemies mons))] in
  let (hp,_) = highest (pure_damage_list mons) in
  let (hr,_) = highest (avg_resists_list mons) in
  let (hd,_) = highest (def_stats_list mons) in
  let (hs,_) = highest (speeds_list mons) in
  let (he,_) = highest (speed_vs_enemies_list enemies mons) in
  let (hn,_) = highest (net_damage_list enemies mons) in
  let funcs = [pure_damage_avgs;avg_resist;sum_def_stats;find_speed;
               (speed_vs_enemies enemies);(net_damage enemies)] in
  let helper acc x = 
    (*let ps = List.fold_left2 (fun acc a b -> (p a x b)::acc) [] funcs highs in
    let total =
      List.fold_left2 (fun acc a b -> a *. b +. acc) 0. (List.rev ps) weights in
    (total, x)::acc in *)
                     (((p pure_damage_avgs x hp)*.a +.
                       (p avg_resist x hr)*.b +.
                       (p sum_def_stats x hd)*.c +.
                       (p find_speed x hs)*.d +.
                       (p (speed_vs_enemies enemies) x he)*.e +.
                       (p (net_damage enemies) x hn)*.f), x)::acc in
  List.fold_left helper [] mons


let handle_request c r =
  match r with
    | StarterRequest(gs)-> begin
        let (a1,b1) = gs in
        let my_team = if c = Red then a1 else b1 in
        let (mons, pack) = my_team in
        let pick = 
          try List.find(fun x -> x.curr_hp > 0) mons with _ -> (List.hd mons) in
        SelectStarter(pick.species) end
    | PickRequest(_, gs, _, mons_list) ->
        (match mons_list with
         | h::t -> begin 
             let (a1,b1) = gs in
             let my_team = if c = Red then a1 else b1 in
             let their_team = if c = Red then b1 else a1 in
             let (mons, _) = my_team and (enemies, _) = their_team in
             let m = float_of_int (List.length enemies) in
             let n = (float_of_int cNUM_PICKS) -. m in
             let pdw = 5.+.n
             and rw = (2.+.n)/.3.
             and dsw = 5.+.n
             and sw = (3.+.n)/.2.
             and svw = 9.+.m/.2.
             and ndw = 21.+2.*.m in
             let weights = (pdw,rw,dsw,sw,svw,ndw) in
             let lst = p_list mons_list enemies weights in
             let (d,my_pick) = highest lst in
             print_endline ("picking "^my_pick.species^": "^(string_of_float d));
             PickSteammon(my_pick.species) end
         | [] -> failwith "no steammon to pick!")
    | ActionRequest (gr) ->
        let (a1, b1) = gr in
        let my_team = if c = Red then a1 else b1 in
        let (mons, [a;b;c;d;e;f;g;h]) = my_team in
        (match mons with
        | h::t ->
            if h.curr_hp < h.max_hp && b > 0 then UseItem(MaxPotion,h.species) else
            if (h.first_attack).pp_remaining >0 then
              let _ = print_endline (h.species ^ " used " ^ ((h.first_attack).name)) in
                UseAttack((h.first_attack).name)
            else if ((h.second_attack).pp_remaining > 0) then
              let _ = print_endline (h.species ^ " used " ^ ((h.second_attack).name)) in
                UseAttack((h.second_attack).name)
            else if ((h.third_attack).pp_remaining >0) then
              let _ = print_endline (h.species ^ " used " ^ ((h.third_attack).name)) in
                UseAttack((h.third_attack).name)
            else
              let _ = print_endline (h.species ^ " used " ^ ((h.fourth_attack).name)) in
                UseAttack((h.fourth_attack).name)
        | _ -> failwith "WHAT IN THE NAME OF ZARDOZ HAPPENED HERE")
	 | PickInventoryRequest (gr) -> 
           PickInventory([cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL;
                          cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XACCURACY;cNUM_XSPEED])
let () = run_bot handle_request
