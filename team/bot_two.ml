open Team
open Definitions
open Constants
open Util

(* Attention Student:
 * You only need to modify the handle_request function. Do not change its arguments.
 * You should change all of the inside and write many helper functions if you
 * want to have a good bot.
 *)
let _ = Random.self_init ()

let return_attacks mon = 
  [mon.first_attack;mon.second_attack;mon.third_attack;mon.fourth_attack]

let types = [Fire;Water;Ice;Grass;Poison;Normal;Flying;Psychic;Ghost;
             Dark;Steel;Rock;Ground;Electric;Bug;Dragon;Fighting]

let cmp_tup_floats (x,_) (y,_) = int_of_float (1000.*.y) - int_of_float (1000.*.x)

let highest lst =
  let helper acc x = if x > acc then x else acc in
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

let find_damage attack mon1 mon2 =
  let hp = float_of_int mon2.curr_hp
  and power = float_of_int attack.power
  and accur = (float_of_int attack.accuracy) /. 100.
  and (stab,mult) = (calc_stab attack mon1, calc_mult attack mon2)
  and (att,def) = (find_att_stat attack mon1, find_def_stat attack mon2)
  and critchance = float_of_int attack.crit_chance in
  let crit = (critchance)/.100.*.(cCRIT_MULTIPLIER -. 1.)+.1. in
  if ((power *. accur *. att *. stab *. mult *. crit /. def) /. hp) > 1. then 1.
  else (power *. accur *. att *. stab *. mult *. crit /. def) /. hp

let find_damages_and_attacks mon1 mon2 = 
  let helper acc x = (find_damage x mon1 mon2, x.name)::acc in
  let damages = List.fold_left helper [] (return_attacks mon1) in
  let cmp (x,_) (y,_) = int_of_float (1000.*.y) - int_of_float (1000.*.x) in
  List.sort cmp damages

let find_enemy_damage mon enemies =
  let helper acc x = fst (List.hd (find_damages_and_attacks mon x)) +. acc in
  List.fold_left helper 0. enemies

let find_enemy_damage_taken mon enemies =
  let helper acc x = fst (List.hd (find_damages_and_attacks x mon)) +. acc in
  List.fold_left helper 0. enemies

let find_net_damage_avg mon enemies = (find_enemy_damage mon enemies) -. 
                                      (find_enemy_damage_taken mon enemies)

let find_net_damage_avgs mons enemies =
  List.fold_left (fun acc x -> (find_net_damage_avg x enemies)::acc) [] mons

let find_speed_vsenemies mon enemies =
  let helper acc x = if mon.speed > x.speed then acc +. 1. else acc in
   List.fold_left helper 0. enemies 

let speeds_vsenemies mons enemies =
  List.fold_left (fun acc x -> (find_speed_vsenemies x enemies)::acc) [] mons

let find_pure_damage attack mon typ =
  let power = float_of_int attack.power
  and accur = (float_of_int attack.accuracy) /. 100.
  and stab = calc_stab attack mon
  and mult = weakness attack.element typ
  and att = if special attack then (float_of_int mon.spl_attack)
            else (float_of_int mon.attack) in
  let critchance = float_of_int attack.crit_chance in
  let crit = critchance /. 100. *. (cCRIT_MULTIPLIER -. 1.) +. 1. in
  power *. accur *. att *. stab *. mult *. crit

let find_pure_damages mon typ =
  let helper acc x = (find_pure_damage x mon typ)::acc in
  let damages = List.fold_left helper [] (return_attacks mon) in
  let cmp x y = int_of_float (1000.*.y) - int_of_float (1000.*.x) in
  List.sort cmp damages

let find_pure_damage_avg mon =
  let helper acc x = (List.hd (find_pure_damages mon x)) +. acc in
  let total = List.fold_left helper 0. types in
  total /. (float_of_int (List.length types))  

let pure_damages_list mons =
  List.fold_left (fun acc x -> (find_pure_damage_avg x)::acc) [] mons

let make_attack typ = {name = "";
                       element = typ;
                       max_pp = 10;
                       pp_remaining = 10;
                       power = 100;
                       accuracy = 100;
                       crit_chance = 0;
                       effect = (Nada,0)}

let find_avg_resist mon = 
  let helper acc x = (calc_mult (make_attack x) mon) +. acc in
  let sum_resists = List.fold_left helper 0. types in
  sum_resists /. (float_of_int (List.length types))

let avg_resists_list mons =
  List.fold_left (fun acc x -> (find_avg_resist x)::acc) [] mons
  
let sum_def_stats mon = mon.max_hp + mon.defense + mon.spl_defense

let def_stats_list mons = 
  List.fold_left (fun acc x -> (float_of_int (sum_def_stats x))::acc) [] mons

let speeds_list mons = 
  List.fold_left (fun acc x -> (float_of_int x.speed)::acc) [] mons

let find_total_proportion mon mons enemies weights hs =
  let def_prop = if List.hd hs = 0. then 0.
                 else (float_of_int (sum_def_stats mon)) /. (List.hd hs) in
  let pda_prop = if List.nth hs 1 = 0. then 0.
                 else (find_pure_damage_avg mon) /. (List.nth hs 1) in
  let spe_prop = if List.nth hs 2 = 0. then 0.
                 else (float_of_int mon.speed) /. (List.nth hs 2) in
  let res_prop = if List.nth hs 3 = 0. then 0.
                 else (find_avg_resist mon) /. (List.nth hs 3) in
  let spv_prop = if List.nth hs 4 = 0. then 0.
                 else (find_speed_vsenemies mon enemies) /. (List.nth hs 4) in
  let nda_prop = if List.nth hs 5 = 0. then 0.
                 else (find_net_damage_avg mon enemies) /. (List.nth hs 5) in
  let props = [def_prop;pda_prop;spe_prop;res_prop;spv_prop;nda_prop] in
  List.fold_left2 (fun acc x y -> x*.y +. acc) 0. props weights
  
let total_proportions mons es ws =
  let high_net_damage = highest (find_net_damage_avgs mons es)
  and high_spevs = highest (speeds_vsenemies mons es) 
  and high_pure_damage = highest (pure_damages_list mons)
  and high_res = highest (avg_resists_list mons)
  and high_speed = highest (speeds_list mons) 
  and high_def = highest (def_stats_list mons) in 
  let highs =
    [high_def;high_pure_damage;high_speed;high_res;high_spevs;high_net_damage] in
  let helper acc x = (find_total_proportion x mons es ws highs,x)::acc in
  List.fold_left helper [] mons

let highest_total_proportion mons es ws =
  let lst = total_proportions mons es ws in
  let helper (acc,n1) (x,n2) = if x > acc then (x,n2) else (acc,n1) in
  let (_,mon) = List.fold_left helper (List.hd lst) lst in mon

let sort_by_total_proportion mons es ws =
  let lst = total_proportions mons es ws in
  List.fast_sort cmp_tup_floats lst

let faster mon enemy = mon.speed > enemy.speed 

let no_miss_damage attack mon1 mon2 =
  let hp = float_of_int mon2.curr_hp
  and power = float_of_int attack.power
  and (stab,mult) = (calc_stab attack mon1, calc_mult attack mon2)
  and (att,def) = (find_att_stat attack mon1, find_def_stat attack mon2) in
  let damage = (power *. att *. stab *. mult /. def) /. hp in
  if attack.accuracy = 100 then
    if damage > 1. then 1. else damage
  else 0.

let no_miss_damages_and_attacks mon1 mon2 =
  let attacks = return_attacks mon1 in
  let helper acc x = (no_miss_damage x mon1 mon2, x.name)::acc in
  let damages = List.fold_left helper [] attacks in
  let cmp (x,_) (y,_) = int_of_float (1000.*.x) - int_of_float (1000.*.y) in
  List.sort cmp damages

let can_one_shot mon1 mon2 =
  let (dmg,att) = List.hd (no_miss_damages_and_attacks mon1 mon2) in
  if dmg = 1. then (true,att) else (false,"")


let handle_request c r =
  match r with
    | StarterRequest(gs)-> begin
        let (a1,b1) = gs in
        let my_team = if c = Red then a1 else b1
        and their_team = if c = Red then b1 else a1 in
        let (mons,_) = my_team and (enemies,_) = their_team in
        let mon = List.hd mons and enemy = List.hd enemies in
        let ats m = find_damages_and_attacks m enemy
        and des m = find_damages_and_attacks enemy m in
        let p m = fst (List.hd (ats m)) /. fst (List.hd (des m)) in
        let l = List.fold_left (fun acc x -> (p x, x)::acc) [] mons in
        let sl = List.sort cmp_tup_floats l in
        let (pr,best) = List.hd sl in
        SelectStarter(best.species) end
    | PickRequest(color, gs, attack_list, mons_list) ->
        let (a1,b1) = gs in
        let their_team = if c = Red then b1 else a1 in
        let (enemies,_) = their_team in
        if mons_list = [] then failwith "No steammon to pick!"
        else begin
          let num_enemies = float_of_int (List.length enemies) in
          let picks_left = float_of_int cNUM_PICKS -. num_enemies in
          let lst = [2.*.picks_left;2.*.picks_left;2.*.picks_left;picks_left;
                     num_enemies;4.*.num_enemies] in
          let s = highest_total_proportion mons_list enemies lst in
          print_endline ("picking "^s.species);
          PickSteammon(s.species) end
    | ActionRequest (gr) ->
        let (a1, b1) = gr in
        let my_team = if c = Red then a1 else b1 in
        let their_team = if c = Red then b1 else a1 in
        let (mons, my_inv) = my_team and (enemies, their_inv) = their_team in
        begin match mons with
        | h::t -> begin
            let mon = List.hd mons and enemy = List.hd enemies in
            let ats m = find_damages_and_attacks m enemy
            and des m = find_damages_and_attacks enemy m in
            let (one_shot,att) = can_one_shot mon enemy in
            let p m = fst (List.hd (ats m)) /. fst (List.hd (des m)) in
            let l = List.fold_left (fun acc x -> (p x, x)::acc) [] mons in
            let sl = List.sort cmp_tup_floats l in
            let (pr,best) = List.hd sl in
            if one_shot && faster mon enemy then UseAttack(att)
            else if (pr > ((p mon) *. 3. /. 2.)) && (best.curr_hp <> 0) then 
              SwitchSteammon(best.species)
            else UseAttack(snd (List.hd (ats mon))) end
        | _ -> failwith "WHAT IN THE NAME OF ZARDOZ HAPPENED HERE" end
    | PickInventoryRequest (gr) -> 
        let (a1, b1) = gr in
        let my_team = if c = Red then a1 else b1 in
        let their_team = if c = Red then b1 else a1 in
        let (mons,_) = my_team and (enemies,_) = their_team in
        let ethers = 0
        and maxpots = 16
        and revives = 0
        and fullheals = 0
        and xattacks = 0
        and xdefenses = 0
        and xaccuracy = 0
        and xspeeds = 0 in
        PickInventory([ethers;maxpots;revives;fullheals;
                       xattacks;xdefenses;xaccuracy;xspeeds])
let () = run_bot handle_request
