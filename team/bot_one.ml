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

let rec append l1 l2 = 
  match l1 with
  | [] -> l2
  | h::t -> append t (h::l2)

let rec append_n things num_times lst =
  if num_times = 0 then lst 
  else append_n things (num_times - 1) (append things lst)

let sort_and_rank cmp lst =
  let sorted = List.fast_sort cmp lst in
  let helper acc x =
    match acc with
    | [] -> [(x,1)]
    | (h,r)::t -> if cmp x h = 0 then (x,r)::acc 
                  else (x,(List.length acc)+1)::acc in
  List.rev (List.fold_left helper [] sorted)

let find_rank mon lst =
  let helper acc (x,r) = if x.species = mon.species then Some r else acc in
  let rank = List.fold_left helper None lst in 
  match rank with None -> failwith "Could not find rank" | Some r -> r

let find_total_rank mon lsts =
  List.fold_left (fun acc x -> (find_rank mon x) + acc) 0 lsts

let sort_by_total_rank steammon_list lsts =
  let cmp x y = (find_total_rank x lsts) - (find_total_rank y lsts) in
  List.sort cmp steammon_list

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
  and crit = (float_of_int attack.crit_chance)*.(cCRIT_MULTIPLIER -. 1.)+.1. in
  if (power *. accur *. att *. stab *. mult *. crit /. def) /. hp > 1. then 1.
  else (power *. accur *. att *. stab *. mult *. crit /. def) /. hp

let find_damages mon1 mon2 = 
  let helper acc x = (find_damage x mon1 mon2)::acc in
  let damages = List.fold_left helper [] (return_attacks mon1) in
  let cmp x y = int_of_float (1000.*.y) - int_of_float (1000.*.x) in
  List.sort cmp damages

let find_enemy_damage_avg mon enemies =
  let helper acc x = (List.hd (find_damages mon x)) +. acc in
  let total = List.fold_left helper 0. enemies in
  total /. (float_of_int (List.length enemies))

let find_enemy_damage_taken_avg mon enemies =
  let helper acc x = (List.hd (find_damages x mon)) +. acc in
  let total =  List.fold_left helper 0. enemies in
  total /. (float_of_int (List.length enemies))

let find_net_damage_avg mon enemies = (find_enemy_damage_avg mon enemies) -. 
                                      (find_enemy_damage_taken_avg mon enemies)

let sort_by_net_damage mons enemies =
  let cmp x y = int_of_float (1000.*.(find_net_damage_avg y enemies)) -
                int_of_float (1000.*.(find_net_damage_avg x enemies)) in
  sort_and_rank cmp mons

let find_speed_vsenemies mon enemies =
  let helper acc x = if mon.speed > x.speed then acc + 1 else acc in
  let total = List.fold_left helper 0 enemies in
  (float_of_int total) /. (float_of_int (List.length enemies))

let sort_by_speed_vsenemies mons enemies =
  let cmp x y = int_of_float (1000.*.(find_speed_vsenemies y enemies)) -
                int_of_float (1000.*.(find_speed_vsenemies x enemies)) in
  sort_and_rank cmp mons

let find_pure_damage attack mon typ =
  let power = float_of_int attack.power
  and accur = (float_of_int attack.accuracy) /. 100.
  and stab = calc_stab attack mon
  and mult = weakness attack.element typ
  and att = if special attack then (float_of_int mon.spl_attack)
            else (float_of_int mon.attack)
  and crit = (float_of_int attack.crit_chance)*.(cCRIT_MULTIPLIER -. 1.)+.1. in
  power *. accur *. att *. stab *. mult *. crit

let find_pure_damage_avg attack mon =
  let helper acc x = (find_pure_damage attack mon x) +. acc in
  let sum = List.fold_left helper 0. types in
  sum /. (float_of_int (List.length types))

let find_pure_damages mon =
  let helper acc x = (find_pure_damage_avg x mon)::acc in
  let damages = List.fold_left helper [] (return_attacks mon) in
  List.sort (fun x y -> (int_of_float (1000.*.y) - (int_of_float (1000.*.x)))) damages

let sort_by_pure_damage mon_list =
  let cmp x y = int_of_float (1000.*.(List.hd (find_pure_damages y))) -
                int_of_float (1000.*.(List.hd (find_pure_damages x))) in
  sort_and_rank cmp mon_list

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

let sum_def_stats mon = mon.max_hp + mon.defense + mon.spl_defense

let sort_by_resists mon_list =
  let cmp x y = int_of_float (find_avg_resist x) -
                int_of_float (find_avg_resist y) in
  sort_and_rank cmp mon_list

let sort_by_def_stats mon_list =
  let cmp x y = (sum_def_stats y) - (sum_def_stats x) in
  sort_and_rank cmp mon_list

let sort_by_speed mon_list =
  sort_and_rank (fun x y -> y.speed - x.speed) mon_list

let handle_request c r =
  match r with
    | StarterRequest(gs)-> begin
        let (a1,b1) = gs in
        let my_team = if c = Red then a1 else b1
        and their_team = if c = Red then b1 else a1 in
        let (mons,_) = my_team and (enemies,_) = their_team in
        let l1 = sort_by_speed_vsenemies mons enemies
        and l2 = sort_by_net_damage mons enemies in
        let sorted = sort_by_total_rank mons [l1;l2;l2] in
        let pick = if sorted = [] then failwith "All pokemon are fainted"
                   else List.hd sorted in
        SelectStarter(pick.species) end
    | PickRequest(color, gs, attack_list, steammon_list) ->
        let (a1,b1) = gs in
        let their_team = if c = Red then b1 else a1 in
        let (enemies,_) = their_team in
        if steammon_list = [] then failwith "No steammon to pick!"
        else begin
          print_endline "Here";
          let l1 = sort_by_def_stats steammon_list in
          print_endline "Got 1 done!";
          let l2 = sort_by_pure_damage steammon_list in
          print_endline "Got 2 done!";
          let l3 = sort_by_speed steammon_list in
          print_endline "Got 3 done!";
          let l4 = sort_by_resists steammon_list in
          print_endline "Now I'm here";
          let l5 = sort_by_speed_vsenemies steammon_list enemies in
          print_endline "Still going";
          let l6 = sort_by_net_damage steammon_list enemies in
          print_endline "Not stopping";
          match List.length enemies with
          | 0 ->
            let sorted = 
              sort_by_total_rank steammon_list [l1;l1;l1;l2;l2;l2;l3;l3;l3;l4] in
            let my_pick = List.hd sorted in
            print_endline ("picking "^my_pick.species^" "^
                           (string_of_int (find_rank my_pick l1))^" "^
                           (string_of_int (find_rank my_pick l2))^" "^
                           (string_of_int (find_rank my_pick l3))^" "^
                           (string_of_int (find_rank my_pick l4)));
            print_endline (List.nth sorted 1).species;
            print_endline (List.nth sorted 2).species;
            PickSteammon(my_pick.species)
          | _ -> begin
            let num_enemies = List.length enemies in
            let picks_left = cNUM_PICKS - num_enemies in
            let lst = append_n [l5] (num_enemies) [] in
            let lst1 = append_n [l6] (4 * num_enemies) lst in
            let lst2 = append_n [l1;l2;l3] (2 * picks_left) lst1 in
            let lst3 = append_n [l4] ((picks_left + 1) * 2 / 3) lst2 in
            print_endline "Woohoo!";
            let sorted = sort_by_total_rank steammon_list lst3 in
            print_endline "Done!";
            (*let rev = List.rev sorted in
            let worst  = List.hd rev in*)
            let my_pick = List.hd sorted in
            print_endline ("picking "^my_pick.species^" "^
                           (string_of_int (find_rank my_pick l1))^" "^
                           (string_of_int (find_rank my_pick l2))^" "^
                           (string_of_int (find_rank my_pick l3))^" "^
                           (string_of_int (find_rank my_pick l4))^" "^
                           (string_of_int (find_rank my_pick l5))^" "^
                           (string_of_int (find_rank my_pick l6)));
            print_endline ((List.nth sorted 1).species^" "^
                           (string_of_int (find_rank (List.nth sorted 1) l1))^" "^
                           (string_of_int (find_rank (List.nth sorted 1) l2))^" "^
                           (string_of_int (find_rank (List.nth sorted 1) l3))^" "^
                           (string_of_int (find_rank (List.nth sorted 1) l4))^" "^
                           (string_of_int (find_rank (List.nth sorted 1) l5))^" "^
                           (string_of_int (find_rank (List.nth sorted 1) l6)));
            print_endline ((List.nth sorted 2).species^" "^
                           (string_of_int (find_rank (List.nth sorted 2) l1))^" "^
                           (string_of_int (find_rank (List.nth sorted 2) l2))^" "^
                           (string_of_int (find_rank (List.nth sorted 2) l3))^" "^
                           (string_of_int (find_rank (List.nth sorted 2) l4))^" "^
                           (string_of_int (find_rank (List.nth sorted 2) l5))^" "^
                           (string_of_int (find_rank (List.nth sorted 2) l6)));
            (*print_endline ("worst: "^worst.species^" "^
                           (string_of_int (find_rank worst l1))^" "^
                           (string_of_int (find_rank worst l2))^" "^
                           (string_of_int (find_rank worst l3))^" "^
                           (string_of_int (find_rank worst l4))^" "^
                           (string_of_int (find_rank worst l5))^" "^
                           (string_of_int (find_rank worst l6))^" ");*)
            PickSteammon(my_pick.species) end end
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
