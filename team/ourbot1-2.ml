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

(****************** FOR CALCULATING DAMAGES and SPEEDS ************************)
let get_attmod s = 
		match s.mods.attack_mod with
		| 1 -> cATTACK_UP1 | 2 -> cATTACK_UP2 | 3 -> cATTACK_UP3
		| -1 -> cATTACK_DOWN1 | -2 -> cATTACK_DOWN2 | -3 -> cATTACK_DOWN3
		| _-> 1.

let get_defmod s = 
		match s.mods.defense_mod with
		| 1 -> cDEFENSE_UP1 | 2 -> cDEFENSE_UP2 | 3 -> cDEFENSE_UP3
		| -1 -> cDEFENSE_DOWN1 | -2 -> cDEFENSE_DOWN2 | -3 -> cDEFENSE_DOWN3
		| _-> 1.

let get_accmod s = 
	match s.mods.accuracy_mod with
	| 1 -> cACCURACY_UP1 | 2 -> cACCURACY_UP2 | 3 -> cACCURACY_UP3
	| -1 -> cACCURACY_DOWN1 | -2 -> cACCURACY_DOWN2 | -3 -> cACCURACY_DOWN3
	| _-> 1.

let get_spdmod s = 
	match s.mods.speed_mod with
	| 1 -> cSPEED_UP1 | 2 -> cSPEED_UP2 | 3 -> cSPEED_UP3
	| -1 -> cSPEED_DOWN1 | -2 -> cSPEED_DOWN2 | -3 -> cSPEED_DOWN3
	| _-> 1.

(*get the actual speed of a steammon*)
let get_speed s = 
	int_of_float ((float_of_int s.speed) *. (get_spdmod s))
	
(*return if s1 is faster than s2*)	
let is_faster s1 s2 = get_speed s1 > get_speed s2

(*return the damage an attack will do from s1 onto s2, assuming no crithit*)
let get_damage (attack:attack) (s1:steammon) (s2:steammon):int = 
	let attackPower = float_of_int attack.power in
	let at = attack.element in
	let stabBonus = 
		if (match s1.first_type, s1.second_type with
				| Some t1, Some t2-> at = t1 || at = t2
				| Some t1, None -> at = t1
				| _-> failwith "a steammon has at least one type")
		then cSTAB_BONUS else 1.0 in
	let attmod = get_attmod s1 and defmod = get_defmod s2 in
	let (attacker, defender) = 
		match at with
		| Fire | Water |Electric | Grass | Ice | Psychic | Dragon | Dark -> 
			float_of_int s1.spl_attack,float_of_int s2.spl_defense
		| _-> (float_of_int s1.attack)*.attmod,(float_of_int s2.defense)*.defmod in
	let steamtypeMulti = 
		match s2.first_type, s2.second_type with
		| Some t1, Some t2 ->(weakness at t1) *. (weakness at t2)
		| Some t1, None -> weakness at t1
		| _->failwith "a steammon has at least one type" in
   int_of_float (attackPower*.attacker*.stabBonus*.steamtypeMulti/.defender)

let all_attacks s = 
	[s.first_attack;s.second_attack;s.third_attack;s.fourth_attack]
	
(*return the attacks of s1 sorted by their damage to s2 in decreasing order*)
let top_damage_attack (s1:steammon) (s2:steammon):attack list = 
	let cmp a1 a2 = (get_damage a2 s1 s2) - (get_damage a1 s1 s2) in
  let l = all_attacks s1 in
	List.sort cmp l

let sum_damage enemies s = 
	let helper2 acc e = 
			(get_damage (List.hd (top_damage_attack e s)) e s)+ acc in
	  List.fold_left helper2 0 enemies

(*return if the given attack has good accuracy*)
let if_good_accuracy attack s = 
	int_of_float ((float_of_int attack.accuracy)*.(get_accmod s)) >= 70

let best_attack s1 s2 = 
	let l = top_damage_attack s1 s2 in
	let rec helper l = 
		match l with
		| h::t -> if (if_good_accuracy h s1)&&(h.pp_remaining > 0) then h 
		          else helper t
		| [] -> (match List.filter (fun a-> a.pp_remaining >0) l with
		  | [] -> failwith "gg"
			| h::t -> h) in
	helper l 			


(*used to sort attacks by accuracy in decreasing order*)
let cmp_accu a1 a2 = a2.accuracy - a1.accuracy 

(*return if the damage is enough to kill s*)
let will_die damage s = damage >= s.curr_hp

(*return the one-shots of s1 on s2 sorted by accuracy*)
let get_one_shot s1 s2 = 
	let l = List.filter (fun a -> let d = get_damage a s1 s2 in 
	will_die d s2) (all_attacks s1) in
  List.sort cmp_accu l

let get_one_shot_withpp s1 s2 = 
	let one_shots = get_one_shot s1 s2 in
	List.filter (fun a-> a.pp_remaining>0) one_shots
				
(*return if s1 can do no damage to s2*)
let do_no_damage s1 s2 = 
	(List.filter (fun a -> (get_damage a s1 s2) > 0 ) (all_attacks s1)) = []

let do_little_damage s1 s2 = 
	(get_damage (best_attack s1 s2) s1 s2)*100/(s2.max_hp) < 15
		
(*return pseudo damage*)
let get_pseudo_damage (attack:attack) (s1:steammon):int = 
	let attackPower = float_of_int attack.power in
	let at = attack.element in
	let stabBonus = 
		if (match s1.first_type, s1.second_type with
				| Some t1, Some t2-> at = t1 || at = t2
				| Some t1, None -> at = t1
				| _-> failwith "a steammon has at least one type")
		then cSTAB_BONUS else 1.0 in
	let attmod = get_attmod s1 in
	let attacker = 
		match at with
		| Fire | Water |Electric | Grass | Ice | Psychic | Dragon | Dark -> 
			float_of_int s1.spl_attack
		| _-> (float_of_int s1.attack)*.attmod in
	let types = [Fire;Water;Ice;Grass;Poison;Normal;Flying;Psychic;Ghost;
	             Dark;Steel;Rock;Ground;Electric;Bug;Dragon;Fighting] in
	let steamtypeMulti = 
      (List.fold_left (fun acc t-> weakness at t +. acc) 0. types)/.17. in
   int_of_float (attackPower*.attacker*.stabBonus*.steamtypeMulti)

(*************************** FOR PICKING ACTIONS ******************************)
(*return a list of the four attacks the given steammon has*)




(*return the attack with paralyze effect with the highest accuracy*)
(*return none if s has no such attack*)
let has_paralyze (s:steammon) = 
	let l = List.filter (fun x-> fst x.effect == Paralyzes 
	&& snd x.effect >= 90 && if_good_accuracy x s) (all_attacks s) in
  match l with
	| [] -> None
	| h::t -> Some (List.hd l)

let is_paralyzed s = 
	match s.status with
	| []-> false
	| [h]-> h = Paralyzed
	| h1::h2::t-> h1 = Paralyzed || h2 = Paralyzed

(*return how many moves it takes for s1 to kill s2 from curr hp*)
let moves_to_kill s1 s2 = 
	let damage = get_damage (List.hd (top_damage_attack s1 s2)) s1 s2 in
	int_of_float (ceil ((float_of_int s2.curr_hp)/.(float_of_int damage)))

(*return how many moves it takes for s1 to kill s2 from full hp*)
let moves_to_kill_from_full s1 s2 = 
	let damage = get_damage (List.hd (top_damage_attack s1 s2)) s1 s2 in
	int_of_float (ceil ((float_of_int s2.max_hp)/.(float_of_int damage)))

(*return if s1 can kill s2 before s2 kill s1*)
let can_kill s1 s2 = 
	let m1 = moves_to_kill s1 s2 and m2 = (moves_to_kill s2 s1) - 1 in
	if is_faster s1 s2 then m1 <= m2 else m1 < m2
	
let good_switch mons s2 = 
	let cmp a b = (moves_to_kill a s2) - (moves_to_kill b s2) in
	List.sort cmp (List.filter (fun s-> can_kill s s2) mons)
	
let get_shield mons s2 = 
	let cmp a b = (sum_damage [s2] a) - (sum_damage [s2] b) in
	let shield = List.hd (List.sort cmp mons) in
	if get_one_shot_withpp s2 shield <> [] then None else Some shield
	



(******************** FOR PICKING A STEAMMON TO SWITCH IN *********************)	

	
																	
(************************** FOR PICKING STEAMMONS *****************************)
(*return the steammon with the greatest sum of stats among the pool*)
let get_stats s =  
		s.max_hp/4 + s.attack + s.spl_attack + s.defense + s.spl_defense+s.speed 
		
let best_stats sp =
	let s1 = List.hd sp in
	let (best, max) = List.fold_left (fun (best,max) s -> 
		let stats = get_stats s in
		if stats > max then (s,stats)
		else if stats = max && s.speed > best.speed then (s,stats)
		else (best,max)) (s1, get_stats s1) sp in
	best

let best_stats_dark sp = 
	let possible = 
	let darks = List.filter (fun s ->
		match s.first_type, s.second_type with
		| Some t1, Some t2 -> t1 = Dark || t2 = Dark
		| Some t1, None -> t1 = Dark
		| _-> failwith "no type at all?") sp in 
		best_stats darks in
	if get_stats possible < 600 then best_stats sp else possible

(*return the pokemon which takes in least damage from the current enemies*)
let best_defender enemies sp = 
	List.fold_left (fun best s-> if (sum_damage enemies s/s.max_hp) < 
	(sum_damage enemies best/best.max_hp) then s else best) (List.hd sp) sp

(*return an array of the top 50 steammons with the highest speed*)
let top_speed (sp:steammon list) = 
	let cmp s1 s2 = s1.speed - s2.speed in
	let tops:steammon array = Array.init 50 (fun n -> List.nth sp n) in
	List.iter (fun s -> Array.sort cmp tops;
		if s.speed > (Array.get tops 0).speed 
		then Array.set tops 0 s else ()) sp;
	Array.sort cmp tops; tops
	
(*return the pokemon which makes the most damage to the current enemies *)
let best_attacker enemies sp =
	let fast_sp = Array.to_list (top_speed sp) in
  let sum_damage s = 
		let helper2 acc e = 
			(get_damage (List.hd (top_damage_attack s e)) s e) + acc in
	  List.fold_left helper2 0 enemies in
	List.fold_left (fun best s-> if sum_damage s > sum_damage best 
	then s else best) (List.hd fast_sp) fast_sp 

(************************** FOR SACRIFICIAL MOVES *****************************)

let is_dead s = s.curr_hp <= 0 

let still_alive mons = 
	List.filter (fun s-> not (is_dead s)) mons

let low_health s = s.curr_hp*100/s.max_hp <= 30

let low_health2 s = s.curr_hp*100/s.max_hp <= 50

let worth_potion s = 
	let attacks = all_attacks s in
	match List.filter (fun a -> a.pp_remaining <= 0 ) attacks with
	| [] -> low_health s && (not (is_dead s))
	| h::t -> false

let worth_potion_less s = 
	let attacks = all_attacks s in
	match List.filter (fun a -> a.pp_remaining <= 0 ) attacks with
	| [] -> low_health2 s && (not (is_dead s))
	| h::t -> false
	 
let worth_revive s = 
	let attacks = all_attacks s in
	match List.filter (fun a -> a.pp_remaining <= 0) attacks with
	| [] -> is_dead s
	| h::t -> false

let worth_xattack s = 
	s.attack > s.spl_attack &&
	s.attack > s.defense &&
	s.attack > s.spl_defense
	
let to_potion mons = 
	let l = List.filter (fun s -> worth_potion s) mons in
	match l with
	| []-> 
		(let l2 = List.filter (fun s -> worth_potion_less s) mons in
			match l2 with
 	  | [] -> None
		| h1::t1 -> let cmp s1 s2 = get_stats s2 - get_stats s1 in
		Some (List.hd (List.sort cmp l2)))
	| h::t-> 
		let cmp s1 s2 = get_stats s2 - get_stats s1 in
		Some (List.hd (List.sort cmp l))

let to_revive mons = 
	let l = List.filter (fun s -> worth_revive s) mons in
	match l with
	| []-> None
	| h::t-> 
		let cmp s1 s2 = get_stats s2 - get_stats s1 in
		Some (List.hd (List.sort cmp l))

let best_move_outofpp s1 s2 = 
	let a = List.hd (top_damage_attack s1 s2) in
	a.pp_remaining <= 0


	
let to_ether mons s2 = 
	let l = still_alive mons in
	match List.filter (fun s -> best_move_outofpp s s2) l with
	| [] -> None
	| h::t -> 
		let cmp s1 s2 = get_stats s2 - get_stats s1 in
		Some (List.hd (List.sort cmp l))
	
(********************** CHECK THE STATUS OF OUR PARTY *************************)	

let can_one_shot s2 mons = 
	let l = List.filter (fun s-> get_one_shot_withpp s s2 <> []) mons in
	let cmp s1 s2 = s2.speed - s1.speed in
	List.sort cmp l



	
(*return an array of the top 5 tanks*)
let top_tanks sp = 
	let tank s = s.max_hp/4 + s.defense + s.spl_defense in
	let cmp s1 s2 = tank s1 - tank s2 in
	let tops:steammon array = Array.init 5 (fun n-> List.nth sp n) in
	List.iter (fun s -> 
		Array.sort cmp tops;
		if tank s > tank (Array.get tops 0) then Array.set tops 0 s else ()) sp;
	Array.sort cmp tops; tops



let check_for_psyc s1 s2 = 
	(List.hd (top_damage_attack s2 s1)).element = Psychic

let find_dark mons = 
	List.hd (List.filter (fun s ->
		match s.first_type, s.second_type with
		| Some t1, Some t2 -> t1 = Dark || t2 = Dark
		| Some t1, None -> t1 = Dark
		| _-> failwith "no type at all?") mons)
(************************ HANDLE REQUEST HELPERS *****************************)
let pick_action gs c = 
  let (x1,y1) = gs in
  let (my_team,e_team) = if c = Red then (x1,y1) else (y1,x1) in
  let (mons,[ether;potion;revive;_;xattack;_;_;_]) = my_team 
	and (enemies,_) = e_team in
	let s1 = List.hd mons and s2 = List.hd enemies in
	let can_use_potion () =	potion > 0 && to_potion mons <> None
	and can_use_revive () = revive > 0 && to_revive mons <> None
	and can_use_ether () = ether > 0 && to_ether mons s2 <> None in
	let sacrifice () = 
		if can_use_potion () then 
		(match to_potion mons with
			| Some s -> UseItem(MaxPotion,s.species)
			| None -> failwith "impossible")
	  else if can_use_revive () then
		(match to_revive mons with
			| Some s -> UseItem(Revive,s.species)
			| None -> failwith "impossible")
		else if can_use_ether () then
		(match to_ether mons s2 with
			| Some s -> UseItem(Ether,s.species)
			| None -> failwith "impossible")
		else (let move = best_attack s1 s2 in
		let _ = print_endline (s1.species ^"used "^ (move.name)) in 
		UseAttack(move.name)) in
  if (check_for_psyc s1 s2)&&(s1 <> find_dark mons) 
  then SwitchSteammon((find_dark mons).species)
	(*if we do little damage then we want to switch*)
	else if (do_little_damage s1 s2 || do_no_damage s1 s2) then 
		(if List.length (still_alive mons)<= 1 
		 then (let move = best_attack s1 s2 in
				  let _ = print_endline (s1.species ^"used "^ (move.name)) in 
					UseAttack(move.name))
			else 
			match good_switch (still_alive mons) s2 with
			| [] -> (match get_shield (still_alive mons) s2 with
			        | Some shield -> SwitchSteammon(shield.species)
							| None -> 
								let move = best_attack s1 s2 in
							  let _ = print_endline (s1.species ^"used "^ (move.name)) in 
							  UseAttack(move.name))
			| h::t -> SwitchSteammon(h.species))
	else if is_faster s1 s2 then(
	(*in this case, we are faster, we first check if we can oneshot s2*)
		match get_one_shot_withpp s1 s2 with
		(*we can oneshot them*)
		| h::t -> 
			let _ = print_endline (s1.species ^"used "^ (h.name)) in UseAttack(h.name)
		(*we don't have available oneshots*)
		| [] ->
			(match get_one_shot s1 s2 with
			(*we have oneshots without pp*)
			| h2::t2 -> 
				(match get_one_shot s2 s1 with
				| [] -> 
					if ether > 0 then UseItem(Ether,s1.species) else 
					(if (moves_to_kill s1 s2 < 5) then 
				( if (get_one_shot s2 s1 <> [])&&(moves_to_kill_from_full s2 s1 >= 3)
				  then (if potion > 0 then UseItem(MaxPotion,s1.species)
						    else sacrifice ())
					else (if moves_to_kill s2 s1 > 2 then 
				 (if best_move_outofpp s1 s2 && ether> 0 then UseItem(Ether,s1.species)
				 else if worth_xattack s1 && xattack >0 then UseItem(XAttack,s1.species)
				  else sacrifice ())
				else (let move = best_attack s1 s2 in
				 let _ = print_endline (s1.species ^"used "^ (move.name)) in 
				 UseAttack(move.name)))) 
				else ( 
					if List.length (still_alive mons)<= 1 
		      then (let move = best_attack s1 s2 in
				        let _ = print_endline (s1.species ^"used "^ (move.name)) in 
					      UseAttack(move.name))
			    else 
					match good_switch (still_alive mons) s2 with
						| [] -> 
							let move = best_attack s1 s2 in
							let _ = print_endline (s1.species ^"used "^ (move.name)) in 
							UseAttack(move.name)
						| h5::t5 -> SwitchSteammon(h5.species)))
				| h3::t3 -> 
					if (moves_to_kill_from_full s2 s1) <= 2 then
					 (if List.length (still_alive mons)<= 1 
		        then (let move = best_attack s1 s2 in
				          let _ = print_endline (s1.species ^"used "^ (move.name)) in 
					        UseAttack(move.name))
			       else 
						 match good_switch (still_alive mons) s2 with
						| [] -> 
							let move = best_attack s1 s2 in
							let _ = print_endline (s1.species ^"used "^ (move.name)) in 
							UseAttack(move.name)
						| h4::t4 -> SwitchSteammon(h4.species))
					else 
						(if potion > 0 then UseItem(MaxPotion,s1.species)
						else let move = best_attack s1 s2 in
							let _ = print_endline (s1.species ^"used "^ (move.name)) in 
							UseAttack(move.name)))
			(*we don't have any oneshots*)
			| [] -> 
				if (moves_to_kill s1 s2 < 5) then 
				( if (get_one_shot s2 s1 <> [])&&(moves_to_kill_from_full s2 s1 >= 3)
				  then (if potion > 0 then UseItem(MaxPotion,s1.species)
						    else sacrifice ())
					else (if moves_to_kill s2 s1 > 2 then 
				 (if best_move_outofpp s1 s2 && ether> 0 then UseItem(Ether,s1.species)
				 else if worth_xattack s1 && xattack >0 then UseItem(XAttack,s1.species)
				  else sacrifice ())
				else (let move = best_attack s1 s2 in
				 let _ = print_endline (s1.species ^"used "^ (move.name)) in 
				 UseAttack(move.name)))) 
				else ( 
					if List.length (still_alive mons)<= 1 
		      then (let move = best_attack s1 s2 in
				        let _ = print_endline (s1.species ^"used "^ (move.name)) in 
					      UseAttack(move.name)) else 
					match good_switch (still_alive mons) s2 with
						| [] -> 
							let move = best_attack s1 s2 in
							let _ = print_endline (s1.species ^"used "^ (move.name)) in 
							UseAttack(move.name)
						| h5::t5 -> SwitchSteammon(h5.species)))
	)else( 
	(*in this case, we are slower, we first check if we can be oneshoted by s2*)
	  match get_one_shot_withpp s2 s1 with
		(*we can be oneshoted*)
		| h::t ->
			if moves_to_kill_from_full s2 s1 <= 4 then
			(if List.length (still_alive mons)<= 1 
		   then (let move = best_attack s1 s2 in
				     let _ = print_endline (s1.species ^"used "^ (move.name)) in 
					   UseAttack(move.name)) else 
				 match good_switch (still_alive mons) s2 with
				| [] -> sacrifice ()
				| h2::t2 -> SwitchSteammon(h2.species))
			else
				 (if potion> 0 then UseItem(MaxPotion,s1.species)	else 
			   (if List.length (still_alive mons)<= 1 
		      then (let move = best_attack s1 s2 in
				  let _ = print_endline (s1.species ^"used "^ (move.name)) in 
					UseAttack(move.name)) else  
					match good_switch (still_alive mons) s2 with
				 | [] -> sacrifice ()
				 | h2::t2 -> SwitchSteammon(h2.species)))
		(*we can not be oneshoted*)
		 | [] -> 
			(match get_one_shot_withpp s1 s2 with
			| h3::t3 -> 
			  let _ = print_endline (s1.species ^"used "^ (h3.name)) in 
			  UseAttack(h3.name)
			| [] ->
				if (moves_to_kill s1 s2 <=2 ) then
				  (let move = best_attack s1 s2 in
					 let _ = print_endline (s1.species ^"used "^ (move.name)) in 
					 UseAttack(move.name))
				else if (moves_to_kill s2 s1 >= 3) then
				(if best_move_outofpp s1 s2 && ether> 0 then UseItem(Ether,s1.species)
				 else if worth_xattack s1 && xattack >0 then UseItem(XAttack,s1.species)
				 else sacrifice ())
				else 
				(if moves_to_kill s1 s2 <= 3 then
				  (if (not (is_paralyzed s2)) then 
					(match has_paralyze s1 with
					| Some pmove -> 
						let _ = print_endline (s1.species ^"used "^ (pmove.name)) in 
				    UseAttack(pmove.name)
					| None ->
						let move = best_attack s1 s2 in
				    let _ = print_endline (s1.species ^"used "^ (move.name)) in 
				    UseAttack(move.name)) else 
						(let move = best_attack s1 s2 in
				    let _ = print_endline (s1.species ^"used "^ (move.name)) in 
				    UseAttack(move.name)))	
				 else 
					( if List.length (still_alive mons)<= 1 
		        then (let move = best_attack s1 s2 in
				    let _ = print_endline (s1.species ^"used "^ (move.name)) in 
					  UseAttack(move.name)) else 
						match good_switch (still_alive mons) s2 with
						| [] -> 
							let move = best_attack s1 s2 in
							let _ = print_endline (s1.species ^"used "^ (move.name)) in 
							UseAttack(move.name)
						| h4::t4 -> SwitchSteammon(h4.species))))
				)

let best_starter mons enemies =
	let sum_damage s = 
		let helper2 acc e = 
			(get_damage (List.hd (top_damage_attack s e)) s e) + acc in
	  List.fold_left helper2 0 enemies in
	List.fold_left (fun best s-> if sum_damage s > sum_damage best 
	then s else best) (List.hd mons) mons

let best_first_starter mons enemies = 
	let fastest_e = 
		List.hd (List.sort (fun a b -> b.speed - a.speed) enemies) in
	let best_e = best_stats enemies in
	best_starter mons [fastest_e;best_e]
	
let pick_starter gs c = 
	let (x1,y1) = gs in
  let (my_team,e_team) = if c = Red then (x1,y1) else (y1,x1) in
  let (mons,[ether;potion;revive;_;xattack;_;_;_]) = my_team 
	and (enemies,_) = e_team in
	let s1 = List.hd mons and s2 = List.hd enemies in
	(*check if this is the first turn ever*)
	if not (is_dead s1) then 
		let pick = best_first_starter mons enemies in SelectStarter(pick.species)
	else (
		match good_switch (still_alive mons) s2 with
		| [] -> 
			let pick = best_starter (still_alive mons) [s2] 
		  in SelectStarter(pick.species)
		| h::t -> SelectStarter(h.species))

let pick_steammon c gs sp = 	
  if sp = [] then failwith "no steammon to pick!"
	else begin
  let (a1,b1) = gs in
  let (my_team,e_team) = if c = Red then (a1,b1) else (b1,a1) in
  let (mons,_) = my_team and (enemies,_) = e_team in
  let my_pick = 
	  begin match List.length mons, List.length enemies with
		| 0,0 |0,1 -> best_stats_dark sp
		| 2,2 |2,3 -> best_attacker enemies sp
	  | _-> best_stats sp 
		end in print_endline ("picking " ^ my_pick.species);
    PickSteammon(my_pick.species)
	end		
				
(************************ ACTUAL REQUEST HANDLING *****************************)
let handle_request c r =
  match r with
    | StarterRequest(gs)-> pick_starter gs c
    | PickRequest(color,gs, ats, sp) ->	pick_steammon color gs sp
     | ActionRequest (gs) -> pick_action gs c
	   | PickInventoryRequest (gr) -> 
			if cINITIAL_CASH = 4800 then PickInventory([3;7;3;0;1;0;0;0])
			else 
				let potions = (cINITIAL_CASH - 3*cCOST_REVIVE)/cCOST_MAXPOTION in
				let ether = 
					(cINITIAL_CASH-3*cCOST_REVIVE-potions*cCOST_MAXPOTION)/cCOST_ETHER in
			  PickInventory([ether;potions;3;0;0;0;0;0])

let () = run_bot handle_request
				
				
				
				
				
				
				
				
				
				
				