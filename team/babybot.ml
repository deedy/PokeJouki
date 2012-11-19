open Team
open Definitions
open Constants

(* Attention Student:
 * You only need to modify the handle_request function. Do not change its arguments.
 * You should change all of the inside and write many helper functions if you
 * want to have a good bot.
 *)
let _ = Random.self_init ()

let handle_request c r =
  match r with
    | StarterRequest(gs)->
        let (a1,b1) = gs in
        let my_team = if c = Red then a1 else b1 in
        let (mons, pack) = my_team in
        let pick = try List.find(fun x -> x.curr_hp > 0) mons with _ -> (List.hd mons) in
          SelectStarter(pick.species)
    | PickRequest(_, _, _, sp) ->
        (match sp with
         | h::t ->
             let length = List.length sp in
             let my_pick = List.nth sp (Random.int length) in
               print_endline ("picking " ^ my_pick.species);
               PickSteammon(my_pick.species)
         | [] -> failwith "no steammon to pick!")
    | ActionRequest (gr) ->
        let (a1, b1) = gr in
        let my_team = if c = Red then a1 else b1 in
        let (mons, [a;b;c;d;e;f;g;h]) = my_team in
        (match mons with
        | h::t ->
					if h.curr_hp < h.max_hp && b > 0 then UseItem(MaxPotion,h.species) else
            if (h.first_attack).pp_remaining >0 then
              let _ = print_endline (h.species ^ "used " ^ ((h.first_attack).name)) in
                UseAttack((h.first_attack).name)
            else if ((h.second_attack).pp_remaining > 0) then
              let _ = print_endline (h.species ^ "used " ^ ((h.second_attack).name)) in
                UseAttack((h.second_attack).name)
            else if ((h.third_attack).pp_remaining >0) then
              let _ = print_endline (h.species ^ "used " ^ ((h.third_attack).name)) in
                UseAttack((h.third_attack).name)
            else
              let _ = print_endline (h.species ^ "used " ^ ((h.fourth_attack).name)) in
                UseAttack((h.fourth_attack).name)
        | _ -> failwith "WHAT IN THE NAME OF ZARDOZ HAPPENED HERE")
	 | PickInventoryRequest (gr) -> PickInventory(
					[cNUM_ETHER;cNUM_MAX_POTION;cNUM_REVIVE;cNUM_FULL_HEAL;
	 				 cNUM_XATTACK;cNUM_XDEFENSE;cNUM_XACCURACY;cNUM_XSPEED])
let () = run_bot handle_request
