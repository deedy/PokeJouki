open Definitions

module Sstring = struct
  type t = string
  let hash s1 = Hashtbl.hash s1
  let equal s1 s2 = String.compare s1 s2 = 0
end

module Table = Hashtbl.Make(Sstring)

let hash_to_list ht = Table.fold(fun k v acc -> v::acc) ht []

let type_of_string str =
  match str with
  | "Fire" -> Fire
  | "Water" ->Water
  | "Ice" -> Ice
  | "Grass" -> Grass
  | "Poison" -> Poison
  | "Normal" -> Normal
  | "Flying" -> Flying
  | "Psychic" -> Psychic
  | "Ghost" -> Ghost
  | "Dark" -> Dark
  | "Steel" -> Steel
  | "Rock" -> Rock
  | "Ground" -> Ground
  | "Electric" -> Electric
  | "Bug" -> Bug
  | "Dragon" -> Dragon
  | "Fighting" -> Fighting
  | _ -> failwith "invalid pokemon type string"


let weakness type1 type2 =
  match type1 with
  | Normal->
     (match type2 with
      | Rock
      | Steel -> 0.5
      | Ghost -> 0.
      | _ -> 1.)
  | Fire ->
     (match type2 with
      | Fire
      | Water
      | Rock
      | Dragon -> 0.5
      | Grass
      | Ice
      | Bug
      | Steel -> 2.
      | _ -> 1.)
  | Water ->
     (match type2 with
      | Fire
      | Ground
      | Rock -> 2.
      | Water
      | Grass
      | Dragon -> 0.5
      | _ -> 1.)
  | Grass ->
     (match type2 with
      | Fire|Grass|Poison|Flying|Bug|Dragon|Steel ->0.5
      | Water|Ground|Rock ->2.
      | _ -> 1.)
  | Electric ->
     (match type2 with
      | Water|Flying -> 2.
      | Grass|Electric|Dragon -> 0.5
      | Ground -> 0.
      | _ -> 1.)
  | Ice ->
     (match type2 with
      | Fire|Water|Ice|Steel -> 0.5
      | Grass|Ground|Flying|Dragon -> 2.
      | _ ->1.)
  | Fighting ->
     (match type2 with
      | Normal|Ice|Rock|Dark|Steel -> 2.
      | Poison|Flying|Psychic|Bug-> 0.5
      | Ghost -> 0.
      | _ -> 1.)
  | Poison ->
     (match type2 with
      | Grass-> 2.
      | Poison|Ground|Rock|Ghost -> 0.5
      | Steel -> 0.
      | _ -> 1.)
  | Ground ->
     (match type2 with
      | Fire|Electric|Poison|Rock|Steel-> 2.
      | Grass|Bug -> 0.5
      | Flying -> 0.
      | _ -> 1.)
  | Flying ->
     (match type2 with
      | Grass|Fighting|Bug-> 2.
      | Electric|Rock|Steel -> 0.5
      | _ -> 1.)
  | Psychic ->
     (match type2 with
      | Fighting|Poison -> 2.
      | Psychic|Steel -> 0.5
      | Dark -> 0.
      | _ -> 1.)
  | Bug ->
     (match type2 with
      | Fire|Fighting|Poison|Flying|Ghost|Steel -> 0.5
      | Grass|Psychic|Dark -> 2.
      | _ ->1.)
  | Rock ->
     (match type2 with
      | Fire|Ice|Flying|Bug -> 2.
      | Fighting|Ground|Steel -> 0.5
      | _ -> 1.)
  | Ghost ->
     (match type2 with
      | Psychic|Ghost -> 2.
      | Dark|Steel -> 0.5
      | Normal -> 0.
      | _ -> 1.)
  | Dragon ->
     (match type2 with
      | Dragon -> 2.
      | Steel -> 0.5
      | _ -> 1.)
  | Dark ->
     (match type2 with
      | Poison|Dark|Steel -> 0.5
      | Psychic|Ghost -> 2.
      | _ -> 1.)
  | Steel ->
     (match type2 with
      | Fire|Water|Electric|Steel -> 0.5
      | Ice|Rock -> 2.
      | _ -> 1.)


let color_to_string t =
  match t with
  | Red -> "Red"
  | Blue -> "Blue"

let string_of_color c =
  match c with
  | Red -> "Red"
  | Blue -> "Blue"

let invert_color c = 
	match c with
		| Red -> Blue
		| Blue -> Red
(*
let string_of_steammon_g s =
   "(" ^ s.species ^ "," ^ s.hp ^ "," ^ s.max_hp ^
     "," ^ (string_of_color (s.color)) ^ ")"

let string_of_update u =
  let str_of_tup (s,c,i) = "(" ^ s ^ "," ^ (string_of_color c) ^ "," ^
    (string_of_int i) ^ ")"
  in
  match u with
  | InitGraphics -> "InitGraphics"
  | UpdateSteammon s -> "UpdateSteammon" ^ (string_of_steammon s)
  | SetChosenSteammon s -> "SetChosenSteammon(" ^ s ^ ")"
  | NegativeEffect a -> "NegativeEffect" ^ (str_of_tup a)
  | PositiveEffect a -> "PositiveEffect" ^ (str_of_tup a)
  | Message msg -> "Message(" ^ msg ^ ")"
*)

let rec remove_leading_spaces str =
  if String.get str 0 = ' ' then
    remove_leading_spaces (String.sub str 1 ((String.length str) - 1))
  else
    str


let wordify str =
  let rec helper str lst =
    if(String.contains str ' ') then
      let last_space = String.rindex str ' ' in
      let word = String.sub str last_space ((String.length str) - last_space) in
      helper (String.sub str 0 last_space) (word::lst)
    else
      str::lst in
  List.map remove_leading_spaces (helper str [])

let opposite_color c =
  match c with
  | Red -> Blue
  | _-> Red

let string_of_item it =
  match it with
  | Ether -> "Ether"
  | MaxPotion -> "MaxPotion"
  | Revive -> "Revive"
  | FullHeal -> "FullHeal"
  | XAttack -> "XAttack"
  | XDefense -> "XDefense"
  | XSpeed -> "XSpeed"
  | XAccuracy -> "XAccuracy"

let effect_of_num num =
  match num with
  | 0 -> Nada
  | 1 -> Poisons
  | 2 -> Confuses
  | 3 -> Sleeps
  | 4 -> Paralyzes
  | 5 -> Freezes
  | 6 -> SelfAttackUp1
  | 7 -> SelfDefenseUp1
  | 8 -> SelfSpeedUp1
  | 9 -> SelfAccuracyUp1
  | 10 -> OpponentAttackDown1
  | 11 -> OpponentDefenseDown1
  | 12 -> OpponentSpeedDown1
  | 13 -> OpponentAccuracyDown1
  | _ -> failwith "illegal attack value"

let read_lines filename =
  let lines = ref [] in
  let chan = open_in filename in
  try
    while true; do
      lines := input_line chan :: !lines
    done;
    []
  with End_of_file ->
    close_in chan;
    List.rev !lines
