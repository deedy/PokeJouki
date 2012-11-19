type steamtype = Fire
  | Water
  | Ice
  | Grass
  | Poison
  | Normal
  | Flying
  | Psychic
  | Ghost
  | Dark
  | Steel
  | Rock
  | Ground
  | Electric
  | Bug
  | Dragon
  | Fighting

type color = Red | Blue

type effects = Nada
  | Poisons
  | Confuses
  | Sleeps
  | Paralyzes
  | Freezes
  | SelfAttackUp1
  | SelfDefenseUp1
  | SelfSpeedUp1
  | SelfAccuracyUp1
  | OpponentAttackDown1
  | OpponentDefenseDown1
  | OpponentSpeedDown1
  | OpponentAccuracyDown1

type attack_effect = effects * int

type item = Ether
  | MaxPotion
  | Revive
  | FullHeal
  | XAttack
  | XDefense
  | XSpeed
  | XAccuracy

type inventory = int list (* above order *)

type status = Paralyzed
  | Poisoned
  | Asleep
  | Confused
  | Frozen

type modifier = {
  attack_mod: int;
  speed_mod: int;
  defense_mod: int;
  accuracy_mod: int
}

type attack = {
  name: string;
  element: steamtype;
  max_pp: int;
  pp_remaining: int;
  power : int;
  accuracy: int;
  crit_chance: int;
  effect: attack_effect
}

type steammon = {
  species: string;
  curr_hp : int;
  max_hp : int;
  first_type: steamtype option;
  second_type: steamtype option;
  first_attack: attack;
  second_attack: attack;
  third_attack: attack;
  fourth_attack: attack;
  attack: int;
	spl_attack : int;
  defense: int;
	spl_defense: int;
  speed: int;
  status: status list;
  mods: modifier
}

(* active steammon is the head of the list of steammon *)
type team_data = steammon list * inventory
(* red team, blue team *)
type game_status_data = team_data * team_data
(* no specified order *)
type attack_set = attack list
(* no specified order *)
type steam_pool = steammon list

type game_result = Winner of color | Tie

(*Battle Actions send by AI*)
type action = SelectStarter of string
  					| PickSteammon of string
						| PickInventory of inventory
  					| SwitchSteammon of string
  					| UseItem of item * string
  					| UseAttack of string

(* Graphic Updates *)
type update = InitGraphics 
            | UpdateSteammon of (string*int*int*color)
            | SetChosenSteammon of string 
            | NegativeEffect of string * color * int 
            | PositiveEffect of string * color * int 
            | SetFirstAttacker of color
            | SetStatusEffects of string * status list 
            | Message of string


(*Control Updates *)
type control = GameStart
             | GameRequest
             | Team of color
             | GameEnd

type request = StarterRequest of game_status_data 
             | PickRequest of color * game_status_data * attack_set * steam_pool 
						 | PickInventoryRequest of game_status_data 
             | ActionRequest of game_status_data

type command = Control of control 
             | Action of action 
             | DoNothing 
             | Request of request 
             | Error of string


type game_output = game_result option * game_status_data * command option * command option
