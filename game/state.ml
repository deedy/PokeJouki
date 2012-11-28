open Definitions
open Util
open Constants
open Netgraphics

type minventory = int ref list

type mmodifier = {
  mutable mattack_mod: int;
  mutable mspeed_mod: int;
  mutable mdefense_mod: int;
  mutable maccuracy_mod: int
}

type mattack = {
  mname: string;
  melement: steamtype;
  mmax_pp: int;
  mutable mpp_remaining: int;
  mpower: int;
  maccuracy: int;
  mcrit_chance: int;
  meffect: attack_effect
}

type msteammon = {
  mspecies: string;
  mutable mcurr_hp: int;
  mmax_hp: int;
  mfirst_type: steamtype option;
  msecond_type: steamtype option;
  mfirst_attack: mattack;
  msecond_attack: mattack;
  mthird_attack: mattack;
  mfourth_attack: mattack;
  mattack: int;
  mspl_attack: int;
  mdefense: int;
  mspl_defense: int;
  mspeed: int;
  mutable mstatus: status list;
  mmods: mmodifier
}

type mteam_data = msteammon list ref * minventory
type mgame_status_data = mteam_data * mteam_data

(* Converts from type Definitions.attack to State.mattack *)
let attack_mutafication attack = {
  mname = attack.name;
  melement = attack.element;
  mmax_pp = attack.max_pp;
  mpp_remaining = attack.pp_remaining;
  mpower = attack.power;
  maccuracy = attack.accuracy;
  mcrit_chance = attack.crit_chance;
  meffect = attack.effect
}

(* Converts from type Definitions.modifier to State.mmodifier *)
let modifier_mutafication modifier = {
  mattack_mod = modifier.attack_mod;
  mspeed_mod = modifier.speed_mod;
  mdefense_mod = modifier.defense_mod;
  maccuracy_mod = modifier.accuracy_mod
}

(* Converts from type Definitions.steammon to State.msteammon *)
let steammon_mutafication steammon = {
  mspecies = steammon.species;
  mcurr_hp = steammon.curr_hp;
  mmax_hp = steammon.max_hp;
  mfirst_type = steammon.first_type;
  msecond_type = steammon.second_type;
  mfirst_attack = attack_mutafication steammon.first_attack;
  msecond_attack = attack_mutafication steammon.second_attack;
  mthird_attack = attack_mutafication steammon.third_attack;
  mfourth_attack = attack_mutafication steammon.fourth_attack;
  mattack = steammon.attack;
  mspl_attack = steammon.spl_attack;
  mdefense = steammon.defense;
  mspl_defense = steammon.spl_defense;
  mspeed = steammon.speed;
  mstatus = steammon.status;
  mmods = modifier_mutafication steammon.mods
}

(* Converts from type State.mattack to Definitions.attack *)
let attack_datafication mattack = {
  name = mattack.mname;
  element = mattack.melement;
  max_pp = mattack.mmax_pp;
  pp_remaining = mattack.mpp_remaining;
  power = mattack.mpower;
  accuracy = mattack.maccuracy;
  crit_chance = mattack.mcrit_chance;
  effect = mattack.meffect
}

(* Converts from type State.mmodifier to Definitions.modifier *)
let modifier_datafication mmodifier = {
  attack_mod = mmodifier.mattack_mod;
  speed_mod = mmodifier.mspeed_mod;
  defense_mod = mmodifier.mdefense_mod;
  accuracy_mod = mmodifier.maccuracy_mod
}

(* Converts from type State.msteammon to Definitions.steammon*)
let steammon_datafication msteammon = {
  species = msteammon.mspecies;
  curr_hp = msteammon.mcurr_hp;
  max_hp = msteammon.mmax_hp;
  first_type = msteammon.mfirst_type;
  second_type = msteammon.msecond_type;
  first_attack = attack_datafication msteammon.mfirst_attack;
  second_attack = attack_datafication msteammon.msecond_attack;
  third_attack = attack_datafication msteammon.mthird_attack;
  fourth_attack = attack_datafication msteammon.mfourth_attack;
  attack = msteammon.mattack;
  spl_attack = msteammon.mspl_attack;
  defense = msteammon.mdefense;
  spl_defense = msteammon.mspl_defense;
  speed = msteammon.mspeed;
  status = msteammon.mstatus;
  mods = modifier_datafication msteammon.mmods
}
