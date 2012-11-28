open Definitions

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

val attack_mutafication: attack -> mattack
val modifier_mutafication: modifier -> mmodifier
val steammon_mutafication: steammon -> msteammon

val attack_datafication: mattack -> attack
val modifier_datafication: mmodifier -> modifier
val steammon_datafication: msteammon -> steammon
