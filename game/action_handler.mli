open Definitions
open State

val find_steammon: steammon list -> string -> steammon
val find_msteammon: msteammon list -> string -> msteammon

val switch_steammons: msteammon list ref -> string -> unit
val add_steammon: msteammon list ref -> steammon list ref -> string -> color -> unit
val pick_inventory: minventory -> inventory -> unit
val use_item: msteammon list ref -> minventory -> string -> item -> color -> unit
val use_attack: msteammon list ref -> msteammon list ref -> string -> color -> unit
