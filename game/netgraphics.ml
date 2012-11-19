open Definitions

let clients = ref []
let updates = ref []
let clients_lock = Mutex.create()
let updates_lock = Mutex.create()

let parse_updates updates =
  let concat l = String.concat "#" l in
  let concat_2 l = String.concat "," l in
  let string_of_team t = if t = Red then "red" else "blue" in
  let string_of_status s = match s with
    | Paralyzed -> "paralyzed"
    | Poisoned -> "poisoned"
    | Asleep -> "asleep"
    | Confused -> "confused"
    | Frozen -> "frozen" in
  let filter s =
    let new_s = ref [] in
      String.iter (fun c -> if c <> '#' && c <> '$' then new_s := String.make 1 c::(!new_s) else ()) s;
      String.concat "" (List.rev !new_s) in
  let parse u =
    match u with
      InitGraphics -> concat ["InitGraphics"]
    | UpdateSteammon (s,hp,max,t) -> concat ["UpdateSteammon";
                                             s;
                                             string_of_int hp;
                                             string_of_int max;
                                             string_of_team t]
    | SetChosenSteammon s -> concat ["SetChosenSteammon"; s]
    | NegativeEffect (s, c, i) -> concat ["NegativeEffect";
                                          s;
                                          string_of_team c;
                                          string_of_int i]
    | PositiveEffect (s, c, i) -> concat ["PositiveEffect";
                                          s;
                                          string_of_team c;
                                          string_of_int i]
    | SetFirstAttacker t -> concat ["SetFirstAttacker"; string_of_team t]
    | SetStatusEffects (s, l) -> concat ["SetStatusEffects";
                                         s;
                                         concat_2 (List.map string_of_status l)]
    | Message s -> concat ["Message"; filter s]
  in
  let s = String.concat "" (List.map (fun u -> "$" ^ (parse u) ^ "$") updates) in
    (*print_endline s;*)
    s

let add_clients server =
  while true do
    let (c, a) = Unix.accept server in
      Mutex.lock clients_lock;
      print_endline "A client connected to gui server";
      clients := (Connection.server a c)::!clients;
      Mutex.unlock clients_lock;
  done

let init_server port =
  let server = Unix.socket Unix.PF_INET Unix.SOCK_STREAM 0 in
    Unix.setsockopt server Unix.SO_REUSEADDR true;
    Unix.setsockopt server Unix.SO_KEEPALIVE false;
    Unix.bind server (Unix.ADDR_INET (Unix.inet_addr_any, port));
    Unix.listen server 100;
    server

let init_single_connection port =
  let server = init_server port in
  let (c, a) = Unix.accept server in
    Mutex.lock clients_lock;
    print_endline "A client connected to gui server";
    clients := (Connection.server a c)::!clients;
    Mutex.unlock clients_lock;
    ignore(Thread.create add_clients server)

let init port = ignore(Thread.create add_clients (init_server port))

let add_update u =
  Mutex.lock updates_lock;
  updates := u::(!updates);
  Mutex.unlock updates_lock

let send u =
  Mutex.lock clients_lock;
  let parsed_updates = parse_updates u in
    (try
       clients := List.fold_left (fun new_clients c ->
                                    if Connection.output_string c parsed_updates then
                                      let _ = () in c::new_clients
                                    else
                                      (Connection.close c; new_clients)) [] !clients;
       ()
     with _ ->
       ());
    Mutex.unlock clients_lock

let send_update u = send [u]

let send_updates () =
  Mutex.lock updates_lock;
  let u = List.rev !updates in
    updates := [];
    Mutex.unlock updates_lock;
    send u
