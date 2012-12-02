let sort_and_rank cmp lst = 
  let smallest l = 
    let helper acc x =
      match acc with
      | None -> Some x 
      | Some m -> if cmp x m < 0. then Some x else acc in
    let smallest_option = List.fold_left helper None l in
    match smallest_option with None -> (List.hd l) | Some m -> m in
  let remove_smallest l = List.filter (fun x -> x != smallest l) l in
  let helper (acc,l) x = 
    let min = smallest l in
    match acc with
    | [] -> ([(min, 1)],(remove_smallest l))
    | (h,r)::t -> if cmp min h = 0. then ((min,r)::acc,(remove_smallest l))
                  else ((min,(List.length acc)+1)::acc,(remove_smallest l)) in
  let (new_lst,_) = List.fold_left helper ([],lst) lst in
  List.rev new_lst

let lst1 = [1.;1.;1.;1.;1.;1.] in
          let so = sort_by_total_proportion mons_list enemies lst1 in
          let (_,s) = List.hd so in
          let (_,s1) = List.hd (List.tl so) in
          let mon = List.fold_left (fun acc x -> 
                      if x.species = s then x else acc) 
                        (List.hd mons_list) mons_list in
          let high_net_damage = highest (find_net_damage_avgs mons_list enemies)
          and high_spevs = highest (speeds_vsenemies mons_list enemies) 
          and high_pure_damage = highest (pure_damages_list mons_list)
          and high_res = highest (avg_resists_list mons_list)
          and high_speed = highest (speeds_list mons_list) 
          and high_def = highest (def_stats_list mons_list) in 
          let hs = 
    [high_def;high_pure_damage;high_speed;high_res;high_spevs;high_net_damage] in
          let props = [(float_of_int (sum_def_stats mon)) /. (List.hd hs);
               (find_pure_damage_avg mon) /. (List.nth hs 1);
               (float_of_int mon.speed) /. (List.nth hs 2);
               (find_avg_resist mon) /. (List.nth hs 3);
               (find_speed_vsenemies mon enemies) /. (List.nth hs 4);
               (find_net_damage_avg mon enemies) /. (List.nth hs 5)] in
          print_endline s1;
          print_endline ("picking "^s);
          print_endline ((string_of_int (sum_def_stats mon))^" "^
                         (string_of_float (find_pure_damage_avg mon))^" "^
                         (string_of_int mon.speed)^" "^
                         (string_of_float (find_avg_resist mon))^" "^
                         (string_of_float (find_speed_vsenemies mon enemies))^" "^
                         (string_of_float(find_net_damage_avg mon enemies)));
          print_endline ((string_of_float (highest (def_stats_list mons_list))^" "^
                         (string_of_float (highest (pure_damages_list mons_list)))^" "^
                         (string_of_float (highest (speeds_list mons_list)))^" "^
                         (string_of_float (highest (avg_resists_list mons_list)))^" "^
                         (string_of_float (highest (speeds_vsenemies mons_list enemies)))^" "^ 
                         (string_of_float (highest (find_net_damage_avgs mons_list enemies)))));
          List.iter (fun x -> print_endline ((string_of_float x)^" ")) props;

