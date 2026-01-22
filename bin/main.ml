open Project_t.Types
open Notty_unix

let enemy_name = function
  | Goblin -> "Goblin"
  | Archer -> "Archer"
  | Brute -> "Brute"

let random_pos grid =
  let rec find () =
    let x = 1 + Random.int (grid.width - 2) in
    let y = 1 + Random.int (grid.height - 2) in
    let pos = { x; y } in
    match Project_t.Grid.get_tile grid pos with
    | Wall | Spike | Lava -> find ()
    | _ -> pos
  in find ()

let spawn_enemies grid floor =
  let num_enemies = 2 + floor in
  let next_id = ref 1 in
  List.init num_enemies (fun _ ->
    let id = !next_id in
    next_id := !next_id + 1;
    let enemy_type = match Random.int 3 with
      | 0 -> Goblin
      | 1 -> Archer
      | _ -> Brute
    in
    let hp = match enemy_type with
      | Goblin -> 2
      | Archer -> 1
      | Brute -> 3
    in
    { id; pos = random_pos grid; enemy_type; hp }
  )

let create_floor floor player =
  let grid = Project_t.Grid.generate 20 12 floor in
  let enemies = spawn_enemies grid floor in
  {
    player = { player with pos = { x = 5; y = 5 } };
    enemies;
    projectiles = [];
    grid;
    floor;
    turn = 0;
    messages = [{ text = Printf.sprintf "Welcome to floor %d!" floor; msg_type = Good }];
  }

let () =
  if Array.length Sys.argv > 1 && (Sys.argv.(1) = "--help" || Sys.argv.(1) = "-h") then begin
    print_endline "Simple ascii dungeon crawler, clear 3 floors to win";
    print_endline "";
    print_endline "Controls:";
    print_endline "  Arrow keys  Move/Attack";
    print_endline "  q/Esc       Quit";
    exit 0
  end;
  Random.self_init ();
  let term = Term.create () in

  let rec win_screen () =
    let (w, h) = Term.size term in
    let img = Game_render.render_win_screen ~term_w:w ~term_h:h in
    Term.image term img;

    match Term.event term with
    | `Key (`ASCII 'q', _) | `Key (`Escape, _) -> ()
    | `Key (`ASCII 'r', _) | `Key (`ASCII 'R', _) ->
        let new_game = create_floor 1 { pos = { x = 5; y = 5 }; hp = 10; max_hp = 10 } in
        game_loop new_game
    | _ -> win_screen ()

  and game_loop state =
    let (w, h) = Term.size term in
    let img = Game_render.render_screen ~term_w:w ~term_h:h state in
    Term.image term img;

    match Term.event term with
    | `Key (`ASCII 'q', _) | `Key (`Escape, _) -> ()
    | `Key (`Arrow dir, _) ->
        let direction = match dir with
          | `Up -> North
          | `Down -> South
          | `Left -> West
          | `Right -> East
        in
        let msgs = ref [] in

        let target_pos = Project_t.Logic.apply_direction state.player.pos direction in
        let attacked_enemy = Project_t.Logic.enemy_at_pos state.enemies target_pos in
        (match attacked_enemy with
        | Some e ->
            let new_hp = e.hp - 1 in
            if new_hp <= 0 then
              msgs := { text = Printf.sprintf "You kill the %s!" (enemy_name e.enemy_type); msg_type = Good } :: !msgs
            else
              msgs := { text = Printf.sprintf "You hit the %s for 1 damage." (enemy_name e.enemy_type); msg_type = Neutral } :: !msgs
        | None -> ());

        let new_state = Project_t.Logic.process_player_action state (Move direction) in

        let env_dmg = state.player.hp - new_state.player.hp in
        if env_dmg > 0 then
          msgs := { text = Printf.sprintf "You take %d damage from hazard!" env_dmg; msg_type = Bad } :: !msgs;

        let enemy_updates = Project_t.Enemy_ai.update_enemies_with_info
          new_state.enemies new_state.player.pos new_state.grid new_state.turn in

        List.iter (fun (upd : Project_t.Enemy_ai.enemy_update) ->
            if upd.hazard_dmg > 0 then begin
              if upd.died then
                msgs := { text = Printf.sprintf "%s dies from hazard!" (enemy_name upd.enemy.enemy_type); msg_type = Good } :: !msgs
              else
                msgs := { text = Printf.sprintf "%s takes %d hazard damage!" (enemy_name upd.enemy.enemy_type) upd.hazard_dmg; msg_type = Good } :: !msgs
            end
        ) enemy_updates;

        let new_enemies = List.filter_map (fun (upd : Project_t.Enemy_ai.enemy_update) ->
            if upd.died then None else Some upd.enemy
        ) enemy_updates in

        let (player_dmg, enemy_hits) = Project_t.Enemy_ai.projectile_hits
          new_state.projectiles new_state.grid new_state.player.pos new_enemies in

        if player_dmg > 0 then
          msgs := { text = Printf.sprintf "Arrow hits you for %d damage!" player_dmg; msg_type = Bad } :: !msgs;

        let enemies_after_hits = List.filter_map (fun (e : enemy) ->
            if List.mem e.id enemy_hits then
                let new_hp = e.hp - 1 in
                if new_hp <= 0 then begin
                  msgs := { text = Printf.sprintf "%s is killed by arrow!" (enemy_name e.enemy_type); msg_type = Good } :: !msgs;
                  None
                end else begin
                  msgs := { text = Printf.sprintf "%s takes 1 damage from arrow." (enemy_name e.enemy_type); msg_type = Neutral } :: !msgs;
                  Some { e with hp = new_hp }
                end
            else
                Some e
        ) new_enemies in

        let moved_projectiles = Project_t.Enemy_ai.move_projectiles
          new_state.projectiles new_state.grid new_state.player.pos enemies_after_hits in

        let old_projectile_count = List.length moved_projectiles in
        let new_projectiles = Project_t.Enemy_ai.archers_shoot
          enemies_after_hits moved_projectiles new_state.player.pos new_state.grid in
        let new_arrows = List.length new_projectiles - old_projectile_count in
        if new_arrows > 0 then
          msgs := { text = "Archer shoots an arrow!"; msg_type = Neutral } :: !msgs;

        let melee_hits = Project_t.Enemy_ai.melee_attacks enemies_after_hits new_state.player.pos in
        let melee_dmg = List.fold_left (fun acc (etype, dmg) ->
          msgs := { text = Printf.sprintf "%s hits you for %d damage!" (enemy_name etype) dmg; msg_type = Bad } :: !msgs;
          acc + dmg
        ) 0 melee_hits in

        let new_player_hp = new_state.player.hp - player_dmg - melee_dmg in
        let final_player = { new_state.player with hp = new_player_hp } in

        let final_state = { new_state with
          player = final_player;
          enemies = enemies_after_hits;
          projectiles = new_projectiles;
          messages = List.rev !msgs;
        } in
        if final_state.player.hp <= 0 then
          ()
        else if List.length final_state.enemies = 0 then
          if final_state.floor >= 3 then
            win_screen ()
          else
            let next_floor = create_floor (final_state.floor + 1) final_state.player in
            game_loop next_floor
        else
          game_loop final_state
    | _ -> game_loop state
  in
  let init_state = create_floor 1 { pos = { x = 5; y = 5 }; hp = 10; max_hp = 10 } in
  game_loop init_state;
  Term.release term
