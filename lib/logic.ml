open Types

let apply_direction = Grid.apply_direction
let is_walkable = Grid.is_walkable

let enemy_at_pos (enemies : enemy list) pos =
    List.find_opt (fun (e : enemy) -> e.pos = pos) enemies

let barrel_action state pos dir =
    Grid.set_tile state.grid pos Empty;
    { pos = pos; direction = dir; typ = Barrel; effec = Normal; owner_id = 99; owner_type = Goblin } :: state.projectiles

let move_player state dir =
    let new_pos = apply_direction state.player.pos dir in
    if not (is_walkable state.grid new_pos) then
        state
    else if enemy_at_pos state.enemies new_pos <> None then
        state
    else
        let updated_player = { state.player with pos = new_pos } in
        let (final_player, final_projectiles) = match Grid.get_tile state.grid new_pos with
            | Spike -> ({ updated_player with hp = updated_player.hp - 1}, state.projectiles)
            | Lava -> ({ updated_player with hp = updated_player.hp - 2 }, state.projectiles)
            | Barrel -> (updated_player, barrel_action state new_pos dir)
            | _ -> (updated_player, state.projectiles)
        in
        { state with
          player = final_player;
          projectiles = final_projectiles;
          turn = state.turn + 1 }

let process_player_action state action =
    (match action with
    | Move dir ->
        let target_pos = apply_direction state.player.pos dir in
        (match enemy_at_pos state.enemies target_pos with
        | Some enemy ->
            let new_hp = enemy.hp - 1 in
            let updated_enemies =
                if new_hp <= 0 then
                    List.filter (fun (e : enemy) -> e.pos <> target_pos) state.enemies
                else
                    List.map (fun (e : enemy) ->
                        if e.pos = target_pos then
                            { e with hp = new_hp }
                        else
                            e
                    ) state.enemies
            in
            { state with
              enemies = updated_enemies;
              turn = state.turn + 1
            }
        | None ->
            move_player state dir)
    | Wait ->
        { state with turn = state.turn + 1 })

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
        { id; pos = Grid.random_pos grid; enemy_type; hp }
    )

let create_floor floor player =
    let grid = Grid.generate 20 12 floor in
    let enemies = spawn_enemies grid floor in
    {
        player = { player with pos = { x = 5; y = 5 } };
        enemies;
        projectiles = [];
        aoes = [];
        grid;
        floor;
        turn = 0;
        messages = [{ text = Printf.sprintf "Welcome to floor %d!" floor; msg_type = Good }];
    }

let process_turn state action =
    let msgs = ref [] in
    let enemy_name = Types.enemy_name in

    let target_pos = match action with
        | Move dir -> apply_direction state.player.pos dir
        | Wait -> state.player.pos
    in

    let attacked_enemy = enemy_at_pos state.enemies target_pos in
    (match attacked_enemy with
    | Some e ->
        let new_hp = e.hp - 1 in
        if new_hp <= 0 then
            msgs := { text = Printf.sprintf "You kill the %s!" (enemy_name e.enemy_type); msg_type = Good } :: !msgs
        else
            msgs := { text = Printf.sprintf "You hit the %s for 1 damage." (enemy_name e.enemy_type); msg_type = Neutral } :: !msgs
    | None -> ());

    let new_state = process_player_action state action in

    (match action with
    | Move _ ->
        let target_tile = Grid.get_tile state.grid target_pos in
        (match target_tile with
        | Barrel -> msgs := { text = "You kick the barrel!"; msg_type = Neutral } :: !msgs
        | _ -> ())
    | Wait -> ());

    let env_dmg = state.player.hp - new_state.player.hp in
    if env_dmg > 0 then
        msgs := { text = Printf.sprintf "You take %d damage from hazard!" env_dmg; msg_type = Bad } :: !msgs;

    let enemy_updates = Enemy_ai.update_enemies_with_info
        new_state.enemies new_state.player.pos new_state.grid new_state.turn in

    List.iter (fun (upd : Enemy_ai.enemy_update) ->
        if upd.hazard_dmg > 0 then begin
            if upd.died then
                msgs := { text = Printf.sprintf "%s dies from hazard!" (enemy_name upd.enemy.enemy_type); msg_type = Good } :: !msgs
            else
                msgs := { text = Printf.sprintf "%s takes %d hazard damage!" (enemy_name upd.enemy.enemy_type) upd.hazard_dmg; msg_type = Good } :: !msgs
        end
    ) enemy_updates;

    let new_enemies = List.filter_map (fun (upd : Enemy_ai.enemy_update) ->
        if upd.died then None else Some upd.enemy
    ) enemy_updates in

    let (player_dmg, enemy_hits) = Enemy_ai.projectile_hits
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

    let (moved_projectiles, new_aoes) = Enemy_ai.move_projectiles
        new_state.projectiles [] new_state.grid new_state.player.pos enemies_after_hits in

    let in_aoe_range (a : aoe) pos =
        abs (a.pos.x - pos.x) <= 1 && abs (a.pos.y - pos.y) <= 1
    in
    let aoe_dmg = 3 in

    let aoe_player_dmg = if List.exists (fun a -> in_aoe_range a new_state.player.pos) new_aoes
        then (msgs := { text = "Barrel explosion hits you for 3 damage!"; msg_type = Bad } :: !msgs; aoe_dmg)
        else 0
    in

    let enemies_after_aoe = List.filter_map (fun (e : enemy) ->
        let hit_by_aoe = List.exists (fun a -> in_aoe_range a e.pos) new_aoes in
        if hit_by_aoe then
            let new_hp = e.hp - aoe_dmg in
            if new_hp <= 0 then begin
                msgs := { text = Printf.sprintf "%s is killed by explosion!" (enemy_name e.enemy_type); msg_type = Good } :: !msgs;
                None
            end else begin
                msgs := { text = Printf.sprintf "%s takes %d damage from explosion!" (enemy_name e.enemy_type) aoe_dmg; msg_type = Good } :: !msgs;
                Some { e with hp = new_hp }
            end
        else
            Some e
    ) enemies_after_hits in

    let old_projectile_count = List.length moved_projectiles in
    let new_projectiles = Enemy_ai.archers_shoot
        enemies_after_aoe moved_projectiles new_state.player.pos new_state.grid in
    let new_arrows = List.length new_projectiles - old_projectile_count in
    if new_arrows > 0 then
        msgs := { text = "Archer shoots an arrow!"; msg_type = Neutral } :: !msgs;

    let melee_hits = Enemy_ai.melee_attacks enemies_after_aoe new_state.player.pos in
    let melee_dmg = List.fold_left (fun acc (etype, dmg) ->
        msgs := { text = Printf.sprintf "%s hits you for %d damage!" (enemy_name etype) dmg; msg_type = Bad } :: !msgs;
        acc + dmg
    ) 0 melee_hits in

    let new_player_hp = new_state.player.hp - player_dmg - melee_dmg - aoe_player_dmg in
    let final_player = { new_state.player with hp = new_player_hp } in

    let remaining_aoes = List.filter_map (fun (a : aoe) ->
        let new_turns = a.turns_left - 1 in
        if new_turns < 0 then None
        else Some { a with turns_left = new_turns }
    ) new_aoes in

    let final_state = { new_state with
        player = final_player;
        enemies = enemies_after_aoe;
        projectiles = new_projectiles;
        aoes = remaining_aoes;
    } in
    (final_state, List.rev !msgs)
