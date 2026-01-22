open Types

let direction_to_target enemy_pos player_pos =
    let dx = player_pos.x - enemy_pos.x in
    let dy = player_pos.y - enemy_pos.y in

    if dx = 0 && dy = 0 then
        None
    else if dx > 0 && dy = 0 then
        Some East
    else if dx < 0 && dy = 0 then
        Some West
    else if dy > 0 then
        Some South
    else
        Some North

let is_adjacent pos1 pos2 =
    let dx = abs (pos1.x - pos2.x) in
    let dy = abs (pos1.y - pos2.y) in
    (dx = 1 && dy = 0) || (dx = 0 && dy = 1)

let hazard_damage grid pos =
    match Grid.get_tile grid pos with
    | Spike -> 1
    | Lava -> 2
    | _ -> 0

let goblin_move grid enemy_pos player_pos =
    match direction_to_target enemy_pos player_pos with
    | None -> (enemy_pos, 0)
    | Some dir ->
        let new_pos = Logic.apply_direction enemy_pos dir in
        if new_pos = player_pos then
            (enemy_pos, 0)
        else if Logic.is_walkable grid new_pos then
            (new_pos, hazard_damage grid new_pos)
        else
            (enemy_pos, 0)

let archer_move enemy_pos =
    (enemy_pos, 0)

let brute_move grid enemy_pos player_pos turn =
    if turn mod 2 = 0 then
        match direction_to_target enemy_pos player_pos with
        | None -> (enemy_pos, 0)
        | Some dir ->
            let new_pos = Logic.apply_direction enemy_pos dir in
            if new_pos = player_pos then
                (enemy_pos, 0)
            else if Logic.is_walkable grid new_pos then
                (new_pos, hazard_damage grid new_pos)
            else
                (enemy_pos, 0)
    else
        (enemy_pos, 0)

let next_move enemy player_pos grid turn =
    match enemy.enemy_type with
    | Goblin -> goblin_move grid enemy.pos player_pos
    | Archer -> archer_move enemy.pos
    | Brute -> brute_move grid enemy.pos player_pos turn

type enemy_update = {
    enemy: enemy;
    hazard_dmg: int;
    died: bool;
}

let update_enemies_with_info enemies player_pos grid turn =
    List.map (fun (e : enemy) ->
        let (new_pos, dmg) = next_move e player_pos grid turn in
        let new_hp = e.hp - dmg in
        if new_hp <= 0 then
            { enemy = e; hazard_dmg = dmg; died = true }
        else
            { enemy = { e with pos = new_pos; hp = new_hp }; hazard_dmg = dmg; died = false }
    ) enemies

let update_enemies enemies player_pos grid turn =
    List.filter_map (fun (e : enemy) ->
        let (new_pos, dmg) = next_move e player_pos grid turn in
        let new_hp = e.hp - dmg in
        if new_hp <= 0 then
            None
        else
            Some { e with pos = new_pos; hp = new_hp }
    ) enemies

let move_projectiles projectiles grid player_pos enemies =
    List.filter_map (fun (p : projectile) ->
        let new_pos = Logic.apply_direction p.pos p.direction in
        if not (Grid.in_bounds grid new_pos) then
            None
        else if new_pos = player_pos then
            None
        else if List.exists (fun (e : enemy) -> e.pos = new_pos) enemies then
            None
        else match Grid.get_tile grid new_pos with
            | Wall -> None
            | _ -> Some { p with pos = new_pos }
    ) projectiles

let projectile_hits projectiles grid player_pos enemies =
    let player_damage = ref 0 in
    let enemy_damage = ref [] in
    List.iter (fun (p : projectile) ->
        let new_pos = Logic.apply_direction p.pos p.direction in
        if Grid.in_bounds grid new_pos then begin
            if new_pos = player_pos then
                player_damage := !player_damage + 1
            else
                List.iter (fun (e : enemy) ->
                    if e.pos = new_pos then
                        enemy_damage := e.id :: !enemy_damage
                ) enemies
        end
    ) projectiles;
    (!player_damage, !enemy_damage)

let archer_has_active_projectile projectiles archer_id =
    List.exists (fun (p : projectile) -> p.owner_id = archer_id) projectiles

let line_of_sight archer_pos player_pos =
    if archer_pos.x = player_pos.x then
        if player_pos.y > archer_pos.y then Some South
        else if player_pos.y < archer_pos.y then Some North
        else None
    else if archer_pos.y = player_pos.y then
        if player_pos.x > archer_pos.x then Some East
        else if player_pos.x < archer_pos.x then Some West
        else None
    else
        None

let archers_shoot enemies projectiles player_pos grid =
    List.fold_left (fun acc (e : enemy) ->
        match e.enemy_type with
        | Archer ->
            if archer_has_active_projectile acc e.id then
                acc
            else
                (match line_of_sight e.pos player_pos with
                | Some dir ->
                    let spawn_pos = Logic.apply_direction e.pos dir in
                    if Grid.in_bounds grid spawn_pos &&
                       Grid.get_tile grid spawn_pos <> Wall then
                        { pos = spawn_pos; direction = dir; owner_id = e.id; owner_type = e.enemy_type } :: acc
                    else
                        acc
                | None -> acc)
        | _ -> acc
    ) projectiles enemies

let melee_damage_for_enemy = function
    | Goblin -> 1
    | Archer -> 0
    | Brute -> 2

let melee_attacks enemies player_pos =
    List.filter_map (fun (e : enemy) ->
        if is_adjacent e.pos player_pos then
            let dmg = melee_damage_for_enemy e.enemy_type in
            if dmg > 0 then Some (e.enemy_type, dmg)
            else None
        else
            None
    ) enemies
