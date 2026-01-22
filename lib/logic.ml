open Types

let apply_direction pos = function
    | North -> { pos with y = pos.y - 1 }
    | South -> { pos with y = pos.y + 1 }
    | East -> { pos with x = pos.x + 1 }
    | West -> { pos with x = pos.x - 1 }

let is_walkable grid pos =
    Grid.in_bounds grid pos &&
    match Grid.get_tile grid pos with
    | Wall -> false
    | _ -> true

let enemy_at_pos (enemies : enemy list) pos =
    List.find_opt (fun (e : enemy) -> e.pos = pos) enemies

let move_player state dir =
    let new_pos = apply_direction state.player.pos dir in
    if not (is_walkable state.grid new_pos) then
        state
    else if enemy_at_pos state.enemies new_pos <> None then
        state
    else
        let updated_player = { state.player with pos = new_pos } in
        let final_player = match Grid.get_tile state.grid new_pos with
            | Spike -> { updated_player with hp = updated_player.hp - 1}
            | Lava -> { updated_player with hp = updated_player.hp - 2 }
            | _ -> updated_player
        in
        { state with
          player = final_player;
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
