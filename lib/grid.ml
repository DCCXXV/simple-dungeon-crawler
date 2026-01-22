open Types

let make_empty width height =
{
    width;
    height;
    tiles = Array.make_matrix height width Empty;
}

let in_bounds grid pos =
    pos.x >= 0 && pos.x < grid.width && pos.y >= 0 && pos.y < grid.height

let get_tile grid pos =
    if in_bounds grid pos then
        grid.tiles.(pos.y).(pos.x)
    else
        Wall

let set_tile grid pos tile =
    if in_bounds grid pos then
        grid.tiles.(pos.y).(pos.x) <- tile

let add_borders grid =
    for x = 0 to grid.width - 1 do
        set_tile grid {x; y = 0} Wall;
        set_tile grid {x; y = grid.height - 1} Wall;
    done;
    for y = 0 to grid.height - 1 do
        set_tile grid {x = 0; y} Wall;
        set_tile grid {x = grid.width - 1; y} Wall;
    done

let generate width height floor_num =
    let grid = make_empty width height in
    add_borders grid;
    Random.self_init ();
    for _ = 1 to (floor_num * 2) do
        let x = 1 + Random.int(width - 2) in
        let y = 1 + Random.int(height - 2) in
        set_tile grid {x; y} Wall;
    done;
    for _ = 1 to (floor_num * 2) do
        let x = 1 + Random.int(width - 2) in
        let y = 1 + Random.int(height - 2) in
        if get_tile grid {x; y} = Empty then
          set_tile grid {x; y} Spike;
    done;
    for _ = 1 to (floor_num * 2) do
        let x = 1 + Random.int(width - 2) in
        let y = 1 + Random.int(height - 2) in
        if get_tile grid {x; y} = Empty then
          set_tile grid {x; y} Lava;
    done;
    grid
