open Project_t.Types
open Notty

(* colors *)
let c_black = A.rgb_888 ~r:0 ~g:0 ~b:0
let c_white = A.rgb_888 ~r:255 ~g:255 ~b:255
let c_gray = A.rgb_888 ~r:80 ~g:80 ~b:80
let c_cyan = A.rgb_888 ~r:85 ~g:255 ~b:255
let c_red = A.rgb_888 ~r:255 ~g:0 ~b:0

let wall_attr = A.(fg c_white ++ bg c_black)
let floor_attr = A.(fg c_gray ++ bg c_black)
let spike_attr = A.(fg (rgb_888 ~r:255 ~g:0 ~b:0) ++ bg c_black)
let lava_attr = A.(fg (rgb_888 ~r:255 ~g:0 ~b:0) ++ bg (rgb_888 ~r:139 ~g:0 ~b:0))
let player_attr = A.(fg (rgb_888 ~r:0 ~g:255 ~b:255) ++ bg c_black)
let goblin_attr = A.(fg (rgb_888 ~r:0 ~g:255 ~b:0) ++ bg c_black)
let archer_attr = A.(fg (rgb_888 ~r:255 ~g:255 ~b:0) ++ bg c_black)
let brute_attr = A.(fg (rgb_888 ~r:255 ~g:0 ~b:255) ++ bg c_black)
let border_attr = A.(fg c_cyan ++ bg c_black)

(* message colors *)
let msg_good_attr = A.(fg (rgb_888 ~r:0 ~g:255 ~b:0) ++ bg c_black)
let msg_bad_attr = A.(fg (rgb_888 ~r:255 ~g:0 ~b:0) ++ bg c_black)
let msg_neutral_attr = A.(fg c_white ++ bg c_black)

(* tiles *)
let tile_glyph = function
  | Empty -> "·", floor_attr
  | Wall -> "#", wall_attr
  | Spike -> "x", spike_attr
  | Lava -> "~", lava_attr

let enemy_glyph = function
  | Goblin -> "Ħ", goblin_attr
  | Archer -> "$", archer_attr
  | Brute -> "ß", brute_attr

let player_glyph = "0", player_attr

let projectile_attr_for_owner = function
  | Goblin -> goblin_attr
  | Archer -> archer_attr
  | Brute -> brute_attr

let projectile_glyph dir owner_type effec =
    let attr = match effec with
        | Normal -> projectile_attr_for_owner owner_type
        | Fire -> A.(fg c_red)
    in
    match dir with
    | North -> "↑", attr
    | South -> "↓", attr
    | East -> "→", attr
    | West -> "←", attr


let make_entity_map (state : game_state) =
  let map = Hashtbl.create 16 in

  Hashtbl.add map (state.player.pos.x, state.player.pos.y) `Player;

  List.iter (fun (e : enemy) ->
    Hashtbl.add map (e.pos.x, e.pos.y) (`Enemy e.enemy_type)
  ) state.enemies;

  List.iter (fun (p : projectile) ->
    Hashtbl.add map (p.pos.x, p.pos.y) (`Projectile (p.direction, p.owner_type, p.effec))
  ) state.projectiles;

  map

(* render cell *)
let render_cell grid entity_map x y =
  let pos = { x; y } in
  let tile = Project_t.Grid.get_tile grid pos in
  match Hashtbl.find_opt entity_map (x, y) with
  | Some `Player ->
      let glyph, attr = player_glyph in
      I.string attr glyph
  | Some (`Enemy etype) ->
      let glyph, attr = enemy_glyph etype in
      I.string attr glyph
  | Some (`Projectile (dir, owner_type, effec)) ->
      let glyph, attr = projectile_glyph dir owner_type effec in
      I.string attr glyph
  | None ->
      let glyph, attr = tile_glyph tile in
      I.string attr glyph

(* render board *)
let render_board (state : game_state) : image =
  let grid = state.grid in
  let entity_map = make_entity_map state in
  let rows = List.init grid.height (fun y ->
    let cells = List.init grid.width (fun x ->
      render_cell grid entity_map x y
    ) in
    I.hcat cells
  ) in
  I.vcat rows

(* render terminal border and black bg *)
let bg_attr = A.(bg c_black)

let msg_attr = function
  | Good -> msg_good_attr
  | Bad -> msg_bad_attr
  | Neutral -> msg_neutral_attr

let render_messages messages inner_w =
  let msg_images = List.map (fun (m : message) ->
    let attr = msg_attr m.msg_type in
    I.string attr m.text
  ) messages in
  let content = if List.length msg_images > 0 then
    I.vcat msg_images
  else
    I.empty
  in
  let bg = I.char bg_attr ' ' inner_w (max 1 (List.length messages)) in
  I.(content </> bg)

let heart_attr = A.(fg c_white ++ bg c_black)
let hud_attr = A.(fg c_white ++ bg c_black)

let hud_width = 6

let render_hud_vertical (state : game_state) board_h =
  let hp_line = I.hcat [
    I.string heart_attr "HP";
    I.string hud_attr (Printf.sprintf " %d" state.player.hp)
  ] in
  let floor_line = I.hcat [
    I.string hud_attr "FL";
    I.string hud_attr (Printf.sprintf " %d" state.floor)
  ] in
  let empty_line = I.char bg_attr ' ' hud_width 1 in
  let separator = I.string border_attr "│" in
  let rows = List.init board_h (fun i ->
    let content = match i with
      | 0 -> hp_line
      | 1 -> floor_line
      | _ -> empty_line
    in
    let padded = I.(content <|> char bg_attr ' ' (hud_width - I.width content) 1) in
    I.(padded <|> separator)
  ) in
  I.vcat rows

let win_title_attr = A.(fg (rgb_888 ~r:255 ~g:215 ~b:0) ++ bg c_black)
let win_text_attr = A.(fg c_white ++ bg c_black)

let render_win_screen ~term_w ~term_h : image =
  let _ = term_w in
  let inner_w = 30 in
  let inner_h = min (term_h - 2) 10 in

  let bg = I.char bg_attr ' ' inner_w inner_h in

  let title = I.string win_title_attr "### YOU WIN! ###" in
  let msg1 = I.string win_text_attr "Congratulations!" in
  let empty = I.char bg_attr ' ' inner_w 1 in
  let prompt = I.string win_text_attr "[R] Restart  [Q] Quit" in

  let center_pad img =
    let left_pad = (inner_w - I.width img) / 2 in
    I.(char bg_attr ' ' left_pad 1 <|> img)
  in

  let content = I.vcat [
    empty;
    center_pad title;
    empty;
    center_pad msg1;
    empty;
    empty;
    center_pad prompt;
  ] in

  let padded_content = I.(content </> bg) in

  let top_border = I.hcat [
    I.string border_attr "╔";
    I.hcat (List.init inner_w (fun _ -> I.string border_attr "═"));
    I.string border_attr "╗"
  ] in
  let bottom_border = I.hcat [
    I.string border_attr "╚";
    I.hcat (List.init inner_w (fun _ -> I.string border_attr "═"));
    I.string border_attr "╝"
  ] in
  let middle_rows = List.init inner_h (fun y ->
    let row_content = I.crop ~t:y ~b:(inner_h - y - 1) padded_content in
    I.hcat [I.string border_attr "║"; row_content; I.string border_attr "║"]
  ) in
  I.vcat ([top_border] @ middle_rows @ [bottom_border])

let render_screen ~term_w:_ ~term_h (state : game_state) : image =
  let board = render_board state in
  let board_h = state.grid.height in

  (* hud *)
  let hud = render_hud_vertical state board_h in
  let board_with_hud = I.(hud <|> board) in

  let inner_w = hud_width + 1 + state.grid.width in
  let msg_box_h = 5 in
  let inner_h = min (term_h - 2) (board_h + 1 + msg_box_h) in

  let bg = I.char bg_attr ' ' inner_w inner_h in

  (* message box *)
  let separator = I.hcat (List.init inner_w (fun _ -> I.string border_attr "─")) in
  let msg_content = render_messages state.messages inner_w in

  let content = I.(board_with_hud <-> separator <-> msg_content </> bg) in

  let top_border = I.hcat [
    I.string border_attr "╔";
    I.hcat (List.init inner_w (fun _ -> I.string border_attr "═"));
    I.string border_attr "╗"
  ] in
  let bottom_border = I.hcat [
    I.string border_attr "╚";
    I.hcat (List.init inner_w (fun _ -> I.string border_attr "═"));
    I.string border_attr "╝"
  ] in
  let middle_rows = List.init inner_h (fun y ->
    let row_content = I.crop ~t:y ~b:(inner_h - y - 1) content in
    I.hcat [I.string border_attr "║"; row_content; I.string border_attr "║"]
  ) in
  I.vcat ([top_border] @ middle_rows @ [bottom_border])
