open Project_t.Types
open Notty_unix

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
            let new_game = Project_t.Logic.create_floor 1 { pos = { x = 5; y = 5 }; hp = 10; max_hp = 10 } in
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
            let (new_state, msgs) = Project_t.Logic.process_turn state (Move direction) in
            let final_state = { new_state with messages = msgs } in
            if final_state.player.hp <= 0 then
            ()
            else if List.length final_state.enemies = 0 then
            if final_state.floor >= 3 then
                win_screen ()
            else
                game_loop (Project_t.Logic.create_floor (final_state.floor + 1) final_state.player)
            else
            game_loop final_state
        | _ -> game_loop state
    in
    let init_state = Project_t.Logic.create_floor 1 { pos = { x = 5; y = 5 }; hp = 10; max_hp = 10 } in
    game_loop init_state;
    Term.release term
