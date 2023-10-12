open Witching_hour.Twenty_fourty_eight 

let _ =
    Js_of_ocaml.Js.export_all 
    (object%js
        method move dir game = move dir game
        method newGame size = new_game size
        method gameToStr game = game_to_str game
        method parse str = parse str
    end)
