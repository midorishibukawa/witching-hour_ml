module type DB = Caqti_lwt.CONNECTION
module D = Dream 
module T = Caqti_type
module WH = Template

let () =
    let handle_htmx req body =
        match D.header req "HX-Request" with 
        | None   -> D.html 
                 @@ WH.page "The Witching Hour"
                    body
        | Some _ -> D.html body 
    in  D.run
    @@  D.logger
    @@  D.sql_pool "postgresql://postgres:postgres@localhost:5432/witching_hour"
    @@  D.memory_sessions
    @@  D.router [

        (*      HOME        *)

        D.get "/" (fun req ->
            let nav   = WH.nav [ "signup"; "signin" ] in
            handle_htmx req nav );


        (*      SIGNUP      *)

        D.get "/signup" (fun req ->
            let form  = WH.form  "POST"      "/signup"
                      [ WH.input "username"  "text"      true 
                      ; WH.input "email"     "email"     true  
                      ; WH.input "password"  "password"  true 
                      ; WH.input "passconf"  "password"  true ]
                        req 
            in handle_htmx req form );

        D.post "/signup" (fun req ->
            let add_user = 
                let query = 
                    let open Caqti_request.Infix in
                    (T.(tup3 string string string) ->. T.unit)
                    "INSERT INTO public.users (username, email, password) values ($1, $2, $3);" in
                fun username email password (module Db : DB) ->
                    let%lwt unit_or_error = Db.exec query (username, email, password) in 
                    Caqti_lwt.or_fail unit_or_error
            in
            match%lwt D.form req with
            | `Ok [ "email",    email
                  ; "passconf", passconf
                  ; "password", password
                  ; "username", username ] 
                 -> if      password = passconf 
                    then    
                        let%lwt () = D.sql req (add_user username email password) in
                            D.html @@ WH.p "OK" 
                    else    D.html ~status:`Bad_Request @@ WH.p "ERROR"
            | _  -> D.html ~status:`Bad_Request 
                 @@ WH.p "INVALID FORM" );


        (*      SIGNIN      *)

        D.get "/signin" (fun req ->
            let form  = WH.form  "POST"      "/signin"
                      [ WH.input "username"  "text"      true
                      ; WH.input "password"  "password"  true ]
                        req 
            in handle_htmx req form );

        D.post "/signin" (fun req ->
            let get_user =
                let query =
                    let open Caqti_request.Infix in
                    (T.(tup2 string string) ->? T.string)
                    "SELECT id FROM public.users WHERE username = $1 AND password = $2" in
                fun username password (module Db : DB) ->
                    let%lwt id_or_error = Db.find_opt query (username, password) in
                    Caqti_lwt.or_fail id_or_error
            in
            let get_body id = 
                match id with 
                | Some id -> D.html @@ WH.p id
                | None   -> D.html ~status:`Not_Found @@ WH.p "NOT FOUND"
            in
            let%lwt body = D.body req in
            match%lwt D.form req with 
            | `Ok [ "password", password
                  ; "username", username ]
                 -> let%lwt id = D.sql req (get_user username password) in
                    get_body id
            | _  -> D.html ~status:`Bad_Request @@ WH.p body );
    ]

