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

        D.get "/signup" (fun req -> handle_htmx req @@ User.signup_form req);

        D.post "/signup" (fun req ->
            let handle_lwt res =
                match%lwt res with 
                | Some _ -> D.html @@ WH.p "OK"
                | None   -> D.html ~status:`Bad_Request @@ WH.p "USER/EMAIL/PASSWORD ALREADY EXISTS"
            in
            match%lwt D.form req with
            | `Ok [ "email",    email
                  ; "passconf", passconf
                  ; "password", password
                  ; "username", username ] 
                 -> if      password = passconf 
                    then    let res = User.signup req username email password in
                            handle_lwt res
                    else    D.html ~status:`Bad_Request @@ WH.p "PASSWORDS DO NOT MATCH"
            | _  -> D.html ~status:`Bad_Request 
                 @@ WH.p "INVALID FORM" );


        (*      SIGNIN      *)

        D.get "/signin" (fun req -> handle_htmx req @@ User.signin_form req);

        D.post "/signin" (fun req ->
            let get_body id = 
                match id with 
                | Some id -> D.html @@ WH.p id
                | None   -> D.html ~status:`Not_Found @@ WH.p "NOT FOUND"
            in
            let%lwt body = D.body req in
            match%lwt D.form req with 
            | `Ok [ "password", password
                  ; "username", username ]
                 -> let%lwt id = User.signin req username password in 
                    get_body id
            | _  -> D.html ~status:`Bad_Request @@ WH.p body );
    ]

