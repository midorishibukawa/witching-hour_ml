module D = Dream 
module P = Pages
module T = Templates

let () =
    let handle_htmx ~req body =
        match D.header req "HX-Request" with 
        | None   -> D.html @@ T.page ~title:"The Witching Hour" ~body
        | Some _ -> D.html body 
    in  D.run 
    @@  D.logger
    @@  D.sql_pool "postgresql://postgres:postgres@localhost:5432/witching_hour"
    @@  D.memory_sessions
    @@  D.router [

        (*      HOME        *)

        D.get "/" (fun req -> handle_htmx ~req @@ P.home); 


        (*      SIGNUP      *)

        D.get "/signup" (fun req -> handle_htmx ~req @@ P.signup ~req);

        D.post "/signup" (fun req ->
            let handle_lwt res =
                match%lwt res with 
                | None       -> D.redirect req "/"
                | Some error -> D.html ~status:`Bad_Request @@ T.p error
            in
            match%lwt D.form req with
            | `Ok [ "email",    email
                  ; "password", password
                  ; "username", username ] 
                 -> let res = User.signup ~req ~username ~email ~password in
                    handle_lwt res
            | _  -> D.html ~status:`Bad_Request 
                 @@ T.p "INVALID FORM" );


        (*      SIGNIN      *)

        D.get "/signin" (fun req -> handle_htmx ~req @@ P.signin ~req);

        D.post "/signin" (fun req ->
            let get_body id = 
                match id with 
                | Some _ -> D.redirect req "/" 
                | None   -> D.html ~status:`Not_Found @@ T.p "NOT FOUND"
            in
            let%lwt body = D.body req in
            match%lwt D.form req with 
            | `Ok [ "password", password
                  ; "username", username ]
                 -> let%lwt id = User.signin ~req ~username ~password in 
                    get_body id
            | _  -> D.html ~status:`Bad_Request @@ T.p body );
    ]

