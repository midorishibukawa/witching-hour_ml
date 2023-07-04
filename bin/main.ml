let () =
    let module D = Dream in
    let module T = Template in
    let handle_htmx req body =
        match D.header req "HX-Request" with 
        | None   -> D.html 
                 @@ T.page "The Witching Hour"
                    body
        | Some _ -> D.html body 
    in  D.run
    @@  D.logger
    @@  D.memory_sessions
    @@  D.router [

        (*      HOME        *)

        D.get "/" (fun req ->
            let nav   = T.nav [ "signup"; "signin" ] in
            handle_htmx req nav );


        (*      SIGNUP      *)

        D.get "/signup" (fun req ->
            let form  = T.form  "POST"      "/signup"
                      [ T.input "username"  "text"      true 
                      ; T.input "email"     "email"     true  
                      ; T.input "password"  "password"  true 
                      ; T.input "passconf"  "password"  true ]
                        req 
            in handle_htmx req form );

        D.post "/signup" (fun req ->
            let%lwt body = D.body req in
            match%lwt D.form req with 
            | `Ok [ "email",    _email
                  ; "passconf", passconf
                  ; "password", password
                  ; "username", _username ] 
                 -> if      password = passconf 
                    then    D.html @@ T.p "SUCCESS" 
                    else    D.html ~status:`Bad_Request @@ T.p "ERROR"
            | _  -> D.html ~status:`Bad_Request 
                 @@ T.p body );


        (*      SIGNIN      *)

        D.get "/signin" (fun req ->
            let form  = T.form  "POST"      "/signin"
                      [ T.input "username"  "text"      true
                      ; T.input "password"  "password"  true ]
                        req 
            in handle_htmx req form );

        D.post "/signin" (fun req ->
            let%lwt body = D.body req in
            match%lwt D.form req with 
            | `Ok [ "password", password
                  ; "username", username ]
                 -> D.html
                 @@ T.ul    [ username 
                            ; password ]
            | _  -> D.html ~status:`Bad_Request
                 @@ T.p body );
    ]

