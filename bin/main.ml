let () =
    let module D = Dream in
    let module T = Template in
        D.run
    @@  D.logger
    @@  D.memory_sessions
    @@  D.router [

        D.get "/" (fun req ->
            let nav =   T.nav ["signup"; "signin"] in
            match D.header req "HX-Request" with
            | None ->   T.page "The Witching Hour"
                    @@  nav |> D.html
            | Some _ -> nav |> D.html );

        D.get "/signup" (fun req ->
            let form = T.form "POST" "/signup"
                    [ T.input "username" "text"     true 
                    ; T.input "email"    "email"    true  
                    ; T.input "password" "password" true 
                    ; T.input "passconf" "password" true ]
                    req in 
            match D.header req "HX-Request" with
            | None -> T.page "The Witching Hour" @@ form |> D.html
            | Some _ -> form |> D.html );


        D.get "/signin" (fun req ->
            let form =  T.form "POST" "/signin"
                            [ T.input "username" "text"     true
                            ; T.input "password" "password" true ]
                            req in
            match D.header req "HX-Request" with 
            | None  ->  T.page "The Witching Hour" @@ form |> D.html
            | Some _ -> form |> D.html );

        D.post "/signup" (fun req ->
            let module D = Dream in
            let module T = Template in
            let%lwt body = D.body req in
            match%lwt D.form req with 
            | `Ok   [ "email",    email
                    ; "passconf", passconf
                    ; "password", password
                    ; "username", username ] ->
                    T.ul    [ username 
                            ; email 
                            ; password 
                            ; passconf ]
                |>  D.html
            | _ ->  T.p body
                |>  D.html ~status:`Bad_Request );

        D.post "/signin" (fun req ->
            let module D = Dream in
            let module T = Template in
            let%lwt body = D.body req in
            match%lwt D.form req with 
            | `Ok   [ "password", password
                    ; "username", username ] ->
                    T.ul    [ username 
                            ; password ]
                |>  D.html
            | _ ->  T.p body
                |>  D.html ~status:`Bad_Request );
    ]

