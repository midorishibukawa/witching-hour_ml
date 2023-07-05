module type DB = Caqti_lwt.CONNECTION
module D = Dream 
module T = Caqti_type
module WH = Template


let signup_form req =
    WH.form  "POST"      "/signup"
  [ WH.input "username"  "text"      true 
  ; WH.input "email"     "email"     true  
  ; WH.input "password"  "password"  true 
  ; WH.input "passconf"  "password"  true ]
  req


let signin_form req = 
    WH.form  "POST"      "/signin"
  [ WH.input "username"  "text"      true
  ; WH.input "password"  "password"  true ]
    req 


let add_user = 
    let query = 
        let open Caqti_request.Infix in
            (T.(tup3 string string string) ->. T.unit)
            "INSERT INTO public.users (username, email, password) values ($1, $2, $3);" in
        fun username email password (module Db : DB) ->
            let%lwt unit_or_error = Db.exec query (username, email, password) in 
            Caqti_lwt.or_fail unit_or_error


let get_user =
    let query =
        let open Caqti_request.Infix in
        (T.(tup2 string string) ->? T.string)
        "SELECT id FROM public.users WHERE username = $1 AND password = $2" in
    fun username password (module Db : DB) ->
        let%lwt id_or_error = Db.find_opt query (username, password) in
        Caqti_lwt.or_fail id_or_error


let signup req username email password =
    let%lwt () = D.sql req (add_user username email password) in
    Lwt.return @@ Some ()

let signin req username password = 
    let%lwt id = D.sql req (get_user username password) in
    Lwt.return id
