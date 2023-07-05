module type DB = Caqti_lwt.CONNECTION
module D = Dream 
module T = Caqti_type

let add_user = 
    let query = 
        let open Caqti_request.Infix in
            (T.(tup3 string string string) ->. T.unit)
            "INSERT INTO public.users (username, email, password) values ($1, $2, $3)" in
        fun ~username ~email ~password (module Db : DB) ->
            let%lwt unit_or_error = Db.exec query (username, email, password) in 
            Caqti_lwt.or_fail unit_or_error

let get_user =
    let query =
        let open Caqti_request.Infix in
        (T.(tup2 string string) ->? T.string)
        "SELECT id FROM public.users WHERE username = $1 AND password = $2" in
    fun ~username ~password (module Db : DB) ->
        let%lwt id_or_error = Db.find_opt query (username, password) in
        Caqti_lwt.or_fail id_or_error

let user_email_exists =
    let query =
        let open Caqti_request.Infix in
        (T.(tup2 string string) ->! T.(tup2 bool bool))
        "SELECT bool_or(username = $1) as username, bool_or(email = $2) as email from public.users" in
    fun ~username ~email (module Db : DB) ->
        let%lwt tuple_or_error = Db.find query (username, email) in
        Caqti_lwt.or_fail tuple_or_error


let signup ~req ~username ~email ~password =
    match%lwt D.sql req (user_email_exists ~username ~email) with
    | true,  _ -> Lwt.return @@ Some "USER ALREADY EXISTS"
    | false, true -> Lwt.return @@ Some "EMAIL ALREADY EXISTS"
    | false, false -> let%lwt () = D.sql req (add_user ~username ~email ~password) in
                      Lwt.return @@ None

let signin ~req ~username ~password = 
    let%lwt id = D.sql req (get_user ~username ~password) in
    Lwt.return id
