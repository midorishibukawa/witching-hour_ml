module T = Templates

let signup_form ~req =
    T.form  
    ~meth:"POST"      
    ~endpoint:"/signup"
    ~inputs:[ T.input "username"  "text"      true 
            ; T.input "email"     "email"     true  
            ; T.input "password"  "password"  true 
            ; T.input "passconf"  "password"  true ]
    ~params:"not passconf"
    ~req


let signin_form ~req = 
    T.form 
    ~meth:"POST"      
    ~endpoint:"/signin"
    ~inputs:[ T.input "username"  "text"      true
            ; T.input "password"  "password"  true ]
    ~params:"*"
    ~req
