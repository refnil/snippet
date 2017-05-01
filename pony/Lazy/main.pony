actor Main
    new create(env : Env) =>
        env.out.print("Hello")
        let f  = {() : String^ => "Hi"}
        let l = Lazy[String](f)
        env.out.print(l())
