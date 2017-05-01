type Maybe[A : Any #share] is (Singleton[A] | None)

type F[A] is { () : A^ } val

class Singleton[A : Any #share]
    let value : A
    new create (value' : A) =>
        value = value'

    fun apply() : A => value

class Lazy[A : Any #share]

    let const : F[A]
    var value : Maybe[A] = None

    new create(constructor: F[A]) =>
        const = constructor

    fun ref apply() : A =>
        try
            let v = value as Singleton[A]
            v()
        else
            let v = const()
            value = Singleton[A](v)
            v
        end
