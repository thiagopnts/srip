
trait Value {
}

type ctor = fn(Value);

struct Fail(String);

struct Lift {
    lf: ctor,
    x: Value,
}

struct Expect {
    x: Parser,
    m: String,
}

type Anchor = fn(char, char) -> int;


#[test]
fn it_works() {
}
