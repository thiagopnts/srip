
#[deriving(Clone)]
struct Value;

// constructor functions
type ctor = fn() -> Option<Value>;

// destructor functions
type dtor = fn(Value) -> Value;

type Fold = fn(int, Vec<Value>) -> Value;

type ApplyFunction = fn(Value) -> Value;

// the second arg is void* on the original lib
type ApplyFunctionTo = fn(Value, Value) -> Value;

#[deriving(Show, PartialEq)]
enum ParserType {
    Undefined,
    Pass,
    Fail,
    Lift,
    LiftVal,
    Expect,
    Anchor,
    State,

    Any,
    Single,
    OneOf,
    NoneOf,
    Range,
    Satisfy,
    String,

    Apply,
    ApplyTo,
    Predict,
    Not,
    Maybe,
    Many,
    Many1,
    Count,

    Or,
    And,
}

enum ParsingResult {
    Error(ParsingError),
    Output(Value),
}

enum ParserData {
    Fail(String),
    Lift(ctor, Value),
    Expect(Parser, String),
    Anchor(fn(char, char) -> int),
    Single(char),
    // or should it be char, char?
    Range(uint, uint),
    Satisfy(fn(char) -> int),
    String(String),
    Apply(Parser, ApplyFunction),
    //value is void*
    ApplyTo(Parser, ApplyFunctionTo, Value),
    Predict(Parser),
    // maybe pass a dtor_null instead of option, needs test
    Not(Parser, Option<dtor>, ctor),
    Repeat(Option<int>, Fold, Parser, Option<dtor>),
    Or(int, Vec<Parser>),
    // we should apply dtor (n - 1) times (?)
    And(int, Fold, Vec<Parser>, Vec<dtor>),
}

struct State {
    // this should all be long
    pos: int,
    row: int,
    col: int,
}

struct ParsingError {
    state: State,
    expected_num: int,
    filename: String,
    failure: String,
    expected: Vec<String>,
    received: char,
}

struct Parser {
    retained: int,
    name: Option<String>,
    parser_type: ParserType,
    data: Option<Box<ParserData>>,
}

impl Parser {
    pub fn new() -> Parser {
        Parser {
            retained: 0,
            parser_type: ParserType::Undefined,
            name: None,
            data: None,
        }
    }
}

struct AST {
    tag: String,
    contents: String,
    state: State,
    children_num: uint,
    children: Vec<Box<AST>>,
}

struct Stack {
    parsers_num: uint,
    parsers_slots: uint,
    parsers: Vec<Parser>,
    states: Vec<int>,
    results_num: uint,
    results_slots: int,
    results: Vec<ParsingResult>,
    returns: Vec<int>,
    err: ParsingError,
}

fn ctor_null() -> Option<Value> {
    None
}

fn dtor_null(x: Value) -> Value { x }

// this all should be generated through macro_rules!
fn string(string: &str) -> Parser {
    let mut p = Parser::new();
    p.parser_type = ParserType::String;
    p.data = Some(box ParserData::String(string.to_string()));
    // TODO: missing expectf(p, "\"%s\"", s); call
    // before return
    p
}

fn strfold(n: int, xs: Vec<Value>) -> Value {
    println!("strfold unimplemented");
    Value
}

fn free(x: Value) -> Value {
    x
}

fn whitespace() -> Parser {
    expect(oneof(" \\f\\n\\r\\t\\v"), "whitespace")
}

fn whitespaces() -> Parser {
    expect(many(strfold, whitespace()), "spaces")
}

fn first(n: int, xs: Vec<Value>) -> Value {
    xs[0].clone()
}

fn blank() -> Parser {
    expect(apply(whitespaces(), free), "whitespace")
}

fn oneof(s: &str) -> Parser {
    let mut p = Parser::new();
    p.parser_type = ParserType::OneOf;
    p.data = Some(box ParserData::String(s.to_string()));
    // TODO: missing expectf(p, "\"%s\"", s); call
    // before return
    p
}

fn tok(a: Parser) -> Parser {
    and(first, vec!(a, blank()), vec!())
}

fn expect(a: Parser, expected: &str) -> Parser {
    let mut p = Parser::new();
    p.parser_type = ParserType::Expect;
    p.data = Some(box ParserData::Expect(a, expected.to_string()));
    p
}

fn apply(a: Parser, f: ApplyFunction) -> Parser {
    let mut p = Parser::new();
    p.parser_type = ParserType::Apply;
    p.data = Some(box ParserData::Apply(a, f));
    p
}

fn apply_to(a: Parser, f: ApplyFunctionTo, x: Value) -> Parser {
    let mut p = Parser::new();
    p.parser_type = ParserType::ApplyTo;
    p.data = Some(box ParserData::ApplyTo(a, f, x));
    p
}

fn predictive(a: Parser) -> Parser {
    let mut p = Parser::new();
    p.parser_type = ParserType::Predict;
    p.data = Some(box ParserData::Predict(a));
    p
}

fn not_lift(a: Parser, da: dtor, lf: ctor) -> Parser {
    let mut p = Parser::new();
    p.parser_type = ParserType::Not;
    p.data = Some(box ParserData::Not(a, Some(da), lf));
    p
}


fn not(a: Parser, da: dtor) -> Parser {
    not_lift(a, da, ctor_null)
}

fn maybe_lift(a: Parser, lf: ctor) -> Parser {
    let mut p = Parser::new();
    p.parser_type = ParserType::Maybe;
    p.data = Some(box ParserData::Not(a, None, lf));
    p
}

fn maybe(a: Parser) -> Parser {
    maybe_lift(a, ctor_null)
}

fn many(f: Fold, a: Parser) -> Parser {
    let mut p = Parser::new();
    p.parser_type = ParserType::Many;
    p.data = Some(box ParserData::Repeat(None, f, a, None));
    p
}

fn many1(f: Fold, a: Parser) -> Parser {
    let mut p = Parser::new();
    p.parser_type = ParserType::Many1;
    p.data = Some(box ParserData::Repeat(None, f, a, None));
    p
}

fn count(n: int, f: Fold, a: Parser, da: dtor) -> Parser {
    let mut p = Parser::new();
    p.parser_type = ParserType::Count;
    p.data = Some(box ParserData::Repeat(Some(n), f, a, Some(da)));
    p
}

fn or(parsers: Vec<Parser>) -> Parser {
    let mut p = Parser::new();
    p.parser_type = ParserType::Or;
    p.data = Some(box ParserData::Or(parsers.len() as int, parsers));
    p
}

fn and(f: Fold, parsers: Vec<Parser>, dtors: Vec<dtor>) -> Parser {
    let mut p = Parser::new();
    p.parser_type = ParserType::And;
    p.data = Some(
        box ParserData::And(parsers.len() as int, f, parsers, dtors)
        );
    p
}

fn sym(s: &str) -> Parser {
    tok(string(s))
}

#[test]
fn maybe_test() {
    let p = Parser::new();
    let a = maybe(p);
    assert!(a.parser_type == ParserType::Maybe);
}

#[test]
fn or_with_sym_test() {
    let adj = or(vec!(sym("wow"), sym("many"), sym("so"), sym("such")));

    assert!(adj.parser_type == ParserType::Or);
    match adj.data {
        Some(data) => match *data {
            ParserData::Or(n, parsers) => {
                assert!(n == 4);
                assert!(parsers.len() == 4);
            },
            _ => assert!(false),
        },
        _ => assert!(false)
    }
}

#[test]
fn and_with_sym_test() {
    let adj = or(vec!(sym("wow"), sym("many"), sym("so"), sym("such")));
    let noun = or(vec!(sym("lisp"), sym("language"), sym("book"), sym("build"), sym("c")));
    let phrase = and(strfold, vec!(adj, noun), vec!());

    assert!(phrase.parser_type == ParserType::And);

    match phrase.data {
        Some(data) => match *data {
            ParserData::And(n, _, parsers, _) => {
                assert!(n == 2);
                assert!(parsers.len() == 2);
                assert!(parsers[0].parser_type == ParserType::Or);
            },
            _ => assert!(false),
        },
        _ => assert!(false),
    }
}
