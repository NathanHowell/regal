use regal_macros::RegalLexer;

#[derive(RegalLexer, Copy, Clone, Debug, PartialEq, Eq, Default)]
enum Token {
    #[default]
    #[regex(r"#[0-9A-Fa-f]{6}")]
    Hex,
    #[regex(r"true|false")]
    Bool,
    #[regex(r"[0-9]+")]
    Integer,
    #[regex(r"[0-9A-F]+")]
    UpperDigits,
    #[regex(r"[A-Za-z]+[0-9]*")]
    Word,
}

fn main() {}
