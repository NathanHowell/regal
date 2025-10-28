use regal_macros::RegalLexer;

#[derive(RegalLexer)]
enum Bad {
    A,
    #[token("a")]
    B,
}

fn main() {}
