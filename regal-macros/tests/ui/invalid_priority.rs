use regal_macros::RegalLexer;

#[derive(RegalLexer)]
enum BadPriority {
    #[regex(r"[a-z]+")]
    Word,
    #[token("=")]
    #[priority("high")]
    Eq,
}

fn main() {}
