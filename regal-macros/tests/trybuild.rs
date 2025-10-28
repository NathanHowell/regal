#[test]
fn ui() {
    let t = trybuild::TestCases::new();
    t.pass("tests/ui/basic.rs");
    t.pass("tests/ui/regex_features.rs");
    t.compile_fail("tests/ui/missing_pattern.rs");
    t.compile_fail("tests/ui/invalid_priority.rs");
}
