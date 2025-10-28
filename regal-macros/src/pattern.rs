use regex_syntax::ParserBuilder;
use regex_syntax::hir::{self, Hir, HirKind, Literal};
use std::vec::Vec;

#[derive(Clone)]
pub(crate) enum PatternExpr {
    Empty,
    Literal(Vec<u8>),
    Class(Vec<ClassAtom>),
    Sequence(Vec<PatternExpr>),
    Alternate(Vec<PatternExpr>),
    Repeat {
        inner: Box<PatternExpr>,
        min: u32,
        max: Option<u32>,
    },
}

#[derive(Clone)]
pub(crate) enum ClassAtom {
    Char(u32),
    Range { start: u32, end: u32 },
}

pub(crate) fn parse_pattern(pattern: &str) -> Result<PatternExpr, String> {
    let hir = ParserBuilder::new()
        .build()
        .parse(pattern)
        .map_err(|err| err.to_string())?;
    convert_hir(&hir)
}

fn convert_hir(hir: &Hir) -> Result<PatternExpr, String> {
    match hir.kind() {
        HirKind::Empty => Ok(PatternExpr::Empty),
        HirKind::Literal(lit) => Ok(PatternExpr::Literal(literal_bytes(lit))),
        HirKind::Class(class) => convert_class(class),
        HirKind::Concat(parts) => {
            let mut seq = Vec::with_capacity(parts.len());
            for part in parts {
                seq.push(convert_hir(part)?);
            }
            Ok(PatternExpr::Sequence(seq))
        }
        HirKind::Alternation(parts) => {
            let mut alts = Vec::with_capacity(parts.len());
            for part in parts {
                alts.push(convert_hir(part)?);
            }
            Ok(PatternExpr::Alternate(alts))
        }
        HirKind::Repetition(rep) => Ok(PatternExpr::Repeat {
            inner: Box::new(convert_hir(rep.sub.as_ref())?),
            min: rep.min,
            max: rep.max,
        }),
        _ => Err("unsupported regex construct".into()),
    }
}

fn literal_bytes(literal: &Literal) -> Vec<u8> {
    literal.0.as_ref().to_vec()
}

fn convert_class(class: &hir::Class) -> Result<PatternExpr, String> {
    match class {
        hir::Class::Unicode(unicode) => {
            let mut atoms = Vec::new();
            for range in unicode.iter() {
                let start = range.start() as u32;
                let end = range.end() as u32;
                if start == end {
                    atoms.push(ClassAtom::Char(start));
                } else {
                    atoms.push(ClassAtom::Range { start, end });
                }
            }
            Ok(PatternExpr::Class(atoms))
        }
        hir::Class::Bytes(bytes) => {
            let mut atoms = Vec::new();
            for range in bytes.iter() {
                let start = range.start() as u32;
                let end = range.end() as u32;
                if start == end {
                    atoms.push(ClassAtom::Char(start));
                } else {
                    atoms.push(ClassAtom::Range { start, end });
                }
            }
            Ok(PatternExpr::Class(atoms))
        }
    }
}
