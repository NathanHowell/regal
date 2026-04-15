mod pattern;

use crate::pattern::parse_pattern;
use proc_macro::TokenStream;
use proc_macro2::Span;
use quote::{format_ident, quote};
use regal_compiler::{
    HostCompiledDfa, NfaError as HostNfaError, NfaSpec as HostNfaSpec, build_dfa, build_nfa,
};
use syn::punctuated::Punctuated;
use syn::{Attribute, Data, DeriveInput, Expr, ExprLit, Meta, parse_macro_input, spanned::Spanned};

#[proc_macro_derive(RegalLexer, attributes(token, regex, priority, skip))]
pub fn derive_regal_lexer(input: TokenStream) -> TokenStream {
    match expand_regal_lexer(parse_macro_input!(input as DeriveInput)) {
        Ok(tokens) => tokens.into(),
        Err(err) => err.to_compile_error().into(),
    }
}

fn expand_regal_lexer(input: DeriveInput) -> syn::Result<proc_macro2::TokenStream> {
    let enum_ident = input.ident;
    let data_enum = match input.data {
        Data::Enum(data) => data,
        _ => {
            return Err(syn::Error::new(
                enum_ident.span(),
                "RegalLexer can only be derived for enums",
            ));
        }
    };

    let mut specs = Vec::new();

    for (variant_index, variant) in data_enum.variants.iter().enumerate() {
        if !variant.fields.is_empty() {
            return Err(syn::Error::new(
                variant.fields.span(),
                "RegalLexer only supports unit variants",
            ));
        }

        let default_priority = variant_index as u16;
        let mut variant_priority = None;
        let mut variant_skip = false;

        for attr in &variant.attrs {
            if attr.path().is_ident("priority") {
                match &attr.meta {
                    Meta::NameValue(nv) => {
                        if let Expr::Lit(ExprLit {
                            lit: syn::Lit::Int(value),
                            ..
                        }) = &nv.value
                        {
                            variant_priority = Some(value.base10_parse()?);
                        } else {
                            return Err(syn::Error::new(
                                nv.value.span(),
                                "priority must be an integer literal",
                            ));
                        }
                    }
                    _ => {
                        return Err(syn::Error::new(
                            attr.span(),
                            "priority attribute expects #[priority = <int>]",
                        ));
                    }
                }
            } else if attr.path().is_ident("skip") && matches!(attr.meta, Meta::Path(_)) {
                variant_skip = true;
            }
        }

        let mut found_pattern = false;

        for attr in &variant.attrs {
            if attr.path().is_ident("token") || attr.path().is_ident("regex") {
                found_pattern = true;
                let is_token = attr.path().is_ident("token");
                let pattern = parse_pattern_attr(attr, is_token)?;
                specs.push(SpecDescriptor {
                    variant_ident: variant.ident.clone(),
                    pattern: pattern.pattern,
                    skip: pattern.skip || variant_skip,
                    priority: pattern
                        .priority
                        .or(variant_priority)
                        .unwrap_or(default_priority),
                });
            }
        }

        if !found_pattern {
            return Err(syn::Error::new(
                variant.ident.span(),
                "Each variant must have a #[token(..)] or #[regex(..)] attribute",
            ));
        }
    }

    let mut nfa_specs = Vec::new();
    let mut token_meta = Vec::new();

    for (index, spec) in specs.iter().enumerate() {
        let parsed =
            parse_pattern(&spec.pattern).map_err(|err| syn::Error::new(Span::call_site(), err))?;
        nfa_specs.push(HostNfaSpec {
            pattern: parsed,
            token_id: index as u16,
            priority: spec.priority,
        });
        token_meta.push(TokenMeta {
            priority: spec.priority,
            skip: spec.skip,
        });
    }

    let nfa = build_nfa(&nfa_specs)
        .map_err(|err| syn::Error::new(Span::call_site(), describe_nfa_error(err)))?;
    let dynamic_dfa = build_dfa(&nfa, token_meta.len());

    let generated = emit_codegen(enum_ident, specs, token_meta, dynamic_dfa)?;

    Ok(generated)
}

struct SpecDescriptor {
    variant_ident: syn::Ident,
    pattern: String,
    skip: bool,
    priority: u16,
}

struct TokenMeta {
    priority: u16,
    skip: bool,
}

struct PatternAttr {
    pattern: String,
    skip: bool,
    priority: Option<u16>,
}

fn parse_pattern_attr(attr: &Attribute, is_token: bool) -> syn::Result<PatternAttr> {
    let mut skip = false;
    let mut priority: Option<u16> = None;
    let mut literal = None;

    match &attr.meta {
        Meta::List(list) => {
            let args: Punctuated<Expr, syn::Token![,]> =
                list.parse_args_with(Punctuated::parse_terminated)?;
            for (index, expr) in args.iter().enumerate() {
                match expr {
                    Expr::Lit(ExprLit {
                        lit: syn::Lit::Str(value),
                        ..
                    }) => {
                        if index != 0 || literal.is_some() {
                            return Err(syn::Error::new(
                                expr.span(),
                                "pattern literal must be the first argument",
                            ));
                        }
                        literal = Some(value.value());
                    }
                    Expr::Path(path) if path.path.is_ident("skip") => {
                        skip = true;
                    }
                    Expr::Assign(assign) => match assign.left.as_ref() {
                        Expr::Path(path) if path.path.is_ident("priority") => {
                            match assign.right.as_ref() {
                                Expr::Lit(ExprLit {
                                    lit: syn::Lit::Int(value),
                                    ..
                                }) => {
                                    priority = Some(value.base10_parse()?);
                                }
                                other => {
                                    return Err(syn::Error::new(
                                        other.span(),
                                        "priority must be an integer literal",
                                    ));
                                }
                            }
                        }
                        other => {
                            return Err(syn::Error::new(
                                other.span(),
                                "unsupported assignment in attribute",
                            ));
                        }
                    },
                    _ => {
                        return Err(syn::Error::new(expr.span(), "unsupported attribute option"));
                    }
                }
            }
        }
        Meta::NameValue(nv) => {
            if let Expr::Lit(ExprLit {
                lit: syn::Lit::Str(lit),
                ..
            }) = &nv.value
            {
                literal = Some(lit.value());
            } else {
                return Err(syn::Error::new(
                    nv.value.span(),
                    "pattern must be provided as string literal",
                ));
            }
        }
        Meta::Path(_) => {
            return Err(syn::Error::new(
                attr.span(),
                "expected #[token(\"...\")] or #[regex(\"...\")]",
            ));
        }
    }

    let mut pattern =
        literal.ok_or_else(|| syn::Error::new(attr.span(), "missing pattern literal"))?;
    if is_token {
        pattern = escape_literal(&pattern);
    }

    Ok(PatternAttr {
        pattern,
        skip,
        priority,
    })
}

fn describe_nfa_error(err: HostNfaError) -> &'static str {
    match err {
        HostNfaError::InvalidRepeat => "invalid repetition range",
    }
}

fn escape_literal(input: &str) -> String {
    let mut escaped = String::with_capacity(input.len() * 2);
    for ch in input.chars() {
        match ch {
            '.' | '+' | '*' | '?' | '(' | ')' | '[' | ']' | '{' | '}' | '^' | '$' | '|' | '\\' => {
                escaped.push('\\');
                escaped.push(ch);
            }
            _ => escaped.push(ch),
        }
    }
    escaped
}

fn emit_codegen(
    enum_ident: syn::Ident,
    specs: Vec<SpecDescriptor>,
    tokens: Vec<TokenMeta>,
    dfa: HostCompiledDfa,
) -> syn::Result<proc_macro2::TokenStream> {
    fn screaming_snake(ident: &syn::Ident) -> String {
        let name = ident.to_string();
        if name.is_empty() {
            return name;
        }

        let chars: std::vec::Vec<char> = name.chars().collect();
        let mut result = String::with_capacity(chars.len() * 2);

        for (idx, ch) in chars.iter().enumerate() {
            let is_alnum = ch.is_ascii_alphanumeric();
            if !is_alnum {
                if !result.ends_with('_') && !result.is_empty() {
                    result.push('_');
                }
                continue;
            }

            let is_upper = ch.is_ascii_uppercase();
            let is_lower = ch.is_ascii_lowercase();
            let prev = if idx > 0 { Some(chars[idx - 1]) } else { None };
            let next = chars.get(idx + 1).copied();

            let mut need_separator = false;
            if let Some(prev_ch) = prev {
                if is_upper {
                    let prev_lower_or_digit =
                        prev_ch.is_ascii_lowercase() || prev_ch.is_ascii_digit();
                    let next_is_lower = next.map(|c| c.is_ascii_lowercase()).unwrap_or(false);
                    let prev_is_upper = prev_ch.is_ascii_uppercase();
                    need_separator = prev_lower_or_digit || (prev_is_upper && next_is_lower);
                } else if is_lower {
                    if prev_ch.is_ascii_digit() {
                        need_separator = true;
                    }
                } else if ch.is_ascii_digit() {
                    if prev_ch.is_ascii_alphabetic() {
                        need_separator = true;
                    }
                }
            }

            if need_separator && !result.is_empty() && !result.ends_with('_') {
                result.push('_');
            }

            if is_upper {
                result.push(*ch);
            } else {
                result.push(ch.to_ascii_uppercase());
            }
        }

        if result.is_empty() { name } else { result }
    }

    let token_count = specs.len();
    let states_len = dfa.states.len();
    let transitions_len = dfa.transitions.len();
    let dense_slots = dfa.dense.len();
    let class_slots = dfa.classes.len();

    let screaming = screaming_snake(&enum_ident);
    let token_const = format_ident!("{}_TOKEN_COUNT", screaming);
    let state_const = format_ident!("{}_DFA_STATE_COUNT", screaming);
    let trans_const = format_ident!("{}_DFA_TRANSITION_COUNT", screaming);
    let dense_const = format_ident!("{}_DFA_DENSE_COUNT", screaming);
    let class_const = format_ident!("{}_DFA_CLASS_COUNT", screaming);

    let compiled_alias = format_ident!("{}CompiledLexer", enum_ident);
    let packed_alias = format_ident!("{}PackedDfa", enum_ident);
    let lexer_alias = format_ident!("{}Lexer", enum_ident);

    let enum_path = specs
        .iter()
        .map(|spec| {
            let variant = &spec.variant_ident;
            quote! { #enum_ident::#variant }
        })
        .collect::<Vec<_>>();

    let token_info = tokens.iter().zip(enum_path.iter()).map(|(meta, expr)| {
        let skip = meta.skip;
        let priority = meta.priority;
        quote! {
            regal::TokenInfo {
                token: #expr,
                skip: #skip,
                priority: #priority,
            }
        }
    });

    let states = dfa.states.iter().map(|state| {
        let first = state.first_transition;
        let len = state.transition_len;
        let accept = state
            .accept_token
            .map(|id| quote! { Some(#id) })
            .unwrap_or_else(|| quote! { None });
        let priority = state.priority;
        let possible_bits: Vec<bool> = state.possible.clone();
        let bit_array = possible_bits.iter().map(|b| {
            let value = *b;
            quote! { #value }
        });
        let dense_offset = state.dense_offset;
        let dense_len = state.dense_len;
        let dense_start = state.dense_start;
        quote! {
            regal::DfaState {
                first_transition: #first,
                transition_len: #len,
                accept_token: #accept,
                priority: #priority,
                possible: regal::Bitset::from_array([#(#bit_array),*]),
                dense_offset: #dense_offset,
                dense_len: #dense_len,
                dense_start: #dense_start,
            }
        }
    });

    let transitions = dfa.transitions.iter().map(|trans| {
        let start = trans.start;
        let end = trans.end;
        let target = trans.target;
        quote! {
            regal::DfaTransition {
                start: #start,
                end: #end,
                target: #target,
            }
        }
    });

    let dense_entries = dfa.dense.iter().map(|entry| {
        let value = *entry;
        quote! { #value }
    });

    let class_entries = dfa.classes.iter().map(|entry| {
        let start = entry.start;
        let end = entry.end;
        let class = entry.class;
        quote! {
            regal::ByteClass {
                start: #start,
                end: #end,
                class: #class,
            }
        }
    });

    let lexer_ident = format_ident!("__REGAL_LEXER");
    let start_state = dfa.start;

    let expanded = quote! {
        pub const #token_const: usize = #token_count;
        pub const #state_const: usize = #states_len;
        pub const #trans_const: usize = #transitions_len;
        pub const #dense_const: usize = #dense_slots;
        pub const #class_const: usize = #class_slots;

        pub type #packed_alias = regal::PackedDfa<
            #states_len,
            #transitions_len,
            #token_count,
            #dense_slots,
            #class_slots
        >;

        pub type #compiled_alias = regal::CompiledLexer<
            #enum_ident,
            #token_count,
            #states_len,
            #transitions_len,
            #dense_slots,
            #class_slots
        >;

        pub type #lexer_alias<'__regal_source> = regal::Lexer<
            '__regal_source,
            #enum_ident,
            #token_count,
            #states_len,
            #transitions_len,
            #dense_slots,
            #class_slots
        >;

        pub static #lexer_ident: regal::CompiledLexer<
            #enum_ident,
            #token_count,
            #states_len,
            #transitions_len,
            #dense_slots,
            #class_slots
        > = regal::CompiledLexer::from_parts(
            regal::PackedDfa::from_parts(
                #start_state,
                [#(#states),*],
                #states_len,
                [#(#transitions),*],
                #transitions_len,
                [#(#dense_entries),*],
                #dense_slots,
                [#(#class_entries),*],
                #class_slots,
            ),
            [#(#token_info),*],
            #token_count,
        );

        impl #enum_ident {
            pub fn lexer() -> &'static regal::CompiledLexer<
                #enum_ident,
                #token_count,
                #states_len,
                #transitions_len,
                #dense_slots,
                #class_slots
            > {
                &#lexer_ident
            }
        }
    };

    Ok(expanded)
}
