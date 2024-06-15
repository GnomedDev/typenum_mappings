//! A proc-macro to generate mappings from typenum's `UInt` types to your own type.
//!
//! This is useful when emulating `const_generic_exprs` on stable, as the third step in the process of:
//! 1. Converting const generics to typenum types
//! 2. Performing the expression via typenum types
//! 3. **Converting those typenum types back to the resulting type**
//!
//! # Example - `concat_array`
//!
//! ```rust
//! use std::ops::Add;
//!
//! use typenum::{U, ToUInt, Const};
//!
//! trait ArrayLike {
//!     fn new() -> Self;
//! }
//!
//! impl<T: Default, const N: usize> ArrayLike for [T; N] {
//!     fn new() -> Self {
//!         std::array::from_fn(|_| T::default())
//!     }
//! }
//!
//! trait TypenumToArray<T> {
//!     type Array: ArrayLike;
//! }
//!
//! typenum_mappings::impl_typenum_mapping!(
//!     impl<const N: usize = 0..=1000, T: Default> TypenumToArray<T> for #TypeNumName {
//!         type Array: = [T; N];
//!     }
//! );
//!
//!
//! type RunAdd<T1, T2> = <T1 as Add<T2>>::Output;
//! type RunTypeToArray<T, N> = <N as TypenumToArray<T>>::Array;
//!
//! fn concat_array<const N1: usize, const N2: usize, T>(a1: [T; N1], a2: [T; N2]) -> RunTypeToArray<T, RunAdd<U<N1>, U<N2>>>
//! where
//!     Const<N1>: ToUInt,
//!     Const<N2>: ToUInt,
//!
//!     U<N1>: Add<U<N2>>,
//!     <U<N1> as Add<U<N2>>>::Output: TypenumToArray<T>
//! {
//! # if false {
//!     todo!()
//! # }
//! #   ArrayLike::new()
//! }
//!
//! let out = concat_array([1, 2, 3], [4, 5, 6]);
//! assert_eq!(out.len(), 6);
//! ```
//!
//! ## Minimum Supported Rust Version
//!
//! This is currently `1.63`, and it is considered a breaking change to increase.

#![warn(clippy::pedantic, rust_2018_idioms)]

use std::ops::Range;

use proc_macro2::Span;
use quote::quote;
use syn::{punctuated::Punctuated, Token};
use to_arraystring::ToArrayString;

/// Parser of an integer range, as [`syn::RangeLimits`] parses `0..1> Other` as `0..(1 > Other)` instead of `(0..1) > Other`.
struct ParsedRange(Range<u32>);

impl syn::parse::Parse for ParsedRange {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        let start = input.parse::<syn::LitInt>()?;
        let needs_increment = if input.peek(Token![..=]) {
            input.parse::<Token![..=]>()?;
            true
        } else if input.peek(Token![..]) {
            input.parse::<Token![..]>()?;
            false
        } else {
            return Err(input.error("Could not parse range syntax, expected `..` or `..=`"));
        };

        let end = input.parse::<syn::LitInt>()?;

        let start = start.base10_parse()?;
        let mut end = end.base10_parse()?;

        if needs_increment {
            end += 1;
        }

        Ok(Self(start..end))
    }
}

/// Parser for the rest of an `impl` item's generics, half way through the list.
struct RestGenerics(Punctuated<syn::GenericParam, Token![,]>);

impl syn::parse::Parse for RestGenerics {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        if input.parse::<Option<Token![,]>>()?.is_none() {
            return Ok(Self(Punctuated::new()));
        }

        let mut out = Punctuated::new();
        while !input.peek(Token![>]) {
            out.push_value(input.parse()?);
            if let Some(comma) = input.parse::<Option<Token![,]>>()? {
                out.push_punct(comma);
            } else {
                break;
            }
        }

        Ok(Self(out))
    }
}

#[derive(Debug)]
struct Arguments {
    index: syn::Ident,
    index_ty: syn::Type,
    range: Range<u32>,
    generics: Punctuated<syn::GenericParam, Token![,]>,
    trait_name: syn::Ident,
    trait_generics: syn::Generics,
    where_clause: Option<syn::WhereClause>,
    rest: proc_macro2::TokenStream,
}

impl syn::parse::Parse for Arguments {
    fn parse(input: syn::parse::ParseStream<'_>) -> syn::Result<Self> {
        input.parse::<Token![impl]>()?;

        input.parse::<Token![<]>()?;
        input.parse::<Token![const]>()?;
        let index = input.parse()?;
        input.parse::<Token![:]>()?;
        let index_ty = input.parse::<syn::Type>()?;
        input.parse::<Token![=]>()?;
        let range = input.parse::<ParsedRange>()?.0;
        let generics = input.parse::<RestGenerics>()?.0;
        input.parse::<Token![>]>()?;

        let trait_name = input.parse()?;
        let trait_generics = input.parse()?;
        input.parse::<Token![for]>()?;

        if input.parse::<Token![#]>().is_err() || input.parse::<syn::Ident>()? != "TypeNumName" {
            return Err(syn::Error::new(
                Span::call_site(),
                "Expected implementation for literal `#TypeNumName`",
            ));
        }

        let where_clause = input.parse::<Option<syn::WhereClause>>()?;

        let inside_parens;
        syn::braced!(inside_parens in input);

        let rest = inside_parens.parse::<proc_macro2::TokenStream>()?;

        Ok(Self {
            index,
            index_ty,
            range,
            generics,
            trait_name,
            trait_generics,
            where_clause,
            rest,
        })
    }
}

#[derive(Debug)]
struct GenerateTypenum(u32);

impl quote::ToTokens for GenerateTypenum {
    fn to_tokens(&self, tokens: &mut proc_macro2::TokenStream) {
        if self.0 == 0 {
            return quote!(::typenum::UTerm).to_tokens(tokens);
        }

        let num_bits_set = u32::BITS - self.0.leading_zeros();
        for _ in 0..num_bits_set {
            quote!(::typenum::UInt<).to_tokens(tokens);
        }

        quote!(::typenum::UTerm,).to_tokens(tokens);

        for n in (0..num_bits_set).rev() {
            if self.0 & (1 << n) == 0 {
                quote!(::typenum::B0>).to_tokens(tokens);
            } else {
                quote!(::typenum::B1>).to_tokens(tokens);
            }

            if n != 0 {
                quote!(,).to_tokens(tokens);
            }
        }
    }
}

#[proc_macro]
pub fn impl_typenum_mapping(tokens: proc_macro::TokenStream) -> proc_macro::TokenStream {
    let Arguments {
        index,
        index_ty,
        range,
        generics,
        trait_name,
        trait_generics,
        where_clause,
        rest,
    } = match syn::parse(tokens) {
        Ok(args) => args,
        Err(err) => return err.into_compile_error().into(),
    };

    let typenum_iter = range.clone().map(GenerateTypenum);
    let range = range.map(|i| syn::LitInt::new(&i.to_arraystring(), Span::call_site()));

    quote!(
        #(const _: () = {
            const #index: #index_ty = #range;
            impl<#generics> #trait_name #trait_generics for #typenum_iter #where_clause {
                #rest
            }
        };)*
    )
    .into()
}
