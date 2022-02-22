#![doc = include_str!("../README.md")]

#[macro_use]
extern crate proc_macro_error;

use proc_macro2::{Ident, Span};
use quote::quote;
use std::path::PathBuf;
use syn::punctuated::Punctuated;
use syn::spanned::Spanned;
use syn::{
    parse_macro_input, parse_quote, AttributeArgs, FnArg, GenericArgument, Item, ItemFn,
    ItemStruct, Lit, NestedMeta, PatType, Path, PathArguments, ReturnType, Token, TraitBound, Type,
    TypeParamBound, TypePath, TypeTuple,
};

fn get_return_iterator_item(function: &ItemFn) -> Option<&Ident> {
    let ret = match &function.sig.output {
        ReturnType::Type(_, typ) => typ.as_ref(),
        ReturnType::Default => {
            emit_error!(
                &function.sig, "function has no return value";
                note = "function must return an `impl Iterator`";
                help = "add `-> impl Iterator<Item = YourRowStruct>`";
            );
            return None;
        }
    };

    let bound = match ret {
        Type::ImplTrait(impl_trait) => impl_trait.bounds.first().unwrap(),
        ty => {
            emit_error!(
                ty, "return value must be an impl Iterator";
                help = "change this type to `impl Iterator<Item = YourRowStruct>`";
            );
            return None;
        }
    };

    let iterator_item = match bound {
        TypeParamBound::Trait(TraitBound {
            path: Path { segments, .. },
            ..
        }) if segments.len() == 1 => {
            let first = segments.first().unwrap();
            if first.ident != "Iterator" {
                emit_error!(first.ident, "should be `Iterator`");
                return None;
            }

            match &first.arguments {
                PathArguments::AngleBracketed(ab) => ab
                    .args
                    .iter()
                    .filter_map(|arg| match arg {
                        GenericArgument::Binding(binding) if binding.ident == "Item" => {
                            Some(&binding.ty)
                        }
                        _ => None,
                    })
                    .next()
                    .expect("no associated type Item on Iterator"),
                _ => abort!(&first.arguments, "the Iterator has no associated types"),
            }
        }
        TypeParamBound::Lifetime(_) => unreachable!(),
        TypeParamBound::Trait(TraitBound {
            path: Path { segments, .. },
            ..
        }) => abort!(
            segments,
            "this trait should be exactly `Iterator` (with no path components before)"
        ),
    };

    let iterator_item = match iterator_item {
        Type::Path(TypePath { path, .. }) if path.segments.len() == 1 => &path.segments[0].ident,
        Type::Path(_) => {
            emit_error!(iterator_item, "expected an identifier (path given)");
            return None;
        }
        _ => {
            emit_error!(iterator_item, "expected an identifier");
            return None;
        }
    };

    Some(iterator_item)
}

#[cfg(not(doctest))]
fn read_struct(source_path_span: Span, iterator_item: &Ident, source_path: &PathBuf) -> ItemStruct {
    // That's gross. That's how I deal with doctests.
    if iterator_item == "IndexedLetter" && source_path.ends_with("path/to/current/file.rs") {
        return parse_quote! {
            pub struct IndexedLetter {
                idx: i8,
                letter: char,
            }
        };
    }

    let source_contents = match std::fs::read_to_string(source_path) {
        Ok(source_contents) => source_contents,
        Err(io_err) => {
            abort!(
                source_path_span,
                "io error opening {}: {}",
                source_path.display(),
                io_err,
            )
        }
    };

    let source = syn::parse_file(&source_contents).unwrap();
    let struct_def = source
        .items
        .into_iter()
        .filter_map(|item| match item {
            Item::Struct(struct_item) if &struct_item.ident == iterator_item => Some(struct_item),
            _ => None,
        })
        .next();

    match struct_def {
        Some(struct_def) => struct_def,
        None => {
            abort!(
                iterator_item,
                "no top-level structure with this name found in the file {}",
                source_path.display();
                info = source_path_span => "the file is specified here";
            )
        }
    }
}

fn struct_to_tuple(s: ItemStruct) -> TypeTuple {
    let fields = s.fields.into_iter().map(|field| {
        let name = &field.ident;
        let ty = &field.ty;

        quote! { ::pgx::name!(#name, #ty) }
    });

    parse_quote! {
        (
            #(#fields,)*
        )
    }
}

/// Defines a [`#[pg_extern]`][::pgx::pg_extern] function, but with its columns specified as the
/// fields of a structure
///
/// ## Conditions
///
/// This proc-macro _may only_ be applied to a top-level function item whose return type is
/// `-> impl Iterator<Item = T>`, where `T` is a top-level structure in the same file as the
/// function item. The trait [`Iterator`] above _must_ be written exactly as-is, with no leading
/// module path (« `impl std::iter::Iterator<Item = T>` » will not work). The macro `#[pg_extern]`
/// _may not_ be applied to an item where `#[pg_extern_columns]`, as the latter will automatically
/// add the former where it should.
///
/// If the macro call doesn't respect one of these conditions, it might unexpectedly stop working at
/// any point in time.
///
/// ## Usage
///
/// The macro must be given — as a single nameless parameter — a string literal corresponding to the
/// current file in which the macro is used. This will be used in order to find the definition of
/// the structure `T`. When [`proc_macro_span`][proc_macro_span] becomes stable, it is likely that
/// this parameter will become useless, and even deprecated. Providing a parameter that isn't a path
/// to the current file is undefined compile-time behavior.
///
/// Currently, passing arguments to the automatically-emitted `#[pg_extern]` _is not_ supported.
///
/// [proc_macro_span](https://doc.rust-lang.org/proc_macro/struct.Span.html#method.source_file)
///
/// ## Example
///
/// ```rust
/// # use pgx::*;
/// # use pgx_named_columns::*;
/// #
/// const ALPHABET: &str = "ABCDEFGHIJKLMNOPQRSTUVWXYZ";
///
/// struct IndexedLetter {
///     idx: i8,
///     letter: char,
/// }
///
/// #[pg_extern_columns("path/to/current/file.rs")]
/// fn alphabet(length: i8) -> impl Iterator<Item = IndexedLetter> {
///     panic!("{}", file!());
///     ALPHABET
///         .chars()
///         .take(length.clamp(0, 25) as usize)
///         .enumerate()
///         .map(|(idx, letter)| IndexedLetter {
///             idx: idx as _,
///             letter,
///         })
/// }
/// ```
#[proc_macro_error]
#[proc_macro_attribute]
pub fn pg_extern_columns(
    attr: proc_macro::TokenStream,
    input: proc_macro::TokenStream,
) -> proc_macro::TokenStream {
    let source_path = match parse_macro_input!(attr as AttributeArgs) {
        attr if attr.is_empty() => {
            emit_error!(
                Span::call_site(), "missing path";
                help = r#"add the path in brackets: #[pg_extern_columns("path/to/this/file.rs")]"#;
            );
            None
        }
        attr if attr.len() == 1 => match &attr[0] {
            nm @ NestedMeta::Lit(Lit::Str(str)) => Some((PathBuf::from(str.value()), nm.span())),
            attr => {
                emit_error!(attr, "only argument should be a string literal");
                None
            }
        },
        attr => {
            emit_error!(attr[1], "too many arguments given to #[pg_extern_columns]");
            None
        }
    };

    let function = parse_macro_input!(input as ItemFn);
    let iterator_item = get_return_iterator_item(&function);

    if let Some(attr) = function
        .attrs
        .iter()
        .find(|attr| attr.path.segments.last().unwrap().ident == "pg_extern")
    {
        emit_error!(attr, "#[pg_extern] shouldn't be applied to this function, #[pg_extern_columns] applies it automatically");
    }

    proc_macro_error::abort_if_dirty();

    // FIXME: when source_file() is stable
    //  let source_path = iterator_item.span().source_file().path();
    let (source_path, source_path_span) = source_path.unwrap();
    let iterator_item = iterator_item.unwrap();

    let struct_def = read_struct(source_path_span, iterator_item, &source_path);

    let struct_name = struct_def.ident.clone();

    let function_name = &function.sig.ident;
    let mut function_sig = function.sig.clone();

    let field_names = struct_def
        .fields
        .iter()
        .map(|field| field.ident.as_ref().unwrap());

    let into_tuple = quote! {
        (
            #(self.#field_names,)*
        )
    };

    let tuple = Type::Tuple(struct_to_tuple(struct_def));

    let args = function
        .sig
        .inputs
        .iter()
        .map(|arg| {
            if let FnArg::Typed(arg) = arg {
                arg
            } else {
                unreachable!()
            }
        })
        .enumerate()
        .map(|(i, arg)| (Ident::new(&format!("arg{}", i), arg.span()), arg))
        .collect::<Vec<_>>();

    let calling_args = args
        .iter()
        .map(|(i, _)| i)
        .collect::<Punctuated<&Ident, Token![,]>>();

    function_sig.inputs = args
        .iter()
        .map::<FnArg, _>(|(name, PatType { ty, .. })| parse_quote! { #name: #ty })
        .collect::<Punctuated<FnArg, Token![,]>>();

    function_sig.output = parse_quote! {
        -> impl std::iter::Iterator<Item = #tuple>
    };

    let wrapping_module_name = Ident::new(
        &format!("__pgx_named_columns_wrapper_{}", function_name),
        function_name.span(),
    );

    let q = quote! {
        #function

        mod #wrapping_module_name {
            #![allow(deprecated)]

            use super::*;
            use pgx::*;

            type Tuple = #tuple;

            trait IntoTuple {
                fn into_tuple(self) -> Tuple;
            }

            impl IntoTuple for #struct_name {
                #[inline]
                fn into_tuple(self) -> Tuple {
                    #into_tuple
                }
            }

            #[pg_extern]
            #function_sig {
                super::#function_name(#calling_args).map(IntoTuple::into_tuple)
            }
        }
    };

    q.into()
}
