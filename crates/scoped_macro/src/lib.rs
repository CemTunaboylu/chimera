use proc_macro::TokenStream;
use quote::quote;
use syn::{Expr, ItemFn, Token, parse::Parse, parse::ParseStream, parse_macro_input};

struct ScopedArgs {
    args: syn::punctuated::Punctuated<Expr, Token![,]>,
}

impl Parse for ScopedArgs {
    fn parse(input: ParseStream) -> syn::Result<Self> {
        Ok(ScopedArgs {
            args: input.parse_terminated(Expr::parse, Token![,])?,
        })
    }
}

#[proc_macro_attribute]
pub fn scoped(attr: TokenStream, item: TokenStream) -> TokenStream {
    let ScopedArgs { args } = parse_macro_input!(attr as ScopedArgs);
    let scope_kind_expr = match args.first() {
        Some(expr) => quote!(#expr),
        None => panic!("Expected scope kind, e.g., #[scoped(ScopeKind::Function)]"),
    };

    let mut function = parse_macro_input!(item as ItemFn);
    let original_body = &function.block;

    function.block = syn::parse_quote!({
        self.start_new_scope(#scope_kind_expr);
        let __scoped_result = (|| #original_body)();
        self.end_new_scope();
        __scoped_result
    });

    quote!(#function).into()
}
