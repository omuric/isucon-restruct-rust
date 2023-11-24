use heck::ToSnakeCase;
use itertools::Itertools;
use proc_macro2::LineColumn;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use structopt::StructOpt;
use syn::__private::{Span, ToTokens};
use syn::spanned::Spanned;
use syn::{Item, ItemFn, Token, Visibility};

#[derive(Clone, Debug)]
enum Fragment {
    EntryPoint {
        content: String,
    },
    ApiResource {
        name: String,
        content: String,
    },
    Function {
        name: String,
        content: String,
    },
    Model {
        name: String,
        content: String,
    },
    Const {
        content: String,
    },
    Use {
        content: String,
    },
    Mod {
        name: String,
        fragments: Option<Vec<Fragment>>,
    },
    Common {
        content: String,
    },
}

fn byte_offset(input: &str, location: LineColumn) -> usize {
    let mut offset = 0;
    for _ in 1..location.line {
        offset += input[offset..].find('\n').unwrap() + 1;
    }
    offset
        + input[offset..]
            .chars()
            .take(location.column)
            .map(char::len_utf8)
            .sum::<usize>()
}

struct ParseContext {
    input: String,
    current_offset: usize,
}

impl ParseContext {
    fn update_offset(&mut self, offset: usize) {
        self.current_offset = offset;
    }
    fn text_between(&self, offset: usize) -> String {
        let start = self.current_offset;
        let end = offset;
        self.input[start..end].to_string()
    }
}

fn parse_fn_code(ctx: &mut ParseContext, item: ItemFn) -> anyhow::Result<String> {
    let mut code = String::new();

    code += &*item
        .attrs
        .into_iter()
        .map(|attr| attr.to_token_stream().to_string())
        .join("");
    code += " ";
    code += &*item.vis.to_token_stream().to_string();
    code += " ";
    code += &*item.sig.to_token_stream().to_string();
    code += " ";

    code += "{";

    ctx.update_offset(byte_offset(
        &ctx.input,
        item.block.brace_token.span.open().end(),
    ));

    for stmt in item.block.stmts {
        let offset = byte_offset(&ctx.input, stmt.span().start());
        let comment = ctx.text_between(offset);

        code += &comment;
        code += &*stmt.to_token_stream().to_string();

        ctx.update_offset(byte_offset(&ctx.input, stmt.span().end()));
    }

    code += "}";

    Ok(code)
}

trait Fragments {
    fn _parse(ctx: &mut ParseContext, items: Vec<Item>) -> anyhow::Result<Self>
    where
        Self: Sized;

    fn parse(input: &str) -> anyhow::Result<Self>
    where
        Self: Sized;

    fn dedup_fragments(self) -> Self
    where
        Self: Sized;
}

impl Fragments for Vec<Fragment> {
    fn _parse(ctx: &mut ParseContext, items: Vec<Item>) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let mut fragments = vec![];

        for item in items {
            let offset = byte_offset(&ctx.input, item.span().start());
            let comment = ctx.text_between(offset);
            let span_end = item.span().end();

            match item {
                Item::Fn(mut item) => {
                    let sig = item.sig.ident.to_token_stream().to_string();
                    if sig == "main" {
                        let content = comment + parse_fn_code(ctx, item)?.as_str();

                        fragments.push(Fragment::EntryPoint { content });
                    } else {
                        item.vis = Visibility::Public(Token![pub](Span::call_site()));
                        let content = comment + parse_fn_code(ctx, item)?.as_str();

                        let is_api_resource = content.contains("GET")
                            || content.contains("POST")
                            || content.contains("PUT")
                            || content.contains("PATCH")
                            || content.contains("DELETE")
                            || content.contains("web ::")
                            || content.contains("HttpResponse")
                            || content.contains("actix_web ::");

                        if is_api_resource {
                            fragments.push(Fragment::ApiResource { name: sig, content });
                        } else {
                            fragments.push(Fragment::Function { name: sig, content });
                        }
                    }
                }
                Item::Struct(mut item) => {
                    item.vis = Visibility::Public(Token![pub](Span::call_site()));
                    item.fields.iter_mut().for_each(|field| {
                        field.vis = Visibility::Public(Token![pub](Span::call_site()));
                    });
                    let sig = item.ident.to_token_stream().to_string().to_snake_case();
                    let content = comment + item.to_token_stream().to_string().as_str();

                    fragments.push(Fragment::Model { name: sig, content });
                }
                Item::Enum(mut item) => {
                    item.vis = Visibility::Public(Token![pub](Span::call_site()));
                    let sig = item.ident.to_token_stream().to_string().to_snake_case();
                    fragments.push(Fragment::Model {
                        name: sig,
                        content: comment + &*item.to_token_stream().to_string(),
                    });
                }
                Item::Const(mut item) => {
                    item.vis = Visibility::Public(Token![pub](Span::call_site()));
                    fragments.push(Fragment::Const {
                        content: comment + &*item.to_token_stream().to_string(),
                    });
                }
                Item::Impl(item) => {
                    let sig = item.self_ty.to_token_stream().to_string().to_snake_case();
                    fragments.push(Fragment::Model {
                        name: sig,
                        content: comment + &*item.to_token_stream().to_string(),
                    });
                }
                Item::Macro(item) => {
                    fragments.push(Fragment::Common {
                        content: comment + &*item.to_token_stream().to_string(),
                    });
                }
                Item::Mod(item) => {
                    let sig = item.ident.to_token_stream().to_string().to_snake_case();
                    fragments.push(Fragment::Mod {
                        name: sig,
                        fragments: item
                            .content
                            .map(|(brace, items)| {
                                ctx.update_offset(byte_offset(&ctx.input, brace.span.open().end()));
                                Self::_parse(ctx, items)
                            })
                            .transpose()?,
                    });
                }
                Item::Static(mut item) => {
                    item.vis = Visibility::Public(Token![pub](Span::call_site()));
                    fragments.push(Fragment::Const {
                        content: comment + &*item.to_token_stream().to_string(),
                    });
                }
                Item::Trait(mut item) => {
                    item.vis = Visibility::Public(Token![pub](Span::call_site()));
                    let sig = item.ident.to_token_stream().to_string().to_snake_case();
                    fragments.push(Fragment::Model {
                        name: sig,
                        content: comment + &*item.to_token_stream().to_string(),
                    });
                }
                Item::TraitAlias(mut item) => {
                    item.vis = Visibility::Public(Token![pub](Span::call_site()));
                    let sig = item.ident.to_token_stream().to_string().to_snake_case();
                    fragments.push(Fragment::Model {
                        name: sig,
                        content: comment + &*item.to_token_stream().to_string(),
                    });
                }
                Item::Type(mut item) => {
                    item.vis = Visibility::Public(Token![pub](Span::call_site()));
                    let sig = item.ident.to_token_stream().to_string().to_snake_case();
                    fragments.push(Fragment::Model {
                        name: sig,
                        content: comment + &*item.to_token_stream().to_string(),
                    });
                }
                Item::Use(item) => {
                    fragments.push(Fragment::Use {
                        content: comment + &*item.to_token_stream().to_string(),
                    });
                }
                Item::Union(mut item) => {
                    item.vis = Visibility::Public(Token![pub](Span::call_site()));
                    let sig = item.ident.to_token_stream().to_string().to_snake_case();
                    fragments.push(Fragment::Model {
                        name: sig,
                        content: comment + &*item.to_token_stream().to_string(),
                    });
                }
                _ => {
                    fragments.push(Fragment::Common {
                        content: comment + &*item.to_token_stream().to_string(),
                    });
                }
            }

            ctx.update_offset(byte_offset(&ctx.input, span_end));
        }

        Ok(fragments)
    }

    fn parse(input: &str) -> anyhow::Result<Self> {
        let ast = syn::parse_file(input)?;
        let mut ctx = ParseContext {
            input: input.to_string(),
            current_offset: 0,
        };
        Self::_parse(&mut ctx, ast.items)
    }

    fn dedup_fragments(self) -> Self
    where
        Self: Sized,
    {
        let mut fragments = vec![];
        let mut models_by_name = HashMap::<String, Vec<Fragment>>::new();

        for fragment in self {
            match &fragment {
                Fragment::Model { name, .. } => models_by_name
                    .entry(name.to_owned())
                    .or_default()
                    .push(fragment),
                _ => {
                    fragments.push(fragment);
                }
            }
        }

        let models = models_by_name
            .into_iter()
            .map(|(name, v)| -> Fragment {
                let content = v
                    .into_iter()
                    .map(|fragment| match fragment {
                        Fragment::Model { content, .. } => content,
                        _ => "".to_string(),
                    })
                    .join("\n");
                Fragment::Model { name, content }
            })
            .collect_vec();

        fragments.extend(models);

        fragments
    }
}

enum Module {
    EntryPoint {
        content: String,
    },
    Lib {
        name: String,
        content: String,
        modules: Vec<Module>,
    },
}

impl Module {
    fn write(&self, path: impl Into<PathBuf>) -> anyhow::Result<()> {
        match self {
            Module::EntryPoint { content } => {
                let path = path.into().join("main.rs");
                fs::write(path, content)?;
            }
            Module::Lib {
                name,
                content,
                modules,
            } => {
                let path = path.into();

                let mod_path = path.join(name);

                if !modules.is_empty() {
                    fs::create_dir_all(&mod_path)?;
                }

                for module in modules {
                    module.write(&mod_path)?;
                }
                let path = path.join(format!("{}.rs", name));
                fs::write(path, content)?;
            }
        }
        Ok(())
    }
}

trait Modules {
    fn parse(fragments: Vec<Fragment>) -> anyhow::Result<Self>
    where
        Self: Sized;

    fn mod_text(&self) -> String;

    fn write(&self, path: impl Into<PathBuf>) -> anyhow::Result<()>;
}

impl Modules for Vec<Module> {
    fn parse(fragments: Vec<Fragment>) -> anyhow::Result<Self>
    where
        Self: Sized,
    {
        let fragments = fragments.dedup_fragments();

        let use_text = fragments
            .iter()
            .filter_map(|fragment| match fragment {
                Fragment::Use { content } => Some(content.to_string()),
                _ => None,
            })
            .join("\n");

        let lib_use_text = format!("{}\nuse crate::*;", use_text);

        let modules = fragments
            .iter()
            .filter_map(|fragment| match fragment {
                Fragment::Function { name, content } => Some(Module::Lib {
                    name: name.to_string(),
                    content: lib_use_text.to_owned() + content,
                    modules: vec![],
                }),
                _ => None,
            })
            .collect_vec();

        let functions = Module::Lib {
            name: "functions".to_string(),
            content: modules.mod_text(),
            modules,
        };

        let modules = fragments
            .iter()
            .filter_map(|fragment| match fragment {
                Fragment::ApiResource { name, content } => Some(Module::Lib {
                    name: name.to_string(),
                    content: lib_use_text.to_owned() + content,
                    modules: vec![],
                }),
                _ => None,
            })
            .collect_vec();

        let api_resources = Module::Lib {
            name: "resources".to_string(),
            content: modules.mod_text(),
            modules,
        };

        let modules = fragments
            .iter()
            .filter_map(|fragment| match fragment {
                Fragment::Model { name, content } => Some(Module::Lib {
                    name: name.to_string(),
                    content: lib_use_text.to_owned() + content,
                    modules: vec![],
                }),
                _ => None,
            })
            .collect_vec();

        let models = Module::Lib {
            name: "models".to_string(),
            content: modules.mod_text(),
            modules,
        };

        let consts = Module::Lib {
            name: "consts".to_string(),
            content: lib_use_text.to_owned()
                + fragments
                    .iter()
                    .filter_map(|fragment| match fragment {
                        Fragment::Const { content } => Some(content.to_string()),
                        _ => None,
                    })
                    .join("\n")
                    .as_str(),
            modules: vec![],
        };

        let modules = fragments
            .iter()
            .map(|fragment| {
                Ok(match fragment {
                    Fragment::Mod {
                        name,
                        fragments: Some(fragments),
                    } => {
                        let modules = Vec::<Module>::parse(fragments.to_owned())?;
                        Some(Module::Lib {
                            name: name.to_string(),
                            content: modules.mod_text(),
                            modules,
                        })
                    }
                    _ => None,
                })
            })
            .collect::<anyhow::Result<Vec<Option<Module>>>>()?
            .into_iter()
            .flatten()
            .collect_vec();

        let mut res = vec![api_resources, functions, models, consts];

        let content = fragments
            .iter()
            .filter_map(|fragment| match fragment {
                Fragment::Common { content } => Some(content.to_string()),
                _ => None,
            })
            .join("\n");

        if !content.is_empty() {
            let common = Module::Lib {
                name: "common".to_string(),
                content: lib_use_text.to_owned() + content.as_str(),
                modules: vec![],
            };
            res.push(common);
        }

        res.extend(modules);

        let entry_point = Module::EntryPoint {
            content: use_text.to_owned()
                + res.mod_text().as_str()
                + fragments
                    .iter()
                    .filter_map(|fragment| match fragment {
                        Fragment::EntryPoint { content } => Some(content.to_string()),
                        Fragment::Mod {
                            name,
                            fragments: None,
                        } => Some(format!("mod {};", name)),
                        _ => None,
                    })
                    .join("\n")
                    .as_str(),
        };

        res.push(entry_point);

        Ok(res)
    }

    fn mod_text(&self) -> String {
        self.iter()
            .map(|module| match module {
                Module::Lib { name, .. } => {
                    format!("pub mod {}; pub use self::{}::*;", name, name)
                }
                _ => "".to_string(),
            })
            .join("\n")
    }

    fn write(&self, path: impl Into<PathBuf>) -> anyhow::Result<()> {
        let path = path.into();
        for module in self {
            module.write(&path)?;
        }
        Ok(())
    }
}

struct Code {
    modules: Vec<Module>,
}

impl Code {
    pub fn parse(path: impl Into<PathBuf>) -> anyhow::Result<Self> {
        let path = path.into().to_string_lossy().to_string();

        let content = fs::read_to_string(path)?;

        let fragments = Vec::<Fragment>::parse(&content)?;
        let modules = Vec::<Module>::parse(fragments)?;

        Ok(Self { modules })
    }

    pub fn write(&self, path: impl Into<PathBuf>) -> anyhow::Result<()> {
        let path = path.into();
        self.modules.write(path)?;
        Ok(())
    }
}

#[derive(StructOpt, Debug)]
#[structopt(name = "isucon-restruct")]
struct Opt {
    #[structopt(short, long)]
    path: PathBuf,
}

fn main() -> anyhow::Result<()> {
    let opt: Opt = Opt::from_args();

    let path = opt.path;

    let src_path = path.join("src");
    let entry_point_path = src_path.join("main.rs");

    if !entry_point_path.exists() {
        eprintln!("main.rs does not exist");
        return Ok(());
    }

    let code = Code::parse(entry_point_path)?;

    code.write(&src_path)?;

    Ok(())
}
