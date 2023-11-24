use heck::ToSnakeCase;
use itertools::Itertools;
use std::collections::HashMap;
use std::fs;
use std::path::PathBuf;
use structopt::StructOpt;
use syn::__private::{Span, ToTokens};
use syn::{Item, Token, Visibility};

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

trait Fragments {
    fn parse(items: Vec<Item>) -> anyhow::Result<Self>
    where
        Self: Sized;

    fn dedup_fragments(self) -> Self
    where
        Self: Sized;
}

impl Fragments for Vec<Fragment> {
    fn parse(items: Vec<Item>) -> anyhow::Result<Self> {
        let mut fragments = vec![];

        for item in items {
            match item {
                Item::Fn(mut item) => {
                    let sig = item.sig.ident.to_token_stream().to_string();
                    if sig == "main" {
                        fragments.push(Fragment::EntryPoint {
                            content: item.to_token_stream().to_string(),
                        });
                        continue;
                    }
                    item.vis = Visibility::Public(Token![pub](Span::call_site()));

                    let content = item.to_token_stream().to_string();

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
                Item::Struct(mut item) => {
                    item.vis = Visibility::Public(Token![pub](Span::call_site()));
                    item.fields.iter_mut().for_each(|field| {
                        field.vis = Visibility::Public(Token![pub](Span::call_site()));
                    });
                    let sig = item.ident.to_token_stream().to_string().to_snake_case();
                    fragments.push(Fragment::Model {
                        name: sig,
                        content: item.to_token_stream().to_string(),
                    });
                }
                Item::Enum(mut item) => {
                    item.vis = Visibility::Public(Token![pub](Span::call_site()));
                    let sig = item.ident.to_token_stream().to_string().to_snake_case();
                    fragments.push(Fragment::Model {
                        name: sig,
                        content: item.to_token_stream().to_string(),
                    });
                }
                Item::Const(mut item) => {
                    item.vis = Visibility::Public(Token![pub](Span::call_site()));
                    fragments.push(Fragment::Const {
                        content: item.to_token_stream().to_string(),
                    });
                }
                Item::Impl(item) => {
                    let sig = item.self_ty.to_token_stream().to_string().to_snake_case();
                    fragments.push(Fragment::Model {
                        name: sig,
                        content: item.to_token_stream().to_string(),
                    });
                }
                Item::Macro(_) => {}
                Item::Mod(item) => {
                    let sig = item.ident.to_token_stream().to_string().to_snake_case();
                    fragments.push(Fragment::Mod {
                        name: sig,
                        fragments: item
                            .content
                            .map(|(_, items)| Self::parse(items))
                            .transpose()?,
                    });
                }
                Item::Static(mut item) => {
                    item.vis = Visibility::Public(Token![pub](Span::call_site()));
                    fragments.push(Fragment::Const {
                        content: item.to_token_stream().to_string(),
                    });
                }
                Item::Trait(mut item) => {
                    item.vis = Visibility::Public(Token![pub](Span::call_site()));
                    let sig = item.ident.to_token_stream().to_string().to_snake_case();
                    fragments.push(Fragment::Model {
                        name: sig,
                        content: item.to_token_stream().to_string(),
                    });
                }
                Item::TraitAlias(mut item) => {
                    item.vis = Visibility::Public(Token![pub](Span::call_site()));
                    let sig = item.ident.to_token_stream().to_string().to_snake_case();
                    fragments.push(Fragment::Model {
                        name: sig,
                        content: item.to_token_stream().to_string(),
                    });
                }
                Item::Type(mut item) => {
                    item.vis = Visibility::Public(Token![pub](Span::call_site()));
                    let sig = item.ident.to_token_stream().to_string().to_snake_case();
                    fragments.push(Fragment::Model {
                        name: sig,
                        content: item.to_token_stream().to_string(),
                    });
                }
                Item::Use(item) => {
                    fragments.push(Fragment::Use {
                        content: item.to_token_stream().to_string(),
                    });
                }
                Item::Union(mut item) => {
                    item.vis = Visibility::Public(Token![pub](Span::call_site()));
                    let sig = item.ident.to_token_stream().to_string().to_snake_case();
                    fragments.push(Fragment::Model {
                        name: sig,
                        content: item.to_token_stream().to_string(),
                    });
                }
                _ => {
                    fragments.push(Fragment::Common {
                        content: item.to_token_stream().to_string(),
                    });
                }
            }
        }

        Ok(fragments)
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

        let lib_use_text = format!("use crate::*;\n{}", use_text);

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
        let ast = syn::parse_file(&content)?;

        let fragments = Vec::<Fragment>::parse(ast.items)?;
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
