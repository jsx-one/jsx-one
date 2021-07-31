use swc_common::Span;
use swc_ecmascript::ast::{
    ClassDecl, Decl, ExportDecl, FnDecl, Function, ImportDecl, Module, ModuleItem, NamedExport,
    Param,
};
#[derive(Debug, PartialEq, Eq)]
pub struct ReactCodgen {
    span: Span,
    body: Vec<ModuleItem>,
    mods: Module,
}

impl ReactCodgen {
    pub fn new(mods: Module) -> Self {
        let Module {
            span,
            body,
            shebang,
        } = mods.clone();
        Self { span, body, mods }
    }
    fn parse_import(&self, import: ImportDecl) -> String {
        let mut stri = String::new();
        let ImportDecl {
            span,
            specifiers,
            src,
            type_only,
            asserts,
        } = import;
        stri = stri + "import ";
        for ident in 0..specifiers.len() {
            let i = &specifiers[ident];
            match i {
                swc_ecmascript::ast::ImportSpecifier::Named(a) => {
                    stri = stri + "{ ";
                    if ident == 0 {
                        stri = stri + &a.local.sym;
                    } else {
                        stri = stri + ", " + &a.local.sym;
                    }
                    stri = stri + " }";
                }
                swc_ecmascript::ast::ImportSpecifier::Default(a) => {
                    if ident == 0 {
                        stri = stri + &a.local.sym;
                    } else {
                        stri = stri + ", " + &a.local.sym;
                    }
                }
                swc_ecmascript::ast::ImportSpecifier::Namespace(a) => {
                    stri = stri + "* as " + &a.local.sym;
                }
            }
        }
        stri = stri;
        stri = stri + " from \"" + &src.value + "\";";
        stri
    }
    fn parse_param(param: &Vec<Param>) -> String {
        let mut mstring = String::new();
        for parm in param.iter() {
            let Param {
                span,
                decorators,
                pat,
            } = parm;
        }
        mstring
    }
    fn parse_function(&self, fndecl: FnDecl) -> String {
        let mut mstring = String::new();
        let FnDecl {
            ident,
            declare,
            function,
        } = fndecl;
        mstring = mstring + "export default function " + &ident.sym + "(";
        let Function {
            params,
            decorators,
            span,
            body,
            is_generator,
            is_async,
            type_params,
            return_type,
        } = function;

        mstring = mstring + ")" + "{";
        mstring = mstring + "}";
        mstring
    }
    /// TODO: Complete this after all the functions for parsing is completed (lazy rn)
    fn parse_export(&self, export: ExportDecl) -> String {
        let mut mstring = String::new();
        let ExportDecl { span, decl } = export;
        match decl {
            swc_ecmascript::ast::Decl::Class(_) => {
                todo!()
            }
            swc_ecmascript::ast::Decl::Fn(fndecl) => {
                mstring = mstring + &self.parse_function(fndecl);
            }
            swc_ecmascript::ast::Decl::Var(_) => todo!(),
            swc_ecmascript::ast::Decl::TsInterface(_) => todo!(),
            swc_ecmascript::ast::Decl::TsTypeAlias(_) => todo!(),
            swc_ecmascript::ast::Decl::TsEnum(_) => todo!(),
            swc_ecmascript::ast::Decl::TsModule(_) => todo!(),
        };
        mstring
    }
    fn parse_decl(&self, decl: &Decl) -> String {
        let mut mstring = String::new();
        match decl {
            Decl::Class(_) => todo!(),
            Decl::Fn(e) => mstring = mstring + &self.parse_function(e.to_owned()),
            Decl::Var(_) => todo!(),
            Decl::TsInterface(_) => todo!(),
            Decl::TsTypeAlias(_) => todo!(),
            Decl::TsEnum(_) => todo!(),
            Decl::TsModule(_) => todo!(),
        }
        mstring
    }
    pub fn parse_react(&self) -> String {
        let mut mstring = String::new();
        let Module {
            span,
            body,
            shebang,
        } = &self.mods;
        for bo in body.iter() {
            match bo {
                swc_ecmascript::ast::ModuleItem::ModuleDecl(modecl) => match modecl {
                    swc_ecmascript::ast::ModuleDecl::Import(importdecl) => {
                        mstring = mstring + &self.parse_import(importdecl.to_owned())
                    }
                    swc_ecmascript::ast::ModuleDecl::ExportDecl(e) => {
                        mstring = mstring + &self.parse_export(e.to_owned());
                    }
                    swc_ecmascript::ast::ModuleDecl::ExportNamed(b) => {
                        let NamedExport {
                            span,
                            specifiers,
                            src,
                            type_only,
                            asserts,
                        } = b;
                        todo!()
                    }
                    swc_ecmascript::ast::ModuleDecl::ExportDefaultDecl(_) => todo!(),
                    swc_ecmascript::ast::ModuleDecl::ExportDefaultExpr(_) => todo!(),
                    swc_ecmascript::ast::ModuleDecl::ExportAll(_) => todo!(),
                    swc_ecmascript::ast::ModuleDecl::TsImportEquals(_) => todo!(),
                    swc_ecmascript::ast::ModuleDecl::TsExportAssignment(_) => todo!(),
                    swc_ecmascript::ast::ModuleDecl::TsNamespaceExport(_) => todo!(),
                },
                swc_ecmascript::ast::ModuleItem::Stmt(a) => match a {
                    swc_ecmascript::ast::Stmt::Block(_) => todo!(),
                    swc_ecmascript::ast::Stmt::Empty(_) => todo!(),
                    swc_ecmascript::ast::Stmt::Debugger(_) => todo!(),
                    swc_ecmascript::ast::Stmt::With(_) => todo!(),
                    swc_ecmascript::ast::Stmt::Return(_) => todo!(),
                    swc_ecmascript::ast::Stmt::Labeled(_) => todo!(),
                    swc_ecmascript::ast::Stmt::Break(_) => todo!(),
                    swc_ecmascript::ast::Stmt::Continue(_) => todo!(),
                    swc_ecmascript::ast::Stmt::If(_) => todo!(),
                    swc_ecmascript::ast::Stmt::Switch(_) => todo!(),
                    swc_ecmascript::ast::Stmt::Throw(_) => todo!(),
                    swc_ecmascript::ast::Stmt::Try(_) => todo!(),
                    swc_ecmascript::ast::Stmt::While(_) => todo!(),
                    swc_ecmascript::ast::Stmt::DoWhile(_) => todo!(),
                    swc_ecmascript::ast::Stmt::For(_) => todo!(),
                    swc_ecmascript::ast::Stmt::ForIn(_) => todo!(),
                    swc_ecmascript::ast::Stmt::ForOf(_) => todo!(),
                    swc_ecmascript::ast::Stmt::Decl(decl) => {
                        mstring = mstring + &self.parse_decl(decl);
                    }
                    swc_ecmascript::ast::Stmt::Expr(_) => todo!(),
                },
            };
        }
        mstring
    }
}
