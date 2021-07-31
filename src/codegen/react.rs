use swc_common::Span;
use swc_ecmascript::ast::{
    BindingIdent, BlockStmt, ClassDecl, Decl, ExportDecl, FnDecl, Function, ImportDecl, Module,
    ModuleItem, NamedExport, Param, Stmt, TsKeywordTypeKind, TsType, VarDecl, VarDeclKind,
    VarDeclarator,
};

#[derive(Debug, PartialEq, Eq)]
pub struct Helper {}
impl Helper {
    pub fn new() -> Self {
        Self {}
    }
    pub fn parse_bindingident(&self, bident: BindingIdent) -> String {
        let mut mstring = String::new();
        let BindingIdent { id, type_ann } = bident;
        mstring = format!("{} {}", mstring, &id.sym);
        match type_ann {
            Some(a) => {
                let type_ann = a.type_ann;
                mstring = mstring + ": " + &self.parse_ts_type(type_ann);
            }
            None => todo!(),
        }
        mstring
    }
    pub fn parse_ts_type(&self, tstype: Box<TsType>) -> String {
        let mut mstring = String::new();
        match *tstype {
            TsType::TsKeywordType(a) => match a.kind {
                TsKeywordTypeKind::TsAnyKeyword => mstring = mstring + "any",
                TsKeywordTypeKind::TsUnknownKeyword => todo!(),
                TsKeywordTypeKind::TsNumberKeyword => mstring = mstring + "number",
                TsKeywordTypeKind::TsObjectKeyword => todo!(),
                TsKeywordTypeKind::TsBooleanKeyword => todo!(),
                TsKeywordTypeKind::TsBigIntKeyword => todo!(),
                TsKeywordTypeKind::TsStringKeyword => todo!(),
                TsKeywordTypeKind::TsSymbolKeyword => todo!(),
                TsKeywordTypeKind::TsVoidKeyword => todo!(),
                TsKeywordTypeKind::TsUndefinedKeyword => todo!(),
                TsKeywordTypeKind::TsNullKeyword => todo!(),
                TsKeywordTypeKind::TsNeverKeyword => todo!(),
                TsKeywordTypeKind::TsIntrinsicKeyword => todo!(),
            },
            TsType::TsThisType(_) => todo!(),
            TsType::TsFnOrConstructorType(_) => todo!(),
            TsType::TsTypeRef(_) => todo!(),
            TsType::TsTypeQuery(_) => todo!(),
            TsType::TsTypeLit(_) => todo!(),
            TsType::TsArrayType(_) => todo!(),
            TsType::TsTupleType(_) => todo!(),
            TsType::TsOptionalType(_) => todo!(),
            TsType::TsRestType(_) => todo!(),
            TsType::TsUnionOrIntersectionType(_) => todo!(),
            TsType::TsConditionalType(_) => todo!(),
            TsType::TsInferType(_) => todo!(),
            TsType::TsParenthesizedType(_) => todo!(),
            TsType::TsTypeOperator(_) => todo!(),
            TsType::TsIndexedAccessType(_) => todo!(),
            TsType::TsMappedType(_) => todo!(),
            TsType::TsLitType(_) => todo!(),
            TsType::TsTypePredicate(_) => todo!(),
            TsType::TsImportType(_) => todo!(),
        }
        mstring
    }
}

#[derive(Debug, PartialEq, Eq)]
pub struct ReactCodgen {
    span: Span,
    helper: Helper,
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
        Self {
            span,
            body,
            mods,
            helper: Helper::new(),
        }
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
    fn parse_param(&self, param: &Vec<Param>) -> String {
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
    fn parse_block(&self, block: BlockStmt) -> String {
        let mut mstring = String::new();
        let BlockStmt { span, stmts } = block;
        for i in stmts {
            mstring = mstring + &self.parse_stmt(i);
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
        mstring = mstring + &self.parse_param(&params);
        mstring = mstring + ")" + "{";

        match body {
            Some(a) => {
                mstring = mstring + "\n";
                mstring = mstring + &self.parse_block(a);
            }
            None => todo!(),
        }
        mstring = mstring + "}";
        mstring
    }
    fn parse_var_decl(&self, vardecl: VarDeclarator) -> String {
        let mut mstring = String::new();
        let VarDeclarator {
            span,
            name,
            init,
            definite,
        } = vardecl;
        match name {
            swc_ecmascript::ast::Pat::Ident(a) => {
                mstring = mstring + &self.helper.parse_bindingident(a);
            }
            swc_ecmascript::ast::Pat::Array(_) => todo!(),
            swc_ecmascript::ast::Pat::Rest(_) => todo!(),
            swc_ecmascript::ast::Pat::Object(_) => todo!(),
            swc_ecmascript::ast::Pat::Assign(_) => todo!(),
            swc_ecmascript::ast::Pat::Invalid(_) => todo!(),
            swc_ecmascript::ast::Pat::Expr(_) => todo!(),
        }
        mstring
    }
    fn parse_var(&self, var: VarDecl) -> String {
        let mut mstring = String::new();
        let VarDecl {
            span,
            kind,
            declare,
            decls,
        } = var;
        match kind {
            VarDeclKind::Var => mstring = mstring + "var",
            VarDeclKind::Let => mstring = mstring + "let",
            VarDeclKind::Const => mstring = mstring + "const",
        }
        for i in decls.iter() {
            let VarDeclarator {
                span,
                name,
                init,
                definite,
            } = i;
            match name {
                swc_ecmascript::ast::Pat::Ident(a) => {
                    mstring = mstring + &self.helper.parse_bindingident(a.to_owned())
                }
                swc_ecmascript::ast::Pat::Array(_) => todo!(),
                swc_ecmascript::ast::Pat::Rest(_) => todo!(),
                swc_ecmascript::ast::Pat::Object(_) => todo!(),
                swc_ecmascript::ast::Pat::Assign(_) => todo!(),
                swc_ecmascript::ast::Pat::Invalid(_) => todo!(),
                swc_ecmascript::ast::Pat::Expr(_) => todo!(),
            }
        }
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
            swc_ecmascript::ast::Decl::Var(s) => mstring = mstring + &self.parse_var(s),
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
            Decl::Var(a) => mstring = mstring + &self.parse_var(a.to_owned()),
            Decl::TsInterface(_) => todo!(),
            Decl::TsTypeAlias(_) => todo!(),
            Decl::TsEnum(_) => todo!(),
            Decl::TsModule(_) => todo!(),
        }
        mstring
    }
    fn parse_stmt(&self, stmt: Stmt) -> String {
        let mut mstring = String::new();
        match stmt {
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
                mstring = mstring + &self.parse_decl(&decl);
            }
            swc_ecmascript::ast::Stmt::Expr(_) => todo!(),
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
                swc_ecmascript::ast::ModuleItem::Stmt(a) => {
                    mstring = mstring + &self.parse_stmt(a.to_owned())
                }
            };
        }
        mstring
    }
}
