use swc_ecmascript::ast::{
    ArrayLit, BindingIdent, BlockStmt, ClassDecl, Decl, ExportDecl, Expr, ExprOrSpread, FnDecl,
    Function, Ident, ImportDecl, JSXAttr, JSXAttrName, JSXAttrOrSpread, JSXClosingElement,
    JSXElement, JSXElementName, JSXExpr, JSXExprContainer, JSXOpeningElement, JSXText,
    KeyValueProp, MemberExpr, Module, ModuleItem, NamedExport, ObjectLit, Param, ParenExpr,
    PropOrSpread, ReturnStmt, Stmt, Str, TsKeywordTypeKind, TsType, VarDecl, VarDeclKind,
    VarDeclarator,
};
use swc_ecmascript::common::Span;

#[derive(Debug, PartialEq, Eq)]
pub struct Helper {}
impl Helper {
    pub fn new() -> Self {
        Self {}
    }
    pub fn parse_bindingident(&self, bident: BindingIdent) -> String {
        let mut mstring = String::new();
        let BindingIdent { id, type_ann } = bident;
        mstring = mstring + " " + &id.sym;
        match type_ann {
            Some(a) => {
                let type_ann = a.type_ann;
                mstring = mstring + ": " + &self.parse_ts_type(type_ann) + " = ";
            }
            None => {}
        }
        mstring
    }
    pub fn parse_ts_type(&self, tstype: Box<TsType>) -> String {
        let mut mstring = String::new();
        match *tstype {
            TsType::TsKeywordType(a) => match a.kind {
                TsKeywordTypeKind::TsAnyKeyword => mstring = format!("{}any", mstring),
                TsKeywordTypeKind::TsUnknownKeyword => mstring = format!("{}unknown", mstring),
                TsKeywordTypeKind::TsNumberKeyword => mstring = format!("{}number", mstring),
                TsKeywordTypeKind::TsObjectKeyword => mstring = format!("{}object", mstring),
                TsKeywordTypeKind::TsBooleanKeyword => mstring = format!("{}boolean", mstring),
                TsKeywordTypeKind::TsBigIntKeyword => mstring = format!("{}bigint", mstring),
                TsKeywordTypeKind::TsStringKeyword => mstring = format!("{}string", mstring),
                TsKeywordTypeKind::TsSymbolKeyword => mstring = format!("{}symbol", mstring),
                TsKeywordTypeKind::TsVoidKeyword => mstring = format!("{}void", mstring),
                TsKeywordTypeKind::TsUndefinedKeyword => mstring = format!("{}undefined", mstring),
                TsKeywordTypeKind::TsNullKeyword => mstring = format!("{}null", mstring),
                TsKeywordTypeKind::TsNeverKeyword => mstring = format!("{}never", mstring),
                TsKeywordTypeKind::TsIntrinsicKeyword => mstring = format!("{}intrinsic", mstring),
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
            mstring = format!("{}{}", mstring, &self.parse_stmt(i));
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
        mstring = format!("{}export default function {}(", mstring, &ident.sym,);
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
        mstring = format!("{}{}", mstring, &self.parse_param(&params));
        mstring = format!("{}){{", mstring);

        match body {
            Some(a) => {
                mstring = format!("{}\n", mstring);
                mstring = format!("{}{}", mstring, &self.parse_block(a));
            }
            None => todo!(),
        }

        mstring = format!("{}}}", mstring);
        mstring
    }
    fn parse_expr(&self, expr: Expr, jsx: bool) -> String {
        let mut mstring = String::new();
        match expr {
            Expr::This(a) => mstring = mstring + "this",
            Expr::Array(a) => {
                let ArrayLit { span, elems } = a;
                let mut i = String::from(" = [");
                for u in elems.iter() {
                    match u {
                        Some(a) => {
                            let ExprOrSpread { spread, expr } = a;
                            i = i + &self.parse_expr(*expr.to_owned(), false) + ","
                        }
                        None => todo!(),
                    }
                }
                mstring = mstring + &i + "]";
            }
            Expr::Object(a) => {
                let ObjectLit { span, props } = a;
                let mut mstringi = String::from("");
                if jsx {
                    mstringi = "= {{".to_string()
                } else {
                    mstringi = "= {".to_string()
                }
                for abc in 0..props.len() {
                    let u = &props[abc];

                    match u {
                        PropOrSpread::Spread(a) => {}
                        PropOrSpread::Prop(a) => match *a.to_owned() {
                            swc_ecmascript::ast::Prop::Shorthand(_) => todo!(),
                            swc_ecmascript::ast::Prop::KeyValue(b) => {
                                let KeyValueProp { key, value } = b;
                                match key {
                                    swc_ecmascript::ast::PropName::Ident(a) => {
                                        let z = BindingIdent {
                                            id: a,
                                            type_ann: None,
                                        };
                                        mstringi = format!(
                                            "{}{}: ",
                                            mstringi,
                                            self.helper.parse_bindingident(z)
                                        )
                                    }
                                    swc_ecmascript::ast::PropName::Str(c) => {
                                        let Str {
                                            span,
                                            value,
                                            has_escape,
                                            kind,
                                        } = c;
                                        mstringi = format!("{}\"{}\":", mstringi, value)
                                    }
                                    swc_ecmascript::ast::PropName::Num(_) => todo!(),
                                    swc_ecmascript::ast::PropName::Computed(_) => todo!(),
                                    swc_ecmascript::ast::PropName::BigInt(_) => todo!(),
                                }
                                mstringi =
                                    format!("{}{}", mstringi, self.parse_expr(*value, false),);
                                if abc <= 1 && props.len() != abc + 1 {
                                    mstringi = format!("{},", mstringi)
                                }
                            }
                            swc_ecmascript::ast::Prop::Assign(_) => todo!(),
                            swc_ecmascript::ast::Prop::Getter(_) => todo!(),
                            swc_ecmascript::ast::Prop::Setter(_) => todo!(),
                            swc_ecmascript::ast::Prop::Method(_) => todo!(),
                        },
                    }
                }
                if jsx {
                    mstring = mstring + &mstringi + "}}";
                } else {
                    mstring = mstring + &mstringi + "}";
                }
            }
            Expr::Fn(_) => todo!(),
            Expr::Unary(_) => todo!(),
            Expr::Update(_) => todo!(),
            Expr::Bin(_) => todo!(),
            Expr::Assign(_) => todo!(),
            Expr::Member(a) => {
                let MemberExpr {
                    span,
                    obj,
                    prop,
                    computed,
                } = a;
                match obj {
                    swc_ecmascript::ast::ExprOrSuper::Super(a) => mstring = mstring + "super",
                    swc_ecmascript::ast::ExprOrSuper::Expr(_) => mstring = mstring + "this",
                }
                mstring = mstring + "." + &self.parse_expr(*prop, false);
            }
            Expr::Cond(_) => todo!(),
            Expr::Call(_) => todo!(),
            Expr::New(_) => todo!(),
            Expr::Seq(_) => todo!(),
            Expr::Ident(a) => {
                let Ident {
                    span,
                    sym,
                    optional,
                } = a;
                mstring = format!("{}{}", mstring, sym)
            }
            Expr::Lit(a) => match a {
                swc_ecmascript::ast::Lit::Str(a) => mstring = format!("{}\"{}\"", mstring, a.value),
                swc_ecmascript::ast::Lit::Bool(_) => todo!(),
                swc_ecmascript::ast::Lit::Null(a) => mstring = format!("{}{}", mstring, "null"),
                swc_ecmascript::ast::Lit::Num(a) => mstring = format!("{}{}", mstring, a.value),
                swc_ecmascript::ast::Lit::BigInt(_) => todo!(),
                swc_ecmascript::ast::Lit::Regex(_) => todo!(),
                swc_ecmascript::ast::Lit::JSXText(_) => todo!(),
            },
            Expr::Tpl(_) => todo!(),
            Expr::TaggedTpl(_) => todo!(),
            Expr::Arrow(_) => todo!(),
            Expr::Class(_) => todo!(),
            Expr::Yield(_) => todo!(),
            Expr::MetaProp(_) => todo!(),
            Expr::Await(_) => todo!(),
            Expr::Paren(a) => {
                let ParenExpr { span, expr } = a;
                mstring = format!("{}{}", mstring, self.parse_expr(*expr, false));
            }
            Expr::JSXMember(_) => todo!(),
            Expr::JSXNamespacedName(_) => todo!(),
            Expr::JSXEmpty(_) => todo!(),
            Expr::JSXElement(a) => {
                let JSXElement {
                    span,
                    opening,
                    children,
                    closing,
                } = *a;

                let JSXOpeningElement {
                    name,
                    span,
                    attrs,
                    self_closing,
                    type_args,
                } = opening;
                match name {
                    JSXElementName::Ident(a) => {
                        let Ident {
                            span,
                            sym,
                            optional,
                        } = a;
                        mstring = format!("{}<{} ", mstring, sym)
                    }
                    JSXElementName::JSXMemberExpr(b) => todo!(),
                    JSXElementName::JSXNamespacedName(c) => todo!(),
                }
                for attr in attrs.iter() {
                    match attr {
                        JSXAttrOrSpread::JSXAttr(a) => {
                            let JSXAttr { span, name, value } = a;
                            match name {
                                JSXAttrName::Ident(a) => {
                                    mstring = format!(" {}{}", mstring, a.sym);
                                    match value {
                                        Some(a) => match a {
                                            swc_ecmascript::ast::JSXAttrValue::Lit(_) => todo!(),
                                            swc_ecmascript::ast::JSXAttrValue::JSXExprContainer(
                                                a,
                                            ) => {
                                                let JSXExprContainer { span, expr } = a;
                                                match expr {
                                                    JSXExpr::JSXEmptyExpr(a) => todo!(),
                                                    JSXExpr::Expr(a) => {
                                                        let parsed_expr =
                                                            self.parse_expr(*a.to_owned(), true);

                                                        mstring =
                                                            format!("{}{}", mstring, parsed_expr)
                                                    }
                                                }
                                            }
                                            swc_ecmascript::ast::JSXAttrValue::JSXElement(_) => {
                                                todo!()
                                            }
                                            swc_ecmascript::ast::JSXAttrValue::JSXFragment(_) => {
                                                todo!()
                                            }
                                        },
                                        None => todo!(),
                                    }
                                }
                                JSXAttrName::JSXNamespacedName(_) => todo!(),
                            }
                        }
                        JSXAttrOrSpread::SpreadElement(_) => todo!(),
                    }
                }
                if self_closing {
                    mstring = format!("{}{}", mstring, "/>")
                } else {
                    mstring = format!("{}{}", mstring, ">")
                }
                for child in children.iter() {
                    match child {
                        swc_ecmascript::ast::JSXElementChild::JSXText(a) => {
                            let JSXText { span, value, raw } = a;
                            mstring = format!("{}{}", mstring, value)
                        }
                        swc_ecmascript::ast::JSXElementChild::JSXExprContainer(_) => todo!(),
                        swc_ecmascript::ast::JSXElementChild::JSXSpreadChild(_) => todo!(),
                        swc_ecmascript::ast::JSXElementChild::JSXElement(_) => todo!(),
                        swc_ecmascript::ast::JSXElementChild::JSXFragment(_) => todo!(),
                    }
                }
                match closing {
                    Some(a) => {
                        let JSXClosingElement { span, name } = a;
                        match name {
                            JSXElementName::Ident(a) => {
                                let Ident {
                                    span,
                                    sym,
                                    optional,
                                } = a;
                                mstring = format!("{}</{}>", mstring, sym)
                            }
                            JSXElementName::JSXMemberExpr(_) => todo!(),
                            JSXElementName::JSXNamespacedName(_) => todo!(),
                        }
                    }
                    None => todo!(),
                }
            }
            Expr::JSXFragment(_) => todo!(),
            Expr::TsTypeAssertion(_) => todo!(),
            Expr::TsConstAssertion(_) => todo!(),
            Expr::TsNonNull(_) => todo!(),
            Expr::TsAs(_) => todo!(),
            Expr::PrivateName(_) => todo!(),
            Expr::OptChain(_) => todo!(),
            Expr::Invalid(_) => todo!(),
        }
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
                mstring = format!("{}{}", mstring, self.helper.parse_bindingident(a));
            }
            swc_ecmascript::ast::Pat::Array(_) => todo!(),
            swc_ecmascript::ast::Pat::Rest(_) => todo!(),
            swc_ecmascript::ast::Pat::Object(_) => todo!(),
            swc_ecmascript::ast::Pat::Assign(_) => todo!(),
            swc_ecmascript::ast::Pat::Invalid(_) => todo!(),
            swc_ecmascript::ast::Pat::Expr(_) => todo!(),
        };
        match init {
            Some(a) => mstring = mstring + &self.parse_expr(*a, false) + ";\n",
            None => todo!(),
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
            VarDeclKind::Var => mstring = format!("{}var", mstring),
            VarDeclKind::Let => mstring = format!("{}let", mstring),
            VarDeclKind::Const => mstring = format!("{}const", mstring),
        };

        for i in decls.iter() {
            mstring = format!("{}{}", mstring, self.parse_var_decl(i.to_owned()));
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
                mstring = format!("{}{}", mstring, &self.parse_function(fndecl));
            }
            swc_ecmascript::ast::Decl::Var(s) => {
                mstring = format!("{}{}", mstring, &self.parse_var(s))
            }
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
            Decl::Fn(e) => mstring = format!("{}{}", mstring, &self.parse_function(e.to_owned())),
            Decl::Var(a) => mstring = format!("{}{}", mstring, &self.parse_var(a.to_owned())),
            Decl::TsInterface(_) => todo!(),
            Decl::TsTypeAlias(_) => todo!(),
            Decl::TsEnum(_) => todo!(),
            Decl::TsModule(_) => todo!(),
        }
        mstring
    }
    fn parse_ret_statement(&self, ret: ReturnStmt) -> String {
        let mut mstring = String::new();
        let ReturnStmt { span, arg } = ret;
        match arg {
            Some(a) => mstring = format!("{}return {};", mstring, self.parse_expr(*a, false)),
            None => todo!(),
        }
        mstring
    }
    fn parse_stmt(&self, stmt: Stmt) -> String {
        let mut mstring = String::new();
        match stmt {
            swc_ecmascript::ast::Stmt::Block(a) => {
                mstring = format!("{}{}", mstring, self.parse_block(a))
            }
            swc_ecmascript::ast::Stmt::Empty(_) => todo!(),
            swc_ecmascript::ast::Stmt::Debugger(_) => todo!(),
            swc_ecmascript::ast::Stmt::With(_) => todo!(),
            swc_ecmascript::ast::Stmt::Return(a) => {
                mstring = format!("{}{}", mstring, self.parse_ret_statement(a))
            }
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
                mstring = format!("{}{}", mstring, &self.parse_decl(&decl));
            }
            swc_ecmascript::ast::Stmt::Expr(a) => {
                let a = a.expr;
                mstring = mstring + &self.parse_expr(*a, false)
            }
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
                        mstring =
                            format!("{}{}", mstring, &self.parse_import(importdecl.to_owned()))
                    }
                    swc_ecmascript::ast::ModuleDecl::ExportDecl(e) => {
                        mstring = format!("{}{}", mstring, &self.parse_export(e.to_owned()));
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
                    mstring = format!("{}{}", mstring, &self.parse_stmt(a.to_owned()))
                }
            };
        }
        mstring
    }
}
