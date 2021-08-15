use std::time::Instant;

use swc_ecmascript::parser::{lexer::Lexer, Capturing, Parser, StringInput, Syntax};
use swc_ecmascript::{
    common::{
        errors::{ColorConfig, Handler},
        sync::Lrc,
        FileName, SourceMap,
    },
    parser::TsConfig,
};

use crate::codegen::react::ReactCodgen;

pub fn code(st: &str) {
    let now = Instant::now();

    let cm: Lrc<SourceMap> = Default::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(cm.clone()));
    let tsconfig = TsConfig {
        tsx: true,
        decorators: true,
        dynamic_import: true,
        dts: true,
        import_assertions: true,
        no_early_errors: false,
    };
    // Real usage
    // let fm = cm
    //     .load_file(Path::new("test.js"))
    //     .expect("failed to load test.js");
    let fm = cm.new_source_file(FileName::Custom("test.js".into()), st.into());
    let lexer = Lexer::new(
        // We want to parse ecmascript
        Syntax::Typescript(tsconfig),
        // JscTarget defaults to es5
        swc_ecmascript::ast::EsVersion::Es5,
        StringInput::from(&*fm),
        None,
    );

    let capturing = Capturing::new(lexer);

    let mut parser = Parser::new_from(capturing);

    for e in parser.take_errors() {
        e.into_diagnostic(&handler).emit();
    }

    let module = parser
        .parse_typescript_module()
        .map_err(|e| e.into_diagnostic(&handler).emit())
        .expect("Failed to parse module.");
    let reactcodegen = ReactCodgen::new(module.clone());
    let i = reactcodegen.parse_react();
    println!("{:#?}", reactcodegen);
    // println!(" {}", i)
    println!("{}", now.elapsed().as_secs());
}
