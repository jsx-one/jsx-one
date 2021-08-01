use swc_ecmascript::common::{
    errors::{ColorConfig, Handler},
    sync::Lrc,
    FileName, SourceMap,
};
use swc_ecmascript::parser::{lexer::Lexer, Capturing, Parser, StringInput, Syntax};

use crate::codegen::react::ReactCodgen;

pub fn code(st: &str) {
    let cm: Lrc<SourceMap> = Default::default();
    let handler = Handler::with_tty_emitter(ColorConfig::Auto, true, false, Some(cm.clone()));
    let fm = cm.new_source_file(FileName::Custom("test.js".into()), st.into());

    let lexer = Lexer::new(
        Syntax::Typescript(Default::default()),
        Default::default(),
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
    println!(" {:#?} {}", &module, i)
}
