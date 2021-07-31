pub mod code;
pub mod codegen;
fn main() {
    let st = "
    
    function maingobrr(abc: oMG){
        let i: number = \"ad\"
    }
    ";
    code::code(st)
}
