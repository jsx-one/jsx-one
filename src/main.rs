pub mod code;
pub mod codegen;
fn main() {
    let st = "
    function maingobrr(abc: oMG){
        let i: number = \"ad\"
        let u = {\"a\": 121, \"a\": 121};
        
        return (
            <h1 style={{fontSize: \"10em\"}}>My Name is barun</h1>
        )
    }
    ";
    code::code(st)
}
