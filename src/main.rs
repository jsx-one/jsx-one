pub mod code;
pub mod codegen;
fn main() {
    let st = "
    function maingobrr(abc: oMG){
        let i: number = \"ad\"
        let u = {\"a\": 121, \"a\": 121};
        
        return (
            <div>
                <h1>Hello World</h1>
            </div>
        )
    }
    ";
    code::code(st)
}
