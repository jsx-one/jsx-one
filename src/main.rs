pub mod code;
pub mod codegen;
fn main() {
    let st = "
    import * as main from \"mainstring\";
    // interface mindblown{
     //    main: string
    // };
    // function maingobrr(){
// 
    // }
    ";
    code::code(st)
}
