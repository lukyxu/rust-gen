struct A([i32;3]);
fn main() {
    let var_498 = A([1,2,3]);
    let x = var_498.0[{
        1
    }];
    println!("{:?}", var_498.0)
}

// struct A([i32;3]);
// fn main() {
//     let var_498 = A([1,2,3]);
//     let x = { var_498.0 }[{
//         var_498;
//         [2]
//     }[0]];
//     println!("{}", x)
// }
