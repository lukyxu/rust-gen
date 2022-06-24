fn main() {
    // let mut checksum: i8 = -30_i8 % -55_i8;
    let mut x = [1,2,3];
    let y: i32;
    let y = x[
        {
            x = [3,2,1];
            println!("{:?}", x);
            2
        }
        ];
    println!("{}", y)
}