extern "C" {
  fn printf(s: *const i8, ...);
}

fn print_int(value: i32) {
  let s = "%d\n\0";
  let s_p = s as *const str;
  let c_p = s_p as *const i8;
  unsafe {
    printf(c_p, value as isize);
  }
}

fn main() {
  let a = 545201568421088164_i128 as i32;
  let b = a / 30313;
  print_int(a);
  print_int(b);
  // println!("{} {}", a, b);
}
