fn main() {
  let a = 545201568421088164_isize as i32 / 30313;
  let b = 6;
  let c = 1495423857_i32.wrapping_div(a);
  let d = b;
  (d as i32).wrapping_rem(c);
}
