set -e
cd /home/lukyxu/repos/rust/rust-gen
rm -rf prog
mkdir prog
mkdir prog/prog-exec
for i in {0..10000}
do
  echo "Seed $i"
  export RUSTFLAGS=-Awarnings
  cargo -q run -- -s $i > prog/$i.rs

  rustc -A warnings -C opt-level=0 prog/$i.rs -o prog/prog-exec/$i-0
  s1=$(./prog/prog-exec/$i-0)
  rustc -A warnings -A unused_comparisons -C opt-level=3 prog/$i.rs -o prog/prog-exec/$i-3
  s2=$(./prog/prog-exec/$i-3)
done
