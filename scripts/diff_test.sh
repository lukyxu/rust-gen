set -e
cd /home/lukyxu/repos/rust/rust-gen
rm -rf prog
mkdir prog
mkdir prog/prog-exec
for i in {0..2}
do
  echo "Seed $i"
  cargo -q run -- -s $i > prog/$i.rs

  rustc -A warnings -A unused -C opt-level=0 prog/$i.rs -o prog/prog-exec/$i-0
  s1=$(./prog/prog-exec/$i-0)
  rustc -A warnings -A unused -C opt-level=3 prog/$i.rs -o prog/prog-exec/$i-3
  s2=$(./prog/prog-exec/$i-3)
done
