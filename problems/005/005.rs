const BOUND: u64 = 20;
fn main() {
    let mut value = 1;

    for f in (2..BOUND+1) {
        value = lcm(value, f);
    }
    println!("{}", value);
}

#[inline]
fn lcm(a: u64, b: u64) -> u64 {
    (a * b) / gcd(a, b)
}

#[inline]
fn gcd(a: u64, b: u64) -> u64 {
    if b == 0 { a }
    else { gcd(b, a % b) }
 }
