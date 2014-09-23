[![Build Status][status]](https://travis-ci.org/japaric/euler_criterion.rs)

# euler_criterion.rs

This is testing ground for the "external benchmarking" feature of
[criterion.rs][criterion].

Here I benchmark solutions to [Project Euler problems][euler] written in
several programming languages and (for now) **crudely** compare them in a
single plot.

## Contents

Plot + source files: [See directories under the problems directory][problems]

Raw data (estimates of the mean/median/etc, all missing units are nanoseconds):
[See the raw directory][raw]
(This is just a fraction of the data that criterion generates)

The CPU used for the benchmark: See [The cpu file][cpu]

Details (which compiler/interpreter/flags) about the benchmarked languages:
[See the languages directory][languages]

Version of each compiler/interpreter: [See the versions directory][versions] or
run `head versions/*`

## How do I repeat these benchmarks?

`make && make bench`

Will run **ALL** the benchmarks, and generate lots of data and plots under the
`.criterion` folder

N.B. You'll need to source the answer files (`*.ans`) to every problem
directory. This answer file must contain the correct answer to the Project
Euler problem. Since you have the solutions at hand, you can generate the
answers like this:

`python problems/001/001.py -a > problems/001.ans`

## License

euler_criterion.rs is dual licensed under the Apache 2.0 license and the MIT
license.

See LICENSE-APACHE and LICENSE-MIT for more details.

[cpu]: /cpu
[criterion]: https://github.com/japaric/criterion.rs
[euler]: https://projecteuler.net/problems
[languages]: /languages/rust.json
[problems]: /problems/001
[raw]: /raw/001/Rust/estimates.json
[status]: https://travis-ci.org/japaric/euler_criterion.rs.svg?branch=master
[versions]: /versions/rustc
