# euler_criterion.rs

This is testing ground for the "external benchmarking" feature of
[criterion.rs][0].

Here I benchmark solutions to [Project Euler problems][1] written in several
programming languages and (for now) **crudely** compare them in a single plot.

## Contents

Plot + source files: [See directories under the problems directory][2]

Raw data (estimates of the mean/median/etc, all missing units are nanoseconds):
[See the raw directory][3]
(This is just a fraction of the data that criterion generates)

Details (which compiler/interpreter/flags) about the benchmarked languages:
[See the languages directory][4]

Version of each compiler/interpreter: [See the versions directory][5] or run
`head versions/*`

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

[0]: https://github.com/japaric/criterion.rs
[1]: https://projecteuler.net/problems
[2]: /problems/001
[3]: /raw/001/Rust/estimates.json
[4]: /languages/rust.json
[5]: /versions/rustc
