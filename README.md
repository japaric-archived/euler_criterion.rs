[![Build Status][status]](https://travis-ci.org/japaric/euler_criterion.rs)

# `euler_criterion.rs`

Benchmark Project Euler solutions written in several programming languages
using [criterion.rs]

This is manily testing ground for the "external benchmarking" feature of
[criterion.rs]

Here I benchmark solutions to [Project Euler] problems written in
several programming languages and (for now) **crudely** compare them in a
single plot.

## [Results]

## How do I repeat these benchmarks?

`make && ./bench.sh`

Will run **ALL** the benchmarks, and generate lots of data and plots under the
`output` folder

## License

euler_criterion.rs is dual licensed under the Apache 2.0 license and the MIT
license.

See LICENSE-APACHE and LICENSE-MIT for more details.

[Project Euler]: https://projecteuler.net/problems
[Results]: ../results
[criterion.rs]: https://github.com/japaric/criterion.rs
[status]: https://travis-ci.org/japaric/euler_criterion.rs.svg?branch=master
