RUSTC_NT=rustc --no-trans --test -A dead-code
srcs=$(shell find problems -name '*.rs')

.PHONY: all bench test

all:
	if [ ! -d rustic ]; then git clone --depth 1 https://github.com/japaric/rustic; fi
	cargo build --release
	cd rustic && cargo build --release && cd ..

clean:
	cargo clean
	rm -rf rustic

bench:
	./bench.sh

test: $(srcs)
	./test-rust-solutions.sh
	./check-line-length.sh
