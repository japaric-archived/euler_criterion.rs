RUSTC_NT=rustc --no-trans --test -A dead-code
srcs=$(shell find problems -name '*.rs')

.PHONY: all bench test

all:
	cargo build --release -u

bench:
	RUST_LOG=euler_criterion=info target/release/euler_criterion

test:
	$(foreach src,$(srcs),$(RUSTC_NT) $(src) || exit;)
