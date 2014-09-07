RUSTC_NT=rustc --no-trans --test -A dead-code
srcs=$(shell find problems -name '*.rs')

.PHONY: all bench test

all:
	cargo build --release

bench:
	RUST_LOG=euler_criterion=info cargo run --release

test:
	$(foreach src,$(srcs),$(RUSTC_NT) $(src) || exit;)
