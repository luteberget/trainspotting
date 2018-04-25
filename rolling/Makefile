.PHONY: rollinglib rollingbin rolling
rolling: rollinglib rollingbin
rollinglib: target/release/librolling.a
rollingbin: target/debug/rolling

target/release/rolling:
	cargo build --release
target/release/librolling.a:
	cargo build --release

target/debug/rolling:
	cargo build
target/debug/librolling.a:
	cargo build
