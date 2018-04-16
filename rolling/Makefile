.PHONY: rolling
rolling: target/release/rolling target/release/librolling.a

target/release/rolling:
	cargo build --release
target/release/librolling.a:
	cargo build --release

target/debug/rolling:
	cargo build
target/debug/librolling.a:
	cargo build
