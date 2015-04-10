#!/bin/bash

export PATH="${PATH}:$(pwd)/rustic/target/release"
export RUST_LOG=euler
target/release/euler
