#!/bin/sh

set -eux

cmake -B build.$1 \
      --toolchain $2 \
      -C cmake/caches/O3.cmake \
      -GNinja \
      -DTEST_SUITE_BENCHMARKING_ONLY=ON \
      -DTEST_SUITE_RUN_BENCHMARKS=OFF
ninja -C build.$1
if ! find build.$1 -type f \( -name '*.s' -o -name '*.S' \) -print -quit | grep -q .; then
  echo "No assembly files were generated in build.$1; expected output from -save-temps=obj" >&2
  exit 1
fi
llvm-lit build.$1 -o results.$1.json
