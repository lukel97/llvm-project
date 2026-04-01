# When /test-suite is commented on a PR, checks out the PR, builds clang and
# then the test-suite in several configurations. It then checks out the base of
# the PR, builds clang and the test-suite again, and then uploads the diff of
# the codegen.

name: Diff test-suite codegen

on:
  issue_comment:
    types:
      - created

jobs:
  test-suite:
    name: Build and diff
    runs-on: ubuntu-24.04
    permissions:
      issues: write
    if: >-
      !startswith(github.event.comment.body, '<!--IGNORE-->') &&
      github.event.issue.pull_request && contains(github.event.comment.body, '/test-suite')
    steps:
      - id: get-pr
        uses: actions/github-script@ed597411d8f924073f98dfc5c65a23a2325f34cd # v8.0.0
        with:
          script: |
            const { data: pr } = await github.rest.pulls.get({
              owner: context.repo.owner,
              repo: context.repo.repo,
              pull_number: context.payload.issue.number
            })
            if (!pr.mergeable)
              await github.rest.issues.createComment({
                owner: context.repo.owner,
                repo: context.repo.repo,
                body: "Can't diff PR, PR isn't mergeable"
              })
            return pr
      - if: ${{ !fromJSON(steps.get-pr.outputs.result).mergeable }}
        run: exit 1
      - uses: actions/github-script@ed597411d8f924073f98dfc5c65a23a2325f34cd # v8.0.0
        with:
          script: |
            github.rest.reactions.createForIssueComment({
              owner: context.repo.owner,
              repo: context.repo.repo,
              comment_id: context.payload.comment.id,
              content: '+1'
            })
      - uses: actions/checkout@8e8c483db84b4bee98b60c0593521ed34d9990e8 # v6.0.1
        with:
          ref: ${{ fromJSON(steps.get-pr.outputs.result).merge_commit_sha }}
          repository: ${{ fromJSON(steps.get-pr.outputs.result).head.repo.full_name }}
          fetch-depth: 2
      - run: |
          echo "HEAD_SHA=$(git rev-parse HEAD)" >> $GITHUB_ENV
          echo "BASE_SHA=$(git rev-parse HEAD^)" >> $GITHUB_ENV
      - uses: actions/checkout@8e8c483db84b4bee98b60c0593521ed34d9990e8 # v6.0.1
        with:
          repository: llvm/llvm-test-suite
          path: llvm-test-suite
      - name: Install system dependencies
        run: |
          sudo apt-get update
          sudo apt-get install -y cmake ninja-build libc6-dev-{arm64,riscv64}-cross libgcc-14-dev-{arm64,riscv64}-cross libstdc++-14-dev-{arm64,riscv64}-cross
      - name: Configure Clang
        run: cmake -B build -DCMAKE_BUILD_TYPE=Release -DLLVM_TARGETS_TO_BUILD='AArch64;X86;RISCV' -DLLVM_ENABLE_PROJECTS='clang;lld' -DLLVM_APPEND_VC_REV=OFF llvm -GNinja
      - name: Build Clang @ head
        run: ninja -C build
      - name: Configure and build test-suite @ head
        run: |
          cat << EOF > rva23u64.cmake
          set(CMAKE_SYSTEM_NAME Linux)
          set(CMAKE_C_COMPILER ${GITHUB_WORKSPACE}/build/bin/clang)
          set(CMAKE_CXX_COMPILER ${GITHUB_WORKSPACE}/build/bin/clang++)
          set(CMAKE_C_COMPILER_TARGET riscv64-linux-gnu)
          set(CMAKE_CXX_COMPILER_TARGET riscv64-linux-gnu)
          set(CMAKE_C_FLAGS_INIT "-march=rva23u64 -save-temps=obj")
          set(CMAKE_CXX_FLAGS_INIT "-march=rva23u64 -save-temps=obj")
          set(CMAKE_SYSTEM_PROCESSOR riscv64)
          set(CMAKE_LINKER_TYPE LLD)
          EOF
          cat << EOF > armv9-a.cmake
          set(CMAKE_SYSTEM_NAME Linux)
          set(CMAKE_C_COMPILER ${GITHUB_WORKSPACE}/build/bin/clang)
          set(CMAKE_CXX_COMPILER ${GITHUB_WORKSPACE}/build/bin/clang++)
          set(CMAKE_C_COMPILER_TARGET aarch64-linux-gnu)
          set(CMAKE_CXX_COMPILER_TARGET aarch64-linux-gnu)
          set(CMAKE_C_FLAGS_INIT "-march=armv9-a -save-temps=obj")
          set(CMAKE_CXX_FLAGS_INIT "-march=armv9-a -save-temps=obj")
          set(CMAKE_SYSTEM_PROCESSOR arm64)
          set(CMAKE_LINKER_TYPE LLD)
          EOF
          cat << EOF > x86_64.cmake
          set(CMAKE_C_COMPILER ${GITHUB_WORKSPACE}/build/bin/clang)
          set(CMAKE_CXX_COMPILER ${GITHUB_WORKSPACE}/build/bin/clang++)
          set(CMAKE_C_FLAGS_INIT "-save-temps=obj")
          set(CMAKE_CXX_FLAGS_INIT "-save-temps=obj")
          set(CMAKE_LINKER_TYPE LLD)
          EOF
          build_llvm_test_suite () {
            cmake -B build.$1 -C cmake/caches/O3.cmake --toolchain $2 -DTEST_SUITE_BENCHMARKING_ONLY=ON -DTEST_SUITE_RUN_BENCHMARKS=OFF -GNinja
            ninja -C build.$1
            $GITHUB_WORKSPACE/build/bin/llvm-lit build.$1 -o results.$1.json
          }
          build_llvm_test_suite rva23u64-O3-b rva23u64.cmake
          build_llvm_test_suite armv9-a-O3-b armv9-a.cmake
          build_llvm_test_suite x86_64-O3-b x86_64.cmake
        working-directory: llvm-test-suite
      - name: Build test-suite @ base
        run: git checkout $BASE_SHA && ninja -C build
      - name: Configure and build test-suite @ base
        run: |
          build_llvm_test_suite () {
            cmake -B build.$1 -C cmake/caches/O3.cmake --toolchain $2 -DTEST_SUITE_BENCHMARKING_ONLY=ON -DTEST_SUITE_RUN_BENCHMARKS=OFF -GNinja
            ninja -C build.$1
            $GITHUB_WORKSPACE/build/bin/llvm-lit build.$1 -o results.$1.json
          }
          build_llvm_test_suite rva23u64-O3-a rva23u64.cmake
          build_llvm_test_suite armv9-a-O3-a armv9-a.cmake
          build_llvm_test_suite x86_64-O3-a x86_64.cmake
        working-directory: llvm-test-suite
      - run: |
          mkdir diffs
          ./utils/tdiff.py -a build.rva23u64-O3-a -b build.rva23u64-O3-b -s all > diffs/rva23u64-O3.diff || true
          ./utils/tdiff.py -a build.armv9-a-O3-a -b build.armv9-a-O3-b -s all > diffs/armv9-a-O3.diff || true
          ./utils/tdiff.py -a build.x86_64-O3-a -b build.x86_64-O3-b -s all > diffs/x86_64-O3.diff || true
        working-directory: llvm-test-suite
      - uses: actions/upload-artifact@bbbca2ddaa5d8feaa63e36b76fdaad77386f024f #v7.0.0
        id: upload-diffs
        with:
          name: diffs
          path: llvm-test-suite/diffs
      - uses: actions/upload-artifact@bbbca2ddaa5d8feaa63e36b76fdaad77386f024f #v7.0.0
        with:
          name: results
          path: llvm-test-suite/results*.json
      - uses: actions/github-script@ed597411d8f924073f98dfc5c65a23a2325f34cd # v8.0.0
        env:
          DIFF_URL: ${{ steps.upload-diffs.outputs.artifact-url }}
        with:
          script: |
            github.rest.issues.createComment({
              owner: context.repo.owner,
              repo: context.repo.repo,
              issue_number: context.issue.number,
              body: `test-suite diff from ${process.env.BASE_SHA}...${process.env.HEAD_SHA}: ${process.env.DIFF_URL}`
            })
