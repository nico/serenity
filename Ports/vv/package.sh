#!/usr/bin/env -S bash ../.port_include.sh
port='vv'
version='3.2'
workdir="vv-${version}"
depends=(
)
useconfigure='true'
configopts=(
    "-DCMAKE_TOOLCHAIN_FILE=${SERENITY_BUILD_DIR}/CMakeToolchain.txt"
)
files=(
    "https://github.com/wolfpld/vv/archive/refs/tags/v${version}.zip"
)

configure() {
    run cmake "${configopts[@]}"
}

install() {
    run make install
}
