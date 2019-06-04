#!/usr/bin/env bash

set -euo pipefail

EXECUTABLE="$(readlink -e "$1")"
TMP_DIR="$(mktemp -d -t ci-XXXXXXXXXX)"
mkdir -p "${TMP_DIR}/debian"
touch "${TMP_DIR}/debian/control"
pushd "${TMP_DIR}" >/dev/null
DEPENDENCIES=$(dpkg-shlibdeps "${EXECUTABLE}" -O)
popd >/dev/null
echo ${DEPENDENCIES##shlibs:Depends=}
