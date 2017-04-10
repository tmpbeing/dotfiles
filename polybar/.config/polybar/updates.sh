#!/usr/bin/env bash

UPDATES=$(checkupdates | wc -l)
[[ "${UPDATES}" = "0" ]] && exit 0

echo "  ${UPDATES} "
exit 0
