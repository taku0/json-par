#!/bin/sh

# Run linter in Docker.  Used in Makefile.

if docker info --format '{{.SecurityOptions}}' | grep -q 'name=rootless'
then
    USER_OPT=""
else
    USER_OPT=--user="$(id -u):$(id -g)"
fi

# WORKAROUND: disable for 24 to mitigate "Argument `_length' should appear
# (as _LENGTH) in the doc string".
# Indentation rules changed since 29.
for version in 30 29 # 28 27 26 25 # 24
do
    docker \
        run \
        --rm \
        --volume="$(pwd)":/src \
        $USER_OPT \
        --workdir="/src" \
        --env=ELDEV_DIR=/src/.eldev \
        --env=HOME=/tmp \
        silex/emacs:${version}-ci-eldev \
        bash -c "./scripts/run_linter.sh" \
        || exit 1
done

echo "done"
