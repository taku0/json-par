#!/bin/sh

# Run tests in Docker.  Used in Makefile.

for version in 27 26 25 24
do
    docker \
        run \
        --rm \
        --volume="$(pwd)":/src \
        --user "$(id -u):$(id -g)" \
        silex/emacs:${version}-ci-eldev \
        bash -c \
        "cd /src && ELDEV_DIR=/src/.eldev HOME=/tmp ./scripts/run_test.sh" \
        || exit 1
done

echo "done"
