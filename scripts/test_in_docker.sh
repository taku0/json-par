#!/bin/sh

# Run tests in Docker.  Used in Makefile.

if docker info --format '{{.SecurityOptions}}' | grep -q 'name=rootless'
then
    # This is required to fetch the treesit grammar for some reason.
    EXTRA_OPT="--network=host"
else
    EXTRA_OPT=--user="$(id -u):$(id -g)"
fi

if ! [ -e .tree-sitter/tree-sitter-json/libtree-sitter-json.so ]
then
    mkdir -p .tree-sitter || exit 1
    rm -rf .tree-sitter/tree-sitter-json || exit 1

    docker \
        run \
        --rm \
        --volume="$(realpath .tree-sitter)":/tree-sitter \
        $EXTRA_OPT \
        --workdir="/tree-sitter" \
        gcc:15 \
        bash -c "git clone --depth 1 https://github.com/tree-sitter/tree-sitter-json && cd tree-sitter-json && make" \
        || exit 1
fi

for version in 31 30 29 28 27 26 25 24
do
    rm -f *.elc test/*.elc
    rm -f *-autoloads.el
    docker \
        run \
        --rm \
        --volume="$(pwd)":/src \
        --volume="$(realpath .tree-sitter)":/tree-sitter \
        $EXTRA_OPT \
        --workdir="/src" \
        --env=ELDEV_DIR=/src/.eldev \
        --env=HOME=/tmp \
        --env=TREESIT_EXTRA_LOAD_PATH=/tree-sitter/tree-sitter-json \
        silex/emacs:${version}-ci-eldev \
        bash -c "./scripts/run_test.sh" \
        || exit 1
done

echo "done"
