#!/bin/bash
set -o errexit -o pipefail -o noclobber -o nounset

VERSION="0.1"
RED="\033[0;31m"
GREEN="\033[0;32m"
BLUE="\033[0;34m"
CLR="\033[0m" # No Color
GCBLUNIT="modules/gcblunit/gcblunit.cbl"

function run-tests {
  ALL_SOURCE_FILES="$(
    find src -type f -name "*.cbl" | xargs echo)"
  ALL_TEST_FILES="$(
    find test -type f -name "*.cbl" | xargs echo)"
  ALL_TEST_PROGRAM_IDS="$(
    find test -type f -name "*.cbl" | xargs basename -s .cbl | xargs echo)"

  echo -e "${BLUE}[CBL] Compiling unit tests.${CLR}"

  # No warnings printed here due to a bug in GnuCOBOL
  if cobc -o tmp/test -x -debug "$GCBLUNIT" $ALL_TEST_FILES $ALL_SOURCE_FILES; then
    echo -e "${GREEN}[CBL] Compilation successful.${CLR}"
  else
    echo -e "${RED}[CBL] Compilation failed.${CLR}"
    exit 1
  fi

  rm -r env/test/run
  cp -a env/test/template env/test/run

  echo -e "${BLUE}[CBL] Running unit tests.${CLR}"
  if (cd env/test/run && ../../../tmp/test $ALL_TEST_PROGRAM_IDS); then
    echo -e "${GREEN}[CBL] Unit tests passed.${CLR}"
  else
    echo -e "${RED}[CBL] Unit tests failed.${CLR}"
    exit 1
  fi

  echo -e "${BLUE}[CBL] Comparing env/test/run and env/test/expected.${CLR}"
  if diff -r env/test/expected env/test/run; then
    echo -e "${GREEN}[CBL] Identical. Pass.${CLR}"
  else
    echo -e "${RED}[CBL] Different. Fail.${CLR}"
    exit 1
  fi
}

function run-main-program {
  ALL_SOURCE_FILES="$(
    find src -type f -name "*.cbl" | xargs echo)"

  cobc \
    -Wall \
    -x \
    -o tmp/main-program \
    main-program.cbl \
    $ALL_SOURCE_FILES

  (cd env/main && ../../tmp/main-program)
}

function show-usage {
  echo "Cobol Runner v${VERSION}"
  echo
  echo "Usage:"
  echo
  echo "  * ./cbl test  # to run unit tests"
  echo "  * ./cbl run   # to run main-program.cbl"
  echo "  * ./cbl setup # to set up this project"
  echo "  * ./cbl help  # show this message"
  echo
}

function setup {
  if ! command -v cobc &> /dev/null; then
    echo -e "${BLUE}GnuCOBOL is not installed.${CLR}"
    echo -e "${BLUE}You'll need to install that first.${CLR}"
    echo -e "${BLUE}Start here: https://gnucobol.sourceforge.io/${CLR}"
    exit
  fi

  if ! [ -d "modules/gcblunit" ]; then
    echo -e "${BLUE}Downloading and unpacking GCBLUnit 1.22.6...${CLR}"
    curl -Ls "https://github.com/OlegKunitsyn/gcblunit/archive/1.22.6.tar.gz" | bsdtar -xf - -C modules/
    mv modules/gcblunit-* modules/gcblunit
    echo -e "${BLUE}Done.${CLR}"
  else
    echo -e "${BLUE}GCBLUnit already installed, skipping.${CLR}"
  fi

  echo "Running check..."
  if run-tests; then
    echo -e "${GREEN}Check passed. You're good to go!${CLR}"
  else
    echo -e "${RED}Check failed. Over to you.${CLR}"
  fi
}

# Arg parsing logic with thanks to https://stackoverflow.com/a/33826763
COMMAND=""

while [[ "$#" -gt 0 ]]; do
  case $1 in
    -h|--help) COMMAND="help" ;;
    *) COMMAND="$1" ;;
  esac
  shift
done

if [ "$COMMAND" == "test" ]; then
  run-tests
elif [ "$COMMAND" == "run" ]; then
  run-main-program
elif [ "$COMMAND" == "setup" ]; then
  setup
elif [ "$COMMAND" == "help" ]; then
  show-usage
else
  echo "Unknown command '$COMMAND'"
  echo
  show-usage
fi
