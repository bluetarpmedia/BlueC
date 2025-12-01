#!/usr/bin/env bash
set -euo pipefail

#
# Pass the directory path of the valid test files that you want to generate
# expected results for.
#
# E.g.
# $ ./generate_expected_results.sh "valid/3"
#

if [ "$#" -ne 1 ]; then
    printf 'Usage: %s <directory>\n' "$0" >&2
    exit 2
fi

dir=$1

if [ ! -d "$dir" ]; then
    printf 'Error: not a directory: %s\n' "$dir" >&2
    exit 2
fi

# dir_stem is the final path component of the directory
dir_stem=$(basename -- "$dir")

shopt -s nullglob

first=true
printf '[\n'

cleanup() {
    rm -f -- "${bins[@]:-}"
}
trap cleanup EXIT

declare -a bins=()

for src in "$dir"/*.c; do
    # basename of the source file (no directory)
    src_base=$(basename -- "$src")

    # Build the combined name: {dir_stem}_{src_base}
    combined_name="${dir_stem}_${src_base}"

    # Create a secure temporary binary path (mktemp creates the file)
    out_bin=$(mktemp) || out_bin="./tmp_bin_$$"
    bins+=("$out_bin")

    # Compile with warnings disabled, and then run the executable
    if gcc -std=c17 -w -o "$out_bin" -- "$src"; then
        # Run the binary and capture its stdout and its exit code.
        # We use an `if` statement so that our script keeps running if the binary exit code is nonzero (because we set -e above)
        if stdout_capture=$("$out_bin"); then
            exit_code=0
        else
            exit_code=$?
        fi
    else
        echo "gcc returned '$?' when compiling '$src'."
        echo "Error: valid test cases should compile successfully."
        exit
    fi

    if [ "$first" = true ]; then
        first=false
    else
        printf ',\n'
    fi

    printf '    {"filename": "%s", "exit_code": %d}' "$combined_name" "$exit_code"
done

printf '\n]\n'
