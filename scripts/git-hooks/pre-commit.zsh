#!/bin/zsh

# Exit on error
set -e

# Get the list of changed files compared to the last commit
changed_files=$(git diff --cached --name-only)

# Function to extract crate name from path
get_crate_name() {
    local file="$1"
    if [[ $file == crates/* ]]; then
        echo "$file" | cut -d'/' -f2
    fi
}

# Get unique crate names that have changes
declare -A changed_crates
for file in $changed_files; do
    crate_name=$(get_crate_name "$file")
    if [ ! -z "$crate_name" ]; then
        changed_crates["$crate_name"]=1
    fi
done

# If no crates have changes, exit successfully
if [ ${#changed_crates[@]} -eq 0 ]; then
    echo "No changes in any crates, skipping tests"
    exit 0
fi

# Run tests for each changed crate
for crate in "${!changed_crates[@]}"; do
    echo "Running tests for crate: $crate"
    cargo test -p "$crate" || exit 1
done

if git rev-parse --verify HEAD >/dev/null 2>&1
then