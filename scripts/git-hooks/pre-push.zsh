#!/bin/zsh

# Exit on error
set -e

# Get the remote branch we're pushing to
remote="$1"
remote_ref="$2"

# Get the remote branch name
while read local_ref local_sha remote_ref remote_sha
do
    remote_branch=${remote_ref#refs/heads/}
done

# Get list of changed files compared to remote HEAD
changed_files=$(git diff --name-only origin/"$remote_branch"...HEAD)

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
    echo "No changes in any crates, skipping checks"
    exit 0
fi

# Run clippy and tests for each changed crate
for crate in "${!changed_crates[@]}"; do
    echo "Running clippy for crate: $crate"
    cargo clippy -p "$crate" -- -D warnings || exit 1
    
    echo "Running tests for crate: $crate"
    cargo test -p "$crate" || exit 1
done