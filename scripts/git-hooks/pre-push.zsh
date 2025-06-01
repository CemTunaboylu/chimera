#!/bin/zsh

source scripts/git-hooks/changed_files_in_branch.zsh

# Exit on error
set -e

# Get the remote branch we're pushing to
remote="$1"
url="$2"

changed_files=$(changed_files_in_branch "$remote" "$url")

# Function to extract crate name from path
get_crate_name() {
    local file="$1"
    if [[ $file == crates/* ]]; then
        echo "$file" | cut -d'/' -f2
    fi
}

# Get unique crate names that have changes
typeset -A changed_crates
for file in ${(f)changed_files}; do
    crate_name=$(get_crate_name "$file")
    if [ ! -z "$crate_name" ]; then
        changed_crates[$crate_name]=1
    fi
done

# If no crates have changes, exit successfully
if [ ${#changed_crates} -eq 0 ]; then
    echo "No changes in any crates, skipping checks"
    exit 0
fi

# Run clippy and tests for each changed crate
for crate in "${(k)changed_crates[@]}"; do
    echo "Running clippy for crate: $crate"
    cargo clippy -p "$crate" -- || exit 1
    
    echo "Running tests for crate: $crate"
    cargo test -p "$crate" || exit 1
done

# Check for changes in .github/workflows between tracking branch and HEAD
if echo "$changed_files" | grep -q '^\.github/workflows/'; then
    echo "Detected changes in .github/workflows. Running act for local workflow test..."
    ./scripts/test-workflow.sh
fi