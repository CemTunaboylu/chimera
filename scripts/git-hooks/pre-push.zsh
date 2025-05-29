#!/bin/zsh

# Exit on error
set -e

# Get the remote branch we're pushing to
remote="$1"
url="$2"

# Get the current branch name
current_branch=$(git symbolic-ref --short HEAD)

# Get list of changed files compared to remote branch
# First try to get the remote tracking branch
tracking_branch=$(git rev-parse --abbrev-ref --symbolic-full-name @{u} 2>/dev/null || echo "")

if [ -z "$tracking_branch" ]; then
    # If no tracking branch, compare with master/main
    if git show-ref --verify --quiet refs/remotes/origin/main; then
        tracking_branch="origin/main"
    elif git show-ref --verify --quiet refs/remotes/origin/master; then
        tracking_branch="origin/master"
    else
        echo "No tracking branch found and neither main nor master exists on remote"
        exit 1
    fi
fi

# Get changed files
changed_files=$(git diff --name-only $tracking_branch...HEAD)

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
    cargo clippy -p "$crate" -- -D warnings || exit 1
    
    echo "Running tests for crate: $crate"
    cargo test -p "$crate" || exit 1
done