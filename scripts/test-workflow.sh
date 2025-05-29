#!/bin/zsh

# Exit on error
set -e

# Colors for output
RED='\033[0;31m'
GREEN='\033[0;32m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "${BLUE}Testing GitHub Actions workflow locally...${NC}"

# Function to extract crate name from path
get_crate_name() {
    local file="$1"
    if [[ $file == crates/* ]]; then
        echo "$file" | cut -d'/' -f2
    fi
}

# Get the current branch name
current_branch=$(git rev-parse --abbrev-ref HEAD)

# Get the main/master branch name
if git show-ref --verify --quiet refs/remotes/origin/main; then
    base_branch="origin/main"
elif git show-ref --verify --quiet refs/remotes/origin/master; then
    base_branch="origin/master"
else
    echo "${RED}Error: Neither main nor master branch found${NC}"
    exit 1
fi

echo "${BLUE}Comparing changes between ${base_branch} and ${current_branch}...${NC}"

# Get changed files
changed_files=$(git diff --name-only $base_branch...HEAD)

# Get unique crate names that have changes
declare -A changed_crates
for file in ${(f)changed_files}; do
    crate_name=$(get_crate_name "$file")
    if [ ! -z "$crate_name" ]; then
        changed_crates[$crate_name]=1
    fi
done

# Check if any Rust files were changed
rust_files_changed=false
if echo "$changed_files" | grep -q "\.rs$"; then
    rust_files_changed=true
fi

echo "\n${BLUE}Changed files:${NC}"
echo "$changed_files"

if [ ${#changed_crates[@]} -eq 0 ]; then
    echo "\n${GREEN}No changes in any crates${NC}"
else
    echo "\n${BLUE}Changed crates:${NC}"
    # Create an array of package arguments
    package_args=()
    for crate in "${(k)changed_crates[@]}"; do
        echo "- $crate"
        package_args+=("-p" "$crate")
    done

    echo "\n${BLUE}Running checks for changed crates...${NC}"
    
    # Build changed crates
    echo "\n${BLUE}Building changed crates...${NC}"
    cargo build --verbose "${package_args[@]}" || { echo "${RED}Build failed${NC}"; exit 1; }

    # Run tests for changed crates
    echo "\n${BLUE}Running tests for changed crates...${NC}"
    cargo test --verbose "${package_args[@]}" || { echo "${RED}Tests failed${NC}"; exit 1; }

    # Run clippy on changed crates
    echo "\n${BLUE}Running clippy on changed crates...${NC}"
    cargo clippy --locked "${package_args[@]}" -- -D warnings || { echo "${RED}Clippy checks failed${NC}"; exit 1; }
fi

# Run fmt if any Rust files were changed
if [ "$rust_files_changed" = true ]; then
    echo "\n${BLUE}Running cargo fmt...${NC}"
    cargo fmt --all -- --check || { echo "${RED}Formatting check failed${NC}"; exit 1; }
fi

echo "\n${GREEN}All checks passed successfully!${NC}" 