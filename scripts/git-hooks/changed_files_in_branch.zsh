#!/bin/zsh

# Exit on error
set -e

changed_files_in_branch() {
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
    echo "$changed_files"
}

changed_files_in_branch "$@"
