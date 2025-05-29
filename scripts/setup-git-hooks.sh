#!/bin/zsh

# Exit on error
set -e

# Get the directory where this script is located
SCRIPT_DIR="$(cd "$(dirname "${BASH_SOURCE[0]}")" && pwd)"
HOOKS_DIR="$SCRIPT_DIR/git-hooks"
GIT_HOOKS_DIR="$(git rev-parse --git-dir)/hooks"

# Create Git hooks directory if it doesn't exist
mkdir -p "$GIT_HOOKS_DIR"

# Function to create a symbolic link for a hook
create_hook_symlink() {
    local hook_name="$1"
    local source_file="$HOOKS_DIR/$hook_name.zsh"
    local target_file="$GIT_HOOKS_DIR/$hook_name"

    # Remove existing hook or symlink if it exists
    if [ -f "$target_file" ] || [ -L "$target_file" ]; then
        rm "$target_file"
    fi

    # Create the symbolic link
    ln -s "$source_file" "$target_file"
    chmod +x "$source_file"

    echo "Created symbolic link for $hook_name hook"
}

# Create symlinks for all our hooks
create_hook_symlink "pre-commit"
create_hook_symlink "pre-push"

echo "Git hooks setup completed successfully!" 