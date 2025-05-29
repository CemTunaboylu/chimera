cat > detect-changes.sh << 'EOF'
          #!/bin/bash
          set -e

          # Function to extract crate name from path
          get_crate_name() {
              local file="$1"
              if [[ $file == crates/* ]]; then
                  echo "$file" | cut -d'/' -f2
              fi
          }

          # Try to use origin/${{ github.base_ref }}, fallback to main, then main
          if git rev-parse --verify origin/${{ github.base_ref }} >/dev/null 2>&1; then
              base_ref="origin/${{ github.base_ref }}"
          elif git rev-parse --verify origin/main >/dev/null 2>&1; then
              base_ref="origin/main"
          else
              base_ref="main"
          fi
          changed_files=$(git diff --name-only $base_ref...HEAD)

          # Get unique crate names that have changes
          changed_crates=()
          for file in $changed_files; do
              crate_name=$(get_crate_name "$file")
              if [ ! -z "$crate_name" ]; then
                  changed_crates["$crate_name"]=1
              fi
          done

          # Output the changed crates
          if [ ${#changed_crates[@]} -eq 0 ]; then
              echo "No changes in any crates"
              exit 0
          fi

          # Create the cargo package list for commands
          packages=""
          for crate in "${!changed_crates[@]}"; do
              packages="$packages -p $crate"
          done
          echo "CHANGED_PACKAGES=$packages" >> $GITHUB_ENV
EOF
chmod +x detect-changes.sh
./detect-changes.sh