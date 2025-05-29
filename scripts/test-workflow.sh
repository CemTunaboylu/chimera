#!/bin/zsh

set -e

RED='\033[0;31m'
BLUE='\033[0;34m'
NC='\033[0m' # No Color

echo "${BLUE}Testing GitHub Actions workflow locally with act...${NC}"

act ||  echo "${RED}Error: Local Github workflow failed {NC}"; exit 1