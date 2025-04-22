#!/usr/bin/env bash
set -euo pipefail

# Recompile on changes and clear the screen
watchexec\
    --no-global-ignore\
    --restart\
    --verbose\
    --exts roc\
    --debounce 500ms\
    --clear\
    -- "roc test $1"
