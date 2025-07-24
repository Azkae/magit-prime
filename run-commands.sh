#!/bin/bash

# Function to run a single command and format output
run_command() {
    local id="$1"
    local command="$2"

    # Run command, capture exit code and first line of output
    local output
    output=$(eval "$command" 2>/dev/null | head -n1)
    local exit_code=$?

    # Print formatted result
    echo "$id:$exit_code:$output"
}

# Read input and run commands in parallel
while IFS=':' read -r id command; do
    # Skip empty lines
    [[ -z "$id" || -z "$command" ]] && continue

    # Run command in background
    run_command "$id" "$command" &
done

# Wait for all background jobs to complete
wait
