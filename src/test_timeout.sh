#!/bin/bash

# Directory containing the .run files
directory="./output"

# Timeout duration in seconds
timeout_duration=1

# Loop through each .run file in the directory
for file in "$directory"/*.run; do
    echo "Testing $file..."
    # Run the executable with a timeout
    timeout "$timeout_duration" "$file"
    # Check the exit status of the timeout command
    if [ $? -eq 124 ]; then
        echo "The file $file did not finish in $timeout_duration second(s)."
    fi
done
