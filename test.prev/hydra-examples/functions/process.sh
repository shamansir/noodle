#!/bin/bash

# Create or clear the output file
output_file="output.txt"
> "$output_file"

# Loop through each file with the pattern "<number>.<group>.<function>.js"
for file in *.js; do
    # echo $file
    # Check if the filename matches the pattern "<number>.<group>.<function>.js"
    if [[ "$file" =~ ^([0-9]+)\.([^.]+)\.([^.]+)\.js$ ]]; then
        echo $file

        # Extract parts of the filename
        number="${BASH_REMATCH[1]}"
        group="${BASH_REMATCH[2]}"
        function="${BASH_REMATCH[3]}"

        echo $number
        echo $group
        echo $function

        # Read the file contents
        spec=""
        examples=()
        current_example=""

        while IFS= read -r line || [[ -n "$line" ]]; do
            # If it's the first line, treat it as the spec
            if [[ -z "$spec" ]]; then
                spec="$line"
            elif [[ -z "$line" ]]; then
                # Empty line: finalize the current example
                if [[ -n "$current_example" ]]; then
                    examples+=("$current_example")
                    current_example=""
                fi
            else
                # Add line to the current example
                if [[ -n "$current_example" ]]; then
                    current_example+=$'\n'
                fi
                current_example+="$line"
            fi
        done < "$file"

        # If there's an example still in progress, add it to the examples array
        if [[ -n "$current_example" ]]; then
            examples+=("$current_example")
        fi

        # Build the output line
        examples_str=$(printf ', "%s"' "${examples[@]}")
        examples_str=${examples_str:2}  # Remove leading ", "

        echo $spec
        echo $examples_str
        output_line="\"$group\" \"$function\" \"$spec\" [ $examples_str ]"

        # Append to the output file
        echo "$output_line" >> "$output_file"
    fi
done

echo "Output written to $output_file"
