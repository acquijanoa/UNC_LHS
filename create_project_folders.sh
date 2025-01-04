#!/bin/bash

# Prompt the user for the project name
read -p "Enter the Project Name: " project_root

# Create the main project folder
mkdir -p "$project_root"

# Create subfolders
mkdir -p "$project_root/data/raw"
mkdir -p "$project_root/data/processed"
mkdir -p "$project_root/data/metadata"
mkdir -p "$project_root/scripts"
mkdir -p "$project_root/results/tables"
mkdir -p "$project_root/results/figures"
mkdir -p "$project_root/results/reports"
mkdir -p "$project_root/logs"
mkdir -p "$project_root/config"
#mkdir -p "$project_root/archive"

# Create config.yaml
config_file="$project_root/config/config.yaml"
cat <<EOL > "$config_file"
# config.yaml
example_key: example_value
EOL
echo "config.yaml file created at $config_file"

# Create empty Snakefile
snakefile="$project_root/Snakefile"
cat <<EOL > "$snakefile"
# Snakefile

configfile: "config/config.yaml"
EOL

# Create placeholder files
echo "Overview of the project" > "$project_root/README.md"
echo "Detailed instructions for collaborators" > "$project_root/instructions.md"
echo "Snakefile file created at $snakefile"

# Confirmation message
echo "Project folder structure for '$project_root' created successfully!"

