# Copy markdown files from obsidian
cp /Users/brendancasey/Dropbox/obsidian_notes/1_projects/piwo/github_documentation/*  ~/Library/CloudStorage/GoogleDrive-bgcasey@ualberta.ca/My\ Drive/1_Projects/pileated_woodpecker/documentation


# remove all yaml headers
remove_yaml_headers() {
  # The first argument is the directory path.
  dir_path="$1"

  # Find all markdown files in the directory.
  find "$dir_path" -name "*.md" -exec sed -i.bak '/^---$/,/^---$/d' {} \;

  # Remove backup files created by sed.
  find "$dir_path" -name "*.bak" -delete
}

remove_yaml_headers ~/Library/CloudStorage/GoogleDrive-bgcasey@ualberta.ca/My\ Drive/1_Projects/pileated_woodpecker/documentation


# Move project readme to root directory
mv -f ~/Library/CloudStorage/GoogleDrive-bgcasey@ualberta.ca/My\ Drive/1_Projects/pileated_woodpecker/documentation/readme.md ~/Library/CloudStorage/GoogleDrive-bgcasey@ualberta.ca/My\ Drive/1_Projects/pileated_woodpecker/

