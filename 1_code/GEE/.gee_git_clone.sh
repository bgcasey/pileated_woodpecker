# ---
# title: "Clone a GEE Project"
# author: "Brendan Casey"
# created: "2024-06-13"
# description: "This .zsh function clones a GEE project, adds .js 
# extension to files, moves them up a directory, and deletes the 
# cloned git folder."
# ---

# The function takes three arguments:
# 1. The URL of the GEE project to clone.
# 2. A flag indicating whether to move .js files to the parent 
#    directory.
# 3. A flag indicating whether to delete the cloned project directory.

clone_gee_project() {
  url=$1
  move_files=${2:-false}
  delete_dir=${3:-false}

  # Extract the project name from the URL using the 'basename' 
  # command.
  project=$(basename "$url")
  
  # Remove the existing directory if it exists.
  rm -rf "$project"


  # Clone the project from the provided URL.
  git clone "$url"

  # Remove the .git directory.
  rm -rf "$project/.git"
  
  # Find all files in the cloned project directory that do not have 
  # an extension and add the .js extension to them.
  find "$project" -type f -not -name "*.*" -exec mv "{}" "{}".js \;

  # If the second argument is true, move all .js files from the 
  # project directory to the parent directory. If a file with the 
  # same name exists in the destination, it is replaced without 
  # prompting.
  if [ "$move_files" = true ]; then
    mv -vf "$project"/*.js .
  fi

  # If the third argument is true, delete the cloned project 
  # directory.
  if [ "$delete_dir" = true ]; then
    rm -r "$project"
  fi
}

# To clone the project, add .js extensions to files without an 
# extension, but not move the .js files to the parent directory or 
# delete the project directory:
# clone_gee_project "https://earthengine.googlesource.com/users/bgcasey/PIWO"

# To clone the project, add .js extensions to files without an 
# extension, move the .js files to the parent directory, but not 
# delete the project directory:
# clone_gee_project "https://earthengine.googlesource.com/users/bgcasey/PIWO" true

# To clone the project, add .js extensions to files without an 
# extension, move the .js files to the parent directory, and delete 
# the project directory:
# clone_gee_project "https://earthengine.googlesource.com/users/bgcasey/PIWO" true

# set directory to clone GEE git into (should be empty)
cd /Users/brendancasey/Library/CloudStorage/GoogleDrive-bgcasey\
@ualberta.ca/My\ Drive/1_Projects/pileated_woodpecker/1_code/GEE

# clone PIWO and functions projects
clone_gee_project "https://earthengine.googlesource.com/users/bgcasey/PIWO" true true
clone_gee_project "https://earthengine.googlesource.com/users/bgcasey/functions" 

