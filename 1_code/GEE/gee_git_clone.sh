
# This function clones a GEE project, adds .js extension to files, 
# moves them up a directory, and deletes the cloned git folder.
clone_gee_project() {
  # The function takes one argument, which is the URL of the GEE 
  # project to clone.
  url=$1

  # Extract the project name from the URL using the 'basename' command.
  project=$(basename "$url")

  # Clone the project from the provided URL.
  git clone "$url"

  # Find all files in the cloned project directory that do not have 
  # an extension and add the .js extension to them.
  find "$project" -type f -not -name "*.*" -exec mv "{}" "{}".js \;

  # Move all .js files from the project directory to the parent 
  # directory. If a file with the same name exists in the destination, 
  # it is replaced without prompting.
  mv -vf "$project"/*.js .

  # Delete the cloned project directory.
  rm -r "$project"
}

# set directory to clone GEE git into (should be empty)
cd /Users/brendancasey/Library/CloudStorage/GoogleDrive-bgcasey\
@ualberta.ca/My\ Drive/1_Projects/pileated_woodpecker/1_code/GEE

# clone PIWO and functions projects
clone_gee_project "https://earthengine.googlesource.com/users/bgcasey/PIWO"
clone_gee_project "https://earthengine.googlesource.com/users/bgcasey/functions"

