# set directory to clone GEE git into (should be empty)
cd /Volumes/Projects/pileated_woodpecker_lidar/1_code/GEE

# delete all .js files in directory
rm *.js

# clone gee scripts
git clone https://earthengine.googlesource.com/users/bgcasey/PIWO

# delete .git folder
rm -r PIWO/

# add the .js file extention to files
find PIWO -type f -not -name "*.*" -exec mv "{}" "{}".js \;

# move .js files up a directory
mv -v PIWO/*.js .
mv -v PIWO/functions/ .

# delete now empty gee git folder
rmdir PIWO

