which git > /dev/null 2>&1 || exit 99

# Disable VCS and enable VCS2 explicitly
GPS="$GPS --traceon=GPS.VCS.MODULE"

init_repo() {
  cd prj
  git init
  git config user.email '<>'
  git config user.name gps
  cp ignored_files .gitignore
  git add a.adb b.adb c.adb .gitignore
  git commit -m "Hello"
  cd ..
}

init_repo > /dev/null 2>&1

$GPS -P prj/default.gpr --load=python:test.py
