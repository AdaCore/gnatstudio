which git > /dev/null 2>&1 || exit 99

# Disable VCS and enable VCS2 explicitly
GPS="$GPS --traceon=GPS.VCS.MODULE"

init_repo() {
  git init
  git config user.email '<>'
  git config user.name gps
  git add p.gpr src1/foo.adb src2/bar.adb
  git commit -m init
}

init_repo > /dev/null 2>&1

$GPS -P p.gpr --load=python:test.py
