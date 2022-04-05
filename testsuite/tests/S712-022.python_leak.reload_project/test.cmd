which git > /dev/null 2>&1 || exit 99

init_repo() {
  git init
  git add main.adb
  git commit -m "Adding main"
}

init_repo > /dev/null 2>&1

$GPS --load=python:test.py -P default.gpr
