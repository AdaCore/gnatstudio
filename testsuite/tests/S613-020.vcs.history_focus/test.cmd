which git > /dev/null 2>&1 || exit 99

# Disable VCS and enable VCS2 explicitly
GPS="$GPS --traceon=GPS.INTERNAL.MODULE_VCS2"

init_repo() {
  git init
  echo 'with "nested/prj2";project prj is end prj;' > prj.gpr
  git config user.email '<>'
  git config user.name gps
  git add prj.gpr
  git commit -m init prj.gpr
  echo '-- new text here' >> prj.gpr
  mkdir nested
  cd nested
  git init
  echo 'project prj2 is end prj2;' > prj2.gpr
  git config user.email '<>'
  git config user.name gps
  git add prj2.gpr
  git commit -m init2 prj2.gpr
  cd ..
}

init_repo > /dev/null 2>&1

$GPS -P prj.gpr --load=python:test.py
