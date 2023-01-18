# This test is not runnable in the GitLab CI
[ "$GITLAB_CI" == "1" ] && exit 99

$GNATSTUDIO --load=python:test.py
