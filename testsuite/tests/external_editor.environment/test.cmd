# Model the launcher's saved user environment and its IDE-specific overrides.
export GPS_STARTUP_XDG_DATA_DIRS="${XDG_DATA_DIRS:-/usr/local/share:/usr/share}"
export XDG_DATA_DIRS="$PWD/ide-data:$GPS_STARTUP_XDG_DATA_DIRS"
export GPS_STARTUP_EXTERNAL_EDITOR_SENTINEL="user value with spaces"
export EXTERNAL_EDITOR_SENTINEL="IDE value"
export GPS_STARTUP_EXTERNAL_EDITOR_ABSENT="_ABSENT_VARIABLE_"
export EXTERNAL_EDITOR_ABSENT="IDE only"
$GNATSTUDIO --load=python:test.py
