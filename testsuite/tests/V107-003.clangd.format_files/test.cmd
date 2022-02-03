cp -f preferences.xml $GNATSTUDIO_HOME/.gnatstudio/
$GPS --load=python:test.py --traceoff=GPS.LSP.CLANGD_SUPPORT.AVOID_DIALOG_ON_PREF_CHANGED --traceon=GPS.LSP.CPP_SUPPORT
