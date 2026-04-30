mkdir -p $GNATSTUDIO_HOME/.gnatstudio/themes
cp test_semantic.tmTheme $GNATSTUDIO_HOME/.gnatstudio/themes/

mkdir -p $GNATSTUDIO_HOME/.gnatstudio/style_overrides
cp namespace_override.tmTheme $GNATSTUDIO_HOME/.gnatstudio/style_overrides/

$GPS --traceon=GPS.INTERNAL.Source_Editor.Buffer_Debug --traceoff=GPS.LSP.SEMANTIC_TOKENS --load=python:test.py
