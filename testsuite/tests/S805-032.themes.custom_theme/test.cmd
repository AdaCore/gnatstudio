if [ "$OS" = "Windows_NT" ]; then
   cp -f preferences.xml.win32 $GNATSTUDIO_HOME/.gnatstudio/
else
   cp -f preferences.xml $GNATSTUDIO_HOME/.gnatstudio/
fi

$GPS --load=python:test.py

