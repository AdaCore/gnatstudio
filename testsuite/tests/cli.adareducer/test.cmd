echo "## TEST HELP"
gnatstudio_cli adareducer --help | sed -e "s/.exe//"
echo ""

echo "## TEST MISSING PROJECT"
gnatstudio_cli adareducer
echo ""

echo "## TEST MISSING ORACLE"
gnatstudio_cli adareducer -P p
echo ""

echo "## TEST FUNCTIONALITY"
gnatstudio_cli adareducer -P p -s oracle.sh > /dev/null
cat hello.adb
