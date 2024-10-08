echo "## TEST HELP"
gnatstudio_cli adareducer --help | sed -e "s/.exe//"
echo ""

echo "## TEST MISSING PROJECT"
gnatstudio_cli adareducer

# Test that we can accept strings that contain "\U"
# without Python trying to decode an Unicode char.
gnatstudio_cli adareducer -P "C:\Users\doesnotexist.gpr" -s "C:\Users\s.sh"

echo ""
echo "## TEST MISSING ORACLE"
gnatstudio_cli adareducer -P p
echo ""

echo "## TESTING THE ORACLE"
./oracle.sh
echo ""

echo "## TEST FUNCTIONALITY"
gnatstudio_cli adareducer -P p -s oracle.sh | grep -v hello.adb
echo ""
cat hello.adb
