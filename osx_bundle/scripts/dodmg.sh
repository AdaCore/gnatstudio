#!/bin/sh

APP=$1
VERSION=$2
PKG=${APP}-${VERSION}.pkg
backgroundPictureName=logo.png

usage() {
   echo "Usage : dodmg.sh <Application> <Version>"
   exit 1
}

if [ x${APP} = x ]; then
  usage
fi

if [ x${VERSION} = x ]; then
  usage
fi

SIZE=`du -k -d 0 ${PKG} | sed -e 's/\([0-9]*\).*/\1/'`
SIZE_SRCS=`du -k -d 0 srcs | sed -e 's/\([0-9]*\).*/\1/'`
let SIZE=${SIZE}+${SIZE_SRCS}
# Add some more space for extra folder metadata, and potential rounding issues
let SIZE=${SIZE}*11/10

rm -f pack.temp.dmg
rm -f ${APP}-${VERSION}.dmg

echo "=== Creating temporary dmg file of ${SIZE} ko"
hdiutil create -srcfolder "${PKG}" -volname "${APP} ${VERSION}" -fs HFS+ \
 -fsargs "-c c=64,a=16,e=16" -format UDRW -size ${SIZE}k pack.temp.dmg
echo "=== Mount the dmg file"
device=$(hdiutil attach -readwrite -noverify -noautoopen "pack.temp.dmg" | \
         egrep '^/dev/' | sed 1q | awk '{print $1}')
volume=`mount | grep $device | sed -e 's^.* on /Volumes/\(.*\) (.*^\1^'`
if [ x${device} = x ]; then
   echo " ERROR !!!"
   exit 1
fi

echo "    Mounted in '${volume}' as ${device}"
echo "=== Setting the folder's background"
mkdir "/Volumes/${volume}/.bg"

cp srcs/${backgroundPictureName} "/Volumes/${volume}/.bg/"

echo '
   tell application "Finder"
     tell disk "'${volume}'"
       open
       set current view of container window to icon view
       set toolbar visible of container window to false
       set statusbar visible of container window to false
       set bounds of container window to {400, 100, 885, 430}
       set theViewOptions to the icon view options of container window
       set arrangement of theViewOptions to not arranged
       set icon size of theViewOptions to 72
       set background picture of theViewOptions to file ".bg:'${backgroundPictureName}'"
       --  make new alias file at container window to POSIX file "/Applications" with properties {name:"Applications"}
       set position of item "'${PKG}'" of container window to {100, 170}
       --  set position of item "Applications" of container window to {275, 170}
       update without registering applications
       close
       open
       delay 5
       close
     end tell
   end tell
' | osascript

echo "=== Set the dmg as read-only, eject and convert"
chmod -Rf go-w /Volumes/"${volume}"/*
hdiutil detach /Volumes/"${volume}"
hdiutil convert -ov -format UDBZ "pack.temp.dmg" -o  "${APP}-${VERSION}"
hdiutil internet-enable "${APP}-${VERSION}.dmg"
rm -f pack.temp.dmg

echo "=== Done"
