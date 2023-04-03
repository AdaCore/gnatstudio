# Making GNAT Studio AppImage

1. Create `gnatstudio.AppDir` directory
2. Unpack binary distribution archive as `gnatstudio.AppDir/usr`.
3. Copy this files to `gnatstudio.AppDir/`
   So you have

    gnatstudio.AppDir/
      .DirIcon -> gnatstudio.png
      AppRun
      gnatstudio.desktop
      gnatstudio.png
      usr/
        bin/gnatstudio
        bin/gnatstudio_exe
        ...

4. Download [appimagetool](https://github.com/AppImage/AppImageKit/releases)
5. Run

    appimagetool gnatstudio.AppDir

6. Result will be in `GNAT_Studio-x86_64.AppImage` file.
