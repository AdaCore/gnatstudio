# Build flatpak

* Place the distribution archive
  (like `gnatstudio-25.1-x86_64-linux-bin.tar.gz`)
  in this folder. Check the name of the distribution archive in the
  `com.adacore.gnatstudio.json` file (`modules/sources/path` field).
  Then run the packaging:

  ```shell
  flatpak-builder --force-clean --user --install-deps-from=flathub --repo=repo \
    --install builddir com.adacore.gnatstudio.json
  ```

* Now you can launch it

  ```shell
  flatpak run --filesystem=/your/project/path \
   --filesystem=/your/toolchain/path:ro \
   --env=PATH=/app/bin:/usr/bin:/your/toolchain/bin \
   com.adacore.gnatstudio
  ```

* You can build a bundle:

  ```shell
  flatpak build-bundle repo gnatstudio.flatpak com.adacore.gnatstudio \
    --runtime-repo=https://flathub.org/repo/flathub.flatpakrepo
  ```

* To uninstall it run:

  ```shell
  flatpak remove com.adacore.gnatstudio
  ```
