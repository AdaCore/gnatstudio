import os
import colorschemes


def on_gps_started(hook):
    target_dir = os.path.join(GPS.pwd(), "share", "color_themes", "snapshots")
    colorschemes.generate_snapshots(target_dir)

GPS.Hook("gps_started").add(on_gps_started)
