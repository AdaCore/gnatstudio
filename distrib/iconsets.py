#!/usr/bin/env python

"""
This script converts all SVG icons from the GPS distribution to
create a lighter set of icons suitable for darker themes.
"""

import os

dir = "../share/icons/dark/svg/"
outdir = "../share/icons/light/svg/"

for icon in os.listdir(dir):
    if icon.endswith(".svg"):
        txt = open(os.path.join(dir, icon)).read()
        txt = txt.replace("stop-color:#000000;stop-opacity:0.7",
                          "stop-color:#FFFFFF;stop-opacity:0.8")
        txt = txt.replace("stop-color:#000000;stop-opacity:0.6",
                          "stop-color:#FFFFFF;stop-opacity:0.7")

        txt = txt.replace("stop-color:#010101;stop-opacity:0.6",
                          "stop-color:#FEFEFE;stop-opacity:0.7")
        txt = txt.replace("stop-color:#010101;stop-opacity:0.7",
                          "stop-color:#FEFEFE;stop-opacity:0.8")

        file(os.path.join(outdir, icon), "w").write(txt)
