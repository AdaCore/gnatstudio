#!/usr/bin/env python

import sys, re

with open(sys.argv[1], "r") as f:
    txt = f.read()

x = re.search(r'window_y_Search" >\s+<value>\s(\d+)<', txt)
if int(x.group(1)) >= 3000:
    sys.stdout.write(x.group())
x = re.search(r'window_x_Search" >\s+<value>\s(\d+)<', txt)
if int(x.group(1)) >= 3000:
    sys.stdout.write(x.group())
