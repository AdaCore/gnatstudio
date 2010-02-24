"""This file is the main driver for the spark.py plug-in.
"""


import os, os.path, sys
import GPS, os_utils

spark_exe = os_utils.locate_exec_on_path ("spark")
if spark_exe != "":
  spark_plugins = os.path.dirname(spark_exe)+"/../share/gps/plug-ins"
  if os.path.isfile(spark_plugins+"/spark.py"):
    sys.path=[spark_plugins]+sys.path
  else:
    sys.path=[GPS.get_system_dir()+'share/gps/plug-ins/spark']+sys.path
  import spark

