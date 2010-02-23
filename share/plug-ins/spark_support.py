"""This file is the main driver for the spark.py plug-in.
"""


import os, os.path, sys
import os_utils

spark_exe = os_utils.locate_exec_on_path ("spark")
if spark_exe != "":
  spark_plugins = os.path.dirname(spark_exe)+"/../share/gps/plug-ins"
  if os.path.isfile(spark_plugins+'/spark.py'):
    sys.path=[spark_plugins]+sys.path
    import spark
  else:
    import gps_utils.spark_default
