"""This file provides utilities used by the SPARK plug-in for GPS.
Copyright (c) 2004-2009 Praxis High Integrity Systems Limited
Copyright (c) 2005-2009 AdaCore

See the GPS documentation for more details.
"""


###########################################################################
## No user customization below this line
###########################################################################

import os, os.path, string
import os_utils
import GPS

b = """<?xml version="1.0"?>
<GPS>
  <doc_path>~</doc_path>

   <submenu before="About">
      <title>/Help/SPARK</title>
   </submenu>

   <submenu>
      <title>/Help/SPARK/Language</title>
   </submenu>

   <submenu>
      <title>/Help/SPARK/Tools</title>
   </submenu>

   <submenu>
      <title>/Help/SPARK/Release Notes</title>
   </submenu>

   <submenu>
      <title>/Help/SPARK/Reference</title>
   </submenu>

   <submenu>
      <title>/Help/SPARK/Example Programs</title>
   </submenu>

  <documentation_file>
     <name>SPARK95.htm</name>
     <descr>SPARK95 LRM (without RavenSPARK)</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Language/SPARK95 LRM (without RavenSPARK)</menu>
  </documentation_file>

  <documentation_file>
     <name>SPARK95_RavenSPARK.htm</name>
     <descr>SPARK95 LRM (with RavenSPARK)</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Language/SPARK95 LRM (with RavenSPARK)</menu>
  </documentation_file>

  <documentation_file>
     <name>SPARK83.htm</name>
     <descr>SPARK83 LRM</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Language/SPARK83 LRM</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_Ravenscar.htm</name>
     <descr>RavenSPARK Rationale and Guide</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Language/RavenSPARK Rationale and Guide</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_UM.htm</name>
     <descr>Examiner User Manual</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Tools/Examiner User Manual</menu>
  </documentation_file>

  <documentation_file>
     <name>Simp_UM.htm</name>
     <descr>Simplifier User Manual</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Tools/Simplifier User Manual</menu>
  </documentation_file>

  <documentation_file>
     <name>Pogs_UM.htm</name>
     <descr>POGS User Manual</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Tools/POGS User Manual</menu>
  </documentation_file>

  <documentation_file>
     <name>SPARKMake_UM.htm</name>
     <descr>SPARKMake User Manual</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Tools/SPARKMake User Manual</menu>
  </documentation_file>

  <documentation_file>
     <name>SPARKFormat.htm</name>
     <descr>SPARKFormat User Manual</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Tools/SPARKFormat User Manual</menu>
  </documentation_file>

  <documentation_file>
     <name>Checker_UM.htm</name>
     <descr>Checker User Manual</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Tools/Checker User Manual</menu>
  </documentation_file>

  <documentation_file>
     <name>Release_Note_8.htm</name>
     <descr>Release Note 8</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 8</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_RN_7p6.htm</name>
     <descr>Release Note 7.6</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 7.6</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_RN_7p5.htm</name>
     <descr>Release Note 7.5</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 7.5</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_RN_7p4.htm</name>
     <descr>Release Note 7.4</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 7.4</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_RN_7p31.htm</name>
     <descr>Release Note 7.3.1</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 7.3.1</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_RN_7p3.htm</name>
     <descr>Release Note 7.3</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 7.3</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_RN_7p2.htm</name>
     <descr>Release Note 7.2</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 7.2</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_RN_7.htm</name>
     <descr>Release Note 7.1</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 7.1</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_RN_6p3.htm</name>
     <descr>Release Note 6.3</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 6.3</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_RN_6p1.htm</name>
     <descr>Release Note 6.1</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 6.1</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_RN_6.htm</name>
     <descr>Release Note 6.0</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 6.0</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_RN_5_03.htm</name>
     <descr>Release Note 5.03</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 5.03</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_RN_5.htm</name>
     <descr>Release Note 5.0</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 5.0</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_SRN_OptFlow.htm</name>
     <descr>Optional Flow Analysis</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Optional Flow Analysis</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_SRN_RealRTC.htm</name>
     <descr>Real-Number RTCs</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Real-Number RTCs</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_GenVCs.htm</name>
     <descr>Generation of VCs</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Reference/Generation of VCs</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_GenRTCs.htm</name>
     <descr>Generation of RTCs</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Reference/Generation of RTCs</menu>
  </documentation_file>

  <documentation_file>
     <name>Examiner_GenPFs.htm</name>
     <descr>Generation of PFs</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Reference/Generation of PFs</menu>
  </documentation_file>

  <documentation_file>
     <name>Informed.htm</name>
     <descr>INFORMED</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Reference/INFORMED Design Method</menu>
  </documentation_file>

  <documentation_file>
     <name>SPARK_IO.htm</name>
     <descr>SPARK_IO</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Reference/SPARK__IO</menu>
  </documentation_file>

  <documentation_file>
     <name>Checker_Rules.htm</name>
     <descr>Checker Rules</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Reference/Checker Rules</menu>
  </documentation_file>

  <documentation_file>
     <name>minepump.htm</name>
     <descr>Mine Pump</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Example Programs/Mine Pump</menu>
  </documentation_file>
</GPS>

"""

spark = os_utils.locate_exec_on_path ("spark")
if spark != "":
  sparkdocdir = os.path.dirname(spark)+os.sep+os.pardir+os.sep+"docs"+os.sep+"HTML"
  b = b.replace('~', sparkdocdir)
  GPS.parse_xml(b)
