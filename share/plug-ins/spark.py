"""This file provides utilities used by the SPARK plug-in for GPS.
Copyright (c) 2004-2008 Praxis High Integrity Systems Limited
Copyright (c) 2005-2008 AdaCore

SPARK Toolset Customization for GPS
-----------------------------------

Introduction
------------

This plug-in provides support for the SPARK language and toolset.

It is intended for use with Release 7.6.2 or later of the
SPARK Toolset.  The customization depends on the presence of the
"brief output" Examiner switch, which is not available
in earlier releases.  If you don't have release 7.6.2 or better,
please contact Praxis High Integrity Systems: sparkinfo@praxis-his.com

This plug-in remains a work-in-progress, and there is lots of
room for improvement.  We welcome contributions.

Use
---

This plug-in will be enabled only if you have spark tools available in
your PATH variable (e.g. SPARK Examiner, POGs, etc).

Edit the project preferences (from the Project menu) and tick the necessary
languages on the Languages tab: Index, Listing, Metafile, Siv, Vcg.

Switches for the tools can be set on the same menu under the Switches tab.

The Examiner output produced is always -brief so that GPS is able to parse
the resulting files.

The tools can be run from the SPARK menu, or right-clicking on files will
produce a contextual menu.  Not all tools can be run on all files and GPS
will produce a warning if the incorrect context is selected.
"""


###########################################################################
## No user customization below this line
###########################################################################

import os, os.path, re, string
import os_utils
import GPS

# Future work
# -----------

# Nice-to-haves include:
#   - Context sensitive navigation in annotations
#   - Hot keys displayed on the SPARK Menu

spark_console="SPARK Output"
spark_category="Examiner"

spark_separator='-'

def on_match (process, match, since_last):
  try:
     process.output += since_last + match
  except:
     process.output = since_last + match
  GPS.Console (spark_console).write (since_last + match)

def on_exit (process, status, remaining_output):
  # Protect e.g. "Flow Error:123:" from being detected as a file reference
  GPS.Locations.parse (
     process.output.replace (" Error:"," Error,").replace \
       (" Warning:"," Warning,"),
     category=spark_category)

def examine_file (file):
  """Examine current file through the SPARK examiner. file is an instance
     of GPS.File"""
  GPS.MDI.save_all (False)
  GPS.Locations.remove_category (spark_category)
  sw = file.project().get_tool_switches_as_string ("Examiner")
  cmd = "spark "+sw + " "+spark_separator+'brief "' + file.name() + '"'
  GPS.Console (spark_console, accept_input=False).clear ()
  GPS.Console (spark_console).write (cmd + "\n")
  GPS.Process (cmd, remote_server="Build_Server", regexp=".+", on_match=on_match, on_exit=on_exit)
  GPS.MDI.get (spark_console).raise_window ()

def pogs_directory():
  """Return the directory in which pogs should be run. This directory
     never contains a trailing directory separator"""
  # If the -output_directory switch is specified, use that directory

  dir = None
  sw = GPS.Project.root().get_tool_switches_as_list ("Examiner")
  for s in sw:
    if s.find (spark_separator + "output_directory=") == 0:
       dir=s[18:]

  # Else take the common parent for all sources dirs
  if not dir:
     src = GPS.Project.root().source_dirs (recursive=True)
     dir = os.path.commonprefix (src)

  # Else take the project's root directory
  if not dir:
     dir = os.path.dirname (GPS.Project.root().file().name())

  dir = dir.rstrip ("/\\")

  return dir

def show_pogs_file():
  """Show the pogs file of the current project"""
  # Pogs looks for .vcg files in current dir and all subdirs. For now,
  # we'll start it in the root project's directory, but we should be
  # smarter
  GPS.MDI.save_all (False)
  dir = pogs_directory()
  GPS.cd (dir)
  sw = GPS.Project.root().get_tool_switches_as_string ("POGS")
  cmd = "pogs " + sw
  GPS.Console (spark_console, accept_input=False).clear ()
  GPS.Console (spark_console).write (cmd + "\n")
  GPS.Console (spark_console).write (GPS.Process (cmd, remote_server="Build_Server").get_result())
  GPS.MDI.get (spark_console).raise_window ()
  dir_name = os.path.basename (dir)
  buf = GPS.EditorBuffer.get (GPS.File (os.path.join (dir,dir_name)+'.sum'), force=True)
  GPS.MDI.get_by_child (buf.current_view()).raise_window()

def do_pogs_xref (context, simplified):
  """Jump to the VC referenced in the current line of the POGS output"""
  editor = GPS.EditorBuffer.get()
  curs = editor.current_view().cursor()
  line = editor.get_chars (curs.beginning_of_line(), curs.end_of_line())
  vc = re.search ("^\s*(\d+)", line).group (1)  # VC rule number

  (frm,to) = curs.search ("^File (.*)$", backward=True, regexp=True)
  vc_file=editor.get_chars (frm+5, to)

  if simplified:
     vc_file = vc_file.replace (".vcg", ".siv")

  f = GPS.EditorBuffer.get (GPS.File (vc_file))
  loc = GPS.EditorLocation (f, 1, 1)
  (frm, to) = loc.search ("^(procedure|function)_\S+_" + vc + "\.$", regexp=True)

  # Workaround to make sure we see at least part of the rule: move forward
  # a number of lines
  f.current_view().goto (frm.forward_line (3))

  GPS.MDI.get_by_child (f.current_view()).raise_window()

def pogs_xref (context):
  do_pogs_xref (context, simplified=False)
def pogs_simplified_xref (context):
  do_pogs_xref (context, simplified=True)

def has_vc (context):
  """Return TRUE if the current line of the POGS output references a VC"""
  try:
     # Avoid doing the work several times for all entries in the menu
     return context.has_vc
  except:
     try:
        if os.path.splitext (context.file().name())[1] != ".sum":
           return False
     except:
        return False  # context.file() does not exist
     editor = GPS.EditorBuffer.get()
     curs = editor.current_view().cursor()
     line = editor.get_chars (curs.beginning_of_line(), curs.end_of_line())
     context.has_vc = re.search ("\|\s+YES\s+\|", line) != None
     return context.has_vc

def sparkmake ():
  GPS.MDI.save_all (False)
  sw = GPS.current_context().project().get_tool_switches_as_string ("SPARKmake")
  cmd = "sparkmake " + sw + " " + GPS.current_context().file().name()
  GPS.Console (spark_console, accept_input=False).clear ()
  GPS.Console (spark_console).write (cmd + "\n")
  GPS.Console (spark_console).write (GPS.Process (cmd, remote_server="Build_Server").get_result())
  GPS.MDI.get (spark_console).raise_window ()

a = """<?xml version="1.0"?>
<!--  Note: do not use the ampersand character in XML comments!!       -->

<SPARK>

  <Language>
    <Name>Metafile</Name>
    <Spec_Suffix>.smf</Spec_Suffix>
  </Language>

  <!-- Define VCG and SIV languages separately so that filtering   -->
  <!-- can be done to avoid simplifying siv files                  -->

  <Language>
    <Name>SIV</Name>
    <Spec_Suffix>.siv</Spec_Suffix>
  </Language>

  <Language>
    <Name>VCG</Name>
    <Spec_Suffix>.vcg</Spec_Suffix>
  </Language>

  <!-- Index and Listing are just set up so that GPS can recognise them -->

  <Language>
    <Name>Index</Name>
    <Spec_Suffix>.idx</Spec_Suffix>
  </Language>

  <Language>
    <Name>Listing</Name>
    <Spec_Suffix>.lss</Spec_Suffix>
    <Body_Suffix>.lsb</Body_Suffix>
  </Language>

  <!-- Set up the Project Preferences menu                           -->
  <!-- This allows you to select all the options for different tools -->

  <tool name="Examiner">
    <language>Ada</language>
    <switches columns ="2" lines="5" switch_char="~">
      <title line="1" column-span="2">Examiner Files</title>
      <field line="1" label="Index File " as-file="true" switch="~index_file" separator="="/>
      <field line="1" label="Warning File " as-file="true" switch="~warning_file" separator="="/>
      <field line="1" label="Configuration File " as-file="true" switch="~config" separator="="/>
      <field line="1" label="Output directory" as-dir="true" switch="~output_directory" separator="="/>
      <check column="1" line="1" label="Ignore spark.sw" switch="~noswitch" />
      <title line="1" column="2" column-span="0" />
      <title column="1" line="2">Language</title>
      <radio column="1" line="2" >
        <radio-entry label="SPARK95" switch="~profile=sequential" />
        <radio-entry label="RavenSPARK" switch="~profile=ravenscar" />
        <radio-entry label="SPARK83" switch="~ada83" />
      </radio>
      <title column="2" line="2">Analysis</title>
      <radio column="2" line="2">
        <radio-entry label="Information and Data Flow" switch="~flow_analysis=information" />
        <radio-entry label="Data Flow only" switch="~flow_analysis=data" />
      </radio>
      <check column="2" line="2" label="Generate VCs" switch="~vcg" />
      <check column="2" line="2" label="Syntax check only" switch="~syntax_check" />
      <title line="3" line-span="2">Replacement rules for composite constants</title>
      <radio line="3">
        <radio-entry label="None" switch="~rules=none" />
        <radio-entry label="Lazy" switch="~rules=lazy" />
		<radio-entry label="Keen" switch="~rules=keen" />
        <radio-entry label="All" switch="~rules=all" />
      </radio>
      <title column="2" line="3">Error Explanations</title>
      <radio column="2" line="3">
        <radio-entry label="Off" switch="" />
        <radio-entry label="First Occurrence" switch="~error_explanations=first" />
	<radio-entry label="Every Occurrence" switch="~error_explanations=every" />
      </radio>
      <title line="4" column="1" line-span="0" />
      <title column="2" line="4">General</title>
      <field column="2" line="4" label=" Annotation Character " switch="~annotation_character" separator="=" tip="Enter a single character to follow '--' as the mark for SPARK annotations (default '#')" />
      <title line="5" column-span="2">Output</title>
      <check line="5" label=" Plain Output " switch="~plain" />
      <check line="5" label=" HTML Output " switch="~html" />
      <field line="5" label=" Listing File Extension " as-file="true" switch="~listing" separator="="/>
      <field line="5" label=" Report File Name " as-file="true" switch="~report" separator="="/>
      <title line="5" column="2" column-span="0" />
    </switches>
  </tool>

  <tool name="SPARKSimp">
    <language>Ada</language>
    <switches lines="3" switch_char="~">
      <title line="1">Simplification order</title>
      <check line="1" label="Simplify all" switch="~a" />
      <check line="1" label="Sort files, largest first" switch="~t" />
      <check line="1" label="Reverse sort order" switch="~r" />
      <title line="2">Output</title>
      <check line="2" label="Log output" switch="~l" />
      <check line="2" label="Verbose output" switch="~v" />
      <check line="2" label="Echo Simplifier output" switch="~e" />
      <title line="3">Process control</title>
      <check line="3" label="Dry run" switch="~n" />
      <spin line="3" label="Multiprocessing" switch="~p=" min="1" max="100" default="1"
            tip="Use N processes to run the Simplifier. On a multiprocessor machine simplifications will occur in parallel" />
    </switches>
  </tool>

  <tool name="Simplifier">
    <language>Ada</language>
    <switches lines="3" switch_char="~">
      <title line="1">Output</title>
      <check line="1" label="No Echo" switch="~noecho" />
      <check line="1" label="Plain Output" switch="~plain" />
      <title line="2">Tactics</title>
      <check line="2" label="No Simplification" switch="~nosimplification" />
      <check line="2" label="No Standardisation" switch="~nostand" />
      <check line="2" label="No Contradiction Hunt" switch="~nocontra" />
      <check line="2" label="No Substitution Elimination" switch="~nosubstitution" />
      <check line="2" label="No Rule Substitution" switch="~norule_substitution" />
      <check line="2" label="No Expression Reduction" switch="~noexp" />
      <title line="3">Limits</title>
      <spin line="3" label="Memory Limit" switch="~memory_limit=" min="250000" max="30000000" default="9000000"
            tip="Max PROLOG Heap.  Default 9000000." />
    </switches>
  </tool>

  <tool name="SPARKFormat">
    <language>Ada</language>
    <switches lines="4" columns="4" switch_char="~">
      <title line="1" column="1">Global variables modes</title>
      <radio line="1" column="1">
        <radio-entry label="Unchanged" switch="~noadd_modes" />
        <radio-entry label="Add modes to procedures" switch="~add_modes" />
      </radio>
      <title line="1" column="2">Function globals</title>
      <radio line="1" column="2">
            <radio-entry label="Unchanged" switch="" />
            <radio-entry label="Force 'in'" switch="~d=i" />
            <radio-entry label="Force unmoded" switch="~d=u" />
      </radio>
      <title line="1" column="3">Order</title>
      <radio line="1" column="3">
        <radio-entry label="Declaration" switch="~order=declaration" />
        <radio-entry label="Alphabetic" switch="~order=alphabetic" />
      </radio>

      <title line="2" column="1">Annotation compression</title>
      <radio line="2" column="1">
        <radio-entry label="Compress" switch="~compress" />
        <radio-entry label="Expand" switch="~expand" />
      </radio>
      <title line="2" column="2">Annotations</title>
      <field line="2" column="2" label="Annotation Character" switch="~annotation_character=" tip="Enter a single character to follow '--' as the mark for SPARK annotations (default '#')" />

      <title line="3" column="1">Default Switch File</title>
      <check line="3" column="1" label="Ignore spark.sw" switch="~noswitch" />
      <title line="4" column="1" column-span="2">Indentation</title>
      <field line="4" column="1" label="Globals indentation" switch="~global_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for the global variables, or state 'inline' (default 'inline')" />
      <field line="4" label="Exports indentation" switch="~export_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for the export variables, or state 'inline' (default 'inline')" />
      <field line="4" label="Imports indentation" switch="~import_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for the import variables, or state 'inline' (default 'inline')" />
      <field line="4" label="Seperators indentation" switch="~separator_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for the separators ('from' and ampersand), or state 'inline' (default 'inline')" />
      <field line="4" label="Inherits indentation" switch="~inherit_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for the package names, or state 'inline' (default 'inline')" />
      <field line="4" label="Own indentation" switch="~own_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for own variables, or state 'inline' (default 'inline')" />
      <field line="4" label="Refinement indentation" switch="~refinement_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for own variables, or state 'inline' (default 'inline')" />
      <field line="4" label="Constituent indentation" switch="~constituent_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for constituents, or state 'inline' (default 'inline')" />
      <field line="4" label="Initialization indentation" switch="~initialization_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for own variables, or state 'inline' (default 'inline')" />
      <field line="4" label="Properties indentation" switch="~properties_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for own variables, or state 'inline' (default 'inline')" />
    </switches>
  </tool>

  <tool name="POGS">
    <language>Ada</language>
    <switches lines="2" switch_char="~">
      <title line="1">Options</title>
      <check line="1" label="Plain Output"
       tip="Prevent release information and file paths being output to .sum file"
       switch="~p" />
      <check line="1" label="Ignore Dates"
       tip="Prevent checking of date and time stamps of VCs and Proof Log files"
       switch="~i" />

      <title line="2">Output</title>
      <radio line="2">
        <radio-entry label="Default" tip="Default output" switch="" />
        <radio-entry label="Short summary"
         tip="Prevent per-subprogram analysis section being output to .sum file"
         switch="~s" />
        <radio-entry label="XML" tip="Output summary information in XML format"
         switch="~x" />
      </radio>
    </switches>
  </tool>

  <tool name="SPARKmake">
    <language>Ada</language>
    <switches lines="2" switch_char="~">
      <title line="1">Input File Options</title>
      <field line="1" label="Directory" as-directory="true" switch="~dir=" />
      <field line="1" label="Include" switch="~inc=" />
      <field line="1" label="Exclude" switch="~e=" />

      <title line="2">Output File Options</title>
      <field line="2" label="Index" as-file="true" switch="~ind=" />
      <field line="2" label="Meta" as-file="true" switch="~m=" />
      <check line="2" label="No index file"
             tip="Suppress generation of index file" switch="~noindexfile" />
      <check line="2" label="No meta file"
             tip="Suppress generation of meta file" switch="~nometafile" />
    </switches>
  </tool>

  <!-- Filtering actions by file extensions is done by setting up -->
  <!-- different languages for each extension. This means if you  -->
  <!-- want to use metafiles or the simplifier, you'll also have  -->
  <!-- to select metafile and VCG as project languages.           -->

  <action name="Examine file" category="Spark" output="none">
     <filter language="SPARK"/>
     <filter language="Ada"/>
     <shell lang="python">spark.examine_file (GPS.File ("%F"))</shell>
  </action>

  <action name="SPARKFormat file" category="Spark" output="none">
     <filter language="SPARK" />
     <filter language="Ada" />
     <shell>MDI.save_all false</shell>
     <shell>Project %p</shell>
     <shell>Project.get_tool_switches_as_string %1 "SPARKFormat" </shell>
     <external output="SPARK Output" server="build_server">sparkformat %1 %F</external>
     <shell>Editor.edit %F 0 0 0 true</shell>
  </action>

  <action name="Examine metafile" category="Spark" output="none">
     <filter language="Metafile" />
     <shell>MDI.save_all false</shell>
     <shell>Locations.remove_category Examiner</shell>
     <shell>Project %p</shell>
     <shell>Project.get_tool_switches_as_string %1 "Examiner" </shell>
     <external output="SPARK Output" server="build_server">spark %1 ~brief @%F</external>
     <on-failure>
          <shell>Locations.parse &quot;&quot;&quot;%1 &quot;&quot;&quot; Examiner</shell>
     </on-failure>
     <shell>Locations.parse &quot;&quot;&quot;%1 &quot;&quot;&quot; Examiner</shell>
  </action>

  <action name="SPARK help" output="SPARK Help" category="Spark">
     <external server="build_server">spark ~help</external>
  </action>

  <action name="Simplify file" category="Spark" output="none">
    <filter language="VCG" />
    <shell>Project %p</shell>
    <shell>Project.get_tool_switches_as_string %1 "Simplifier" </shell>
    <external server="build_server" output="Simplifier Output">spadesimp %F %1</external>
  </action>

  <action name="Simplify all" category="Spark" output="none">
    <shell>Project %P</shell>
    <shell>Project.get_tool_switches_as_string %1 "Simplifier" </shell>
    <shell>Project %P</shell>
    <shell>Project.get_tool_switches_as_string %1 "SPARKSimp" </shell>
    <external server="build_server" output="SPARKSimp Output">sparksimp %1 ~sargs %3 </external>
  </action>

  <action name="POGS" category="Spark" output="none">
    <shell lang="python">spark.show_pogs_file()</shell>
  </action>

  <action name="SPARKmake" category="Spark" output="none">
    <filter language="SPARK" />
    <filter language="Ada" />
    <shell lang="python">spark.sparkmake ()</shell>
  </action>


 <!-- Set up SPARK menu -->

 <submenu before="Window">
    <Title>_SPARK</Title>
      <menu action="Examine file">
        <Title>_Examine File</Title>
      </menu>
      <menu action="Examine metafile">
        <Title>Examine _Metafile</Title>
      </menu>
      <menu action="SPARK help">
        <Title>_Help</Title>
      </menu>
      <menu action="SPARKFormat file">
        <Title>SPARK _Format File</Title>
      </menu>
      <menu action="Simplify file">
        <Title>_Simplify File</Title>
      </menu>
      <menu action="Simplify all">
        <Title>Simplify _All</Title>
      </menu>
      <menu action="POGS">
        <Title>P_OGS</Title>
      </menu>
      <menu action="SPARKmake">
        <Title>SPARKma_ke</Title>
      </menu>
 </submenu>

  <contextual action="Examine metafile" >
    <Title>SPARK/Examine Metafile</Title>
  </contextual>

  <contextual action="Examine file" >
    <Title>SPARK/Examine File</Title>
  </contextual>

  <contextual action="SPARKFormat file" >
    <Title>SPARK/SPARKFormat File</Title>
  </contextual>

  <contextual action="Simplify file" >
    <Title>SPARK/Simplify File</Title>
  </contextual>

  <contextual action="SPARKmake" >
    <Title>SPARK/SPARKmake</Title>
  </contextual>

  <!-- Shortcut keys -->

  <key action="/SPARK/Examine File">F8</key>
  <key action="/SPARK/Simplify All">F10</key>
  <key action="/SPARK/POGS">F11</key>
  <key action="/SPARK/SPARK Format File">F12</key>

</SPARK>

"""

if os_utils.locate_exec_on_path ("spark") != "":
  a = a.replace('~', spark_separator)
  GPS.parse_xml(a)
  GPS.Contextual ("SPARK/Show VC").create (
     on_activate=pogs_xref,
     filter=has_vc)
  GPS.Contextual ("SPARK/Show Simplified VC").create (
     on_activate=pogs_simplified_xref,
     filter=has_vc)
