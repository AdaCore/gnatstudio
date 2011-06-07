"""This file provides SPARK support in GPS.

Copyright (c) 2004-2010 Altran Praxis Limited
Copyright (c) 2005-2010 AdaCore
"""


###########################################################################
## No user customization below this line
###########################################################################

import os, os.path, re, string, tempfile
import os_utils, text_utils
import re
import GPS
from gps_utils import *

spark_module="import spark; spark"
spark_console="SPARK Output"
spark_category="Examiner"

spark_separator='-'

# Global variable to pass filename to on_exit which will then raise a
# window containing the file.

focus_file = ""

def on_match (process, match, since_last):
  try:
     process.output += since_last + match
  except:
     process.output = since_last + match
  GPS.Console (spark_console).write (since_last + match)

def on_exit (process, status, remaining_output):
  global focus_file

  # Protect e.g. "Flow Error:123:" from being detected as a file reference
  try:
    output = process.output.replace (" Error:"," Error,").replace \
               (" Warning:"," Warning,")
    GPS.Locations.parse (output, category=spark_category)
    GPS.Console (spark_console).write (focus_file + "\n")
  except:
    pass

  # Take into account new files and directories created by the Examiner,
  # in particular in the project view.

  if focus_file != "":
    buf = GPS.EditorBuffer.get (GPS.File (focus_file))
    GPS.MDI.get_by_child (buf.current_view()).raise_window()
    focus_file = ""

  GPS.Project.recompute ()

@with_save_excursion
def examine_file (file):
  """Examine current file through the SPARK Examiner. file is an instance
     of GPS.File"""
  GPS.MDI.save_all (False)
  GPS.Locations.remove_category (spark_category)
  sw = file.project().get_tool_switches_as_string ("Examiner")
  cmd = "spark "+sw + " "+spark_separator+'brief "' + file.name() + '"'
  GPS.Console (spark_console, accept_input=False).clear ()
  GPS.Console (spark_console).write (cmd + "\n")
  GPS.Process (cmd, remote_server="Build_Server", regexp=".+", on_match=on_match, on_exit=on_exit)
  GPS.MDI.get (spark_console).raise_window ()

def _spawn_cmd (cmd_name, prj_attr, input=None):
  """
  Prepare the SPARK console and spawn a command that sends its output to it.
  See _spawn_spark_tool for a description of the arguments
  """

  # ??? In this case, the process should be run asynchronously so as not to
  # block GPS

  result = _spawn_spark_tool (cmd_name, prj_attr, show_cmd=True, input=input)
  GPS.Console (spark_console).write (result)
  GPS.MDI.get (spark_console).raise_window ()

def _spawn_spark_tool (cmd_name, prj_attr, input=None,
                       show_cmd=False, ctx=None):
  """
  Spawn a SPARK tool. Get its switches from the project file (attribute
  name specified as prj_attr)
  If input is a string, it is sent to stdin for the process. Otherwise it
  should be an instance of GPS.File, and is added to the command line. If it
  is none, the current file is used
  Returns the output of the process. This is run synchronously.
  """

  result = ""

  if not ctx:
     ctx = GPS.current_context()

  try:
     sw = ctx.project().get_tool_switches_as_string (prj_attr)
  except:
     sw = GPS.Project.root().get_tool_switches_as_string (prj_attr)

  cmd = cmd_name + " " + sw

  if input == None:
     input = ctx.file ()

  if not isinstance (input, str):
     GPS.MDI.save_all (False)
     cmd = cmd + " " + input.name()

  if show_cmd:
     GPS.Console (spark_console, accept_input=False).clear ()
     GPS.Console (spark_console).write (cmd + "\n")

  proc = GPS.Process (cmd, remote_server="Build_Server")
  if isinstance (input, str):
     proc.send (input, add_lf=True)
     proc.send (chr (4), add_lf=False) # End of transmission

  return proc.get_result ()

@save_dir
def show_pogs_file():
  """Show the POGS file of the current project"""
  global focus_file

  sw = GPS.Project.root().get_tool_switches_as_string ("pogs")

  cmd = "pogs "+ sw

  if not re.search("-d=", cmd):
    # The default directory from where POGS will be run is the directory
    # of the project file.
    cmd = cmd + " -d=" + os.path.dirname (GPS.Project.root().file().name())

  summary_file_option = re.search("-o=[^ ]+", cmd)
  if not summary_file_option:
    # If the user has not specified an output file then the summary file
    # is set to <project_name>.sum
    summary_file = re.sub("gpr$", "sum", GPS.Project.root().file().name())
    cmd = cmd + " -o=" + summary_file
  else:
    summary_file = \
        summary_file_option.string[summary_file_option.start():summary_file_option.end()].lstrip("-o=")

  GPS.Console (spark_console, accept_input=False).clear ()
  GPS.Console (spark_console).write (cmd + "\n")
  # Pass the summary_file to on_exit which raise a window with the file open.
  focus_file = summary_file
  GPS.Process (cmd, remote_server="Build_Server", regexp=".+", on_match=on_match, on_exit=on_exit)
  GPS.MDI.get (spark_console).raise_window ()

def do_pogs_xref (context, siv, dpc, zlg):
  """Jump to the path number referenced in the current line of the POGS output"""
  editor = GPS.EditorBuffer.get()
  curs = editor.current_view().cursor()
  line = editor.get_chars (curs.beginning_of_line(), curs.end_of_line())
  number = re.search ("^\|\s*(\d+)", line).group (1)  # path number

  if dpc:
     (frm,to) = curs.search ("^File (.*)\.dpc$", backward=True, regexp=True)
     proof_file=editor.get_chars (frm+5, to-1)
     if zlg:
        proof_file = proof_file.replace (".dpc", ".zlg")
  else:
     (frm,to) = curs.search ("^File (.*)\.vcg$", backward=True, regexp=True)
     proof_file=editor.get_chars (frm+5, to-1)
     if siv:
        proof_file = proof_file.replace (".vcg", ".siv")

  f = GPS.EditorBuffer.get (GPS.File (proof_file))
  loc = GPS.EditorLocation (f, 1, 1)
  if zlg:
     (frm,to) = loc.search ("^@@@@@@@@@@  VC: (procedure|function)_\S+_" + number + "\.", regexp=True)
  else:
     (frm,to) = loc.search ("^(procedure|function)_\S+_" + number + "\.$", regexp=True)

  GPS.MDI.get_by_child (f.current_view()).raise_window()

  # Goto the VC or DPC and then scroll the window down so the selected VC or DPC is not at the
  # bottom of the page.

  f.current_view().goto (frm)
  cursor = f.current_view().cursor()
  f.current_view().center(cursor)

def pogs_xref (context):
  do_pogs_xref (context, siv=False, dpc=False, zlg=False)
def pogs_siv_xref (context):
  do_pogs_xref (context, siv=True, dpc=False, zlg=False)
def pogs_dpc_xref (context):
  do_pogs_xref (context, siv=False, dpc=True, zlg=False)
def pogs_zlg_xref (context):
  do_pogs_xref (context, siv=False, dpc=True, zlg=True)

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
     context.has_vc = re.search ("\|   (S|U|E|I|X|P|C|R|F).   \|", line) != None
     return context.has_vc

def has_dpc (context):
  """Return TRUE if the current line of the POGS output references a DPC"""
  try:
     # Avoid doing the work several times for all entries in the menu
     return context.has_dpc
  except:
     try:
        if os.path.splitext (context.file().name())[1] != ".sum":
           return False
     except:
        return False  # context.file() does not exist
     editor = GPS.EditorBuffer.get()
     curs = editor.current_view().cursor()
     line = editor.get_chars (curs.beginning_of_line(), curs.end_of_line())
     context.has_dpc = re.search ("\|   .(S|U|D|L)   \|", line) != None
     return context.has_dpc

@save_dir
def sparkmake ():
  dir = os.path.dirname (GPS.current_context().file().name())
  GPS.cd (dir)
  _spawn_cmd (cmd_name="sparkmake", prj_attr="SPARKMake")

def format_file ():
  buffer = GPS.EditorBuffer.get ()
  _spawn_cmd (cmd_name="sparkformat", prj_attr="SPARKFormat")
  GPS.EditorBuffer.get (file=buffer.file(), force=True)

def format_selection ():
  """sparkformat the selection or the current line"""
  ctx = GPS.current_context()
  buffer = GPS.EditorBuffer.get ()
  start = buffer.selection_start ().beginning_of_line ()
  end   = buffer.selection_end ().end_of_line () - 1
  buffer.start_undo_group ()
  selection = buffer.get_chars (start, end)

  # Go through a temporary file, instead of sending the contents on stdin,
  # because in the latter case we are sometimes getting duplicate output
  # (both the input and the output)

  fd, name = tempfile.mkstemp (suffix=".ada")
  os.write (fd, selection)
  os.close (fd)

  _spawn_spark_tool (cmd_name="sparkformat", prj_attr="SPARKFormat",
                     show_cmd=False, ctx=ctx, input=GPS.File (name))

  f = file (name)
  text_utils.replace (start, end, f.read())
  f.close ()

  buffer.finish_undo_group ()
  os.unlink (name)

def simplify_file (file):
  """Simplify current file through the SPARK simplifier. file is an instance
     of GPS.File"""
  global focus_file

  GPS.MDI.save_all (False)
  GPS.Locations.remove_category (spark_category)
  sw = file.project().get_tool_switches_as_string ("Simplifier")
  relative_filename = file.name().replace(file.project().file().directory(), "")
  siv_filename = relative_filename.replace(".vcg", ".siv")

  cmd = "spadesimp "+sw + " " + relative_filename
  GPS.Console (spark_console, accept_input=False).clear ()
  GPS.Console (spark_console).write (cmd + "\n")
  # Pass the siv_file to on_exit which raise a window with the file open.
  focus_file = siv_filename
  GPS.Process (cmd, remote_server="Build_Server", regexp=".+", on_match=on_match, on_exit=on_exit)
  GPS.MDI.get (spark_console).raise_window ()

def victor_file (file):
  """Apply victor to the current file. file is an instance of GPS.File"""
  global focus_file

  GPS.MDI.save_all (False)
  GPS.Locations.remove_category (spark_category)
  sw = file.project().get_tool_switches_as_string ("ViCToR")
  relative_filename = file.name().replace(file.project().file().directory(), "")
  vct_filename = relative_filename.replace(".vcg", ".vct").replace(".siv", ".vct")

  cmd = "victor " + sw + " " + relative_filename.replace(".vcg", "").replace(".siv", "")
  GPS.Console (spark_console, accept_input=False).clear()
  GPS.Console (spark_console).write (cmd + "\n")
  # Pass the vct_file to on_exit which raise a window with the file open.
  focus_file = vct_filename
  GPS.Process (cmd, remote_server="Build_Server", regexp=".+", on_match=on_match, on_exit=on_exit)
  GPS.MDI.get (spark_console).raise_window ()

def zombiescope_file (file):
  """Run ZombieScope on file, where file is an instance of GPS.File"""
  global focus_file

  GPS.MDI.save_all (False)
  GPS.Locations.remove_category (spark_category)
  sw = file.project().get_tool_switches_as_string ("ZombieScope")
  relative_filename = file.name().replace(file.project().file().directory(), "")
  sdp_filename = relative_filename.replace(".dpc", ".sdp")

  cmd = "zombiescope "+sw + " " + relative_filename
  GPS.Console (spark_console, accept_input=False).clear ()
  GPS.Console (spark_console).write (cmd + "\n")
  # Pass the sdp_file to on_exit which raise a window with the file open.
  focus_file = sdp_filename
  GPS.Process (cmd, remote_server="Build_Server", regexp=".+", on_match=on_match, on_exit=on_exit)
  GPS.MDI.get (spark_console).raise_window ()

def sparksimp_project ():
  """Simplify all files in the project"""
  GPS.MDI.save_all (False)
  sparksimp_sw = GPS.Project.root().get_tool_switches_as_string ("SPARKSimp")
  simplifier_sw = GPS.Project.root().get_tool_switches_as_string ("Simplifier")
  zombiescope_sw = GPS.Project.root().get_tool_switches_as_string ("ZombieScope")
  victor_sw = GPS.Project.root().get_tool_switches_as_string ("ViCToR")

  cmd = ("sparksimp " + sparksimp_sw)
  if len(simplifier_sw.strip()) > 0:
    cmd = (cmd + " -sargs " + simplifier_sw)
  if len(zombiescope_sw.strip()) > 0:
    cmd = (cmd + " -zargs " + zombiescope_sw)
  if len(victor_sw.strip()) > 0:
    cmd = (cmd + " -vargs " + victor_sw)

  GPS.Console (spark_console, accept_input=False).clear ()
  GPS.Console (spark_console).write (cmd + "\n")
  GPS.Process (cmd, remote_server="Build_Server", regexp=".+", on_match=on_match, on_exit=on_exit)
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

  <!-- Define other SPARK languages -->

  <Language>
    <Name>POGS Summary</Name>
    <Spec_Suffix>.sum</Spec_Suffix>
  </Language>

  <Language>
    <Name>RLU</Name>
    <Spec_Suffix>.rlu</Spec_Suffix>
  </Language>

  <Language>
    <Name>PRV</Name>
    <Spec_Suffix>.prv</Spec_Suffix>
  </Language>

  <Language>
    <Name>PLG</Name>
    <Spec_Suffix>.plg</Spec_Suffix>
  </Language>

  <Language>
    <Name>RUL</Name>
    <Spec_Suffix>.rul</Spec_Suffix>
  </Language>

  <Language>
    <Name>RLS</Name>
    <Spec_Suffix>.rls</Spec_Suffix>
  </Language>

  <Language>
    <Name>FDL</Name>
    <Spec_Suffix>.fdl</Spec_Suffix>
  </Language>

  <Language>
    <Name>SLG</Name>
    <Spec_Suffix>.slg</Spec_Suffix>
  </Language>

  <Language>
    <Name>CMD</Name>
    <Spec_Suffix>.cmd</Spec_Suffix>
  </Language>

  <Language>
    <Name>DPC</Name>
    <Spec_Suffix>.dpc</Spec_Suffix>
  </Language>

  <Language>
    <Name>SDP</Name>
    <Spec_Suffix>.sdp</Spec_Suffix>
  </Language>

  <Language>
    <Name>ZLG</Name>
    <Spec_Suffix>.zlg</Spec_Suffix>
  </Language>

  <Language>
    <Name>VCT</Name>
    <Spec_Suffix>.vct</Spec_Suffix>
  </Language>

  <Language>
    <Name>VLG</Name>
    <Spec_Suffix>.vlg</Spec_Suffix>
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
      <field line="1" label="Output directory" as-directory="true" switch="~output_directory" separator="="/>
      <check column="1" line="1" label="Ignore spark.sw" switch="~noswitch" />
      <title line="1" column="2" column-span="0" />
      <title column="1" line="2">Language</title>
      <combo label="Language" switch="~language" separator="="
             noswitch="95" column="1" line="2" >
        <combo-entry label="SPARK83" value="83" />
        <combo-entry label="SPARK95" value="95" />
        <combo-entry label="SPARK2005" value="2005" />
      </combo>
      <combo label="Profile" switch="~profile" separator="="
             noswitch="sequential" column="1" line="2" >
        <combo-entry label="Sequential" value="sequential" />
        <combo-entry label="Ravenscar" value="ravenscar" />
      </combo>
      <check column="1" line="2" label="Use SPARK Library" switch="~sparklib" />

      <title column="2" line="2">Analysis</title>
      <radio column="2" line="2">
        <radio-entry label="Information and Data Flow" switch="~flow_analysis=information" />
        <radio-entry label="Automatic Selection" switch="~flow_analysis=auto" />
        <radio-entry label="Data Flow only" switch="~flow_analysis=data" />
      </radio>
      <check column="2" line="2" label="Generate VCs" switch="~vcg" />
      <check column="2" line="2" label="Generate DPCs" switch="~dpc" />
      <check column="2" line="2" label="Casing checks" switch="~casing" />
      <check column="2" line="2" label="Syntax check only" switch="~syntax_check" />
      <combo label="Policy" switch="~policy" separator="="
             noswitch=" " column="2" line="2" >
        <combo-entry label="Safety" value="safety" />
        <combo-entry label="Security" value="security" />
        <combo-entry label="Off" value=" " />
      </combo>
      <title line="3" column-span="2">General</title>
      <combo label="Replacement Rules" switch="~rules" separator="="
             noswitch="none" column="1" line="3"
             tip="Replacement rules for composite constants">
        <combo-entry label="None" value="none" />
        <combo-entry label="Lazy" value="lazy" />
	<combo-entry label="Keen" value="keen" />
        <combo-entry label="All" value="all" />
      </combo>
      <combo label="Error Explanations" switch="~error_explanations"
             separator="=" noswitch="off" column="1" line="3">
        <combo-entry label="Off" value="off" />
        <combo-entry label="First Occurrence" value="first" />
	<combo-entry label="Every Occurrence" value="every" />
      </combo>
      <field column="2" line="3" label="Annotation Character" switch="~annotation_character" separator="=" tip="Enter a single character to follow '--' as the mark for SPARK annotations (default '#')" />
      <title line="5" column-span="2">Output</title>
      <check line="5" column="1" label="Plain Output" switch="~plain" />
      <check line="5" column="2" label="HTML Output" switch="~html" />
      <field line="5" column="1" label="Listing File Extension" as-file="true" switch="~listing" separator="="/>
      <field line="5" column="1" label="Report File Name" as-file="true" switch="~report" separator="="/>
    </switches>
  </tool>

  <tool name="SPARKSimp">
    <language>Ada</language>
    <switches columns="2" lines="6" switch_char="~">
      <title line="1">Analysis order</title>
      <check line="1" label="Process all files" switch="~a" />
      <check line="1" label="Sort files, largest first" switch="~t" />
      <check line="1" label="Reverse sort order" switch="~r" />
      <title line="2">Output</title>
      <check line="2" label="Log output" switch="~l" />
      <check line="2" label="Verbose output" switch="~v" />
      <check line="2" label="Echo Simplifier output" switch="~e" />
      <title line="3">Simplification</title>
      <check line="3" label="No Simplification" switch="~ns" />
      <title line="4">ViCToR (Currently available on GNU/Linux and Windows)</title>
      <check line="4" label="Prove with ViCToR" switch="~victor" />
      <title line="5">ZombieScope</title>
      <check line="5" label="No ZombieScope" switch="~nz" />
      <title line="6">Process control</title>
      <check line="6" label="Dry run" switch="~n" />
      <spin line="6" label="Multiprocessing" switch="~p=" min="1" max="100" default="1"
            tip="Use N processes to run the Simplifier/ZombieScope/ViCToR. On a multiprocessor machine analysis will occur in parallel" />
    </switches>
  </tool>

  <tool name="Simplifier">
    <language>Ada</language>
    <switches lines="3" switch_char="~">
      <title line="1">Output</title>
      <check line="1" label="No Echo" switch="~noecho" />
      <check line="1" label="Plain Output" switch="~plain" />
      <check line="1" label="Don't renumber hypotheses" switch="~norenum" />
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

  <tool name="ViCToR">
    <language>Ada</language>
    <switches lines="2" switch_char="~">
      <title line="1">General behaviour</title>
      <check line="1" label="Plain output" switch="~plain" />
      <check line="1" label="Ignore SIV files" switch="~v" />
      <combo line="1" label="SMT solver used" switch="~solver" separator="=" noswitch="alt-ergo">
        <combo-entry label="Alt-Ergo" value="alt-ergo" />
      </combo>
      <title line="2">Limits</title>
      <spin line="2" label="Proof step limit for Alt-Ergo" switch="~steps=" min="0" max="10000" default="5000"
            tip="A deterministic (unlike timeouts) proof step limit. Zero means no limit." />
      <spin line="2" label="Timeout (in s) (GNU/Linux only)" switch="~t=" min="0" max="1000" default="0"
            tip="Timeout for each invocation of the prover. No timeout by default." />
      <spin line="2" label="Memory Limit (in megabytes) (GNU/Linux only)" switch="~m=" min="0" max="10000" default="0"
            tip="Memory limit for each invocation of the prover. No limit by default." />
    </switches>
  </tool>

  <tool name="ZombieScope">
    <language>Ada</language>
    <switches lines="1" switch_char="~">
      <title line="1">Output</title>
      <check line="1" label="Plain Output" switch="~plain" />
      <check line="1" label="Don't renumber hypotheses" switch="~norenum" />
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

      <title line="2" column="1">Annotation compression</title>
      <radio line="2" column="1">
        <radio-entry label="Compress" switch="~compress" />
        <radio-entry label="Expand" switch="~expand" />
      </radio>
      <title line="2" column="2">Annotations</title>
      <field line="2" column="2" label="Annotation Character" switch="~annotation_character=" tip="Enter a single character to follow '--' as the mark for SPARK annotations (default '#')" />

      <title line="3" column="1">Default Switch File</title>
      <check line="3" column="1" label="Ignore spark.sw" switch="~noswitch" />

      <title line="4" column="1" column-span="3">Indentation</title>
      <spin line="4" column="1" label="Globals"
         switch="~global_indent" separator="=" min="0" max="256" default="0"
         tip="Amount of indentation from '--#' for the global variables (or 0 for default)" />
      <spin line="4" column="2" label="Exports"
         switch="~export_indent" separator="=" min="0" max="256" default="0"
         tip="Amount of indentation from '--#' for the export variables (or 0 for default)" />
      <spin line="4" column="3" label="Imports"
         switch="~import_indent" separator="=" min="0" max="256" default="0"
         tip="Amount of indentation from '--#' for the import variables (or 0 for default)" />
      <spin line="4" column="1" label="Separators"
         switch="~separator_indent" separator="=" min="0" max="256" default="0"
         tip="Amount of indentation from '--#' for the separators ('from' and ampersand) (or 0 for default)" />
      <spin line="4" column="2" label="Inherits"
         switch="~inherit_indent" separator="=" min="0" max="256" default="0"
         tip="Amount of indentation from '--#' for the package names (or 0 for default)" />
      <spin line="4" column="3" label="Own"
         switch="~own_indent" separator="=" min="0" max="256" default="0"
         tip="Amount of indentation from '--#' for own variables (or 0 for default)" />
      <spin line="4" column="1" label="Refinement"
         switch="~refinement_indent" separator="=" min="0" max="256" default="0"
         tip="Amount of indentation from '--#' for own variables (or 0 for default)" />
      <spin line="4" column="2" label="Constituent"
         switch="~constituent_indent" separator="=" min="0" max="256" default="0"
         tip="Amount of indentation from '--#' for constituents (or 0 for default)" />
      <spin line="4" column="3" label="Initialization"
         switch="~initialization_indent" separator="=" min="0" max="256" default="0"
         tip="Amount of indentation from '--#' for own variables (or 0 for default)" />
      <spin line="4" column="1" label="Properties"
         switch="~properties_indent" separator="=" min="0" max="256" default="0"
         tip="Amount of indentation from '--#' for own variables (or 0 for default)" />
    </switches>
  </tool>

  <tool name="POGS">
    <language>Ada</language>
    <switches lines="3" switch_char="~">
      <title line="1">POGS Configuration </title>
      <field line="1" label="Input Directory " as-directory="true" switch="~d" separator="="/>
      <field line="1" label="Output File" as-file="true" switch="~o" separator="="/>

      <title line="2">Options</title>
      <check line="2" label="Plain Output"
       tip="Prevent release information and file paths being output to .sum file"
       switch="~p" />
      <check line="2" label="Ignore Dates"
       tip="Prevent checking of date and time stamps of VCs and Proof Log files"
       switch="~i" />

      <title line="3">Output</title>
      <radio line="3">
        <radio-entry label="Default" tip="Default output" switch="" />
        <radio-entry label="Short summary"
         tip="Prevent per-subprogram analysis section being output to .sum file"
         switch="~s" />
        <radio-entry label="XML" tip="Output summary information in XML format"
         switch="~x" />
      </radio>
    </switches>
  </tool>

  <tool name="SPARKMake">
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
     <filter language="Ada"/>
     <shell lang="python">"""+spark_module+""".examine_file (GPS.File ("%F"))</shell>
  </action>

  <action name="Simplify file" category="Spark" output="none">
    <filter language="VCG" />
     <shell lang="python">"""+spark_module+""".simplify_file (GPS.File("%F"))</shell>
  </action>

  <action name="ViCToR file" category="Spark" output="none">
    <filter language="VCG" />
    <filter language="SIV" />
     <shell lang="python">"""+spark_module+""".victor_file (GPS.File("%F"))</shell>
  </action>

  <action name="ZombieScope file" category="Spark" output="none">
    <filter language="DPC" />
     <shell lang="python">spark.zombiescope_file (GPS.File("%F"))</shell>
  </action>

  <action name="SPARKFormat file" category="Spark" output="none">
     <filter language="Ada" />
     <shell lang="python">"""+spark_module+""".format_file ()</shell>
  </action>

  <action name="SPARKFormat selection" category="Spark" output="none">
     <filter language="SPARK" />
     <filter language="Ada" />
     <shell lang="python">"""+spark_module+""".format_selection ()</shell>
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
     <shell lang="python">GPS.Project.recompute()</shell>
  </action>

  <action name="SPARKSimp" category="Spark" output="none">
    <shell lang="python">"""+spark_module+""".sparksimp_project ()</shell>
  </action>

  <action name="POGS" category="Spark" output="none">
    <shell lang="python">"""+spark_module+""".show_pogs_file()</shell>
  </action>

  <action name="SPARKMake" category="Spark" output="none">
    <filter language="Ada" />
    <shell lang="python">"""+spark_module+""".sparkmake ()</shell>
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
      <menu action="SPARKFormat file">
        <Title>SPARK_Format File</Title>
      </menu>
      <menu action="SPARKFormat selection">
        <Title>SPARKFormat _Selection</Title>
      </menu>
      <menu action="Simplify file">
        <Title>_Simplify File</Title>
      </menu>
      <menu action="ViCToR file">
        <Title>_ViCToR File</Title>
      </menu>
      <menu action="ZombieScope file">
        <Title>_ZombieScope File</Title>
      </menu>
      <menu action="SPARKSimp">
        <Title>SP_ARKSimp</Title>
      </menu>
      <menu action="POGS">
        <Title>P_OGS</Title>
      </menu>
      <menu action="SPARKMake">
        <Title>SPARKMa_ke</Title>
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

  <contextual action="SPARKFormat selection" >
    <Title>SPARK/SPARKFormat Selection</Title>
  </contextual>

  <contextual action="Simplify file" >
    <Title>SPARK/Simplify File</Title>
  </contextual>

  <contextual action="ViCToR file" >
    <Title>SPARK/ViCToR File</Title>
  </contextual>

  <contextual action="ZombieScope file" >
    <Title>SPARK/ZombieScope File</Title>
  </contextual>

  <contextual action="SPARKSimp" >
    <Title>SPARK/SPARKSimp</Title>
  </contextual>

  <contextual action="POGS" >
    <Title>SPARK/POGS</Title>
  </contextual>

  <contextual action="SPARKMake" >
    <Title>SPARK/SPARKMake</Title>
  </contextual>

  <!-- Shortcut keys -->

  <key action="/SPARK/Examine File">F8</key>
  <key action="/SPARK/SPARKSimp">F10</key>
  <key action="/SPARK/POGS">F11</key>
  <key action="/SPARK/SPARKFormat File">F12</key>

</SPARK>

"""

b = """<?xml version="1.0"?>
<GPS>
  <doc_path>~</doc_path>

   <submenu before="About">
      <title>/Help/SPARK</title>
   </submenu>

   <submenu>
      <title>/Help/SPARK/Documentation Index</title>
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
     <name>Global_Index.htm</name>
     <descr>Global Documentation Index</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Documentation Index/Global Documentation Index</menu>
  </documentation_file>

  <documentation_file>
     <name>SPARK_LRM.htm</name>
     <descr>SPARK Language Reference Manual</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Language/SPARK Language Reference Manual</menu>
  </documentation_file>

  <documentation_file>
     <name>SPARK83_LRM.htm</name>
     <descr>SPARK83 Language Reference Manual</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Language/SPARK83 Language Reference Manual</menu>
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
     <name>Simplifier_UM.htm</name>
     <descr>Simplifier User Manual</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Tools/Simplifier User Manual</menu>
  </documentation_file>

  <documentation_file>
     <name>SPARKSimp_UM.htm</name>
     <descr>SPARKSimp User Manual</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Tools/SPARKSimp User Manual</menu>
  </documentation_file>

  <documentation_file>
     <name>Pogs_UM.htm</name>
     <descr>POGS User Manual</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Tools/POGS User Manual</menu>
  </documentation_file>

  <documentation_file>
     <name>Zombiescope_UM.htm</name>
     <descr>ZombieScope User Manual</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Tools/ZombieScope User Manual</menu>
  </documentation_file>

  <documentation_file>
     <name>VictorWrapper_UM.htm</name>
     <descr>Victor Wrapper User Manual</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Tools/Victor Wrapper User Manual</menu>
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
     <name>Release_Note_10.htm</name>
     <descr>Release Note 10.0</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 10.0</menu>
  </documentation_file>

  <documentation_file>
     <name>Release_Note_9p1p0.htm</name>
     <descr>Release Note 9.1</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 9.1</menu>
  </documentation_file>

  <documentation_file>
     <name>Release_Note_9.htm</name>
     <descr>Release Note 9.0</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 9.0</menu>
  </documentation_file>

  <documentation_file>
     <name>Release_Note_GPL.htm</name>
     <descr>Release Note</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note</menu>
  </documentation_file>

  <documentation_file>
     <name>Release_Note_8p1p4.htm</name>
     <descr>Release Note 8.1.4</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 8.1.4</menu>
  </documentation_file>

  <documentation_file>
     <name>Release_Note_8p1p1.htm</name>
     <descr>Release Note 8.1.1</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 8.1.1</menu>
  </documentation_file>

  <documentation_file>
     <name>Release_Note_8p1p0.htm</name>
     <descr>Release Note 8.1.0</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Release Notes/Release Note 8.1.0</menu>
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
     <name>SPARK_GPS.htm</name>
     <descr>Using SPARK with GPS</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Reference/Using SPARK with GPS</menu>
  </documentation_file>

  <documentation_file>
     <name>SPARK_QRG1.htm</name>
     <descr>Quick Reference Guide 1 - Toolset and Annotations</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Reference/Quick Reference Guide 1 - Toolset and Annotations</menu>
  </documentation_file>

  <documentation_file>
     <name>SPARK_QRG2.htm</name>
     <descr>Quick Reference Guide 2 - Patterns</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Reference/Quick Reference Guide 2 - Patterns</menu>
  </documentation_file>

  <documentation_file>
     <name>SPARK_QRG3.htm</name>
     <descr>Quick Reference Guide 3 - RavenSPARK Patterns</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Reference/Quick Reference Guide 3 - RavenSPARK Patterns</menu>
  </documentation_file>

  <documentation_file>
     <name>SPARK_QRG4.htm</name>
     <descr>Quick Reference Guide 4 - Proof Guide</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Reference/Quick Reference Guide 4 - Proof Guide</menu>
  </documentation_file>

  <documentation_file>
     <name>SPARK_QRG5.htm</name>
     <descr>Quick Reference Guide 5 - Proof Checker Commands</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Reference/Quick Reference Guide 5 - Proof Checker Commands</menu>
  </documentation_file>

  <documentation_file>
     <name>SPARK_QRG6.htm</name>
     <descr>Quick Reference Guide 6 - Proof Checker Rules</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Reference/Quick Reference Guide 6 - Proof Checker Rules</menu>
  </documentation_file>

  <documentation_file>
     <name>Proof_Manual.htm</name>
     <descr>Proof Manual</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Reference/Proof Manual</menu>
  </documentation_file>

  <documentation_file>
     <name>Informed.htm</name>
     <descr>INFORMED</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Reference/INFORMED Design Method</menu>
  </documentation_file>

  <documentation_file>
     <name>SPARK_Library_UM.htm</name>
     <descr>SPARK Library</descr>
     <category>Spark</category>
     <menu before="About">/Help/SPARK/Reference/SPARK Library</menu>
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
  a = a.replace('~', spark_separator)
  GPS.parse_xml(a)
  sparkdocdir = os.path.dirname(spark)+os.sep+os.pardir+os.sep+"docs"+os.sep+"HTML"
  b = b.replace('~', sparkdocdir)
  GPS.parse_xml(b)

  GPS.Contextual ("SPARK/Show VC").create (
     on_activate=pogs_xref,
     filter=has_vc)
  GPS.Contextual ("SPARK/Show Simplified VC").create (
     on_activate=pogs_siv_xref,
     filter=has_vc)
  GPS.Contextual ("SPARK/Show DPC").create (
     on_activate=pogs_dpc_xref,
     filter=has_dpc)
  GPS.Contextual ("SPARK/Show ZLG").create (
     on_activate=pogs_zlg_xref,
     filter=has_dpc)
