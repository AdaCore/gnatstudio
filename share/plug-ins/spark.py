## This file provides utilities used by the SPARK plug-in for GPS.
## Copyright (c) 2004-2007 Praxis High Integrity Systems Limited
## Copyright (c) 2005-2007 AdaCore

#####################################
# SPARK Toolset Customization for GPS
# -----------------------------------
#
# Introduction
# ------------
#
# This package is a customization of AdaCore's GPS system for use with the
# SPARK language and toolset.
#
# The customization is intended for use with Release 7.4 of the
# SPARK Toolset.  The customization depends on the presence of the
# "brief output" Examiner switch, which is not available
# in earlier releases.  If you don't have release 7.4 or better,
# please contact Praxis High Integrity Systems: sparkinfo@praxis-his.com
#
# This customization remains a work-in-progress, and there is lots of
# room for improvement.  We welcome contributions.
#
# Use
# ---
#
# This plug-in will be enabled only if you have spark tools available in
# your PATH variable (e.g. Spark Examiner, POGs, etc).
#
# Edit the project preferences (from the Project menu) and tick the necessary
# languages on the Languages tab: Spark, Index, Listing, Metafile, Siv, Vcg.
#
# Switches for the tools can be set on the same menu under the Switches tab.
#
# The Examiner output produced is always /brief so that GPS is able to parse
# the resulting files.
#
# The tools can be run from the SPARK menu, or right-clicking on files will
# produce a contextual menu.  Not all tools can be run on all files and GPS
# will produce a warning if the incorrect context is selected.
#
# Future work
# -----------
#
# Nice-to-haves include:
#   - Context sensitive navigation in annotations
#   - Hot keys displayed on the SPARK Menu
#   - Better EMACS integration (particularly on Windows...)
#
######################################

a = """<?xml version="1.0"?>
<!--  Note: do not use the ampersand character in XML comments!!       -->

<SPARK>

  <Language>
    <!-- Base SPARK on Ada -->
    <Name>SPARK</Name>
    <Parent>Ada</Parent>
    <Spec_Suffix>.ads</Spec_Suffix>
    <Body_Suffix>.adb</Body_Suffix>
    <Extension>.ada</Extension>

    <!-- Declarations overwrite parent fields, so to add SPARK     -->
    <!-- keywords we need to repeat the Ada ones (copied from      -->
    <!-- gvd/common/language-ada.adb).                             -->
    <!-- The SPARK keywords are listed first, for maintainability. -->
    <Keywords>^(assert|check|derives|global|from|h(ide|old)|</Keywords>
    <Keywords>in(herit|itializes|variant)|main_program|own|p(re|ost)|</Keywords>
    <Keywords>some|</Keywords>
    <Keywords>a(b(ort|s(tract)?)|cce(pt|ss)|l(iased|l)|nd|rray|t)|</Keywords>
    <Keywords>b(egin|ody)|c(ase|onstant)|d(e(clare|l(ay|ta))|igits|o)|</Keywords>
    <Keywords>e(ls(e|if)|n(d|try)|x(ception|it))|f(or|unction)|</Keywords>
    <Keywords>g(eneric|oto)|i[fns]|l(imited|oop)|mod|n(ew|ot|ull)|</Keywords>
    <Keywords>o(thers|ut|[fr])|p(ackage|r(agma|ivate|o(cedure|tected)))|</Keywords>
    <Keywords>r(a(ise|nge)|e(cord|m|names|queue|turn|verse))|</Keywords>
    <Keywords>s(e(lect|parate)|ubtype)|t(a(gged|sk)|erminate|hen|ype)|</Keywords>
    <Keywords>u(ntil|se)|w(h(en|ile)|ith)|xor)</Keywords>

    <Context>
      <!-- This hard-codes # and $ as annotation characters.       -->
      <!-- In a perfect world we would default the annotation      -->
      <!-- character to #, and overwrite it if there is a value    -->
      <!-- set up in the tool switches.                            -->
      <New_Line_Comment_Start>--[^#$]</New_Line_Comment_Start>
      <String_Delimiter>&quot;</String_Delimiter>
      <Constant_Character>&apos;</Constant_Character>
      <Can_Indent>True</Can_Indent>
      <Syntax_Highlighting>True</Syntax_Highlighting>
      <Case_Sensitive>False</Case_Sensitive>
    </Context>
  </Language>

  <!-- Although defined as a child language of Ada, GPS tries to   -->
  <!-- build SPARK programs as if SPARK is a different compiler    -->
  <!-- language. It is easy to set up a makefile that does the     -->
  <!-- job of building the program, but the (g!)natty navigation   -->
  <!-- doesn't work.                                               -->

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
    <language>SPARK</language>
    <language>Ada</language>
    <switches columns ="2" lines="5" switch_char="~">
      <title line="1" column-span="2">Examiner Files</title>
      <field line="1" label=" Index File " as-file="true" switch="~index_file=" />
      <field line="1" label=" Warning File " as-file="true" switch="~warning_file=" />
      <field line="1" label=" Configuration File " as-file="true" switch="~config=" />
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
      <check column="2" line="2" label="Generate Run Time Checks" switch="~exp_checks" />
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
      <field column="2" line="4" label=" Annotation Character " switch="~annotation_character=" tip="Enter a single character to follow '--' as the mark for SPARK annotations (default '#')" />
      <title line="5" column-span="2">Output</title>
      <check line="5" label=" Plain Output " switch="~plain" />
      <check line="5" label=" HTML Output " switch="~html" />
      <field line="5" label=" Listing File Extension " switch="~listing=" />
      <field line="5" label=" Report File Name " switch="~report=" />
      <title line="5" column="2" column-span="0" />
    </switches>
  </tool>

  <tool name="SPARKSimp">
    <language>SPARK</language>
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
    <language>SPARK</language>
    <language>Ada</language>
    <switches lines="3" switch_char="~">
      <title line="1">Output</title>
      <check line="1" label="No Echo" switch="/noecho" />
      <check line="1" label="Plain Output" switch="/plain" />
      <title line="2">Tactics</title>
      <check line="2" label="No Simplification" switch="/nosimplification" />
      <check line="2" label="No Standardisation" switch="/nostand" />
      <check line="2" label="No Contradiction Hunt" switch="/nocontra" />
      <check line="2" label="No Substitution Elimination" switch="/nosubstitution" />
      <check line="2" label="No Rule Substitution" switch="/norule_substitution" />
      <check line="2" label="No Expression Reduction" switch="/noexp" />
      <title line="3">Limits</title>
      <spin line="3" label="Memory Limit" switch="/memory_limit=" min="250000" max="30000000" default="9000000"
            tip="Max PROLOG Heap.  Default 9000000." />
    </switches>
  </tool>

  <tool name="SPARKFormat">
    <language>SPARK</language>
    <language>Ada</language>
    <switches lines="3" columns="2" switch_char="~">
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
      <title line="3" column="1" column-span="2">Indentation</title>
      <field line="3" column="1" label="Globals indentation" switch="~global_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for the global variables, or state 'inline' (default 'inline')" />
      <field line="3" label="Exports indentation" switch="~export_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for the export variables, or state 'inline' (default 'inline')" />
      <field line="3" label="Imports indentation" switch="~import_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for the import variables, or state 'inline' (default 'inline')" />
      <field line="3" label="Seperators indentation" switch="~separator_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for the separators ('from' and ampersand), or state 'inline' (default 'inline')" />
      <field line="3" label="Inherits indentation" switch="~inherit_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for the package names, or state 'inline' (default 'inline')" />
      <field line="3" label="Own indentation" switch="~own_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for own variables, or state 'inline' (default 'inline')" />
      <field line="3" label="Refinement indentation" switch="~refinement_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for own variables, or state 'inline' (default 'inline')" />
      <field line="3" label="Constituent indentation" switch="~constituent_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for constituents, or state 'inline' (default 'inline')" />
      <field line="3" label="Initialization indentation" switch="~initialization_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for own variables, or state 'inline' (default 'inline')" />
      <field line="3" label="Properties indentation" switch="~properties_indent=" tip="Enter a number ( >0 )for the amount of indentation from '--#' for own variables, or state 'inline' (default 'inline')" />
    </switches>
  </tool>


  <tool name="POGS">
    <language>SPARK</language>
    <language>Ada</language>
    <switches lines="1" switch_char="~">
      <title line="1">Options</title>
      <check line="1" label="Plain Output" switch="~p" />
      <check line="1" label="Ignore Dates" switch="~i" />
    </switches>
  </tool>

  <tool name="SPARKmake">
    <language>SPARK</language>
    <language>Ada</language>
    <switches lines="2" switch_char="~">
      <title line="1">Input File Options</title>
         <field line="1" label=" Directory "  switch="~dir=" />
         <field line="1" label=" Include " switch="~inc=" />
         <field line="1" label=" Exclude " switch="~e=" />
      <title line="2">Input File Options</title>
         <field line="2" label=" Index "  switch="~ind=" />
         <field line="2" label=" Meta " switch="~m=" />
    </switches>
  </tool>


  <!-- Filtering actions by file extensions is done by setting up -->
  <!-- different languages for each extension. This means if you  -->
  <!-- want to use metafiles or the simplifier, you'll also have  -->
  <!-- to select metafile and VCG as project languages.           -->

  <action name="Examine file" category="Spark">
     <filter language="SPARK" />
     <filter language="Ada" />
     <shell>MDI.save_all false</shell>
	 <shell>Locations.remove_category Examiner</shell>  <!-- clears the Location window to remove previous errors -->
     <shell>Project %p</shell>
     <shell>Project.get_tool_switches_as_string %1 "Examiner" </shell>
     <external output="SPARK Output">spark %1 ~brief "%F"</external> <!-- force /brief option as can't parse output otherwise -->
     <on-failure>
          <shell>Locations.parse &quot;&quot;&quot;%1 &quot;&quot;&quot; Examiner</shell>
     </on-failure>
     <shell>Locations.parse &quot;&quot;&quot;%1 &quot;&quot;&quot; Examiner</shell>
  </action>

  <action name="SPARKFormat file" category="Spark">
     <filter language="SPARK" />
     <filter language="Ada" />
     <shell>MDI.save_all false</shell>
     <shell>Project %p</shell>
     <shell>Project.get_tool_switches_as_string %1 "SPARKFormat" </shell>
     <external output="SPARK Output">sparkformat %1 %f</external>
     <shell>Editor.edit %f 0 0 0 true</shell>
  </action>

  <action name="Examine metafile" category="Spark">
     <filter language="Metafile" />
     <shell>MDI.save_all false</shell>
	 <shell>Locations.remove_category Examiner</shell>
     <shell>Project %p</shell>
     <shell>Project.get_tool_switches_as_string %1 "Examiner" </shell>
     <external output="SPARK Output">spark %1 ~brief @%f</external>
     <on-failure>
          <shell>Locations.parse &quot;&quot;&quot;%1 &quot;&quot;&quot; Examiner</shell>
     </on-failure>
     <shell>Locations.parse &quot;&quot;&quot;%1 &quot;&quot;&quot; Examiner</shell>
  </action>

  <action name="SPARK help" output="SPARK Help" category="Spark">
     <external>spark /help</external>
  </action>

  <action name="Simplify file" category="Spark">
    <filter language="VCG" />
    <shell>Project %p</shell>
    <shell>Project.get_tool_switches_as_string %1 "Simplifier" </shell>
    <external output="Simplifier Output">spadesimp %f %1</external>
  </action>

  <action name="Simplify all" category="Spark">
    <shell>Project %P</shell>
    <shell>Project.get_tool_switches_as_string %1 "Simplifier" </shell>
    <shell>Project %P</shell>
    <shell>Project.get_tool_switches_as_string %1 "SPARKSimp" </shell>
    <external output="SPARKSimp Output">sparksimp %1 ~sargs %3 </external>
  </action>

  <action name="POGS" category="Spark">
    <shell>MDI.save_all false</shell>
    <shell>cd %d</shell>
    <shell>Project %p</shell>
    <shell>Project.get_tool_switches_as_string %1 "POGS" </shell>
    <external output="POGS Output">pogs %1</external>
    <!-- Python is C-like - use non-zero (-1) for true -->
    <shell lang="python">GPS.Editor.edit(spark.pogs_file(), 0, 0, 0, -1)</shell>
  </action>

  <action name="SPARKmake" category="Spark">
     <filter language="SPARK" />
     <filter language="Ada" />
     <shell>MDI.save_all false</shell>
     <shell>Project %p</shell>
     <shell>Project.get_tool_switches_as_string %1 "SPARKmake" </shell>
     <external output="SPARKmake Output">sparkmake %1 %f</external>
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
    <Title>Examine Metafile</Title>
  </contextual>

  <contextual action="Examine file" >
    <Title>Examine File</Title>
  </contextual>

  <contextual action="SPARKFormat file" >
    <Title>SPARKFormat File</Title>
  </contextual>

  <contextual action="Simplify file" >
    <Title>Simplify File</Title>
  </contextual>

  <contextual action="SPARKmake" >
    <Title>SPARKmake</Title>
  </contextual>

  <!-- Shortcut keys -->

  <key action="Examine file">F8</key>
  <key action="SPARKFormat file">F9</key>
  <key action="Simplify all">F10</key>
  <key action="POGS">F11</key>


</SPARK>

"""

import os, re, string
import GPS

def pogs_file ():
  """Return the filename of the POGS file based on the current directory"""
  ## If no context is currently selected, then this will fail with an exception.
  full_path = GPS.current_context().directory()
  match_obj = re.search(r'.*[\\/]([^\\/]*)[\\/]', full_path)
  dir_name = match_obj.group(1)
  return (full_path + dir_name + '.sum')

def on_path (executable):
  """ locates an executable named 'executable' on the path.
no extension is expected in the name supplied as the parameter.
returns True if found, False otherwise."""

  if os.name == 'nt':
    extension = ".exe"
  else:
    extension = ""
  target = executable + extension;
  the_path = os.environ["PATH"];
  path_dirs = string.split (the_path, os.pathsep);
  for D in path_dirs:
    test = os.path.join (D, target);
    if os.path.exists (test):
      return True;
  return False;

if on_path("spark"):
  if os.name == 'nt':
    a = a.replace('~', '/')
  else:
    a = a.replace('~', '-')

  GPS.parse_xml(a)
