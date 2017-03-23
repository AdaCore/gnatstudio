"""
This file provides enhanced support for C and C++ files.

It declares a number of project attributes (and how they should be
edited graphically in the project properties editor),
It also adds a number of predefined search patterns to match
function or class declarations,...

Syntax highlighting and cross-references are built-in in GPS, and are
not defined in this file.

"""

###########################################################################
# No user customization below this line
###########################################################################

import GPS
from gps_utils import hook

XML = r"""<?xml version="1.0" ?>
<GPS>
   <tool name="C" package="Compiler" index="c" >
      <language>C</language>
      <switches>
        <title line="1" column="1" >Code generation</title>
        <title line="1" column="2" >Debugging</title>
        <title line="2" column="1" >Messages</title>
        <title line="2" column="2" >Source Navigation</title>

        <combo switch="-O" noswitch="0" nodigit="1" >
           <combo-entry label="No optimization" value="0" />
           <combo-entry label="Some optimization" value="1" />
           <combo-entry label="Full optimization" value="2" />
           <combo-entry label="Full + Automatic inline" value="3" />
        </combo>
        <check label="Unroll loops" switch="-funroll-loops"
               tip="Perform the optimization of loop unrolling. This is only done for loops whose number of iterations can be determined at compile time or run time" />
        <check label="Position independent code" switch="-fPIC"
               tip="If supported for the target machine, emit position-independent code, suitable for dynamic linking and avoiding any limit of the size of the global offset table" />
        <check label="Profiling" switch="-pg"
               tip="Generate extra code to write profile information suitable for the analysis program gprof" />
        <check label="Code coverage" switch="-ftest-coverage"
               tip="Create data files for the gcov code-coverage utility" />
        <check label="Instrument arcs" switch="-fprofile-arcs"
               tip="Instrument arcs during compilation. For each function of your program, gcc creates a program flow graph, then finds a spanning tree for the graph. Only arcs that are not on the spanning tree have to be instrumented: the compiler adds code to count the number of times that these arcs are executed" />
        <dependency master-page="C" slave-page="C"
                    master-switch="-ftest-coverage"
                    slave-switch="-fprofile-arcs"
                    master-status="on" slave-status="on" />

        <check label="Separate function sections" switch="-ffunction-sections"
               tip="Generate each function in a separate section. See also -fdata-sections and --gc-sections linker flag" />

        <check label="Separate data sections" switch="-fdata-sections"
               tip="Generate each global data in a separate section. See also -ffunction-sections and --gc-sections linker flag" />

        <check label="Debug information" switch="-g" column="2"
                tip="Produce debugging information in the operating system's native format" />

        <check label="All warnings" switch="-Wall" line="2"
               tip="This enables all the warnings about constructions that some users consider questionable, and that are easy to avoid" />
        <check label="Strict ANSI" switch="-ansi" line="2"
               tip="In C mode, support all ANSI standard C programs" />
      </switches>
   </tool>

   <tool name="C++" package="Compiler" index="c++" >
      <language>C++</language>
      <switches>
        <title line="1" column="1" >Code generation</title>
        <title line="1" column="2" >Debugging</title>
        <title line="2" column="1" >Messages</title>
        <title line="2" column="2" >Source Navigation</title>

        <combo switch="-O" noswitch="0" nodigit="1" >
           <combo-entry label="No optimization" value="0" />
           <combo-entry label="Some optimization" value="1" />
           <combo-entry label="Full optimization" value="2" />
           <combo-entry label="Full + Automatic inline" value="3" />
        </combo>
        <check label="Unroll loops" switch="-funroll-loops"
               tip="Perform the optimization of loop unrolling. This is only done for loops whose number of iterations can be determined at compile time or run time" />
        <check label="Position independent code" switch="-fPIC"
               tip="If supported for the target machine, emit position-independent code, suitable for dynamic linking and avoiding any limit of the size of the global offset table" />
        <check label="Profiling" switch="-pg"
               tip="Generate extra code to write profile information suitable for the analysis program gprof" />
        <check label="Code coverage" switch="-ftest-coverage"
               tip="Create data files for the gcov code-coverage utility" />
        <check label="Instrument arcs" switch="-fprofile-arcs"
               tip="Instrument arcs during compilation. For each function of your program, gcc creates a program flow graph, then finds a spanning tree for the graph. Only arcs that are not on the spanning tree have to be instrumented: the compiler adds code to count the number of times that these arcs are executed" />
        <dependency master-page="C" slave-page="C"
                    master-switch="-ftest-coverage"
                    slave-switch="-fprofile-arcs"
                    master-status="on" slave-status="on" />
        <check label="Elide constructors" switch="-felide-constructor" />
        <check label="Conserve space" switch="-fconserve-space"
               tip="Put uninitialized or runtime-initialized global variables into the common segment. This saves space in the executable at the cost of not diagnosing duplicate definitions" />
        <check label="Separate function sections" switch="-ffunction-sections"
               tip="Generate each function in a separate section. See also -fdata-sections and --gc-sections linker flag" />

        <check label="Separate data sections" switch="-fdata-sections"
               tip="Generate each global data in a separate section. See also -ffunction-sections and --gc-sections linker flag" />


        <check label="Debug information" switch="-g" column="2"
                tip="Produce debugging information in the operating system's native format" />

        <check label="All warnings" switch="-Wall" line="2"
               tip="This enables all the warnings about constructions that some users consider questionable, and that are easy to avoid" />
        <check label="Overloaded virtual" switch="-Woverloaded-virtual"
               line="2" />
      </switches>
   </tool>

   <tool name="C Linker" package="Linker" index="c">
      <language>C</language>
      <switches>
         <check label="Strip symbols" switch="-s" />
         <check label="Debug information" switch="-g" />
         <check label="Code coverage" switch="-fprofile-generate"
                tip="Create data files for the gcov code-coverage utility" />
         <dependency master-page="C" slave-page="C Linker"
                     master-switch="-ftest-coverage"
                     slave-switch="-fprofile-generate"
                     master-status="on" slave-status="on" />
         <check label="Remove unused sections (GNU ld only)"
                switch="-Wl,--gc-sections"
                tip="Remove all unused sections from the link output. This is a GNU ld switch. See also -ffunction-sections and -fdata-sections compiler flags" />
      </switches>
   </tool>

   <tool name="C++ Linker" package="Linker" index="c++">
      <language>C++</language>
      <switches>
         <check label="Strip symbols" switch="-s" />
         <check label="Debug information" switch="-g" />
         <check label="Code coverage" switch="-fprofile-generate"
                tip="Create data files for the gcov code-coverage utility" />
         <dependency master-page="C++" slave-page="C++ Linker"
                     master-switch="-ftest-coverage"
                     slave-switch="-fprofile-generate"
                     master-status="on" slave-status="on" />
         <check label="Remove unused sections (GNU ld only)"
                switch="-Wl,--gc-sections"
                tip="Remove all unused sections from the link output. This is a GNU ld switch. See also -ffunction-sections and -fdata-sections compiler flags" />
      </switches>
   </tool>

   <vsearch-pattern>
     <name>C: -&gt;MEMBER</name>
     <regexp>\s*(\w+)\s*[^(]</regexp>
   </vsearch-pattern>

   <vsearch-pattern>
     <name>C: -&gt;MEMBER(</name>
     <regexp>-&gt;\s*(\w+)\s*(</regexp>
   </vsearch-pattern>

   <vsearch-pattern>
     <name>C assignment</name>
     <regexp>(\b(\w+)\s*(([-+*/%&amp;|^]|&lt;&lt;|&gt;&gt;)?=[^=]|\+\+|--))|((\+\+|--)\s*(\w+))</regexp>
   </vsearch-pattern>

   <vsearch-pattern>
     <name>C call</name>
     <regexp>\b(\w+)\s*(</regexp>
   </vsearch-pattern>

   <vsearch-pattern>
     <name>C comparison</name>
     <regexp>\b(\w+)\s*(==|!=|&gt;=|&lt;=|&gt;[^&gt;]|&lt;[^&lt;])|(==|!=|[^&gt;]&gt;=|[^&lt;]&lt;=|[^-&gt;]&gt;|[^&lt;]&lt;)\s*(\w+)</regexp>
   </vsearch-pattern>

   <vsearch-pattern>
     <name>C++ CLASS::member</name>
     <regexp>\b(\w+)\s*::\s*\w+\s*[^(]</regexp>
   </vsearch-pattern>

   <vsearch-pattern>
     <name>C++ class::MEMBER</name>
     <regexp>\b\w+\s*::\s*(\w+)\s*[^(]</regexp>
   </vsearch-pattern>

   <vsearch-pattern>
     <name>C++ CLASS::member(</name>
     <regexp>\b(\w+)\s*::\s*\w+\s*\(</regexp>
   </vsearch-pattern>

   <vsearch-pattern>
     <name>C++ class::MEMBER(</name>
     <regexp>\b\w+\s*::\s*(\w+)\s*\(</regexp>
   </vsearch-pattern>

   <vsearch-pattern>
     <name>C++ CLASS&lt;...&gt;</name>
     <regexp>\b(\w+)&lt;[\w,\s]+&gt;</regexp>
   </vsearch-pattern>

   <vsearch-pattern>
     <name>C++ OBJECT-&gt;member</name>
     <regexp>\b(\w+)\s*-&gt;\s*\w+\s*[^(]</regexp>
   </vsearch-pattern>

   <vsearch-pattern>
     <name>C++ OBJECT-&gt;member(</name>
     <regexp>\b(\w+)\s*-&gt;\s*\w+\s*\(</regexp>
   </vsearch-pattern>

  <alias name="c_main">
    <param name="name" />
    <param name="params" >int argc, char **argv</param>
    <text>int main (%(params)) {
  %_

  return 0;
}</text>
  </alias>
  <alias name="c++_class">
    <param name="name" />
    <text>class %(name) {
  public:
    %_
};</text>
  </alias>
</GPS>
"""

GPS.parse_xml(XML)


@hook('gps_started')
def __on_gps_started():

    GPS.FileTemplate.register(
        alias_name="c_main",
        label="C Main File",
        unit_param="name",
        language="c",
        is_impl=True)

    GPS.FileTemplate.register(
        alias_name="c++_class",
        label="C++ Class",
        unit_param="name",
        language="c++",
        is_impl=False)
