-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2003                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib.Generic_Properties;
with Glib.Object;              use Glib, Glib.Object;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Size_Group;           use Gtk.Size_Group;
with Gtk.Table;                use Gtk.Table;
with Glide_Kernel;             use Glide_Kernel;
with Src_Info.ALI;             use Src_Info.ALI;
with Language.Ada;             use Language.Ada;
with Language_Handlers.Glide;  use Language_Handlers.Glide;
with Vsearch_Ext;              use Vsearch_Ext;
with Glide_Intl;               use Glide_Intl;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Language;                 use Language;
with Switches_Editors;         use Switches_Editors;
with Projects;                 use Projects;
with Project_Viewers;          use Project_Viewers;
with Naming_Editors;           use Naming_Editors;
with Ada_Naming_Editors;       use Ada_Naming_Editors;

package body Ada_Module is

   Ada_Automatic_Indentation : Param_Spec_Enum;
   Ada_Use_Tabs              : Param_Spec_Boolean;
   Ada_Indentation_Level     : Param_Spec_Int;
   Ada_Continuation_Level    : Param_Spec_Int;
   Ada_Declaration_Level     : Param_Spec_Int;
   Ada_Indent_Case_Extra     : Param_Spec_Boolean;
   Ada_Reserved_Casing       : Param_Spec_Enum;
   Ada_Ident_Casing          : Param_Spec_Enum;
   Ada_Format_Operators      : Param_Spec_Boolean;

   package Casing_Properties is new
     Glib.Generic_Properties.Generic_Enumeration_Property
     ("Casing_Type", Language.Casing_Type);

   --  The following constants are defined to avoid allocating dynamic memory
   --  in Gtk_New. This should eventually be integrated in the kernel.
   --  The comments are there so that the script to handle internationalization
   --  properly finds the constants.

   Cst_No_Optimization : aliased constant String := "No optimization";
   --  -"No optimization"
   Cst_Some_Optimization : aliased constant String := "Some optimization";
   --  -"Some optimization"
   Cst_Full_Optimization : aliased constant String := "Full optimization";
   --  -"Full optimization"
   Cst_Full_Inline_Optimization : aliased constant String :=
     "Full + Automatic inline";
   --  -"Full + Automatic inline"
   Cst_Zero     : aliased constant String := "0";
   Cst_One      : aliased constant String := "1";
   Cst_Two      : aliased constant String := "2";
   Cst_Three    : aliased constant String := "3";
   Cst_Lower_Case : aliased constant String := "Lower case";
   --  -"Lower case"
   Cst_Upper_Case : aliased constant String := "Upper case";
   --  -"Upper case"
   Cst_Mixed_Case : aliased constant String := "Mixed case";
   --  -"Mixed case"
   Cst_As_Declared : aliased constant String := "As declared";
   --  -"As declared"
   Cst_Gnat_Style : aliased constant String := "GNAT style";
   --  -"GNAT style"
   Cst_Compact    : aliased constant String := "Compact";
   --  -"Compact"
   Cst_Uncompact  : aliased constant String := "Uncompact";
   --  -"Uncompact"
   Cst_Gnat_Indent : aliased constant String := "GNAT style line indentation";
   --  -"GNAT style line indentation"
   Cst_Std_Indent  : aliased constant String := "Standard line indentation";
   --  -"Standard line indentation"
   Cst_Static      : aliased constant String := "Static GNAT run time";
   --  -"Static GNAT run time"
   Cst_Shared      : aliased constant String := "Shared GNAT run time";
   --  -"Shared GNAT run time"
   Cst_Static_S : aliased constant String := "-static";
   Cst_Shared_S : aliased constant String := "-shared";
   Cst_L        : aliased constant String := "L";
   Cst_M        : aliased constant String := "M";
   Cst_U        : aliased constant String := "U";
   Cst_D        : aliased constant String := "D";
   Cst_Gnat_Y3  : aliased constant String := "-gnaty3";
   Cst_Gnat_Ya  : aliased constant String := "-gnatya";
   Cst_Gnat_Yb  : aliased constant String := "-gnatyb";
   Cst_Gnat_Yc  : aliased constant String := "-gnatyc";
   Cst_Gnat_Ye  : aliased constant String := "-gnatye";
   Cst_Gnat_Yf  : aliased constant String := "-gnatyf";
   Cst_Gnat_Yh  : aliased constant String := "-gnatyh";
   Cst_Gnat_Yi  : aliased constant String := "-gnatyi";
   Cst_Gnat_Yk  : aliased constant String := "-gnatyk";
   Cst_Gnat_Yl  : aliased constant String := "-gnatyl";
   Cst_Gnat_Ym  : aliased constant String := "-gnatym";
   Cst_Gnat_Yn  : aliased constant String := "-gnatyn";
   Cst_Gnat_Yp  : aliased constant String := "-gnatyp";
   Cst_Gnat_Yr  : aliased constant String := "-gnatyr";
   Cst_Gnat_Ys  : aliased constant String := "-gnatys";
   Cst_Gnat_Yt  : aliased constant String := "-gnatyt";
   Cst_Gnat_Wc  : aliased constant String := "-gnatwc";
   Cst_Gnat_Wf  : aliased constant String := "-gnatwf";
   Cst_Gnat_Wi  : aliased constant String := "-gnatwi";
   Cst_Gnat_Wk  : aliased constant String := "-gnatwk";
   Cst_Gnat_Wm  : aliased constant String := "-gnatwm";
   Cst_Gnat_Wo  : aliased constant String := "-gnatwo";
   Cst_Gnat_Wp  : aliased constant String := "-gnatwp";
   Cst_Gnat_We  : aliased constant String := "-gnatwe";
   Cst_Gnat_Wu  : aliased constant String := "-gnatwu";
   Cst_Gnat_Wv  : aliased constant String := "-gnatwv";
   Cst_Gnat_Wz  : aliased constant String := "-gnatwz";
   Cst_Gnat_Vc  : aliased constant String := "-gnatVc";
   Cst_Gnat_Vd  : aliased constant String := "-gnatVd";
   Cst_Gnat_Vf  : aliased constant String := "-gnatVf";
   Cst_Gnat_Vi  : aliased constant String := "-gnatVi";
   Cst_Gnat_Vm  : aliased constant String := "-gnatVm";
   Cst_Gnat_Vo  : aliased constant String := "-gnatVo";
   Cst_Gnat_Vr  : aliased constant String := "-gnatVr";
   Cst_Gnat_Vs  : aliased constant String := "-gnatVs";
   Cst_Gnat_Vt  : aliased constant String := "-gnatVt";

   procedure On_Preferences_Changed
     (Kernel : access GObject_Record'Class; K : Kernel_Handle);
   --  Called when the preferences have changed

   function Create_Make_Switches
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Switches_Edit_Record'Class)
      return Switches_Editor_Page;
   function Create_Compiler_Switches
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Switches_Edit_Record'Class)
      return Switches_Editor_Page;
   function Create_Pretty_Printer_Switches
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Switches_Edit_Record'Class)
      return Switches_Editor_Page;
   function Create_Binder_Switches
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Switches_Edit_Record'Class)
      return Switches_Editor_Page;
   function Create_Linker_Switches
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Switches_Edit_Record'Class)
      return Switches_Editor_Page;
   --  Create the various switches pages

   function Naming_Scheme_Editor
     (Kernel : access Kernel_Handle_Record'Class)
      return Language_Naming_Editor;
   --  Create the naming scheme editor page

   --------------------------
   -- Naming_Scheme_Editor --
   --------------------------

   function Naming_Scheme_Editor
     (Kernel : access Kernel_Handle_Record'Class)
      return Language_Naming_Editor
   is
      pragma Unreferenced (Kernel);
      Naming : Ada_Naming_Editor;
   begin
      Gtk_New (Naming);
      return Language_Naming_Editor (Naming);
   end Naming_Scheme_Editor;

   --------------------------
   -- Create_Make_Switches --
   --------------------------

   function Create_Make_Switches
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Switches_Edit_Record'Class)
      return Switches_Editor_Page
   is
      pragma Unreferenced (Editor);
      Page   : Switches_Editor_Page;
      Frame  : Gtk_Frame;
      Box    : Gtk_Box;
   begin
      Gtk_New (Page, "Make", Builder_Package, Ada_String, 1, 2,
               Get_Tooltips (Kernel));
      Gtk_New (Frame, -"Dependencies");
      Set_Border_Width (Frame, 5);
      Attach (Page, Frame, 0, 1, 0, 1);
      Gtk_New_Vbox (Box, False, 0);
      Add (Frame, Box);
      Create_Check
        (Page, Box, -"Consider all files", "-a",
         -("Consider all files, even locked files. Locked files are"
           & " file whose ALI file is write-protected"));
      Create_Check
        (Page, Box, -"Recompile if switches changed", "-s",
         -("Recompile if compiler switches have changed since last"
           & " last compilation"));
      Create_Check
        (Page, Box, -"Minimal recompilation", "-m",
         -("Specifies that the minimum necessary amount of"
           & " recompilation be performed. In this mode, gnatmake"
           & " ignores time stamp differences when the only"
           & " modification to a source file consist in adding or"
           & " removing comments, empty lines, spaces or tabs"));

      Gtk_New (Frame, -"Compilation");
      Set_Border_Width (Frame, 5);
      Attach (Page, Frame, 1, 2, 0, 1);
      Gtk_New_Vbox (Box, False, 0);
      Add (Frame, Box);
      Create_Spin
        (Page, Box, -"Multiprocessing", "-j", 1, 100, 1,
         -("Use N processes to carry out the compilations. On a"
           & " multiprocessor machine compilations will occur in"
           & " parallel"));
      Create_Check
        (Page, Box, -"Keep going", "-k",
         -("Continue as much as possible after a compilation error"));
      Create_Check
        (Page, Box, -"Debug information", "-g",
         -("Add debugging information. This forces the corresponding"
           & " switch for the compiler, binder and linker"));
      Create_Check
        (Page, Box, -"Use mapping file", "-C",
         -("Use a mapping file. A mapping file is a way to"
           & " communicate to the compiler two mappings: from unit"
           & " name to file names, and from file names to path"
           & " names. This will generally improve the compilation"
           & " time"));
      return Page;
   end Create_Make_Switches;

   ------------------------------
   -- Create_Compiler_Switches --
   ------------------------------

   function Create_Compiler_Switches
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Switches_Edit_Record'Class)
      return Switches_Editor_Page
   is
      Page   : Switches_Editor_Page;
      Frame  : Gtk_Frame;
      Box    : Gtk_Box;
      Style_Box : Gtk_Box;
      Warn_Box  : Gtk_Box;
   begin
      Gtk_New (Page, "Ada", Compiler_Package, Ada_String, 3, 2,
               Get_Tooltips (Kernel));

      Gtk_New (Frame, -"Code generation");
      Set_Border_Width (Frame, 5);
      Attach (Page, Frame, 0, 1, 0, 1);
      Gtk_New_Vbox (Box, False, 0);
      Add (Frame, Box);
      Pack_Start
        (Box, Create_Combo
         (Page, "",
          Switch               => "-O",
          Default_No_Switch    => "0",
          Default_No_Digit     => "1",
          Buttons => (1 => (Cst_No_Optimization'Access,
                            Cst_Zero'Access),
                      2 => (Cst_Some_Optimization'Access,
                            Cst_One'Access),
                      3 => (Cst_Full_Optimization'Access,
                            Cst_Two'Access),
                      4 => (Cst_Full_Inline_Optimization'Access,
                            Cst_Three'Access)),
          Tip     => -"Controls the optimization level"),
         False, False);
      Create_Check
        (Page, Box, -"front-end inlining", "-gnatN",
         -("The front end inlining activated by this switch is"
           & " generally more extensive and quite often more"
           & " effective than the -gnatn inlining"));
      Create_Check
        (Page, Box, -"Unroll loops", "-funroll-loops",
         -("Perform the optimization of loop unrolling. This is only"
           & " done for loops whose number of iterations can be"
           & " determined at compile time or run time"));
      Create_Check
        (Page, Box, -"Position independent code", "-fPIC",
         -("If supported for the target machine, emit"
           & " position-independent code, suitable for dynamic"
           & " linking and avoiding any limit of the size of the"
           & " global offset table"));
      Create_Check
        (Page, Box, -"Code coverage", "-ftest-coverage",
         -"Create data files for the gcov code-coverage utility");
      Create_Check
        (Page, Box, -"Instrument arcs", "-fprofile-arcs",
         -("Instrument arcs during compilation. For each function of"
           & " your program, gcc creates a program flow graph, then"
           & " finds a spanning tree for the graph. Only arcs that"
           & " are not on the spanning tree have to be instrumented:"
           & " the compiler adds code to count the number of times"
           & " that these arcs are executed"));
      Add_Dependency (Master_Page    => Page,
                      Master_Switch  => "-ftest-coverage",
                      Master_Status  => False,
                      Slave_Page     => Page,
                      Slave_Switch   => "-fprofile-arcs",
                      Slave_Activate => False);

      Gtk_New (Frame, -"Run-time checks");
      Set_Border_Width (Frame, 5);
      Attach (Page, Frame, 1, 2, 0, 1);
      Gtk_New_Vbox (Box, False, 0);
      Add (Frame, Box);
      Create_Check
        (Page, Box, -"Overflow checking", "-gnato",
         -"Enable numerics overflow checking");
      Create_Check
        (Page, Box, -"Suppress all checks", "-gnatp",
         -"Suppress all checks");
      Create_Check
        (Page, Box, -"Stack checking", "-fstack-check",
         -("Generate code to verify that you do not go beyond the"
           & " boundary of the stack. You should specify this flag"
           & " if you are running in an environment with multiple"
           & " threads, but only rarely need to specify it in a"
           & " single-threaded environment"));
      Create_Check
        (Page, Box, -"Dynamic elaboration", "-gnatE",
         -"Full dynamic elaboration checks");

      Gtk_New (Frame, -"Messages");
      Set_Border_Width (Frame, 5);
      Attach (Page, Frame, 0, 1, 1, 3);
      Gtk_New_Vbox (Box, False, 0);
      Add (Frame, Box);
      Create_Check
        (Page, Box, -"Full errors", "-gnatf",
         -("Full Errors. Multiple errors per line, all undefined"
           & " references"));

      --  Warnings

      Gtk_New_Vbox (Warn_Box, False, 0);
      Create_Check
        (Page, Warn_Box, -"Biased rounding", "-gnatwb",
         -("This warning message alerts you to instances where"
           & " compile-time rounding and run-time rounding are not"
           & " equivalent"));
      Create_Check
        (Page, Warn_Box, -"Constant conditional", "-gnatwc",
         -("Activates warnings for conditional expression used in"
           & " tests that are known to be True or False at compile"
           & " time"));
      Create_Check
        (Page, Warn_Box, -"Implicit dereference", "-gnatwd",
         -("If set, the use of a prefix of an access type in an"
           & " indexed component, slice or selected component without"
           & " an explicit .all will generate a warning. With this"
           & " warning enabled, access checks occur only at points"
           & " where an explicit .all appears"));
      Create_Check
        (Page, Warn_Box, -"Warnings=Errors", "-gnatwe",
         -"Causes warning messages to be treated as errors");
      Create_Check
        (Page, Warn_Box, -"Unreferenced formal", "-gnatwf",
         -("Causes warnings to be generated if a formal parameter is"
           & " not referenced in the body"));
      Create_Check
        (Page, Warn_Box, -"Hiding variable", "-gnatwh",
         -("This switch activates warnings on hiding declarations."
           & " A declaration is considered hiding if it is for a non-"
           & "overloadable entity, and if it declares an entity with"
           & " the same name as some other entity that is directly"
           & " or use-visible"));
      Create_Check
        (Page, Warn_Box, -"Implementation unit", "-gnatwi",
         -("This switch activates warnings for a with of an internal"
           & " GNAT implementation unit"));
      Create_Check
        (Page, Warn_Box, -"Obsolescent feature", "-gnatwj", "");
      Create_Check
        (Page, Warn_Box, -"Constant variable", "-gnatwk",
         "");
      Create_Check
        (Page, Warn_Box, -"Missing elaboration pragma", "-gnatwl",
         -("This switch activates warnings on missing pragma"
           & " Elaborate_All statements"));
      Create_Check
        (Page, Warn_Box,
         -"Variable assigned but not read", "-gnatwm",
         "");
      Create_Check
        (Page, Warn_Box, -"Address clause overlay", "-gnatwo",
         -("This switch activates warnings for possible unintended"
           & " initialization effects of defining address clauses"
           & " that cause one variable to overlap another"));
      Create_Check
        (Page, Warn_Box, -"Ineffective pragma inline", "-gnatwp",
         -("This switch activates warnings for a failure of front"
           & " end inlining to inline a particular call"));
      Create_Check
        (Page, Warn_Box, -"Redundant construct", "-gnatwr",
         -("This switch activates warnings for redundant constructs:"
           & ASCII.LF
           & " - Assignment of an item to itself" & ASCII.LF
           & " - Type conversion that converts an expression to its"
           & " own type" & ASCII.LF
           & " - ..."));
      Create_Check
        (Page, Warn_Box, -"Unused entity", "-gnatwu",
         -("This switch activates warnings to be generated for"
           & " entities that are defined but not referenced"));
      Create_Check
        (Page, Warn_Box, -"Unassigned variable", "-gnatwv", "");
      Create_Check
        (Page, Warn_Box,
         -"Size/align warnings for unchecked conversion", "-gnatwz",
         "");
      Pack_Start (Box,
                  Create_Popup (-"Warnings", Warn_Box),
                  False, False);
      Add_Coalesce_Switch (Page, "-gnatw", "-gnatwfikmopeuvz");
      Add_Custom_Expansion
        (Page, "-gnatwa",
         (Cst_Gnat_Wc'Access,
          Cst_Gnat_Wf'Access,
          Cst_Gnat_Wi'Access,
          Cst_Gnat_Wk'Access,
          Cst_Gnat_Wm'Access,
          Cst_Gnat_Wo'Access,
          Cst_Gnat_Wp'Access,
          Cst_Gnat_We'Access,
          Cst_Gnat_Wu'Access,
          Cst_Gnat_Wv'Access,
          Cst_Gnat_Wz'Access));

      --  Validity checking
      Gtk_New_Vbox (Warn_Box, False, 0);
      Create_Check
        (Page, Warn_Box, -"Checking for copies", Cst_Gnat_Vc,
         -("The right hand side of assignments, and the initializing"
           & " values of object declarations are validity checked"));
      Create_Check
        (Page, Warn_Box,
         -"Default Reference Manual checking", Cst_Gnat_Vd);
      Create_Check
        (Page, Warn_Box, -"Checking for floating-point", "-gnatVf");
      Create_Check
        (Page, Warn_Box,
         -"Checking for ""in"" parameters", Cst_Gnat_Vi,
         -("Arguments for parameters of mode in are validity checked"
           & " in function and procedure calls at the point of"
           & " call"));
      Create_Check
        (Page, Warn_Box,
         -"Checking for ""in out"" parameters", Cst_Gnat_Vm,
         -("Arguments for parameters of mode in out are validity"
           & " checked in procedure calls at the point of call"));
      Create_Check
        (Page, Warn_Box,
         -"Checking for operators and attributes", Cst_Gnat_Vo,
         -("Arguments for predefined operations and attributes are"
           & " validity checked"));
      Create_Check
        (Page, Warn_Box, -"Checking for returns", Cst_Gnat_Vr,
         -("The expression in return statements in functions is"
           & " validity checked"));
      Create_Check
        (Page, Warn_Box, -"Checking for subscripts", Cst_Gnat_Vs,
         -"All subscripts expressions are checked for validty");
      Create_Check
        (Page, Warn_Box, -"Checking for tests", Cst_Gnat_Vt,
         -("Expressions used as conditions in if, while or exit"
           & " statements are checked, as well as guard expressions"
           & " in entry calls"));
      Pack_Start (Box,
                  Create_Popup (-"Validity checking mode", Warn_Box),
                  False, False);
      Add_Coalesce_Switch (Page, "-gnatV");
      Add_Custom_Expansion
        (Page, "-gnatVa",
         (Cst_Gnat_Vc'Access,
          Cst_Gnat_Vd'Access,
          Cst_Gnat_Vf'Access,
          Cst_Gnat_Vi'Access,
          Cst_Gnat_Vm'Access,
          Cst_Gnat_Vo'Access,
          Cst_Gnat_Vr'Access,
          Cst_Gnat_Vs'Access,
          Cst_Gnat_Vt'Access));

      --  Styles
      Gtk_New_Vbox (Style_Box, False, 0);
      Create_Spin
        (Page, Style_Box, -"indentation", "-gnaty", 1, 9, 3);
      Create_Check (Page, Style_Box, -"Check casing", "-gnatya");
      Create_Check
        (Page, Style_Box, -"Check end of line blanks", "-gnatyb");
      Create_Check
        (Page, Style_Box, -"Check comment format", "-gnatyc");
      Create_Check
        (Page, Style_Box, -"Check end/exit labels", "-gnatye");
      Create_Check
        (Page, Style_Box, -"Check no form feeds", "-gnatyf");
      Create_Check
        (Page, Style_Box, -"Check no horizontal tabs", "-gnatyh");
      Create_Check
        (Page, Style_Box, -"Check if-then layout", "-gnatyi");
      Create_Check
        (Page, Style_Box, -"Check casing rules", "-gnatyk");
      Create_Check
        (Page, Style_Box,
         -"Check reference manual layout", "-gnatyl");
      Create_Check
        (Page, Style_Box,
         -"Check line length <= 79 characters", "-gnatym");
      Create_Check
        (Page, Style_Box,
         -"Check casing of Standard identifiers", "-gnatyn");
      Create_Check
        (Page, Style_Box,
         -"Check subprogram bodies in alphabetical order", "-gnatyo");
      Create_Check
        (Page, Style_Box, -"Check pragma casing", "-gnatyp");
      Create_Check
        (Page, Style_Box, -"Check RM column layout", "-gnatyr");
      Create_Check
        (Page, Style_Box, -"Check separate specs present", "-gnatys");
      Create_Check
        (Page, Style_Box, -"Check token separation rules", "-gnatyt");
      Create_Spin
        (Page, Style_Box, -"Line length", "-gnatyM", 0, 255, 79);

      Pack_Start (Box,
                  Create_Popup (-"Style checks", Style_Box),
                  False, False);
      Add_Coalesce_Switch (Page, "-gnaty", "-gnatyabcefhiklmnprst");
      Add_Custom_Expansion (Page, "-gnaty",
                            (Cst_Gnat_Y3'Access,
                             Cst_Gnat_Ya'Access,
                             Cst_Gnat_Yb'Access,
                             Cst_Gnat_Yc'Access,
                             Cst_Gnat_Ye'Access,
                             Cst_Gnat_Yf'Access,
                             Cst_Gnat_Yh'Access,
                             Cst_Gnat_Yi'Access,
                             Cst_Gnat_Yk'Access,
                             Cst_Gnat_Yl'Access,
                             Cst_Gnat_Ym'Access,
                             Cst_Gnat_Yn'Access,
                             Cst_Gnat_Yp'Access,
                             Cst_Gnat_Yr'Access,
                             Cst_Gnat_Ys'Access,
                             Cst_Gnat_Yt'Access));

      Gtk_New (Frame, -"Debugging");
      Set_Border_Width (Frame, 5);
      Attach (Page, Frame, 1, 2, 1, 2);
      Gtk_New_Vbox (Box, False, 0);
      Add (Frame, Box);
      Create_Check (Page, Box, -"Debug Information", "-g");
      Add_Dependency (Master_Page    => Get_Page (Editor, "Make"),
                      Master_Switch  => "-g",
                      Master_Status  => True,
                      Slave_Page     => Page,
                      Slave_Switch   => "-g",
                      Slave_Activate => True);

      Create_Check
        (Page, Box, -"Enable assertions", "-gnata",
         -("Assertions enabled. Pragma Assert and pragma Debug are"
           & " activated"));
      Create_Check
        (Page, Box, -"Debug expanded code", "-gnatD",
         -"Output expanded source files for source level debugging");

      Gtk_New (Frame, -"Syntax");
      Set_Border_Width (Frame, 5);
      Attach (Page, Frame, 1, 2, 2, 3);
      Gtk_New_Vbox (Box, False, 0);
      Add (Frame, Box);
      Create_Check
        (Page, Box, -"Language extensions", "-gnatX",
         -("Activates various GNAT-specific extensions to the"
           & " language"));
      Create_Check
        (Page, Box, -"Ada 83 mode", "-gnat83",
         -"Enforces Ada 83 restrictions");

      return Page;
   end Create_Compiler_Switches;

   ------------------------------------
   -- Create_Pretty_Printer_Switches --
   ------------------------------------

   function Create_Pretty_Printer_Switches
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Switches_Edit_Record'Class)
      return Switches_Editor_Page
   is
      pragma Unreferenced (Editor);
      Page   : Switches_Editor_Page;
      Frame  : Gtk_Frame;
      Box    : Gtk_Box;
      Group  : Gtk_Size_Group;
      Table  : Gtk_Table;
   begin
      Gtk_New (Page, "Pretty Printer", "pretty_Printer", Ada_String,
               5, 1, Get_Tooltips (Kernel));

      Gtk_New (Frame, -"Spacing");
      Set_Border_Width (Frame, 5);
      Attach (Page, Frame, 0, 1, 0, 1);
      Gtk_New_Hbox (Box, False, 0);
      Add (Frame, Box);
      Create_Spin (Page, Box, -"Indentation", "-i", 1, 100, 3);
      Create_Spin
        (Page, Box, -"Maximum line length", "-M", 20, 100, 79);

      Gtk_New (Frame, -"Casing");
      Set_Border_Width (Frame, 5);
      Attach (Page, Frame, 0, 1, 1, 2);
      Gtk_New (Table, 2, 2, False);
      Add (Frame, Table);
      Set_Col_Spacings (Table, 5);
      Gtk_New (Group);
      Attach
        (Table, Create_Combo
         (Page, -"Keyword: ",
          Switch               => "-k",
          Default_No_Switch    => "L",
          Default_No_Digit     => "L",
          Buttons =>           (1 => (Cst_Lower_Case'Access,
                                      Cst_L'Access),
                                2 => (Cst_Upper_Case'Access,
                                      Cst_U'Access)),
          Label_Size_Group     => Group),
         0, 1, 0, 1);
      Attach
        (Table, Create_Combo
         (Page, -"Name: ",
          Switch               => "-n",
          Default_No_Switch    => "D",
          Default_No_Digit     => "D",
          Buttons              => (1 => (Cst_As_Declared'Access,
                                         Cst_D'Access),
                                   2 => (Cst_Mixed_Case'Access,
                                         Cst_M'Access),
                                   3 => (Cst_Lower_Case'Access,
                                         Cst_L'Access),
                                   4 => (Cst_Upper_Case'Access,
                                         Cst_U'Access)),
          Label_Size_Group     => Group),
         0, 1, 1, 2);
      Gtk_New (Group);
      Attach
        (Table, Create_Combo
         (Page, -"Attribute: ",
          Switch               => "-a",
          Default_No_Switch    => "M",
          Default_No_Digit     => "M",
          Buttons              => (1 => (Cst_Mixed_Case'Access,
                                         Cst_M'Access),
                                   2 => (Cst_Lower_Case'Access,
                                         Cst_L'Access),
                                   3 => (Cst_Upper_Case'Access,
                                         Cst_U'Access)),
          Label_Size_Group     => Group),
         1, 2, 0, 1);
      Attach
        (Table, Create_Combo
         (Page, -"Pragma: ",
          Switch               => "-p",
          Default_No_Switch    => "M",
          Default_No_Digit     => "M",
          Buttons              => (1 => (Cst_Mixed_Case'Access,
                                         Cst_M'Access),
                                   2 => (Cst_Lower_Case'Access,
                                         Cst_L'Access),
                                   3 => (Cst_Upper_Case'Access,
                                         Cst_U'Access)),
          Label_Size_Group     => Group),
         1, 2, 1, 2);

      Gtk_New (Frame, -"Layout");
      Set_Border_Width (Frame, 5);
      Attach (Page, Frame, 0, 1, 2, 3);
      Gtk_New_Vbox (Box, False, 0);
      Add (Frame, Box);
      Gtk_New (Group);
      Pack_Start
        (Box, Create_Combo
         (Page, -"Construct: ",
          Switch               => "-l",
          Default_No_Switch    => "1",
          Default_No_Digit     => "1",
          Buttons => (1 => (Cst_Gnat_Style'Access,
                            Cst_One'Access),
                      2 => (Cst_Compact'Access,
                            Cst_Two'Access),
                      3 => (Cst_Uncompact'Access,
                            Cst_Three'Access)),
          Label_Size_Group => Group),
         False, False);
      Pack_Start
        (Box, Create_Combo
         (Page, -"Comment: ",
          Switch               => "-c",
          Default_No_Switch    => "1",
          Default_No_Digit     => "1",
          Buttons => (1 => (Cst_Gnat_Indent'Access, Cst_One'Access),
                      2 => (Cst_Std_Indent'Access,  Cst_Two'Access)),
          Label_Size_Group => Group),
         False, False);
      Create_Check (Page, Box, -"GNAT style beginning", "-c3");
      Create_Check (Page, Box, -"Reformat blocks", "-c4");

      Gtk_New (Frame, -"Alignment");
      Set_Border_Width (Frame, 5);
      Attach (Page, Frame, 0, 1, 3, 4);
      Gtk_New_Vbox (Box, False, 0);
      Add (Frame, Box);
      Create_Check (Page, Box, -"Colons in declarations", "-A1");
      Create_Check (Page, Box, -"Assignments in declarations", "-A2");
      Create_Check (Page, Box, -"Assignments in statements", "-A3");
      Create_Check (Page, Box, -"Arrow delimiters in associations",
                    "-A4");

      Gtk_New (Frame, -"General");
      Set_Border_Width (Frame, 5);
      Attach (Page, Frame, 0, 1, 4, 5);
      Gtk_New_Vbox (Box, False, 0);
      Add (Frame, Box);
      Create_Check (Page, Box, -"Set missing end/exit labels", "-e");

      return Page;
   end Create_Pretty_Printer_Switches;

   ----------------------------
   -- Create_Binder_Switches --
   ----------------------------

   function Create_Binder_Switches
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Switches_Edit_Record'Class)
      return Switches_Editor_Page
   is
      pragma Unreferenced (Editor);
      Page   : Switches_Editor_Page;
      Box    : Gtk_Box;
   begin
      Gtk_New (Page, "Binder", Binder_Package, Ada_String, 1, 1,
               Get_Tooltips (Kernel));
      Gtk_New_Vbox (Box, False, 0);
      Attach (Page, Box, 0, 1, 0, 1);
      Create_Check
        (Page, Box, -"Store call stack in exceptions", "-E",
         -("Store tracebacks in exception occurrences when the target"
           & " supports it"));
      Create_Check
        (Page, Box, -"List possible restrictions", "-r");
      Create_Radio
        (Page, Box,
         (1 => (Cst_Static'Access, Cst_Static_S'Access, null),
          2 => (Cst_Shared'Access, Cst_Shared_S'Access, null)));
      return Page;
   end Create_Binder_Switches;

   ----------------------------
   -- Create_Linker_Switches --
   ----------------------------

   function Create_Linker_Switches
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Switches_Edit_Record'Class)
      return Switches_Editor_Page
   is
      Page   : Switches_Editor_Page;
      Box    : Gtk_Box;
   begin
      Gtk_New (Page, "Linker", Linker_Package, Ada_String, 1, 1,
               Get_Tooltips (Kernel));
      Gtk_New_Vbox (Box, False, 0);
      Attach (Page, Box, 0, 1, 0, 1);
      Create_Check (Page, Box, -"Strip symbols", "-s");
      Create_Check (Page, Box, -"Debug information", "-g");
      Add_Dependency (Master_Page    => Get_Page (Editor, "Make"),
                      Master_Switch  => "-g",
                      Master_Status  => True,
                      Slave_Page     => Page,
                      Slave_Switch   => "-g",
                      Slave_Activate => True);
      --  Create_Check (Page, Box, -"Profiling", "-pg");
      return Page;
   end Create_Linker_Switches;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access GObject_Record'Class; K : Kernel_Handle)
   is
      pragma Unreferenced (Kernel);
   begin
      Set_Indentation_Parameters
        (Ada_Lang,
         Indent_Style => Indentation_Kind'Val
            (Get_Pref (K, Ada_Automatic_Indentation)),
         Params       =>
           (Indent_Level      =>
              Integer (Get_Pref (K, Ada_Indentation_Level)),
            Indent_Continue   =>
              Integer (Get_Pref (K, Ada_Continuation_Level)),
            Indent_Decl       =>
              Integer (Get_Pref (K, Ada_Declaration_Level)),
            Tab_Width         => Integer (Get_Pref (K, Tab_Width)),
            Indent_Case_Extra => Get_Pref (K, Ada_Indent_Case_Extra),
            Reserved_Casing   => Casing_Type'Val
              (Get_Pref (K, Ada_Reserved_Casing)),
            Ident_Casing      => Casing_Type'Val
              (Get_Pref (K, Ada_Ident_Casing)),
            Format_Operators  => Get_Pref (K, Ada_Format_Operators),
            Use_Tabs          => Get_Pref (K, Ada_Use_Tabs)));
   end On_Preferences_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Handler : constant Glide_Language_Handler := Glide_Language_Handler
        (Get_Language_Handler (Kernel));
   begin
      Register_LI_Handler
        (Handler, "Ada", new Src_Info.ALI.ALI_Handler_Record);

      Register_Language (Handler, "Ada", Ada_Lang);
      Add_Language_Info
        (Handler, "Ada",
         LI                  => Get_LI_Handler_By_Name (Handler, "Ada"),
         Default_Spec_Suffix => ".ads",
         Default_Body_Suffix => ".adb");

      Register_Search_Pattern
        (Kernel,
         "type NAME is array (...)",
         "\btype\s+(\w+)\s+is\s+array\s+\((.*?)\)\s+of\s+\w+\s*;");
      Register_Search_Pattern
        (Kernel,
         "when CHOICE =>",
         "\bwhen\s+((\w+)\s+:\s+)?[\w\s|]+\s*=>");
      Register_Search_Pattern
        (Kernel,
         "(sub)type NAME is",
         "\b((sub)?type\s+(\w+)|type\s+(\w+)\s+(\(.*?\))?)\s+is\b");
      Register_Search_Pattern
        (Kernel,
         "type NAME (...) is",
         "\btype\s+(\w+)\s+\((.*?)\)\s+is\b");
      Register_Search_Pattern
        (Kernel,
         "for VAR in ... loop",
         "\bfor\s+(\w+)\s+in\s+(reverse\s+)?(.+?)(\s+"
         & "range\s+(.*?))?\s+loop\b");
      Register_Search_Pattern
        (Kernel, "Ada assignment", "\b(\w+)\s*:=");

      Ada_Automatic_Indentation := Param_Spec_Enum
        (Indentation_Properties.Gnew_Enum
           (Name    => "Ada-Auto-Indentation",
            Default => Extended,
            Blurb   => -"How the editor should indent Ada sources",
            Nick    => -"Auto indentation"));
      Register_Property
        (Kernel, Param_Spec (Ada_Automatic_Indentation), -"Editor:Ada");

      Ada_Use_Tabs := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Ada-Use-Tabs",
           Default => False,
           Blurb   =>
             -("Whether the editor should use tabulations when indenting"),
           Nick    => -"Use tabulations"));
      Register_Property (Kernel, Param_Spec (Ada_Use_Tabs), -"Editor:Ada");

      Ada_Indentation_Level := Param_Spec_Int
        (Gnew_Int
          (Name    => "Ada-Indent-Level",
           Minimum => 1,
           Maximum => 9,
           Default => 3,
           Blurb   => -"The number of spaces for the default Ada indentation",
           Nick    => -"Default indentation"));
      Register_Property
        (Kernel, Param_Spec (Ada_Indentation_Level), -"Editor:Ada");

      Ada_Continuation_Level := Param_Spec_Int
        (Gnew_Int
          (Name    => "Ada-Continuation-Level",
           Minimum => 0,
           Maximum => 9,
           Default => 2,
           Blurb   => -"The number of extra spaces for continuation lines",
           Nick    => -"Continuation lines"));
      Register_Property
        (Kernel, Param_Spec (Ada_Continuation_Level), -"Editor:Ada");

      Ada_Declaration_Level := Param_Spec_Int
        (Gnew_Int
          (Name    => "Ada-Declaration-Level",
           Minimum => 0,
           Maximum => 9,
           Default => 0,
           Blurb   =>
             -"The number of extra spaces for multiple line declarations",
           Nick    => -"Declaration lines"));
      Register_Property
        (Kernel, Param_Spec (Ada_Declaration_Level), -"Editor:Ada");

      Ada_Indent_Case_Extra := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Ada-Indent-Case-Extra",
           Default => True,
           Blurb   => -"Indent case statements with an extra level",
           Nick    => -"RM style case indentation"));
      Register_Property
        (Kernel, Param_Spec (Ada_Indent_Case_Extra), -"Editor:Ada");

      Ada_Reserved_Casing := Param_Spec_Enum
        (Casing_Properties.Gnew_Enum
           (Name    => "Ada-Reserved-Casing",
            Default => Unchanged,
            Blurb   => -"How the editor should handle reserved words casing",
            Nick    => -"Reserved word casing"));
      Register_Property
        (Kernel, Param_Spec (Ada_Reserved_Casing), -"Editor:Ada");

      Ada_Ident_Casing := Param_Spec_Enum
        (Casing_Properties.Gnew_Enum
           (Name    => "Ada-Ident-Casing",
            Default => Unchanged,
            Blurb   => -"How the editor should handle identifiers casing",
            Nick    => -"Identifier casing"));
      Register_Property
        (Kernel, Param_Spec (Ada_Ident_Casing), -"Editor:Ada");

      Ada_Format_Operators := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Ada-Format-Operators",
           Default => False,
           Blurb   =>
             -"Whether to add spaces around operators and delimiters",
           Nick    => -"Format operators/delimiters"));
      Register_Property
        (Kernel, Param_Spec (Ada_Format_Operators), -"Editor:Ada");

      Kernel_Callback.Connect
        (Kernel, "preferences_changed",
         Kernel_Callback.To_Marshaller (On_Preferences_Changed'Access),
         Kernel_Handle (Kernel));

      On_Preferences_Changed (Kernel, Kernel_Handle (Kernel));

      Register_Switches_Page (Kernel, Create_Make_Switches'Access);
      Register_Switches_Page (Kernel, Create_Pretty_Printer_Switches'Access);
      Register_Switches_Page (Kernel, Create_Binder_Switches'Access);
      Register_Switches_Page (Kernel, Create_Linker_Switches'Access);
      Register_Switches_Page (Kernel, Create_Compiler_Switches'Access);

      Register_Naming_Scheme_Editor
        (Kernel, "Ada", Naming_Scheme_Editor'Access);
   end Register_Module;

end Ada_Module;
