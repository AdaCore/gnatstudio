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

with Glib.Object;             use Glib.Object;
with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Console;    use Glide_Kernel.Console;
with Glide_Kernel.Project;    use Glide_Kernel.Project;
with Language_Handlers.Glide; use Language_Handlers.Glide;
with Language.C;              use Language.C;
with Language.Cpp;            use Language.Cpp;
with Src_Info;                use Src_Info;
with Src_Info.CPP;            use Src_Info.CPP;
with Traces;                  use Traces;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Unchecked_Deallocation;
with Glide_Intl;              use Glide_Intl;
with Projects;                use Projects;
with Projects.Editor;         use Projects.Editor;
with Ada.Exceptions;          use Ada.Exceptions;
with Vsearch_Ext;             use Vsearch_Ext;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Glide_Intl;               use Glide_Intl;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glib.Object;              use Glib, Glib.Object;
with Language;                 use Language;
with Project_Viewers;          use Project_Viewers;
with Switches_Editors;         use Switches_Editors;
with Gtk.Frame;                use Gtk.Frame;
with Gtk.Box;                  use Gtk.Box;
with Naming_Editors;           use Naming_Editors;
with Foreign_Naming_Editors;   use Foreign_Naming_Editors;
with Snames;                   use Snames;

package body Cpp_Module is

   Me : constant Debug_Handle := Create ("Cpp_Module");

   CPP_LI_Handler_Name : constant String := "c/c++";
   --  The name the source navigator is registered under.

   C_Automatic_Indentation   : Param_Spec_Enum;
   C_Use_Tabs                : Param_Spec_Boolean;
   C_Indentation_Level       : Param_Spec_Int;

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

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (CPP_LI_Handler_Record'Class, CPP_LI_Handler);

   procedure Project_View_Changed
     (K : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called when the project view has changed in the kernel.
   --  This resets the internal data for the C/C++ handler.

   procedure On_Preferences_Changed
     (Kernel : access GObject_Record'Class; K : Kernel_Handle);
   --  Called when the preferences have changed

   function Create_C_Switches
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Switches_Edit_Record'Class)
      return Switches_Editor_Page;
   function Create_Cpp_Switches
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Switches_Edit_Record'Class)
      return Switches_Editor_Page;

   function C_Naming_Scheme_Editor
     (Kernel : access Kernel_Handle_Record'Class)
      return Language_Naming_Editor;
   function Cpp_Naming_Scheme_Editor
     (Kernel : access Kernel_Handle_Record'Class)
      return Language_Naming_Editor;
   --  Create the naming scheme editor page

   ----------------------------
   -- C_Naming_Scheme_Editor --
   ----------------------------

   function C_Naming_Scheme_Editor
     (Kernel : access Kernel_Handle_Record'Class)
      return Language_Naming_Editor
   is
      pragma Unreferenced (Kernel);
      Naming : Foreign_Naming_Editor;
   begin
      Gtk_New (Naming, Name_C);
      return Language_Naming_Editor (Naming);
   end C_Naming_Scheme_Editor;

   ------------------------------
   -- Cpp_Naming_Scheme_Editor --
   ------------------------------

   function Cpp_Naming_Scheme_Editor
     (Kernel : access Kernel_Handle_Record'Class)
      return Language_Naming_Editor
   is
      pragma Unreferenced (Kernel);
      Naming : Foreign_Naming_Editor;
   begin
      Gtk_New (Naming, Get_String (Cpp_String));
      return Language_Naming_Editor (Naming);
   end Cpp_Naming_Scheme_Editor;

   -----------------------
   -- Create_C_Switches --
   -----------------------

   function Create_C_Switches
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Switches_Edit_Record'Class)
      return Switches_Editor_Page
   is
      pragma Unreferenced (Editor);
      Frame  : Gtk_Frame;
      Box    : Gtk_Box;
      Page   : Switches_Editor_Page;
   begin
      Gtk_New (Page, "C", Compiler_Package, C_String, 2, 2,
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
          Tip     => -"Optimization level"),
         False, False);
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
        (Page, Box, -"Profiling", "-pg",
         -("Generate extra code to write profile information suitable"
           & " for the analysis program gprof"));
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

      Gtk_New (Frame, -"Debugging");
      Set_Border_Width (Frame, 5);
      Attach (Page, Frame, 1, 2, 0, 1);
      Gtk_New_Vbox (Box, False, 0);
      Add (Frame, Box);
      Create_Check
        (Page, Box, -"Debug information", "-g",
         -("Produce debugging information in the operating system's"
           & " native format"));

      Gtk_New (Frame, -"Messages");
      Set_Border_Width (Frame, 5);
      Attach (Page, Frame, 0, 2, 1, 2);
      Gtk_New_Vbox (Box, False, 0);
      Add (Frame, Box);
      Create_Check
        (Page, Box, -"All warnings", "-Wall",
         -("This enables all the warnings about constructions that"
           & " some users consider questionable, and that are easy"
           & " to avoid"));
      Create_Check
        (Page, Box, -"Strict ANSI", "-ansi",
         -("In C mode, support all ANSI standard C programs"));
      return Page;
   end Create_C_Switches;

   -------------------------
   -- Create_Cpp_Switches --
   -------------------------

   function Create_Cpp_Switches
     (Kernel : access Kernel_Handle_Record'Class;
      Editor : access Switches_Edit_Record'Class)
      return Switches_Editor_Page
   is
      pragma Unreferenced (Editor);
      Frame  : Gtk_Frame;
      Box    : Gtk_Box;
      Page   : Switches_Editor_Page;
   begin
      Gtk_New (Page, "C++", Compiler_Package, Cpp_String, 2, 2,
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
          Tip     => -"Optimization level"),
         False, False);
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
        (Page, Box, -"Profiling", "-pg",
         -("Generate extra code to write profile information suitable"
           & " for the analysis program gprof"));
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
      Create_Check
        (Page, Box, -"Elide constructor", "-felide-constructor");
      Create_Check
        (Page, Box, -"Conserve space", "-fconserve-space",
         -("Put uninitialized or runtime-initialized global variables"
           & " into the common segment. This saves space in the"
           & " executable at the cost of not diagnosing duplicate"
           & " definitions"));

      Gtk_New (Frame, -"Debugging");
      Set_Border_Width (Frame, 5);
      Attach (Page, Frame, 1, 2, 0, 1);
      Gtk_New_Vbox (Box, False, 0);
      Add (Frame, Box);
      Create_Check
        (Page, Box, -"Debug information", "-g",
         -("Produce debugging information in the operating system's"
           & " native format"));

      Gtk_New (Frame, -"Messages");
      Set_Border_Width (Frame, 5);
      Attach (Page, Frame, 0, 2, 1, 2);
      Gtk_New_Vbox (Box, False, 0);
      Add (Frame, Box);
      Create_Check
        (Page, Box, -"All warnings", "-Wall",
         -("This enables all the warnings about constructions that"
           & " some users consider questionable, and that are easy"
           & " to avoid"));
      Create_Check
        (Page, Box, -"Overloaded virtual", "-Woverloaded-virtual");
      return Page;
   end Create_Cpp_Switches;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access GObject_Record'Class; K : Kernel_Handle)
   is
      pragma Unreferenced (Kernel);
      Style : constant Indentation_Kind := Indentation_Kind'Val
        (Get_Pref (K, C_Automatic_Indentation));
      Tabs  : constant Boolean := Get_Pref (K, C_Use_Tabs);
      Params : constant Indent_Parameters :=
        (Indent_Level      => Integer (Get_Pref (K, C_Indentation_Level)),
         Indent_Continue   => 0,
         Indent_Decl       => 0,
         Tab_Width         => Integer (Get_Pref (K, Tab_Width)),
         Indent_Case_Extra => False);
   begin
      Set_Indentation_Parameters
        (C_Lang,
         Indent_Style  => Style,
         Use_Tabs      => Tabs,
         Params        => Params);
      Set_Indentation_Parameters
        (Cpp_Lang,
         Indent_Style  => Style,
         Use_Tabs      => Tabs,
         Params        => Params);
   end On_Preferences_Changed;

   --------------------------
   -- Project_View_Changed --
   --------------------------

   procedure Project_View_Changed
     (K : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (K);
      Handler : constant Glide_Language_Handler := Glide_Language_Handler
        (Get_Language_Handler (Kernel));
   begin
      if Object_Path (Get_Project (Kernel), False) = "" then
         Insert (Kernel,
                 -("The root project must have an object directory set, or"
                   & " C/C++ browsing is disabled"), Mode => Error);
      end if;

      Reset
        (CPP_LI_Handler
         (Get_LI_Handler_By_Name (Handler, CPP_LI_Handler_Name)),
         Get_Project (Kernel));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Project_View_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Handler : constant Glide_Language_Handler := Glide_Language_Handler
        (Get_Language_Handler (Kernel));
      LI      : CPP_LI_Handler := new Src_Info.CPP.CPP_LI_Handler_Record;
      Msg     : constant String := Set_Executables (LI);

   begin
      if Msg /= "" then
         --  No parser will be available. However, we still want the
         --  highlighting for C and C++ files

         Insert (Kernel, Msg, Mode => Error);
         Unchecked_Free (LI);
      else
         Kernel_Callback.Connect
           (Kernel, "project_view_changed",
            Kernel_Callback.To_Marshaller (Project_View_Changed'Access),
            Kernel_Handle (Kernel));
      end if;

      Register_LI_Handler (Handler, CPP_LI_Handler_Name, LI_Handler (LI));

      Register_Language (Handler, "c", C_Lang);
      Add_Language_Info
        (Handler, "c",
         LI                  => LI_Handler (LI),
         Default_Spec_Suffix => ".h",
         Default_Body_Suffix => ".c");

      Register_Language (Handler, "c++", Cpp_Lang);
      Add_Language_Info
        (Handler, "c++",
         LI                  => LI_Handler (LI),
         Default_Spec_Suffix => ".hh",
         Default_Body_Suffix => ".cpp");

      Register_Search_Pattern
        (Kernel,
         "->MEMBER",
         "->\s*(\w+)\s*[^(]");

      Register_Search_Pattern
        (Kernel,
         "->MEMBER(",
         "->\s*(\w+)\s*(");

      Register_Search_Pattern
        (Kernel,
         "C assignment",
         "(\b(\w+)\s*(([-+*/%&|^]|<<|>>)?=[^=]|\+\+|--))|((\+\+|--)\s*(\w+))");

      Register_Search_Pattern
        (Kernel,
         "call",
         "\b(\w+)\s*(");

      Register_Search_Pattern
        (Kernel,
         "CLASS::member",
         "\b(\w+)\s*::\s*\w+\s*[^(]");

      Register_Search_Pattern
        (Kernel,
         "class::MEMBER",
         "\b\w+\s*::\s*(\w+)\s*[^(]");

      Register_Search_Pattern
        (Kernel,
         "CLASS::member(",
         "\b(\w+)\s*::\s*\w+\s*\(");

      Register_Search_Pattern
        (Kernel,
         "class::MEMBER(",
         "\b\w+\s*::\s*(\w+)\s*\(");

      Register_Search_Pattern
        (Kernel,
         "CLASS<...>",
         "\b(\w+)<[\w,\s]+>");

      Register_Search_Pattern
        (Kernel,
         "comparison",
         "\b(\w+)\s*(==|!=|>=|<=|>[^>]|<[^<])|" &
         "(==|!=|[^>]>=|[^<]<=|[^->]>|[^<]<)\s*(\w+)");

      Register_Search_Pattern
        (Kernel,
         "OBJECT->member",
         "\b(\w+)\s*->\s*\w+\s*[^(]");

      Register_Search_Pattern
        (Kernel,
         "OBJECT->member(",
         "\b(\w+)\s*->\s*\w+\s*\(");

      C_Automatic_Indentation := Param_Spec_Enum
        (Indentation_Properties.Gnew_Enum
           (Name    => "C-Auto-Indentation",
            Default => Extended,
            Blurb   => -"How the editor should indent C/C++ sources",
            Nick    => -"Auto indentation"));
      Register_Property
        (Kernel, Param_Spec (C_Automatic_Indentation), -"Editor:C/C++");

      C_Use_Tabs := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "C-Use-Tabs",
           Default => True,
           Blurb   =>
             -("Whether the editor should use tabulations when indenting"),
           Nick    => -"Use tabulations"));
      Register_Property
        (Kernel, Param_Spec (C_Use_Tabs), -"Editor:C/C++");

      C_Indentation_Level := Param_Spec_Int
        (Gnew_Int
          (Name    => "C-Indent-Level",
           Minimum => 1,
           Maximum => 9,
           Default => 2,
           Blurb   => -"The number of spaces for the default indentation",
           Nick    => -"Default indentation"));
      Register_Property
        (Kernel, Param_Spec (C_Indentation_Level), -"Editor:C/C++");

      Kernel_Callback.Connect
        (Kernel, "preferences_changed",
         Kernel_Callback.To_Marshaller (On_Preferences_Changed'Access),
         Kernel_Handle (Kernel));

      On_Preferences_Changed (Kernel, Kernel_Handle (Kernel));

      Register_Switches_Page (Kernel, Create_C_Switches'Access);
      Register_Switches_Page (Kernel, Create_Cpp_Switches'Access);

      Register_Naming_Scheme_Editor
        (Kernel, C_String, C_Naming_Scheme_Editor'Access);
      Register_Naming_Scheme_Editor
        (Kernel, Cpp_String, Cpp_Naming_Scheme_Editor'Access);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception in Register_Module: "
                & Exception_Information (E));
   end Register_Module;

end Cpp_Module;
