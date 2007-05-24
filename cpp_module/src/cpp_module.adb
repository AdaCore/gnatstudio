-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2007                       --
--                             AdaCore                               --
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

with Ada.Unchecked_Deallocation;

with Glib.Properties.Creation; use Glib, Glib.Properties.Creation;

with CPP_Parser;               use CPP_Parser;
with Case_Handling;            use Case_Handling;
with Entities;                 use Entities;
with Foreign_Naming_Editors;   use Foreign_Naming_Editors;
with GPS.Intl;                 use GPS.Intl;
with GPS.Kernel.Console;       use GPS.Kernel.Console;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with GPS.Kernel;               use GPS.Kernel;
with Language.C;               use Language.C;
with Language.Cpp;             use Language.Cpp;
with Language;                 use Language;
with Language_Handlers;    use Language_Handlers;
with Naming_Editors;           use Naming_Editors;
with Project_Viewers;          use Project_Viewers;
with Projects.Registry;        use Projects.Registry;
with Projects;                 use Projects;
with Traces;                   use Traces;

package body Cpp_Module is

   C_Automatic_Indentation   : Param_Spec_Enum;
   C_Use_Tabs                : Param_Spec_Boolean;
   C_Indentation_Level       : Param_Spec_Int;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (LI_Handler_Record'Class, LI_Handler);

   procedure Project_View_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project view has changed in the kernel.
   --  This resets the internal data for the C/C++ handler.

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   function C_Naming_Scheme_Editor
     (Kernel : access Kernel_Handle_Record'Class; Lang : String)
      return Language_Naming_Editor;
   --  Create the naming scheme editor page

   ----------------------------
   -- C_Naming_Scheme_Editor --
   ----------------------------

   function C_Naming_Scheme_Editor
     (Kernel : access Kernel_Handle_Record'Class; Lang : String)
      return Language_Naming_Editor
   is
      pragma Unreferenced (Kernel);
      Naming : Foreign_Naming_Editor;
   begin
      Gtk_New (Naming, Lang);
      return Language_Naming_Editor (Naming);
   end C_Naming_Scheme_Editor;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
      Style  : constant Indentation_Kind := Indentation_Kind'Val
        (Get_Pref (C_Automatic_Indentation));
      Tabs   : constant Boolean := Get_Pref (C_Use_Tabs);
      Params : constant Indent_Parameters :=
                 (Indent_Level        =>
                    Integer (Get_Pref (C_Indentation_Level)),
                  Indent_Continue     => 0,
                  Indent_Decl         => 0,
                  Indent_Conditional  => 0,
                  Indent_Record       => 0,
                  Tab_Width           => Integer (Get_Pref (Tab_Width)),
                  Indent_Case_Extra   => Automatic,
                  Casing_Policy       => Case_Handling.Disabled,
                  Reserved_Casing     => Case_Handling.Unchanged,
                  Ident_Casing        => Case_Handling.Unchanged,
                  Format_Operators    => False,
                  Use_Tabs            => Tabs,
                  Align_On_Colons     => False,
                  Align_On_Arrows     => False,
                  Align_Decl_On_Colon => False,
                  Indent_Comments     => True,
                  Stick_Comments      => False);

   begin
      Set_Indentation_Parameters (C_Lang, Params, Style);
      Set_Indentation_Parameters (Cpp_Lang, Params, Style);
   end On_Preferences_Changed;

   --------------------------
   -- Project_View_Changed --
   --------------------------

   procedure Project_View_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Handler : constant Language_Handler := Language_Handler
        (Get_Language_Handler (Kernel));
   begin
      if Object_Path (Get_Project (Kernel), False) = "" then
         Insert (Kernel,
                 -("The root project must have an object directory set, or"
                   & " C/C++ browsing is disabled"), Mode => Error);
      end if;

      CPP_Parser.On_Project_View_Changed
        (Get_LI_Handler_By_Name (Handler, CPP_LI_Handler_Name));

   exception
      when E : others => Trace (Exception_Handle, E);
   end Project_View_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Handler : constant Language_Handler := Language_Handler
        (Get_Language_Handler (Kernel));
      LI      : LI_Handler := Create_CPP_Handler
        (Get_Database (Kernel), Project_Registry (Get_Registry (Kernel).all));
      Msg     : constant String :=
        Set_Executables (Get_System_Dir (Kernel), LI);

   begin
      if Msg /= "" then
         --  No parser will be available. However, we still want the
         --  highlighting for C and C++ files

         Insert (Kernel, Msg, Mode => Error);
         Unchecked_Free (LI);
      else
         Add_Hook
           (Kernel, Project_View_Changed_Hook,
            Wrapper (Project_View_Changed'Access),
            Name => "cpp_module.project_view_changed");
         On_Project_View_Changed (LI);
      end if;

      Register_Language (Handler, C_Lang, null, LI => LI);
      Register_Default_Language_Extension
        (Get_Registry (Kernel).all,
         Language_Name       => "c",
         Default_Spec_Suffix => ".h",
         Default_Body_Suffix => ".c");

      Register_Language (Handler, Cpp_Lang, null, LI => LI);
      Register_Default_Language_Extension
        (Get_Registry (Kernel).all,
         Language_Name       => "c++",
         Default_Spec_Suffix => ".hh",
         Default_Body_Suffix => ".cpp");

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

      Add_Hook
        (Kernel, Preferences_Changed_Hook,
         Wrapper (On_Preferences_Changed'Access),
         Name => "cpp_module.preferences_changed");
      On_Preferences_Changed (Kernel);

      Register_Naming_Scheme_Editor
        (Kernel, C_String, C_Naming_Scheme_Editor'Access);
      Register_Naming_Scheme_Editor
        (Kernel, Cpp_String, C_Naming_Scheme_Editor'Access);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Register_Module;

end Cpp_Module;
