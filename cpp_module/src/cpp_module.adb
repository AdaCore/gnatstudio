-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2002-2009, AdaCore                --
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
with ALI_Parser;               use ALI_Parser;
with CPP_Parser;               use CPP_Parser;
with Case_Handling;            use Case_Handling;
with Default_Preferences;      use Default_Preferences;
with Entities;                 use Entities;
with Foreign_Naming_Editors;   use Foreign_Naming_Editors;
with GNATCOLL.Traces;
with GNATCOLL.Filesystem;      use GNATCOLL.Filesystem;
with GPS.Intl;                 use GPS.Intl;
with GPS.Kernel.Console;       use GPS.Kernel.Console;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with GPS.Kernel;               use GPS.Kernel;
with Language.C;               use Language.C;
with Language.Cpp;             use Language.Cpp;
with Language;                 use Language;
with Language_Handlers;        use Language_Handlers;
with Naming_Editors;           use Naming_Editors;
with Project_Viewers;          use Project_Viewers;
with Projects.Registry;        use Projects.Registry;
with Projects;                 use Projects;
with Traces;                   use Traces;

package body Cpp_Module is

   Use_GLI_Trace : constant Debug_Handle :=
                     Create ("CPP.GLI", GNATCOLL.Traces.Off);

   C_Automatic_Indentation   : Indentation_Kind_Preferences.Preference;
   C_Use_Tabs                : Boolean_Preference;
   C_Indentation_Level       : Integer_Preference;

   type GLI_Handler_Record is new ALI_Handler_Record with null record;
   type GLI_Handler is access all GLI_Handler_Record'Class;
   --  GCC LI Handler.

   overriding function Get_Name (LI : access GLI_Handler_Record) return String;
   overriding function Case_Insensitive_Identifiers
     (Handler : access GLI_Handler_Record) return Boolean;
   overriding function Get_ALI_Ext
     (LI : access GLI_Handler_Record) return Filesystem_String;
   overriding function Get_ALI_Filename
     (Handler   : access GLI_Handler_Record;
      Base_Name : Filesystem_String) return Filesystem_String;
   --  See doc for inherited subprograms

   function Create_GLI_Handler
     (Db       : Entities.Entities_Database;
      Registry : Projects.Registry.Project_Registry)
      return Entities.LI_Handler;
   --  Create a new ALI handler

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   function C_Naming_Scheme_Editor
     (Kernel : access Kernel_Handle_Record'Class; Lang : String)
      return Language_Naming_Editor;
   --  Create the naming scheme editor page

   procedure Project_View_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the project view has changed in the kernel.
   --  This resets the internal data for the C/C++ handler.

   ----------------------------
   -- GLI_Handler primitives --
   ----------------------------

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (LI : access GLI_Handler_Record) return String
   is
      pragma Unreferenced (LI);
   begin
      return "GNU C/C++";
   end Get_Name;

   ----------------------------------
   -- Case_Insensitive_Identifiers --
   ----------------------------------

   overriding function Case_Insensitive_Identifiers
     (Handler : access GLI_Handler_Record) return Boolean
   is
      pragma Unreferenced (Handler);
   begin
      return False;
   end Case_Insensitive_Identifiers;

   -----------------
   -- Get_ALI_Ext --
   -----------------

   overriding function Get_ALI_Ext
     (LI : access GLI_Handler_Record) return Filesystem_String
   is
      pragma Unreferenced (LI);
   begin
      return ".gli";
   end Get_ALI_Ext;

   ----------------------
   -- Get_ALI_Filename --
   ----------------------

   overriding function Get_ALI_Filename
     (Handler   : access GLI_Handler_Record;
      Base_Name : Filesystem_String) return Filesystem_String is
   begin
      return Base_Name & Get_ALI_Ext (Handler);
   end Get_ALI_Filename;

   -------------------------------
   -- Non primitive subprograms --
   -------------------------------

   ------------------------
   -- Create_GLI_Handler --
   ------------------------

   function Create_GLI_Handler
     (Db       : Entities.Entities_Database;
      Registry : Projects.Registry.Project_Registry) return Entities.LI_Handler
   is
      CPP : constant GLI_Handler := new GLI_Handler_Record;
   begin
      CPP.Db            := Db;
      CPP.Registry      := Registry;
      return LI_Handler (CPP);
   end Create_GLI_Handler;

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
      Style  : constant Indentation_Kind := C_Automatic_Indentation.Get_Pref;
      Params : constant Indent_Parameters :=
                 (Indent_Level        => C_Indentation_Level.Get_Pref,
                  Indent_Continue     => 0,
                  Indent_Decl         => 0,
                  Indent_Conditional  => 0,
                  Indent_Record       => 0,
                  Tab_Width           => Tab_Width.Get_Pref,
                  Indent_Case_Extra   => Automatic,
                  Casing_Policy       => Case_Handling.Disabled,
                  Reserved_Casing     => Case_Handling.Unchanged,
                  Ident_Casing        => Case_Handling.Unchanged,
                  Format_Operators    => False,
                  Use_Tabs            => C_Use_Tabs.Get_Pref,
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
      if Object_Path (Get_Project (Kernel), False, True) = "" then
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
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (LI_Handler_Record'Class, LI_Handler);

      Handler : constant Language_Handler := Language_Handler
        (Get_Language_Handler (Kernel));
      LI      : LI_Handler;

   begin
      if Active (Use_GLI_Trace) then
         LI := Create_GLI_Handler
                 (Get_Database (Kernel),
                  Project_Registry (Get_Registry (Kernel).all));
      else
         LI := Create_CPP_Handler
                 (Get_Database (Kernel),
                  Project_Registry (Get_Registry (Kernel).all));

         declare
            Msg : constant String :=
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
         end;
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

      C_Automatic_Indentation := Indentation_Kind_Preferences.Create
        (Get_Preferences (Kernel),
         Name    => "C-Auto-Indentation",
         Default => Extended,
         Page    => -"Editor/C & C++",
         Doc     => -"How the editor should indent C/C++ sources",
         Label   => -"Auto indentation");

      C_Use_Tabs := Create
        (Get_Preferences (Kernel),
         Name    => "C-Use-Tabs",
         Default => True,
         Page    => -"Editor/C & C++",
         Doc     =>
             -("Whether the editor should use tabulations when indenting"),
         Label   => -"Use tabulations");

      C_Indentation_Level := Create
        (Get_Preferences (Kernel),
         Name    => "C-Indent-Level",
         Minimum => 1,
         Maximum => 9,
         Default => 2,
         Page    => -"Editor/C & C++",
         Doc     => -"The number of spaces for the default indentation",
         Label   => -"Default indentation");

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
