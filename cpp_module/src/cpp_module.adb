------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2014, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

pragma Warnings (Off);
with GNAT.Expect.TTY;            use GNAT.Expect.TTY;
pragma Warnings (On);

with GNAT.Expect;                use GNAT.Expect;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.VFS;               use GNATCOLL.VFS;

with Case_Handling;              use Case_Handling;
with Default_Preferences;        use Default_Preferences;
with Foreign_Naming_Editors;     use Foreign_Naming_Editors;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with GPS.Kernel;                 use GPS.Kernel;
with Language.C;                 use Language.C;
with Language.Cpp;               use Language.Cpp;
with Language;                   use Language;
with Language_Handlers;          use Language_Handlers;
with Naming_Editors;             use Naming_Editors;
with Project_Viewers;            use Project_Viewers;
with Projects;                   use Projects;
with Language.Libclang_Tree; use Language.Libclang_Tree;
with GPS.Core_Kernels; use GPS.Core_Kernels;

package body Cpp_Module is
   Me : constant Trace_Handle := Create ("CPP");

   C_Automatic_Indentation : Indentation_Kind_Preferences.Preference;
   C_Use_Tabs              : Boolean_Preference;
   C_Indentation_Level     : Integer_Preference;
   C_Indent_Extra          : Boolean_Preference;
   C_Indent_Comments       : Boolean_Preference;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function C_Naming_Scheme_Editor
     (Kernel : access Kernel_Handle_Record'Class; Lang : String)
      return Language_Naming_Editor;
   --  Create the naming scheme editor page. Subsidiary of Register_Module
   --  but must be defined at library level because it is invoked from the
   --  gps kernel.

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class);
   --  Called when the preferences have changed. Subsidiary of Register_Module
   --  but must be defined at library level because it is invoked from the
   --  gps kernel.

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
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      pragma Unreferenced (Kernel, Data);
      Style  : constant Indentation_Kind := C_Automatic_Indentation.Get_Pref;
      Params : constant Indent_Parameters :=
                 (Indent_Level        => C_Indentation_Level.Get_Pref,
                  Indent_Continue     => 0,
                  Indent_Decl         => 0,
                  Indent_Conditional  => 0,
                  Indent_Record       => 0,
                  Indent_Case_Extra   => Automatic,
                  Casing_Policy       => Case_Handling.Disabled,
                  Reserved_Casing     => Case_Handling.Unchanged,
                  Ident_Casing        => Case_Handling.Unchanged,
                  Format_Operators    => False,
                  Use_Tabs            => C_Use_Tabs.Get_Pref,
                  Align_On_Colons     => C_Indent_Extra.Get_Pref,
                  Align_On_Arrows     => False,
                  Align_Decl_On_Colon => False,
                  Indent_Comments     => C_Indent_Comments.Get_Pref,
                  Stick_Comments      => False);

   begin
      Set_Indentation_Parameters (C_Lang, Params, Style);
      Set_Indentation_Parameters (Cpp_Lang, Params, Style);
   end On_Preferences_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Handler : constant Language_Handler := Get_Language_Handler (Kernel);
   begin
      Register_Language (Handler, C_Lang, null);
      Get_Registry (Kernel).Environment.Register_Default_Language_Extension
        (Language_Name       => "c",
         Default_Spec_Suffix => ".h",
         Default_Body_Suffix => ".c",
         Obj_Suffix          => ".o");

      Register_Language (Handler, Cpp_Lang, null);
      Get_Registry (Kernel).Environment.Register_Default_Language_Extension
        (Language_Name       => "c++",
         Default_Spec_Suffix => ".hh",
         Default_Body_Suffix => ".cpp",
         Obj_Suffix          => ".o");

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

      C_Indent_Extra := Create
        (Get_Preferences (Kernel),
         Name    => "C-Indent-Extra",
         Default => True,
         Page    => -"Editor/C & C++",
         Doc     => -("Whether to indent if/loop/switch constructs an extra"
                      & " level after '{'"),
         Label   => -"Extra indentation");

      C_Indent_Comments := Create
        (Get_Preferences (Kernel),
         Name    => "C-Indent-Comments",
         Default => True,
         Page    => -"Editor/C & C++",
         Doc     => -"Whether to indent lines with comments only",
         Label   => -"Indent comments");

      --  Register tree providers based on clang for both C and C++ languages
      Kernel.Register_Tree_Provider
        (C_Lang, new Clang_Tree_Provider'(Kernel => Core_Kernel (Kernel)));
      Kernel.Register_Tree_Provider
        (Cpp_Lang, new Clang_Tree_Provider'(Kernel => Core_Kernel (Kernel)));

      Add_Hook
        (Kernel, Preference_Changed_Hook,
         Wrapper (On_Preferences_Changed'Access),
         Name => "cpp_module.preferences_changed");
      On_Preferences_Changed (Kernel, Data => null);

      Register_Naming_Scheme_Editor
        (Kernel, "c", C_Naming_Scheme_Editor'Access);
      Register_Naming_Scheme_Editor
        (Kernel, "c++", C_Naming_Scheme_Editor'Access);

   exception
      when E : others => Trace (Me, E);
   end Register_Module;

end Cpp_Module;
