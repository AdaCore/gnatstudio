------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2019, AdaCore                     --
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

with GNAT.Expect;
with GNATCOLL.Traces;            use GNATCOLL.Traces;

with Case_Handling;              use Case_Handling;
with Default_Preferences;        use Default_Preferences;
with Foreign_Naming_Editors;     use Foreign_Naming_Editors;
with GPS.Intl;                   use GPS.Intl;
with GPS.Core_Kernels;           use GPS.Core_Kernels;
with GPS.Kernel;                 use GPS.Kernel;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Project;         use GPS.Kernel.Project;
with Language;                   use Language;
with Language.C;                 use Language.C;
with Language.Cpp;               use Language.Cpp;
with Language_Handlers;          use Language_Handlers;
with Project_Viewers;            use Project_Viewers;
with Projects;                   use Projects;
with Language.Libclang_Tree;     use Language.Libclang_Tree;

package body Cpp_Module is
   Me : constant Trace_Handle := Create ("GPS.CPP.MODULE");

   C_Automatic_Indentation : Indentation_Kind_Preferences.Preference;
   C_Use_Tabs              : Boolean_Preference;
   C_Indentation_Level     : Integer_Preference;
   C_Indent_Extra          : Boolean_Preference;
   C_Indent_Comments       : Boolean_Preference;

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed. Subsidiary of Register_Module
   --  but must be defined at library level because it is invoked from the
   --  gps kernel.

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self, Kernel, Pref);
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
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Handler : constant Language_Handler := Get_Language_Handler (Kernel);
      Manager : constant Preferences_Manager := Kernel.Get_Preferences;
      Hook    : Preferences_Hooks_Function_Access;

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

      Manager.Register_Page
        (Name             => "Editor/C & C++",
         Page             => new Default_Preferences_Page_Record,
         Priority         => -2,
         Replace_If_Exist => True);

      C_Automatic_Indentation := Indentation_Kind_Preferences.Create
        (Manager,
         Path    => -"Editor/C & C++:Indentation",
         Name    => "C-Auto-Indentation",
         Default => Extended,
         Doc     => -"Enable auto-indentation for C and C++ sources.",
         Label   => -"Auto indentation");

      C_Indentation_Level := Manager.Create
        (Path    => -"Editor/C & C++:Indentation",
         Name    => "C-Indent-Level",
         Minimum => 1,
         Maximum => 9,
         Default => 2,
         Doc     => -"Number of spaces for the default indentation.",
         Label   => -"Default indentation");

      C_Use_Tabs := Manager.Create
        (Path    => -"Editor/C & C++:Indentation",
         Name    => "C-Use-Tabs",
         Default => True,
         Doc     => -"Use tabulations when indenting.",
         Label   => -"Use tabulations");

      C_Indent_Extra := Manager.Create
        (Path    => -"Editor/C & C++:Indentation",
         Name    => "C-Indent-Extra",
         Default => True,
         Doc     =>
            -"Indent if/loop/switch constructs an extra level after '{'.",
         Label   => -"Extra indentation");

      C_Indent_Comments := Manager.Create
        (Path    => -"Editor/C & C++:Indentation",
         Name    => "C-Indent-Comments",
         Default => True,
         Doc     => -"Indent lines with only comments.",
         Label   => -"Indent comments");

      --  Register tree providers based on clang for both C and C++ languages
      Kernel.Register_Tree_Provider
        (C_Lang, new Clang_Tree_Provider'(Kernel => Core_Kernel (Kernel)));
      Kernel.Register_Tree_Provider
        (Cpp_Lang, new Clang_Tree_Provider'(Kernel => Core_Kernel (Kernel)));

      Hook := new On_Pref_Changed;
      Preferences_Changed_Hook.Add (Hook);
      Hook.Execute (Kernel, null);

      Register_Naming_Scheme_Editor
        (Kernel, "c", Naming_Editor_Factory'Access);
      Register_Naming_Scheme_Editor
        (Kernel, "c++", Naming_Editor_Factory'Access);

   exception
      when E : others => Trace (Me, E);
   end Register_Module;

end Cpp_Module;
