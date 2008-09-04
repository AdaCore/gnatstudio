-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2008, AdaCore              --
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

with Default_Preferences;      use Default_Preferences;
with Default_Preferences.Enums;
with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with GPS.Intl;                 use GPS.Intl;
with ALI_Parser;               use ALI_Parser;
with Entities;                 use Entities;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with Language.Ada;             use Language.Ada;
with Language.Tree.Ada;        use Language.Tree.Ada;
with Language_Handlers;        use Language_Handlers;
with Language;                 use Language;
with Project_Viewers;          use Project_Viewers;
with Naming_Editors;           use Naming_Editors;
with Ada_Naming_Editors;       use Ada_Naming_Editors;
with Projects.Registry;        use Projects.Registry;
with Case_Handling;            use Case_Handling;

package body Ada_Module is

   package Casing_Policy_Preferences is new
     Default_Preferences.Enums.Generics (Casing_Policy);

   package Casing_Preferences is new
     Default_Preferences.Enums.Generics (Casing_Type);

   package Indent_Preferences is new
     Default_Preferences.Enums.Generics (Indent_Style);

   Ada_Automatic_Indentation : Indentation_Kind_Preferences.Preference;
   Ada_Use_Tabs              : Boolean_Preference;
   Ada_Indentation_Level     : Integer_Preference;
   Ada_Continuation_Level    : Integer_Preference;
   Ada_Declaration_Level     : Integer_Preference;
   Ada_Conditional_Level     : Integer_Preference;
   Ada_Record_Level          : Integer_Preference;
   Ada_Indent_Case_Extra     : Indent_Preferences.Preference;
   Ada_Casing_Policy         : Casing_Policy_Preferences.Preference;
   Ada_Reserved_Casing       : Casing_Preferences.Preference;
   Ada_Ident_Casing          : Casing_Preferences.Preference;
   Ada_Format_Operators      : Boolean_Preference;
   Ada_Align_On_Colons       : Boolean_Preference;
   Ada_Align_On_Arrows       : Boolean_Preference;
   Ada_Align_Decl_On_Colon   : Boolean_Preference;
   Ada_Indent_Comments       : Boolean_Preference;
   Ada_Stick_Comments        : Boolean_Preference;

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed

   function Naming_Scheme_Editor
     (Kernel : access Kernel_Handle_Record'Class; Lang : String)
      return Language_Naming_Editor;
   --  Create the naming scheme editor page

   --------------------------
   -- Naming_Scheme_Editor --
   --------------------------

   function Naming_Scheme_Editor
     (Kernel : access Kernel_Handle_Record'Class; Lang : String)
      return Language_Naming_Editor
   is
      pragma Unreferenced (Kernel, Lang);
      Naming : Ada_Naming_Editor;
   begin
      Gtk_New (Naming);
      return Language_Naming_Editor (Naming);
   end Naming_Scheme_Editor;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Set_Indentation_Parameters
        (Ada_Lang,
         Indent_Style => Indentation_Kind'(Ada_Automatic_Indentation.Get_Pref),
         Params       =>
           (Indent_Level        => Ada_Indentation_Level.Get_Pref,
            Indent_Continue     => Ada_Continuation_Level.Get_Pref,
            Indent_Decl         => Ada_Declaration_Level.Get_Pref,
            Indent_Conditional  => Ada_Conditional_Level.Get_Pref,
            Indent_Record       => Ada_Record_Level.Get_Pref,
            Tab_Width           => Tab_Width.Get_Pref,
            Indent_Case_Extra   => Ada_Indent_Case_Extra.Get_Pref,
            Casing_Policy       => Ada_Casing_Policy.Get_Pref,
            Reserved_Casing     => Ada_Reserved_Casing.Get_Pref,
            Ident_Casing        => Ada_Ident_Casing.Get_Pref,
            Format_Operators    => Ada_Format_Operators.Get_Pref,
            Use_Tabs            => Ada_Use_Tabs.Get_Pref,
            Align_On_Colons     => Ada_Align_On_Colons.Get_Pref,
            Align_On_Arrows     => Ada_Align_On_Arrows.Get_Pref,
            Align_Decl_On_Colon => Ada_Align_Decl_On_Colon.Get_Pref,
            Indent_Comments     => Ada_Indent_Comments.Get_Pref,
            Stick_Comments      => Ada_Stick_Comments.Get_Pref));
   end On_Preferences_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Handler : constant Language_Handler := Language_Handler
        (Get_Language_Handler (Kernel));
      LI      : constant Entities.LI_Handler := Create_ALI_Handler
        (Get_Database (Kernel), Project_Registry (Get_Registry (Kernel).all));
   begin
      Register_Language (Handler, Ada_Lang, Ada_Tree_Lang, LI => LI);
      Register_Default_Language_Extension
        (Get_Registry (Kernel).all,
         Language_Name       => "Ada",
         Default_Spec_Suffix => ".ads",
         Default_Body_Suffix => ".adb");

      Ada_Automatic_Indentation := Indentation_Kind_Preferences.Create
        (Get_Preferences (Kernel),
         Name    => "Ada-Auto-Indentation",
         Default => Extended,
         Doc     => -"How the editor should indent Ada sources",
         Page    => -"Editor/Ada",
         Label   => -"Auto indentation");

      Ada_Use_Tabs := Create
        (Get_Preferences (Kernel),
         Name    => "Ada-Use-Tabs",
         Default => False,
         Doc     =>
         -"Whether the editor should use tabulations when indenting",
         Page     => -"Editor/Ada",
         Label    => -"Use tabulations");

      Ada_Indentation_Level := Create
        (Get_Preferences (Kernel),
         Name    => "Ada-Indent-Level",
         Minimum => 1,
         Maximum => 9,
         Default => 3,
         Page    => -"Editor/Ada",
         Doc     => -"The number of spaces for the default Ada indentation",
         Label   => -"Default indentation");

      Ada_Continuation_Level := Create
        (Get_Preferences (Kernel),
         Name    => "Ada-Continuation-Level",
         Minimum => 0,
         Maximum => 9,
         Default => 2,
         Page    => -"Editor/Ada",
         Doc     => -"The number of extra spaces for continuation lines",
         Label   => -"Continuation lines");

      Ada_Declaration_Level := Create
        (Get_Preferences (Kernel),
         Name    => "Ada-Declaration-Level",
         Minimum => 0,
         Maximum => 9,
         Default => 0,
         Page  => -"Editor/Ada",
         Doc   => -"The number of extra spaces for multiple line declarations",
         Label => -"Declaration lines");

      Ada_Conditional_Level := Create
        (Get_Preferences (Kernel),
         Name    => "Ada-Conditional-Level",
         Minimum => 0,
         Maximum => 9,
         Default => 1,
         Page    => -"Editor/Ada",
         Doc   => -"The number of extra spaces for multiple line conditionals",
         Label   => -"Conditional continuation lines");

      Ada_Record_Level := Create
        (Get_Preferences (Kernel),
         Name    => "Ada-Record-Level",
         Minimum => 0,
         Maximum => 9,
         Default => 3,
         Page    => -"Editor/Ada",
         Doc     =>
         -"The number of extra spaces for multiple line record types",
         Label   => -"Record indentation");

      Ada_Indent_Case_Extra := Indent_Preferences.Create
        (Get_Preferences (Kernel),
         Name    => "Ada-Indent-Case-Style",
         Default => Automatic,
         Page    => -"Editor/Ada",
         Doc     => -"Whether to indent case statements with an extra level",
         Label   => -"Case indentation");

      Ada_Casing_Policy := Casing_Policy_Preferences.Create
        (Get_Preferences (Kernel),
         Name    => "Ada-Casing-Policy",
         Label   => -"Casing policy",
         Doc     => -"Keywords and Identifiers casing policy",
         Page    => -"Editor/Ada",
         Default => End_Of_Line);

      Ada_Reserved_Casing := Casing_Preferences.Create
        (Get_Preferences (Kernel),
         Name    => "Ada-Reserved-Casing",
         Default => Unchanged,
         Page    => -"Editor/Ada",
         Doc     => -"How the editor should handle reserved words casing",
         Label   => -"Reserved word casing");

      Ada_Ident_Casing := Casing_Preferences.Create
        (Get_Preferences (Kernel),
         Name    => "Ada-Ident-Casing",
         Default => Unchanged,
         Page    => -"Editor/Ada",
         Doc     => -"How the editor should handle identifiers casing",
         Label   => -"Identifier casing");

      Ada_Format_Operators := Create
        (Get_Preferences (Kernel),
         Name    => "Ada-Format-Operators",
         Default => False,
         Page    => -"Editor/Ada",
         Doc     =>
         -"Whether to add spaces around operators and delimiters",
         Label   => -"Format operators/delimiters");

      Ada_Align_On_Colons := Create
        (Get_Preferences (Kernel),
         Name    => "Ada-Align-On-Colons",
         Default => False,
         Page    => -"Editor/Ada",
         Doc     => -"Whether to align colons in declaration statements",
         Label   => -"Align colons in declarations");

      Ada_Align_On_Arrows := Create
        (Get_Preferences (Kernel),
         Name    => "Ada-Align-On-Arrows",
         Default => False,
         Page    => -"Editor/Ada",
         Doc     =>
         -"Whether to align associations on arrow delimiters",
         Label   => -"Align associations on arrows");

      Ada_Align_Decl_On_Colon := Create
        (Get_Preferences (Kernel),
         Name    => "Ada-Align-Decl-On_Colon",
         Default => False,
         Page    => -"Editor/Ada",
         Doc     =>
         -("Whether to align continuation lines after a declaration " &
           "based on the colon character"),
         Label   => -"Align declarations after colon");

      Ada_Indent_Comments := Create
        (Get_Preferences (Kernel),
         Name    => "Ada-Indent-Comments",
         Default => True,
         Page    => -"Editor/Ada",
         Doc     => -"Whether to indent lines with comments only",
         Label   => -"Indent comments");

      Ada_Stick_Comments := Create
        (Get_Preferences (Kernel),
         Name    => "Ada-Stick-Comments",
         Default => False,
         Page    => -"Editor/Ada",
         Doc     =>
         -("Whether to align comment lines following 'record' and " &
           "'is' keywords immediately with no extra space"),
         Label   => -"Align comments on keywords");

      Add_Hook
        (Kernel, Preferences_Changed_Hook,
         Wrapper (On_Preferences_Changed'Access),
         "ada_module_preferences_changed");
      On_Preferences_Changed (Kernel);

      Register_Naming_Scheme_Editor
        (Kernel, "Ada", Naming_Scheme_Editor'Access);
   end Register_Module;

end Ada_Module;
