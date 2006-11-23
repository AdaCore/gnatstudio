-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002-2006                       --
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

with Glib.Generic_Properties;  use Glib;
with Glib.Properties.Creation; use Glib.Properties.Creation;

with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GPS.Kernel.Project;       use GPS.Kernel.Project;
with GPS.Intl;                 use GPS.Intl;
with ALI_Parser;               use ALI_Parser;
with Entities;                 use Entities;
with Language.Ada;             use Language.Ada;
with Language_Handlers;        use Language_Handlers;
with Language;                 use Language;
with Project_Viewers;          use Project_Viewers;
with Naming_Editors;           use Naming_Editors;
with Ada_Naming_Editors;       use Ada_Naming_Editors;
with Projects.Registry;        use Projects.Registry;
with Case_Handling;            use Case_Handling;

package body Ada_Module is

   Ada_Automatic_Indentation : Param_Spec_Enum;
   Ada_Use_Tabs              : Param_Spec_Boolean;
   Ada_Indentation_Level     : Param_Spec_Int;
   Ada_Continuation_Level    : Param_Spec_Int;
   Ada_Declaration_Level     : Param_Spec_Int;
   Ada_Conditional_Level     : Param_Spec_Int;
   Ada_Record_Level          : Param_Spec_Int;
   Ada_Indent_Case_Extra     : Param_Spec_Enum;
   Ada_Casing_Policy         : Param_Spec_Enum;
   Ada_Reserved_Casing       : Param_Spec_Enum;
   Ada_Ident_Casing          : Param_Spec_Enum;
   Ada_Format_Operators      : Param_Spec_Boolean;
   Ada_Align_On_Colons       : Param_Spec_Boolean;
   Ada_Align_On_Arrows       : Param_Spec_Boolean;
   Ada_Align_Decl_On_Colon   : Param_Spec_Boolean;
   Ada_Indent_Comments       : Param_Spec_Boolean;
   Ada_Stick_Comments        : Param_Spec_Boolean;

   package Casing_Policy_Properties is new
     Glib.Generic_Properties.Generic_Enumeration_Property
       ("Casing_Policy", Casing_Policy);

   package Casing_Properties is new
     Glib.Generic_Properties.Generic_Enumeration_Property
       ("Casing_Type", Casing_Type);

   package Indent_Properties is new
     Glib.Generic_Properties.Generic_Enumeration_Property
       ("Indent_Style", Indent_Style);

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
         Indent_Style => Indentation_Kind'Val
           (Get_Pref (Ada_Automatic_Indentation)),
         Params       =>
           (Indent_Level        =>
              Integer (Get_Pref (Ada_Indentation_Level)),
            Indent_Continue     =>
              Integer (Get_Pref (Ada_Continuation_Level)),
            Indent_Decl         =>
              Integer (Get_Pref (Ada_Declaration_Level)),
            Indent_Conditional  =>
              Integer (Get_Pref (Ada_Conditional_Level)),
            Indent_Record       =>
              Integer (Get_Pref (Ada_Record_Level)),
            Tab_Width           => Integer (Get_Pref (Tab_Width)),
            Indent_Case_Extra   => Indent_Style'Val
              (Get_Pref (Ada_Indent_Case_Extra)),
            Casing_Policy       => Casing_Policy'Val
              (Get_Pref (Ada_Casing_Policy)),
            Reserved_Casing     => Casing_Type'Val
              (Get_Pref (Ada_Reserved_Casing)),
            Ident_Casing        => Casing_Type'Val
              (Get_Pref (Ada_Ident_Casing)),
            Format_Operators    => Get_Pref (Ada_Format_Operators),
            Use_Tabs            => Get_Pref (Ada_Use_Tabs),
            Align_On_Colons     => Get_Pref (Ada_Align_On_Colons),
            Align_On_Arrows     => Get_Pref (Ada_Align_On_Arrows),
            Align_Decl_On_Colon =>
              Get_Pref (Ada_Align_Decl_On_Colon),
            Indent_Comments     => Get_Pref (Ada_Indent_Comments),
            Stick_Comments      => Get_Pref (Ada_Stick_Comments)));
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
      Register_Language (Handler, Ada_Lang, LI => LI);
      Register_Default_Language_Extension
        (Get_Registry (Kernel).all,
         Language_Name       => "Ada",
         Default_Spec_Suffix => ".ads",
         Default_Body_Suffix => ".adb");

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

      Ada_Conditional_Level := Param_Spec_Int
        (Gnew_Int
          (Name    => "Ada-Conditional-Level",
           Minimum => 0,
           Maximum => 9,
           Default => 1,
           Blurb   =>
             -"The number of extra spaces for multiple line conditionals",
           Nick    => -"Conditional continuation lines"));
      Register_Property
        (Kernel, Param_Spec (Ada_Conditional_Level), -"Editor:Ada");

      Ada_Record_Level := Param_Spec_Int
        (Gnew_Int
          (Name    => "Ada-Record-Level",
           Minimum => 0,
           Maximum => 9,
           Default => 3,
           Blurb   =>
             -"The number of extra spaces for multiple line record types",
           Nick    => -"Record indentation"));
      Register_Property
        (Kernel, Param_Spec (Ada_Record_Level), -"Editor:Ada");

      Ada_Indent_Case_Extra := Param_Spec_Enum
        (Indent_Properties.Gnew_Enum
          (Name    => "Ada-Indent-Case-Style",
           Default => Automatic,
           Blurb   => -"Whether to indent case statements with an extra level",
           Nick    => -"Case indentation"));
      Register_Property
        (Kernel, Param_Spec (Ada_Indent_Case_Extra), -"Editor:Ada");

      Ada_Casing_Policy := Param_Spec_Enum
        (Casing_Policy_Properties.Gnew_Enum
           (Name    => "Ada-Casing-Policy",
            Nick    => -"Casing policy",
            Blurb   => -"Keywords and Identifiers casing policy",
            Default => End_Of_Line));
      Register_Property
        (Kernel, Param_Spec (Ada_Casing_Policy), -"Editor:Ada");

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

      Ada_Align_On_Colons := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Ada-Align-On-Colons",
           Default => False,
           Blurb   => -"Whether to align colons in declaration statements",
           Nick    => -"Align colons in declarations"));
      Register_Property
        (Kernel, Param_Spec (Ada_Align_On_Colons), -"Editor:Ada");

      Ada_Align_On_Arrows := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Ada-Align-On-Arrows",
           Default => False,
           Blurb   =>
             -"Whether to align associations on arrow delimiters",
           Nick    => -"Align associations on arrows"));
      Register_Property
        (Kernel, Param_Spec (Ada_Align_On_Arrows), -"Editor:Ada");

      Ada_Align_Decl_On_Colon := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Ada-Align-Decl-On_Colon",
           Default => False,
           Blurb   =>
             -("Whether to align continuation lines after a declaration " &
               "based on the colon character"),
           Nick    => -"Align declarations after colon"));
      Register_Property
        (Kernel, Param_Spec (Ada_Align_Decl_On_Colon), -"Editor:Ada");

      Ada_Indent_Comments := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Ada-Indent-Comments",
           Default => True,
           Blurb   => -"Whether to indent lines with comments only",
           Nick    => -"Indent comments"));
      Register_Property
        (Kernel, Param_Spec (Ada_Indent_Comments), -"Editor:Ada");

      Ada_Stick_Comments := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Ada-Stick-Comments",
           Default => False,
           Blurb   =>
             -("Whether to align comment lines following 'record' and " &
               "'is' keywords immediately with no extra space"),
           Nick    => -"Align comments on keywords"));
      Register_Property
        (Kernel, Param_Spec (Ada_Stick_Comments), -"Editor:Ada");

      Add_Hook
        (Kernel, Preferences_Changed_Hook,
         Wrapper (On_Preferences_Changed'Access),
         "ada_module_preferences_changed");
      On_Preferences_Changed (Kernel);

      Register_Naming_Scheme_Editor
        (Kernel, "Ada", Naming_Scheme_Editor'Access);
   end Register_Module;

end Ada_Module;
