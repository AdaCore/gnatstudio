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

with Glib.Generic_Properties;  use Glib;
with Glib.Properties.Creation; use Glib.Properties.Creation;
with Glide_Kernel;             use Glide_Kernel;
with Glide_Kernel.Hooks;       use Glide_Kernel.Hooks;
with Glide_Kernel.Preferences; use Glide_Kernel.Preferences;
with Glide_Kernel.Project;     use Glide_Kernel.Project;
with Src_Info.ALI;             use Src_Info.ALI;
with Language.Ada;             use Language.Ada;
with Language_Handlers.Glide;  use Language_Handlers.Glide;
with Glide_Intl;               use Glide_Intl;
with Language;                 use Language;
with Project_Viewers;          use Project_Viewers;
with Naming_Editors;           use Naming_Editors;
with Ada_Naming_Editors;       use Ada_Naming_Editors;
with Projects.Registry;        use Projects.Registry;

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
   Ada_Align_On_Colons       : Param_Spec_Boolean;
   Ada_Align_On_Arrows       : Param_Spec_Boolean;

   package Casing_Properties is new
     Glib.Generic_Properties.Generic_Enumeration_Property
     ("Casing_Type", Language.Casing_Type);

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
     (Kernel : access Kernel_Handle_Record'Class) is
   begin
      Set_Indentation_Parameters
        (Ada_Lang,
         Indent_Style => Indentation_Kind'Val
            (Get_Pref (Kernel, Ada_Automatic_Indentation)),
         Params       =>
           (Indent_Level      =>
              Integer (Get_Pref (Kernel, Ada_Indentation_Level)),
            Indent_Continue   =>
              Integer (Get_Pref (Kernel, Ada_Continuation_Level)),
            Indent_Decl       =>
              Integer (Get_Pref (Kernel, Ada_Declaration_Level)),
            Tab_Width         => Integer (Get_Pref (Kernel, Tab_Width)),
            Indent_Case_Extra => Get_Pref (Kernel, Ada_Indent_Case_Extra),
            Reserved_Casing   => Casing_Type'Val
              (Get_Pref (Kernel, Ada_Reserved_Casing)),
            Ident_Casing      => Casing_Type'Val
              (Get_Pref (Kernel, Ada_Ident_Casing)),
            Format_Operators  => Get_Pref (Kernel, Ada_Format_Operators),
            Use_Tabs          => Get_Pref (Kernel, Ada_Use_Tabs),
            Align_On_Colons   => Get_Pref (Kernel, Ada_Align_On_Colons),
            Align_On_Arrows   => Get_Pref (Kernel, Ada_Align_On_Arrows)));
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
      Set_Language_Handler
        (Handler, "Ada",
         LI                  => Get_LI_Handler_By_Name (Handler, "Ada"));
      Register_Default_Language_Extension
        (Get_Registry (Kernel),
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

      Ada_Align_On_Colons := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Ada-Align-On-Colons",
           Default => False,
           Blurb   => -"Whether to align declarations on colons",
           Nick    => -"Align declarations on colons"));
      Register_Property
        (Kernel, Param_Spec (Ada_Align_On_Colons), -"Editor:Ada");

      Ada_Align_On_Arrows := Param_Spec_Boolean
        (Gnew_Boolean
          (Name    => "Ada-Align-On_Arrows",
           Default => False,
           Blurb   =>
             -"Whether to align associations on arrow delimiters",
           Nick    => -"Align associations on arrows"));
      Register_Property
        (Kernel, Param_Spec (Ada_Align_On_Arrows), -"Editor:Ada");

      Add_Hook
        (Kernel, Preferences_Changed_Hook, On_Preferences_Changed'Access);
      On_Preferences_Changed (Kernel);

      Register_Naming_Scheme_Editor
        (Kernel, "Ada", Naming_Scheme_Editor'Access);
   end Register_Module;

end Ada_Module;
