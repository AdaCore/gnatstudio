------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2018, AdaCore                     --
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

with GNATCOLL.Projects;         use GNATCOLL.Projects;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Default_Preferences;       use Default_Preferences;
with Default_Preferences.Enums;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Intl;                  use GPS.Intl;

with Language.Ada;              use Language.Ada;
with Ada_Semantic_Tree.Lang;    use Ada_Semantic_Tree.Lang;
with Language_Handlers;         use Language_Handlers;
with Language;                  use Language;
with Project_Viewers;           use Project_Viewers;
with Ada_Naming_Editors;        use Ada_Naming_Editors;
with Projects;                  use Projects;
with Case_Handling;             use Case_Handling;

package body Ada_Module is

   -----------------
   -- Preferences --
   -----------------

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

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);
   --  Called when the preferences have changed

   function Naming_Scheme_Editor
     (Dummy_Kernel : not null access Kernel_Handle_Record'Class;
      Dummy_Lang : String)
      return not null access Project_Editor_Page_Record'Class is
      (new Ada_Naming_Editor_Record);
   --  Create the naming scheme editor page

   -------------
   -- Filters --
   -------------

   type Ada_Body_Filter_Record is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Context : access Ada_Body_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;

   type Ada_Spec_Filter_Record is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Context : access Ada_Spec_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean;

   type Has_Other_File_On_Disk_Filter_Record is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Other_File_On_Disk_Filter_Record;
      Context : GPS.Kernel.Selection_Context) return Boolean;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference)
   is
      pragma Unreferenced (Self, Kernel, Pref);
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
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access Ada_Body_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      if Has_File_Information (Ctxt) then
         declare
            File        : constant Virtual_File := File_Information (Ctxt);
            Lang        : constant String :=
                            Get_Language_From_File
                              (Get_Language_Handler (Get_Kernel (Ctxt)),
                               File);
            Is_Ada_File : constant Boolean :=
                            Equal (Lang, "ada", Case_Sensitive => False);
            Project     : constant Project_Type := Project_Information (Ctxt);
            Body_Suffix : constant String :=
                            Project.Attribute_Value
                              (Impl_Suffix_Attribute,
                               Index   => "ada",
                               Default => Default_Gnat_Body_Suffix);
         begin
            return (Is_Ada_File
                    and then Ends_With (File.Display_Full_Name, Body_Suffix));
         end;
      end if;

      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Context : access Ada_Spec_Filter_Record;
      Ctxt    : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Context);
   begin
      if Has_File_Information (Ctxt) then
         declare
            File        : constant Virtual_File := File_Information (Ctxt);
            Lang        : constant String :=
                            Get_Language_From_File
                              (Get_Language_Handler (Get_Kernel (Ctxt)),
                               File);
            Is_Ada_File : constant Boolean :=
                            Equal (Lang, "ada", Case_Sensitive => False);
            Project     : constant Project_Type := Project_Information (Ctxt);
            Spec_Suffix : constant String :=
                            Project.Attribute_Value
                              (Spec_Suffix_Attribute,
                               Index   => "ada",
                               Default => Default_Gnat_Spec_Suffix);
         begin
            return (Is_Ada_File
                    and then Ends_With (File.Display_Full_Name, Spec_Suffix));
         end;
      end if;

      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Has_Other_File_On_Disk_Filter_Record;
      Context : GPS.Kernel.Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
   begin
      if Has_File_Information (Context) then
         declare
            File       : constant Virtual_File := File_Information (Context);
            Other_File : constant Virtual_File :=
                           Get_Registry (Kernel).Tree.Other_File (File);
         begin
            return Other_File /= No_File
              and then Other_File /= File
              and then Other_File.Is_Regular_File;
         end;
      end if;

      return False;
   end Filter_Matches_Primitive;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Handler                 : constant Language_Handler :=
                                Get_Language_Handler (Kernel);
      Has_Other_File_Filter   : constant Action_Filter :=
                                  new Has_Other_File_On_Disk_Filter_Record;
      Is_Ada_Body_Filter      : constant Action_Filter :=
                                  new Ada_Body_Filter_Record;
      Is_Ada_Spec_Filter      : constant Action_Filter :=
                                  new Ada_Spec_Filter_Record;
      Body_Has_Spec_Filter    : constant Action_Filter :=
                                  Is_Ada_Body_Filter and Has_Other_File_Filter;
      Spec_Has_Body_Filter    : constant Action_Filter :=
                                  Is_Ada_Spec_Filter and Has_Other_File_Filter;
      Body_Has_No_Spec_Filter : constant Action_Filter :=
                                  (Is_Ada_Body_Filter
                                   and not Has_Other_File_Filter);
      Spec_Has_No_Body_Filter : constant Action_Filter :=
                                  (Is_Ada_Spec_Filter
                                   and not Has_Other_File_Filter);
      Manager                 : constant Preferences_Manager :=
                                  Kernel.Get_Preferences;
      Page                    : Preferences_Page;
      Group                   : Preferences_Group;
   begin
      if not Use_LAL_In_Indent.Get_Pref then
         Register_Language (Handler, Ada_Lang, Ada_Tree_Lang);
      end if;

      --  Register some general Ada-related filters so that they can be used
      --  from other GPS parts.

      Register_Filter
        (Kernel,
         Filter => Is_Ada_Body_Filter,
         Name   => "Is_Ada_Body");
      Register_Filter
        (Kernel,
         Filter => Is_Ada_Spec_Filter,
         Name   => "Is_Ada_Spec");
      Register_Filter
        (Kernel,
         Filter => Has_Other_File_Filter,
         Name   => "Has_Other_File_On_Disk");
      Register_Filter
        (Kernel,
         Filter => Body_Has_Spec_Filter,
         Name   => "Body_Has_Spec");
      Register_Filter
        (Kernel,
         Filter => Spec_Has_Body_Filter,
         Name   => "Spec_Has_Body");
      Register_Filter
        (Kernel,
         Filter => Body_Has_No_Spec_Filter,
         Name   => "Body_Has_No_Spec");
      Register_Filter
        (Kernel,
         Filter => Spec_Has_No_Body_Filter,
         Name   => "Spec_Has_No_Body");

      --  Register the default language extensions for Ada

      Register_Default_Language_Extension
        (Get_Registry (Kernel).Environment.all,
         Language_Name       => "Ada",
         Default_Spec_Suffix => ".ads",
         Default_Body_Suffix => ".adb",
         Obj_Suffix          => ".o");

      --  Register the default Ada-related preferences

      Ada_Automatic_Indentation := Indentation_Kind_Preferences.Create
        (Manager,
         Path    => -"Editor/Ada:Indentation",
         Name    => "Ada-Auto-Indentation",
         Default => Extended,
         Doc     => -"Enable auto-indentation for Ada sources.",
         Label   => -"Auto indentation");

      Ada_Indentation_Level := Create
        (Manager,
         Path    => -"Editor/Ada:Indentation",
         Name    => "Ada-Indent-Level",
         Minimum => 1,
         Maximum => 9,
         Default => 3,
         Doc     => -"Number of spaces for the default Ada indentation.",
         Label   => -"Default indentation");

      Ada_Continuation_Level := Create
        (Manager,
         Path    => -"Editor/Ada:Indentation",
         Name    => "Ada-Continuation-Level",
         Minimum => 0,
         Maximum => 9,
         Default => 2,
         Doc     => -"Number of extra spaces for continuation lines.",
         Label   => -"Continuation lines");

      Ada_Declaration_Level := Create
        (Manager,
         Path  => -"Editor/Ada:Indentation",
         Name    => "Ada-Declaration-Level:Indentation",
         Minimum => 0,
         Maximum => 9,
         Default => 0,
         Doc   => -"Number of extra spaces for multi line declarations.",
         Label => -"Declaration lines");

      Ada_Conditional_Level := Create
        (Manager,
         Path    => -"Editor/Ada:Indentation",
         Name    => "Ada-Conditional-Level",
         Minimum => 0,
         Maximum => 9,
         Default => 1,
         Doc   => -"Number of extra spaces for multiple line conditionals.",
         Label   => -"Conditional continuation lines");

      Ada_Record_Level := Create
        (Manager,
         Path    => -"Editor/Ada:Indentation",
         Name    => "Ada-Record-Level",
         Minimum => 0,
         Maximum => 9,
         Default => 3,
         Doc     => -"Number of extra spaces for multiple line record types.",
         Label   => -"Record indentation");

      Ada_Indent_Case_Extra := Indent_Preferences.Create
        (Manager,
         Path    => -"Editor/Ada:Indentation",
         Name    => "Ada-Indent-Case-Style",
         Default => Automatic,
         Doc     => -"Indent case statement with an extra level.",
         Label   => -"Case indentation");

      Ada_Casing_Policy := Casing_Policy_Preferences.Create
        (Manager,
         Path    => -"Editor/Ada:Casing",
         Name    => "Ada-Casing-Policy",
         Label   => -"Keywords and Identifiers casing",
         Doc     => "",
         Default => Disabled);

      Ada_Reserved_Casing := Casing_Preferences.Create
        (Manager,
         Path    => -"Editor/Ada:Casing",
         Name    => "Ada-Reserved-Casing",
         Default => Lower,
         Doc     => "",
         Label   => -"Reserved word casing");

      Ada_Ident_Casing := Casing_Preferences.Create
        (Manager,
         Path    => -"Editor/Ada:Casing",
         Name    => "Ada-Ident-Casing",
         Default => Smart_Mixed,
         Doc     => "",
         Label   => -"Identifier casing");

      Ada_Use_Tabs := Create
        (Manager,
         Path     => -"Editor/Ada:Indentation",
         Name    => "Ada-Use-Tabs",
         Default => False,
         Doc     =>
         -"Use tabulations when indenting.",
         Label    => -"Use tabulations");

      Ada_Format_Operators := Create
        (Manager,
         Path    => -"Editor/Ada:Indentation",
         Name    => "Ada-Format-Operators",
         Default => False,
         Doc     => -"Add spaces around operators and delimiters.",
         Label   => -"Format operators/delimiters");

      Ada_Align_On_Colons := Create
        (Manager,
         Path    => -"Editor/Ada:Indentation",
         Name    => "Ada-Align-On-Colons",
         Default => False,
         Doc     => -"Align colons in declaration statements.",
         Label   => -"Align colons in declarations");

      Ada_Align_On_Arrows := Create
        (Manager,
         Path    => -"Editor/Ada:Indentation",
         Name    => "Ada-Align-On-Arrows",
         Default => False,
         Doc     => -"Align associations on arrow delimiters.",
         Label   => -"Align associations on arrows");

      Ada_Align_Decl_On_Colon := Create
        (Manager,
         Path    => -"Editor/Ada:Indentation",
         Name    => "Ada-Align-Decl-On_Colon",
         Default => False,
         Doc     =>
         -("Align continuation lines after a declaration " &
           "based on the colon character."),
         Label   => -"Align declarations after colon");

      Ada_Indent_Comments := Create
        (Manager,
         Path    => -"Editor/Ada:Indentation",
         Name    => "Ada-Indent-Comments",
         Default => True,
         Doc     => -"Indent lines with only comments.",
         Label   => -"Indent comments");

      Ada_Stick_Comments := Create
        (Manager,
         Path    => -"Editor/Ada:Indentation",
         Name    => "Ada-Stick-Comments",
         Default => False,
         Doc     =>
         -("Align comment lines following 'record' and " &
           "'is' keywords immediately with no extra space"),
         Label   => -"Align comments on keywords");

      --  Register some of the Ada casing preferences in the 'General' page of
      --  the preferences assistant too.

      Page := Kernel.Get_Preferences.Get_Registered_Page
        (Name             => "Preferences Assistant General",
         Create_If_Needed => False);

      Group := new Preferences_Group_Record;
      Page.Register_Group
        (Name             => "Ada Casing",
         Group            => Group,
         Priority         => -2,
         Replace_If_Exist => False);
      Group.Add_Pref
        (Manager => Manager,
         Pref    => Preference (Ada_Casing_Policy));
      Group.Add_Pref
        (Manager => Manager,
         Pref    => Preference (Ada_Reserved_Casing));
      Group.Add_Pref
        (Manager => Manager,
         Pref    => Preference (Ada_Ident_Casing));

      Preferences_Changed_Hook.Add (new On_Pref_Changed);

      Register_Naming_Scheme_Editor
        (Kernel, "Ada", Naming_Scheme_Editor'Access);
   end Register_Module;

end Ada_Module;
