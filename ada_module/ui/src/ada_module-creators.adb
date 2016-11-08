------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2016, AdaCore                     --
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

with Ada.Characters.Handling;            use Ada.Characters.Handling;
with Ada.Containers.Doubly_Linked_Lists;
with Ada.Exceptions;                     use Ada.Exceptions;
with Ada.Strings.Unbounded;              use Ada.Strings.Unbounded;

with GNATCOLL.Projects;                  use GNATCOLL.Projects;
with GNATCOLL.Utils;                     use GNATCOLL.Utils;
with GNATCOLL.VFS;                       use GNATCOLL.VFS;

with Glib;                               use Glib;
with Gtk.Combo_Box_Text;                 use Gtk.Combo_Box_Text;
with Gtk.Dialog;                         use Gtk.Dialog;
with Gtk.GEntry;                         use Gtk.GEntry;
with Gtk.Stock;                          use Gtk.Stock;
with Gtk.Widget;                         use Gtk.Widget;

with Ada_Naming_Editors;                 use Ada_Naming_Editors;
with Commands;                           use Commands;
with Commands.Interactive;               use Commands.Interactive;
with Dialog_Utils;                       use Dialog_Utils;
with GPS.Kernel.Actions;                 use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;                use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;                   use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;                 use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;              use GPS.Kernel.Modules.UI;
with GPS.Kernel.Project;                 use GPS.Kernel.Project;
with GPS.Main_Window;                    use GPS.Main_Window;

package body Ada_Module.Creators is

   Ada_Creators_Contextual_Group : constant := -1;
   --  The contextual group for all ada components creation commands

   Package_Spec_Template         : constant String :=
                                     "package <name> is"
                                     & ASCII.LF
                                     & "end <name>";
   Package_Body_Template         : constant String :=
                                     "package body <name> is"
                                     & ASCII.LF
                                     & "end <name>";
   Main_Unit_Template            : constant String :=
                                     "procedure <name> is"
                                     & ASCII.LF
                                     & "end <name>";
   --  The templates used to fill the created files

   type Ada_Component_Kind is
     (Package_Spec_And_Body,
      Package_Spec,
      Main_Unit);
   --  Type used to represent the different kinds of Ada components that can be
   --  added.

   function Get_Label (Kind : Ada_Component_Kind) return String;
   --  Return a suitable label for the given ada component kind

   type Create_Component_Command is new Interactive_Command with record
      Kind : Ada_Component_Kind;
   end record;
   overriding function Execute
     (Command : access Create_Component_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Commmand used to create a given ada component.
   --  Display a dialog asking for the component's name and kind.

   procedure Create_Files_For_Component
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Kind    : Ada_Component_Kind;
      Name    : String;
      Dir     : Virtual_File;
      Project : Project_Type);
   --  Create all the files needed for the given Ada component kind in Dir,
   --  using Name and the naming convention set in Project for this purpose.

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Kind : Ada_Component_Kind) return String is
   begin
      case Kind is
         when Package_Spec_And_Body =>
            return "Package Specification and Body";

         when Package_Spec =>
            return "Package Specification";

         when Main_Unit =>
            return "Main Unit";
      end case;
   end Get_Label;

   --------------------------------
   -- Create_Files_For_Component --
   --------------------------------

   procedure Create_Files_For_Component
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Kind    : Ada_Component_Kind;
      Name    : String;
      Dir     : Virtual_File;
      Project : Project_Type)
   is
      package Virtual_Files_Lists is new Ada.Containers.Doubly_Linked_Lists
        (Element_Type => Virtual_File,
         "="          => "=");

      Dot_Replacement   : constant String :=
                            Project.Attribute_Value
                              (Dot_Replacement_Attribute,
                               Default => Default_Gnat_Dot_Replacement);
      Casing            : constant String :=
                            Project.Attribute_Value
                              (Casing_Attribute,
                               Default => "lowercase");
      Spec_Suffix       : constant String :=
                            Project.Attribute_Value
                              (Spec_Suffix_Attribute,
                               Index   => "ada",
                               Default => Default_Gnat_Spec_Suffix);
      Body_Suffix       : constant String :=
                            Project.Attribute_Value
                              (Impl_Suffix_Attribute,
                               Index   => "ada",
                               Default => Default_Gnat_Body_Suffix);
      Cased_Name        : constant String :=
                            (if Casing = "lowercase" then
                                To_Lower (Name)
                             elsif Casing = "uppercase" then
                                To_Upper (Name)
                             else
                                Name);
      Dot_Replaced_Name : constant String :=
                            GNATCOLL.Utils.Replace
                              (Cased_Name,
                               Pattern     => ".",
                               Replacement => Dot_Replacement);
      Files_To_Open     : Virtual_Files_Lists.List :=
                            Virtual_Files_Lists.Empty_List;

      procedure Create_Unit_File
        (Suffix   : String;
         Template : String);
      --  Create a unit file from Suffix and write the expanded Template in it

      ----------------------
      -- Create_Unit_File --
      ----------------------

      procedure Create_Unit_File
        (Suffix   : String;
         Template : String)
      is
         File_Name : constant String := Dot_Replaced_Name & Suffix;
         File      : Virtual_File;
         W_File    : GNATCOLL.VFS.Writable_File;
      begin
         File := Create_From_Dir (Dir, +File_Name);
         W_File := GNATCOLL.VFS.Write_File (File);

         if W_File = Invalid_File then
            raise VFS_Invalid_File_Error
              with "Cannot create file " & File.Display_Full_Name;
         end if;

         GNATCOLL.VFS.Write
           (W_File,
            GNATCOLL.Utils.Replace (S           => Template,
                                    Pattern     => "<name>",
                                    Replacement => Name));
         GNATCOLL.VFS.Close (W_File);

         File_Saved_Hook.Run (Kernel, File);

         Files_To_Open.Append (File);
      end Create_Unit_File;

   begin
      case Kind is
         when Package_Spec_And_Body =>
            Create_Unit_File (Suffix   => Body_Suffix,
                              Template => Package_Body_Template);
            Create_Unit_File (Suffix   => Spec_Suffix,
                              Template => Package_Spec_Template);

         when Package_Spec =>
            Create_Unit_File (Suffix   => Spec_Suffix,
                              Template => Package_Spec_Template);

         when Main_Unit =>
            Create_Unit_File (Suffix   => Body_Suffix,
                              Template => Main_Unit_Template);
      end case;

      if Project /= No_Project then
         Recompute_View (Kernel);
      end if;

      for File of Files_To_Open loop
         Open_File_Action_Hook.Run
           (Kernel,
            File    => File,
            Project => Project);
      end loop;

   exception
      when E : VFS_Invalid_File_Error =>
         Kernel.Insert
           (Exception_Message (E),
            Mode => Error);
   end Create_Files_For_Component;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Create_Component_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel       : constant Kernel_Handle := Get_Kernel (Context.Context);
      Dir          : constant Virtual_File :=
                       Directory_Information (Context.Context);
      Project      : constant Project_Type :=
                       Project_Information (Context.Context);
      Name         : constant String := "Create New Ada Package";
      Dialog       : Gtk_Dialog;
      Main_View    : Dialog_View;
      Group_Widget : Dialog_Group_Widget;
      Button       : Gtk_Widget;
      Ent          : Gtk_Entry;
      Combo        : Gtk_Combo_Box_Text;
   begin
      Gtk.Dialog.Gtk_New
        (Dialog,
         Title  => Name,
         Parent => Get_Main_Window (Kernel),
         Flags  => Modal or Destroy_With_Parent);
      Set_Default_Size_From_History
        (Dialog,
         Name   => Name,
         Kernel => Kernel,
         Width  => 300,
         Height => 100);

      Button := Dialog.Add_Button (Stock_Ok, Gtk_Response_OK);
      Button.Grab_Default;
      Button := Dialog.Add_Button (Stock_Cancel, Gtk_Response_Cancel);

      Main_View := new Dialog_View_Record;
      Dialog_Utils.Initialize (Main_View);
      Dialog.Get_Content_Area.Pack_Start (Main_View);

      Group_Widget := new Dialog_Group_Widget_Record;
      Initialize
        (Group_Widget,
         Parent_View => Main_View,
         Group_Name  => "General");

      Gtk_New (Ent);
      Ent.Set_Activates_Default (True);
      Group_Widget.Create_Child
        (Ent,
         Label  => "Name",
         Doc    => "The name of the Ada component.",
         Expand => True);

      Gtk_New (Combo);

      for Kind in Ada_Component_Kind'Range loop
         Combo.Append_Text (Get_Label (Kind));
      end loop;

      --  Preselect the component kind from the command itself
      Combo.Set_Active (Ada_Component_Kind'Pos (Command.Kind));

      Group_Widget.Create_Child
        (Combo,
         Label  => "Kind",
         Doc    => "The kind of Ada component to add.",
         Expand => True);

      Dialog.Show_All;
      Ent.Grab_Focus;

      case Dialog.Run is
         when Gtk_Response_OK =>
            Create_Files_For_Component
              (Kernel,
               Kind    => Ada_Component_Kind'Val (Combo.Get_Active),
               Name    => Ent.Get_Text,
               Dir     => Dir,
               Project => Project);
         when others =>
            null;
      end case;

      Dialog.Destroy;

      return Commands.Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      File_View_Filter : constant Action_Filter :=
                           Lookup_Filter (Kernel, "File_View")
                             or Create (Module => Explorer_Module_Name);
      Ada_Filter       : constant Action_Filter :=
                           Create (Language => "Ada");
      Dir_Filter       : constant Action_Filter :=
                           Lookup_Filter (Kernel, "Dir_Filter");
      Command          : Interactive_Command_Access;
   begin
      --  Create the 'New/Ada Package' contextual menu

      Command := new Create_Component_Command'(Root_Command with
                                             Kind => Package_Spec_And_Body);
      Register_Action
        (Kernel,
         Name        => "create new ada package",
         Command     => Command,
         Description => "Create a new Ada package in the selected directory",
         Filter      => File_View_Filter and Dir_Filter and Ada_Filter);
      Register_Contextual_Menu
        (Kernel,
         Action => "create new ada package",
         Label  => "New/Ada Package",
         Group  => Ada_Creators_Contextual_Group);

      --  Create the 'New/Ada Main Unit' contextual menu

      Command := new Create_Component_Command'(Root_Command with
                                               Kind => Main_Unit);
      Register_Action
        (Kernel,
         Name        => "create new ada main unit",
         Command     => Command,
         Description => "Create a new Ada main unir in the selected directory",
         Filter      => File_View_Filter and Dir_Filter and Ada_Filter);
      Register_Contextual_Menu
        (Kernel,
         Action => "create new ada main unit",
         Label  => "New/Ada Main Unit",
         Group  => Ada_Creators_Contextual_Group);
   end Register_Module;

end Ada_Module.Creators;
