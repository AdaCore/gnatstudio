-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Radio_Button;          use Gtk.Radio_Button;
with Gtk.Table;                 use Gtk.Table;
with Gtk.Widget;                use Gtk.Widget;

with Glide_Kernel.Project;      use Glide_Kernel.Project;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Intl;                use Glide_Intl;

with Traces;                    use Traces;

with VCS_View_API;              use VCS_View_API;
with VCS_View_Pkg;              use VCS_View_Pkg;
with Basic_Types;               use Basic_Types;

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Prj;                       use Prj;
with Prj.Tree;                  use Prj.Tree;
with Prj_API;                   use Prj_API;

with Log_Utils;

package body VCS_Module is

   VCS_Module_Name : constant String := "VCS_Interface";
   Me : constant Debug_Handle := Create (VCS_Module_Name);

   Auto_Detect : constant String := "None";

   type VCS_Module_ID_Record is new Module_ID_Record with record
      VCS_List : Argument_List_Access;
      --  The list of all VCS systems recognized by the kernel.
   end record;
   type VCS_Module_ID_Access is access all VCS_Module_ID_Record'Class;

   procedure Destroy (Module : in out VCS_Module_ID_Record);
   --  Free the memory occupied by Module.

   type VCS_Selector_Record is new Gtk_Table_Record with record
      Selected : Gtk_Radio_Button;
   end record;
   type VCS_Selector is access all VCS_Selector_Record'Class;


   procedure VCS_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Fill Menu with the contextual menu for the VCS module,
   --  if Context is appropriate.

   procedure On_Open_Interface
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle);
   --  Display the VCS explorer

   procedure Toggled
     (Radio    : access Glib.Object.GObject_Record'Class;
      Selector : Glib.Object.GObject);
   --  Called when a new VCS has been selector in the project creation wizard
   --  or the project properties editor.

   type VCS_Editor_Record is new Project_Editor_Page_Record
     with null record;
   function Widget_Factory
     (Page         : access VCS_Editor_Record;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class)
      return Gtk_Widget;
   function Project_Editor
     (Page         : access VCS_Editor_Record;
      Project      : Project_Node_Id;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class)
      return Boolean;

   -----------------------
   -- On_Open_Interface --
   -----------------------

   procedure On_Open_Interface
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
   begin
      Open_Explorer (Kernel, null);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Open_Interface;

   -------------------------
   -- VCS_Contextual_Menu --
   -------------------------

   procedure VCS_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Submenu      : Gtk_Menu;
      Menu_Item    : Gtk_Menu_Item;
   begin
      if Context.all in File_Selection_Context'Class then
         Gtk_New (Menu_Item, Label => -"VCS");
         Gtk_New (Submenu);
         VCS_View_API.VCS_Contextual_Menu (Object, Context, Submenu);
         Set_Submenu (Menu_Item, Gtk_Widget (Submenu));
         Append (Menu, Menu_Item);
      end if;
   end VCS_Contextual_Menu;

   ------------------
   -- Get_VCS_List --
   ------------------

   function Get_VCS_List
     (Module : Module_ID) return Argument_List is
   begin
      return VCS_Module_ID_Access (Module).VCS_List.all;
   end Get_VCS_List;

   ------------------
   -- Register_VCS --
   ------------------

   procedure Register_VCS (Module : Module_ID; VCS_Identifier : String) is
      M   : VCS_Module_ID_Access := VCS_Module_ID_Access (Module);
      Old : Argument_List_Access;
   begin
      if M.VCS_List = null then
         M.VCS_List := new Argument_List'
           (1 => new String' (VCS_Identifier));
      else
         Old := M.VCS_List;
         M.VCS_List := new Argument_List (1 .. M.VCS_List'Length + 1);
         M.VCS_List (Old'Range) := Old.all;
         M.VCS_List (M.VCS_List'Last) := new String' (VCS_Identifier);
         Basic_Types.Unchecked_Free (Old);
      end if;
   end Register_VCS;

   -------------
   -- Toggled --
   -------------

   procedure Toggled
     (Radio    : access Glib.Object.GObject_Record'Class;
      Selector : Glib.Object.GObject)
   is
      R : Gtk_Radio_Button := Gtk_Radio_Button (Radio);
   begin
      if Get_Active (R) then
         VCS_Selector (Selector).Selected := R;
      end if;
   end Toggled;

   --------------------
   -- Widget_Factory --
   --------------------

   function Widget_Factory
     (Page : access VCS_Editor_Record;
      Project_View : Project_Id; Kernel : access Kernel_Handle_Record'Class)
      return Gtk_Widget
   is
      pragma Unreferenced (Page, Kernel);
      Systems : Argument_List := Get_VCS_List (VCS_Module_ID);
      Radio : Gtk_Radio_Button;
      Current : constant String := Get_Vcs_Kind (Project_View);
      Table : VCS_Selector;
   begin
      Table := new VCS_Selector_Record;
      Initialize
        (Table, Rows => Systems'Length + 1, Columns => 1, Homogeneous => True);

      for S in Systems'Range loop
         if Systems (S).all = "" then
            Gtk_New (Radio, Group => Radio, Label => -Auto_Detect);
         else
            Gtk_New (Radio, Group => Radio, Label => Systems (S).all);
         end if;
         if To_Lower (Systems (S).all) = To_Lower (Current) then
            Set_Active (Radio, True);
            Table.Selected := Radio;
         end if;
         Attach (Table, Radio, 0, 1,
                 Guint (S - Systems'First),
                 Guint (S - Systems'First + 1),
                 Xoptions => Fill, Yoptions => 0);

         Object_User_Callback.Connect
           (Radio, "toggled",
            Object_User_Callback.To_Marshaller (Toggled'Access),
            User_Data => GObject (Table));
      end loop;

      return Gtk_Widget (Table);
   end Widget_Factory;

   --------------------
   -- Project_Editor --
   --------------------

   function Project_Editor
     (Page         : access VCS_Editor_Record;
      Project      : Project_Node_Id;
      Project_View : Project_Id;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class)
      return Boolean
   is
      pragma Unreferenced (Page);
      Selector : VCS_Selector := VCS_Selector (Widget);
   begin
      if Selector.Selected /= null then
         if Project_View = No_Project then
            if Get_Label (Selector.Selected) /= -Auto_Detect then
               Update_Attribute_Value_In_Scenario
                 (Project            => Project,
                  Pkg_Name           => Ide_Package,
                  Scenario_Variables => (1 .. 0 => Empty_Node),
                  Attribute_Name     => Vcs_Kind_Attribute,
                  Value              => Get_Label (Selector.Selected));
            end if;
            return True;

         elsif Get_Label (Selector.Selected) /=
           Get_Vcs_Kind (Project_View)
         then
            if Get_Label (Selector.Selected) /= -Auto_Detect then
               Update_Attribute_Value_In_Scenario
                 (Project           => Project,
                  Pkg_Name          => Ide_Package,
                  Scenario_Variables =>
                    Scenario_Variables (Kernel),
                  Attribute_Name    => Vcs_Kind_Attribute,
                  Value             => Get_Label (Selector.Selected));

            else
               Delete_Attribute
                 (Project           => Project,
                  Pkg_Name          => Ide_Package,
                  Scenario_Variables =>
                    Scenario_Variables (Kernel),
                  Attribute_Name    => Vcs_Kind_Attribute);
            end if;
            return True;
         end if;
      end if;
      return False;
   end Project_Editor;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out VCS_Module_ID_Record) is
   begin
      Free (Module.VCS_List);
   end Destroy;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Menu_Item : Gtk_Menu_Item;
      VCS_Root  : constant String := -"VCS";
      VCS       : constant String := '/' & VCS_Root;

   begin
      VCS_Module_ID := new VCS_Module_ID_Record;
      Register_Module
        (Module                  => VCS_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => VCS_Module_Name,
         Priority                => Default_Priority,
         MDI_Child_Tag           => VCS_View_Record'Tag,
         Contextual_Menu_Handler => VCS_Contextual_Menu'Access,
         Default_Context_Factory => VCS_View_API.Context_Factory'Access);

      Register_Menu
        (Kernel,
         "/_" & VCS_Root,
         Ref_Item => -"Navigate",
         Add_Before => False);

      Register_Menu (Kernel, VCS, -"Explorer", "", On_Open_Interface'Access);
      Register_Menu (Kernel, VCS, -"Update project", "", Update_All'Access);
      Register_Menu
        (Kernel, VCS, -"Query status for project", "",
         Query_Status_For_Project'Access);
      Gtk_New (Menu_Item);
      Register_Menu (Kernel, VCS, Menu_Item);
      Register_Menu (Kernel, VCS, -"Update", "", Update'Access);
      Register_Menu (Kernel, VCS, -"Start Editing", "", Open'Access);
      Register_Menu (Kernel, VCS, -"Compare against head", "",
                     View_Diff'Access);
      Register_Menu (Kernel, VCS, -"Edit log", "", Edit_Log'Access);
      Register_Menu (Kernel, VCS, -"Commit", "", Commit'Access);
      Register_Menu (Kernel, VCS, -"Annotate", "", View_Annotate'Access);
      Register_Menu (Kernel, VCS, -"View Changelog", "", View_Log'Access);
      Register_Menu (Kernel, VCS, -"Revert", "", Revert'Access);
      Register_Menu (Kernel, VCS, -"Add to repository", "", Add'Access);
      Register_Menu
        (Kernel, VCS, -"Remove from repository", "", Remove'Access);

      Log_Utils.Initialize (Kernel);

      Register_Project_Editor_Page
        (Kernel,
         Page  => new VCS_Editor_Record,
         Label => -"VCS",
         Toc   => -"Select VCS",
         Title => -"Where are the sources ?");
   end Register_Module;

end VCS_Module;
