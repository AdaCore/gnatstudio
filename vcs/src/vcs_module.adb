-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
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
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Xml_Int;              use Glib.Xml_Int;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Event_Box;             use Gtk.Event_Box;
with Gtk.GEntry;                use Gtk.GEntry;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Label;                 use Gtk.Label;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Radio_Button;          use Gtk.Radio_Button;
with Gtk.Table;                 use Gtk.Table;
with Gtk.Tooltips;              use Gtk.Tooltips;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.MDI;                use Gtkada.MDI;

with Glide_Kernel.Contexts;       use Glide_Kernel.Contexts;
with Glide_Kernel.Console;        use Glide_Kernel.Console;
with Glide_Kernel.Hooks;          use Glide_Kernel.Hooks;
with Glide_Kernel.Modules;        use Glide_Kernel.Modules;
with Glide_Kernel.Project;        use Glide_Kernel.Project;
with Glide_Kernel.Scripts;        use Glide_Kernel.Scripts;
with Glide_Kernel.Standard_Hooks; use Glide_Kernel.Standard_Hooks;
with Glide_Intl;                  use Glide_Intl;

with Traces;                    use Traces;

with VCS;                       use VCS;
with VCS_View_API;              use VCS_View_API;
with VCS_View_Pkg;              use VCS_View_Pkg;
with Basic_Types;               use Basic_Types;

with VCS.Unknown_VCS;           use VCS.Unknown_VCS;
with VCS.Generic_VCS;           use VCS.Generic_VCS;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Projects;                  use Projects;
with Projects.Editor;           use Projects.Editor;
with Projects.Registry;         use Projects.Registry;
with VFS;                       use VFS;
with Project_Viewers;           use Project_Viewers;

with String_List_Utils;
with Log_Utils;

package body VCS_Module is

   Me : constant Debug_Handle := Create (VCS_Module_Name);

   Auto_Detect  : constant String := "None";

   type VCS_Module_ID_Record is new Module_ID_Record with record
      VCS_List : Argument_List_Access;
      --  The list of all VCS systems recognized by the kernel.
   end record;
   type VCS_Module_ID_Access is access all VCS_Module_ID_Record'Class;

   procedure Destroy (Module : in out VCS_Module_ID_Record);
   --  Free the memory occupied by Module.

   type VCS_Selector_Record is new Gtk_Box_Record with record
      Selected : Gtk_Radio_Button;
      Log_Checker : Gtk.GEntry.Gtk_Entry;
      File_Checker : Gtk.GEntry.Gtk_Entry;
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

   procedure File_Edited_Cb
     (Kernel  : access Kernel_Handle_Record'Class;
      Data    : Hooks_Data'Class);
   --  Callback for the "file_edited" signal.

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child;
   --  Restore the status of the explorer from a saved XML tree.

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
      return Node_Ptr;
   --  Save the status of the project explorer to an XML tree

   type VCS_Editor_Record is new Project_Editor_Page_Record
     with null record;
   function Widget_Factory
     (Page         : access VCS_Editor_Record;
      Project      : Project_Type;
      Full_Project : String;
      Kernel       : access Kernel_Handle_Record'Class)
      return Gtk_Widget;
   function Project_Editor
     (Page         : access VCS_Editor_Record;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project  : Project_Type)
      return Boolean;

   procedure Status_Parse_Handler
     (Data    : in out Glide_Kernel.Scripts.Callback_Data'Class;
      Command : String);
   --  Handler for the command "vcs_status_parse".

   procedure Annotations_Parse_Handler
     (Data    : in out Glide_Kernel.Scripts.Callback_Data'Class;
      Command : String);
   --  Handler for the command "VCS.annotations_parse".

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
      Submenu   : Gtk_Menu;
      Menu_Item : Gtk_Menu_Item;

      use type VCS.VCS_Access;

   begin
      if Context.all in File_Selection_Context'Class
        and then Get_Current_Ref (Selection_Context_Access (Context)) /=
          Unknown_VCS_Reference
      then
         --  ??? This is wrong, since it will create a submenu even if it is
         --  empty (see in the type browser for instance)
         Gtk_New (Menu_Item, Label => -"Version Control");
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
           (1 => new String'(VCS_Identifier));
      else
         Old := M.VCS_List;
         M.VCS_List := new Argument_List (1 .. M.VCS_List'Length + 1);
         M.VCS_List (Old'Range) := Old.all;
         M.VCS_List (M.VCS_List'Last) := new String'(VCS_Identifier);
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
      R : constant Gtk_Radio_Button := Gtk_Radio_Button (Radio);
   begin
      if Get_Active (R) then
         VCS_Selector (Selector).Selected := R;
      end if;
   end Toggled;

   --------------------
   -- Widget_Factory --
   --------------------

   function Widget_Factory
     (Page         : access VCS_Editor_Record;
      Project      : Project_Type;
      Full_Project : String;
      Kernel       : access Kernel_Handle_Record'Class)
      return Gtk_Widget
   is
      pragma Unreferenced (Page, Full_Project);
      Systems : constant Argument_List := Get_VCS_List (VCS_Module_ID);
      Radio   : Gtk_Radio_Button;
      Main    : VCS_Selector;
      Frame   : Gtk_Frame;
      Box     : Gtk_Box;
      Table   : Gtk_Table;
      Label   : Gtk_Label;
      Event   : Gtk_Event_Box;

   begin
      Main := new VCS_Selector_Record;
      Initialize_Vbox (Main, Homogeneous => False);

      --  System frame

      Gtk_New (Frame, -"System");
      Set_Border_Width (Frame, 5);
      Pack_Start (Main, Frame, Expand => False);

      Gtk_New_Vbox (Box, Homogeneous => True);
      Add (Frame, Box);

      for S in Systems'Range loop
         if Systems (S).all = "" then
            Gtk_New (Radio, Group => Radio, Label => -Auto_Detect);
         else
            Gtk_New (Radio, Group => Radio, Label => Systems (S).all);
         end if;

         if Project /= No_Project
           and then To_Lower (Systems (S).all) = To_Lower
             (Get_Attribute_Value (Project, Vcs_Kind_Attribute))
         then
            Set_Active (Radio, True);
            Main.Selected := Radio;
         elsif S = Systems'First then
            Main.Selected := Radio;
            Set_Active (Radio, True);
         else
            Set_Active (Radio, False);
         end if;
         Pack_Start (Box, Radio);

         Object_User_Callback.Connect
           (Radio, "toggled",
            Object_User_Callback.To_Marshaller (Toggled'Access),
            User_Data => GObject (Main));
      end loop;

      --  Actions frame

      Gtk_New (Frame, -"Actions");
      Set_Border_Width (Frame, 5);
      Pack_Start (Main, Frame, Expand => False);

      Gtk_New (Table, Rows => 2, Columns => 2, Homogeneous => False);
      Add (Frame, Table);

      Gtk_New (Event);
      Attach (Table, Event, 0, 1, 0, 1, Xoptions => Fill);
      Gtk_New (Label, -"Log checker:");
      Add (Event, Label);
      Set_Alignment (Label, 0.0, 0.5);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -("Application run on the log file/revision history just before"
                 & " commiting a file. If it returns anything other than 0,"
                 & " the commit will not be performed. The only parameter to"
                 & " this script is the name of a log file"));

      Gtk_New (Main.Log_Checker);
      Attach (Table, Main.Log_Checker, 1, 2, 0, 1);

      Gtk_New (Event);
      Attach (Table, Event, 0, 1, 1, 2, Xoptions => Fill);
      Gtk_New (Label, -"File checker:");
      Add (Event, Label);
      Set_Alignment (Label, 0.0, 0.5);
      Set_Tip (Get_Tooltips (Kernel), Event,
               -("Application run on the source file just before"
                 & " commiting a file. If it returns anything other than 0,"
                 & " the commit will not be performed. The only parameter to"
                 & " this script is a file name"));

      Gtk_New (Main.File_Checker);
      Attach (Table, Main.File_Checker, 1, 2, 1, 2);

      if Project /= No_Project then
         Set_Text
           (Main.Log_Checker,
            Get_Attribute_Value (Project, Vcs_Log_Check));
         Set_Text
           (Main.File_Checker,
            Get_Attribute_Value (Project, Vcs_File_Check));
      end if;

      Show_All (Main);
      return Gtk_Widget (Main);
   end Widget_Factory;

   --------------------
   -- Project_Editor --
   --------------------

   function Project_Editor
     (Page         : access VCS_Editor_Record;
      Project      : Project_Type;
      Kernel       : access Kernel_Handle_Record'Class;
      Widget       : access Gtk_Widget_Record'Class;
      Scenario_Variables : Scenario_Variable_Array;
      Ref_Project  : Project_Type)
      return Boolean
   is
      pragma Unreferenced (Page, Kernel, Ref_Project);
      Selector : constant VCS_Selector := VCS_Selector (Widget);
      Changed  : Boolean := False;
      VCS_Kind : constant String := To_Lower (Get_Attribute_Value
        (Project, Vcs_Kind_Attribute));

   begin
      if (Project = No_Project
          and then Get_Label (Selector.Selected) /= -Auto_Detect)
        or else
          (Project /= No_Project
           and then To_Lower (Get_Label (Selector.Selected)) /= VCS_Kind
           and then (VCS_Kind /= ""
                     or else Get_Label (Selector.Selected) /= -Auto_Detect))
      then
         if Get_Label (Selector.Selected) /= -Auto_Detect then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Vcs_Kind_Attribute,
               Value              => Get_Label (Selector.Selected));
         else
            Delete_Attribute
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Vcs_Kind_Attribute);
         end if;

         Trace (Me, "Vcs_Kind is different");
         Changed := True;
      end if;

      if Project = No_Project
        or else Get_Text (Selector.Log_Checker) /=
        Get_Attribute_Value (Project, Vcs_Log_Check)
      then
         if Get_Text (Selector.Log_Checker) /= "" then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Vcs_Log_Check,
               Value              => Get_Text (Selector.Log_Checker));
         else
            Delete_Attribute
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Vcs_Log_Check);
         end if;
         Trace (Me, "Vcs_Log_Check is different");
         Changed := True;
      end if;

      if Project = No_Project
        or else Get_Text (Selector.File_Checker) /= Get_Attribute_Value
          (Project, Vcs_File_Check)
      then
         if Get_Text (Selector.File_Checker) /= "" then
            Update_Attribute_Value_In_Scenario
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Vcs_File_Check,
               Value              => Get_Text (Selector.File_Checker));
         else
            Delete_Attribute
              (Project            => Project,
               Scenario_Variables => Scenario_Variables,
               Attribute          => Vcs_File_Check);
         end if;
         Trace (Me, "Vcs_File_Check is different");
         Changed := True;
      end if;

      return Changed;
   end Project_Editor;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Module : in out VCS_Module_ID_Record) is
   begin
      Free (Module.VCS_List);
   end Destroy;

   ------------------
   -- Load_Desktop --
   ------------------

   function Load_Desktop
     (MDI  : MDI_Window;
      Node : Node_Ptr;
      User : Kernel_Handle) return MDI_Child
   is
      pragma Unreferenced (MDI);
   begin
      if Node.Tag.all = "VCS_View_Record" then
         return Open_Explorer (User, Context => null);
      end if;

      return null;
   end Load_Desktop;

   ------------------
   -- Save_Desktop --
   ------------------

   function Save_Desktop
     (Widget : access Gtk.Widget.Gtk_Widget_Record'Class)
     return Node_Ptr
   is
      N : Node_Ptr;
   begin
      if Widget.all in VCS_View_Record'Class then
         N := new Node;
         N.Tag := new String'("VCS_View_Record");
         return N;
      end if;

      return null;
   end Save_Desktop;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Menu_Item : Gtk_Menu_Item;

      VCS_Root    : constant String := -"VCS";
      VCS         : constant String := '/' & VCS_Root;
      VCS_Dir     : constant String := VCS & "/_" & (-"Directory");

      VCS_Class : constant Class_Type := New_Class
        (Kernel, "VCS", -"General interface to version control systems");

   begin
      VCS_Module_ID := new VCS_Module_ID_Record;
      Register_Module
        (Module                  => VCS_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => VCS_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => VCS_Contextual_Menu'Access,
         Default_Context_Factory => VCS_View_API.Context_Factory'Access);
      Glide_Kernel.Kernel_Desktop.Register_Desktop_Functions
        (Save_Desktop'Access, Load_Desktop'Access);

      Register_Menu
        (Kernel,
         "/_" & VCS_Root,
         Ref_Item => -"Navigate",
         Add_Before => False);

      Register_Menu (Kernel, VCS, -"_Explorer", "", On_Open_Interface'Access);
      Register_Menu (Kernel, VCS, -"Update all _projects", "",
                     Update_All'Access);
      Register_Menu
        (Kernel, VCS, -"_Query status for all projects", "",
         Query_Status_For_Project'Access);

      Register_Menu
        (Kernel, VCS_Dir, -"_Query status for directory", "",
         Query_Status_For_Directory'Access);
      Register_Menu
        (Kernel, VCS_Dir, -"Query _status for directory recursively", "",
         Query_Status_For_Directory_Recursive'Access);
      Register_Menu
        (Kernel, VCS_Dir, -"_Update directory", "",
         Update_Directory'Access);
      Register_Menu
        (Kernel, VCS_Dir, -"Update _directory recursively", "",
         Update_Directory_Recursive'Access);

      Gtk_New (Menu_Item);
      Register_Menu (Kernel, VCS, Menu_Item);
      Register_Menu (Kernel, VCS, -"_Update", "", Update'Access);
      Register_Menu (Kernel, VCS, -"_Commit", "", Commit'Access);
      Register_Menu (Kernel, VCS, -"Start _Editing", "", Open'Access);
      Register_Menu (Kernel, VCS, -"_View revision history", "",
                     View_Log'Access);

      Gtk_New (Menu_Item);
      Register_Menu (Kernel, VCS, Menu_Item);
      Register_Menu (Kernel, VCS, -"Compare against _head rev.", "",
                     View_Head_Diff'Access);
      Register_Menu (Kernel, VCS, -"Compare against _working rev.", "",
                     View_Work_Diff'Access);
      Register_Menu (Kernel, VCS,
                     -"Compare working _against head rev.", "",
                     View_Work_Head_Diff'Access);
      Register_Menu (Kernel, VCS,
                     -"Compare against _revision...", "",
                     View_Specific_Diff'Access);
      Gtk_New (Menu_Item);

      Register_Menu (Kernel, VCS, Menu_Item);

      Register_Menu (Kernel, VCS, -"_Annotate", "", View_Annotate'Access);
      Register_Menu (Kernel, VCS, -"Remove a_nnotations", "",
                     Remove_Annotations'Access);
      Register_Menu (Kernel, VCS, -"Edit revision _log", "", Edit_Log'Access);
      Register_Menu (Kernel, VCS,
                     -"Edit global ChangeLog", "", Edit_ChangeLog'Access);
      Register_Menu (Kernel, VCS, -"_Revert", "", Revert'Access);
      Gtk_New (Menu_Item);
      Register_Menu (Kernel, VCS, Menu_Item);
      Register_Menu (Kernel, VCS, -"A_dd to repository", "", Add'Access);
      Register_Menu
        (Kernel, VCS, -"R_emove from repository", "", Remove'Access);

      Log_Utils.Initialize (Kernel);

      Register_Project_Editor_Page
        (Kernel,
         Page      => new VCS_Editor_Record,
         Label     => -"VCS",
         Toc       => -"Select VCS",
         Title     => -"Version Control System Configuration",
         Ref_Page  => -"Sources",
         Add_After => False);

      Standard.VCS.Unknown_VCS.Register_Module (Kernel);
      Standard.VCS.Generic_VCS.Register_Module (Kernel);

      Add_Hook (Kernel, File_Edited_Hook, File_Edited_Cb'Access);

      --  Register VCS commands.
      --  ??? We should create a Class for VCS.

      Register_Command
        (Kernel       => Kernel,
         Command      => "get_status",
         Params       => "(file)",
         Description  => -"Query the status for file",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => VCS_Command_Handler'Access);

      Register_Command
        (Kernel       => Kernel,
         Command      => "update",
         Params       => "(file)",
         Description  => -"Update file",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => VCS_Command_Handler'Access);

      Register_Command
        (Kernel       => Kernel,
         Command      => "commit",
         Params       => "(file)",
         Description  => -"Commit file",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler       => VCS_Command_Handler'Access);

      Register_Command
        (Kernel       => Kernel,
         Command      => "diff_head",
         Params       => "(file)",
         Description  =>
           -"Show differences between local file and the head revision",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => VCS_Command_Handler'Access);

      Register_Command
        (Kernel       => Kernel,
         Command      => "diff_working",
         Params       => "(file)",
         Description  =>
           -"Show differences between local file and the working revision",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => VCS_Command_Handler'Access);

      Register_Command
        (Kernel       => Kernel,
         Command      => "annotate",
         Params       => "(file)",
         Description  => -"Display the annotations for file",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => VCS_Command_Handler'Access);

      Register_Command
        (Kernel       => Kernel,
         Command      => "remove_annotations",
         Params       => "(file)",
         Description  => -"Remove the annotations for file",
         Minimum_Args => 1,
         Maximum_Args => 1,
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => VCS_Command_Handler'Access);

      Register_Command
        (Kernel       => Kernel,
         Command      => "log",
         Params       => "(file, [revision])",
         Description  =>
           -("Get the revision changelog for file."
             & " If revision is specified, query the changelog for this "
             & "specific revision, otherwise query the entire changelog"),
         Minimum_Args => 1,
         Maximum_Args => 2,
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => VCS_Command_Handler'Access);

      Register_Command
        (Kernel       => Kernel,
         Command      => "status_parse",
         Params       => "(vcs_identifier, string, clear_logs, local)",
         Description  =>
         -("Parses string for vcs status."
            & " See the GPS documentation for detailed usage description."),
         Minimum_Args => 4,
         Maximum_Args => 4,
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => Status_Parse_Handler'Access);

      Register_Command
        (Kernel       => Kernel,
         Command      => "annotations_parse",
         Params       => "(vcs_identifier, file, string)",
         Description  =>
         -("Parses string for vcs annotations."
            & " See the GPS documentation for detailed usage description."),
         Minimum_Args => 3,
         Maximum_Args => 3,
         Class         => VCS_Class,
         Static_Method => True,
         Handler      => Annotations_Parse_Handler'Access);
   end Register_Module;

   --------------------------
   -- Status_Parse_Handler --
   --------------------------

   procedure Status_Parse_Handler
     (Data    : in out Glide_Kernel.Scripts.Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);

      Kernel : constant Kernel_Handle := Get_Kernel (Data);

      Ref    : VCS_Access;

      VCS_Identifier : constant String := Nth_Arg (Data, 1);
      S              : constant String := Nth_Arg (Data, 2);

      Clear_Logs     : constant Boolean := Nth_Arg (Data, 3);
      Local          : constant Boolean := Nth_Arg (Data, 4);

      Status : File_Status_List.List;
   begin
      Ref := Get_VCS_From_Id (VCS_Identifier);

      if Ref = null then
         Insert (Kernel,
                 -"Could not find registered VCS corresponding to identifier: "
                 & VCS_Identifier);
         return;
      end if;

--        Open_Explorer (Kernel, null);

      Status := Parse_Status (Ref, S, Local);
      Display_File_Status (Kernel, Status, Ref, True, True, Clear_Logs);

      --  ??? Should we free Status ?
   end Status_Parse_Handler;

   -------------------------------
   -- Annotations_Parse_Handler --
   -------------------------------

   procedure Annotations_Parse_Handler
     (Data    : in out Glide_Kernel.Scripts.Callback_Data'Class;
      Command : String)
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      Ref    : VCS_Access;
      VCS_Identifier : constant String := Nth_Arg (Data, 1);
      File           : constant VFS.Virtual_File :=
        Create (Nth_Arg (Data, 2));
      S              : constant String := Nth_Arg (Data, 3);
   begin
      Ref := Get_VCS_From_Id (VCS_Identifier);

      if Ref = null then
         Insert (Kernel,
                 -"Could not find registered VCS corresponding to identifier: "
                 & VCS_Identifier);
         return;
      end if;

      Parse_Annotations (Ref, File, S);
   end Annotations_Parse_Handler;

   --------------------
   -- File_Edited_Cb --
   --------------------

   procedure File_Edited_Cb
     (Kernel  : access Kernel_Handle_Record'Class;
      Data    : Hooks_Data'Class)
   is
      use String_List_Utils.String_List;
      D : constant File_Hooks_Args := File_Hooks_Args (Data);
      Files  : List;
      Ref    : VCS_Access;

   begin
      Append (Files, Full_Name (D.File).all);
      Ref    := Get_Current_Ref
        (Get_Project_From_File (Get_Registry (Kernel), D.File, True));

      --  ??? We could try to retrieve the status from the VCS Explorer cache.

      Get_Status (Ref, Files, False, Local => True);
      Free (Files);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end File_Edited_Cb;

end VCS_Module;
