------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2023, AdaCore                   --
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

with Ada.Strings.Unbounded;         use Ada.Strings.Unbounded;
with GNATCOLL.JSON;
with GNATCOLL.Traces;               use GNATCOLL.Traces;
with GNATCOLL.VFS;                  use GNATCOLL.VFS;

with GPS.Kernel.Preferences;
with VSS.Strings.Conversions;

with Glib;                          use Glib;
with Glib.Convert;                  use Glib.Convert;
with Gtk.Box;                       use Gtk.Box;
with Gtk.Check_Button;              use Gtk.Check_Button;
with Gtk.Dialog;                    use Gtk.Dialog;
with Gtk.GEntry;                    use Gtk.GEntry;
with Gtk.Widget;                    use Gtk.Widget;
with Gtkada.Stock_Labels;           use Gtkada.Stock_Labels;

with Dialog_Utils;                  use Dialog_Utils;
with GPS.Dialogs;                   use GPS.Dialogs;
with GPS.Editors;                   use GPS.Editors;
with GPS.Kernel.Actions;            use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;           use GPS.Kernel.Contexts;
with GPS.Kernel.Modules.UI;         use GPS.Kernel.Modules.UI;
with GPS.Main_Window;               use GPS.Main_Window;

with Basic_Types;
with Commands;                      use Commands;
with Commands.Interactive;          use Commands.Interactive;
with Histories;                     use Histories;
with Language;

with Refactoring.Rename;
with Src_Editor_Module.Shell;

with GPS.LSP_Module;
with GPS.LSP_Client.Edit_Workspace;
with GPS.LSP_Client.Requests.Rename;
with GPS.LSP_Client.Configurations;
with GPS.LSP_Client.Utilities;
with LSP.Messages;

package body GPS.LSP_Client.Refactoring.Rename is

   Me : constant Trace_Handle := Create ("GPS.REFACTORING.LSP_RENAME");

   Refactoring_Module : GPS.Kernel.Modules.Module_ID;

   type Rename_Entity_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Rename_Entity_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called for "Rename Entity" menu

   -- Rename_Request --

   type Rename_Request is
     new GPS.LSP_Client.Requests.Rename.Abstract_Rename_Request with
      record
         Old_Name            : VSS.Strings.Virtual_String;
         Make_Writable       : Boolean;
         Auto_Save           : Boolean;
         Allow_File_Renaming : Boolean;
      end record;
   type Rename_Request_Access is access all Rename_Request;
   --  Used for communicate with LSP

   overriding procedure On_Result_Message
     (Self   : in out Rename_Request;
      Result : LSP.Messages.WorkspaceEdit);

   overriding procedure On_Error_Message
     (Self    : in out Rename_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   -- Entity_Renaming_Dialog_Record --

   type Entity_Renaming_Dialog_Record is new GPS_Dialog_Record with record
      New_Name          : Gtk_GEntry;
      Auto_Save         : Gtk_Check_Button;
      Make_Writable     : Gtk_Check_Button;
      Allow_File_Renaming : Gtk_Check_Button;
      In_Comments       : Gtk_Check_Button;
   end record;
   type Entity_Renaming_Dialog is access all
     Entity_Renaming_Dialog_Record'Class;

   procedure Gtk_New
     (Dialog        : out Entity_Renaming_Dialog;
      Kernel        : access Kernel_Handle_Record'Class;
      Entity        : String;
      With_Comments : Boolean);
   --  Create a new dialog for renaming entities

   procedure Refactoring_Rename_Procedure
     (Kernel             : Kernel_Handle;
      File               : GNATCOLL.VFS.Virtual_File;
      Location           : Editor_Location'Class;
      Name               : String;
      New_Name           : String;
      Make_Writable      : Boolean;
      Auto_Save          : Boolean;
      Rename_Files       : Boolean;
      Rename_In_Comments : Boolean);

   Auto_Save_Hist     : constant History_Key := "refactor_auto_save";
   Make_Writable_Hist : constant History_Key := "refactor_make_writable";
   Rename_Files_Hist  : constant History_Key := "refactor_rename_files";

   procedure Set_Rename_In_Comments_Option
     (Kernel : access Kernel_Handle_Record'Class;
      Lang   : Language.Language_Access;
      Value  : Boolean);
   --  Set server configuration option

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Dialog        : out Entity_Renaming_Dialog;
      Kernel        : access Kernel_Handle_Record'Class;
      Entity        : String;
      With_Comments : Boolean)
   is
      Box       : Gtk_Box;
      Button    : Gtk_Widget;
      Main_View : Dialog_View;
      Group     : Dialog_Group_Widget;
      pragma Unreferenced (Button);
   begin
      Dialog := new Entity_Renaming_Dialog_Record;
      GPS.Dialogs.Initialize
        (Dialog,
         Title  => "Renaming entity",
         Kernel => Kernel);
      Set_Default_Size_From_History
        (Win    => Dialog,
         Name   => "Renaming entity",
         Kernel => Kernel,
         Width  => 400,
         Height => 300);

      Main_View := new Dialog_View_Record;
      Dialog_Utils.Initialize (Main_View);
      Dialog.Get_Content_Area.Pack_Start (Main_View);

      Group := new Dialog_Group_Widget_Record;
      Dialog_Utils.Initialize
        (Self        => Group,
         Parent_View => Main_View,
         Group_Name  => "Renaming " & Entity);

      Gtk_New_Hbox (Box);
      Pack_Start (Get_Content_Area (Dialog), Box, Expand => False);

      Gtk_New (Dialog.New_Name);
      Dialog.New_Name.Set_Name ("new_name");
      Set_Text (Dialog.New_Name, Entity);
      Select_Region (Dialog.New_Name, 0, -1);
      Set_Activates_Default (Dialog.New_Name, True);

      Group.Create_Child
        (Widget => Dialog.New_Name,
         Label  => "New name:");

      Group := new Dialog_Group_Widget_Record;
      Dialog_Utils.Initialize
        (Self        => Group,
         Parent_View => Main_View,
         Group_Name  => "Options");

      Gtk_New (Dialog.Auto_Save, "Automatically save modified files");
      Associate (Get_History (Kernel).all, Auto_Save_Hist, Dialog.Auto_Save);
      Group.Create_Child (Dialog.Auto_Save);

      Gtk_New (Dialog.Make_Writable, "Make files writable");
      Set_Tooltip_Text
        (Dialog.Make_Writable,
         "If a read-only file contains references to the entity, this"
         & " switch will make the file writable so that changes can be made."
         & " If the switch is off, then the file will not be edited, but the"
         & " renaming will only be partial.");
      Create_New_Boolean_Key_If_Necessary
        (Hist          => Get_History (Kernel).all,
         Key           => Make_Writable_Hist,
         Default_Value => True);
      Associate (Get_History (Kernel).all,
                 Make_Writable_Hist,
                 Dialog.Make_Writable);
      Group.Create_Child (Widget => Dialog.Make_Writable);

      Gtk_New (Dialog.Allow_File_Renaming, "Allow file renaming");
      Set_Tooltip_Text
        (Dialog.Allow_File_Renaming,
         "Allow file renaming when appropriate (e.g: when "
         & "renaming a package). This is only supported when LSP is active.");
      Create_New_Boolean_Key_If_Necessary
        (Hist          => Get_History (Kernel).all,
         Key           => Rename_Files_Hist,
         Default_Value => True);
      Associate (Get_History (Kernel).all,
                 Rename_Files_Hist,
                 Dialog.Allow_File_Renaming);
      Group.Create_Child (Widget => Dialog.Allow_File_Renaming);

      if With_Comments then
         Gtk_New (Dialog.In_Comments, "Rename in comments");
         Set_Tooltip_Text
           (Dialog.In_Comments,
            "Also rename entities in all comments.");
         Set_Active
           (Dialog.In_Comments,
            GPS.Kernel.Preferences.LSP_Ada_Rename_In_Comment.Get_Pref);
         Group.Create_Child (Widget => Dialog.In_Comments);
      end if;

      Grab_Default (Add_Button (Dialog, Stock_Ok, Gtk_Response_OK));
      Button := Add_Button (Dialog, Stock_Cancel, Gtk_Response_Cancel);
   end Gtk_New;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Rename_Entity_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);

      Kernel      : constant Kernel_Handle := Get_Kernel (Context.Context);
      Entity      : constant String := Entity_Name_Information
        (Context.Context);
      Dialog      : Entity_Renaming_Dialog;
      Lang        : constant Language.Language_Access :=
        Kernel.Get_Language_Handler.Get_Language_From_File
          (File_Information (Context.Context));

      Request : Rename_Request_Access;

   begin
      if Entity = "" then
         return Failure;
      end if;

      Gtk_New
        (Dialog        => Dialog,
         Kernel        => Get_Kernel (Context.Context),
         Entity        => Entity,
         With_Comments => GPS.LSP_Module.LSP_Is_Enabled (Lang)
         and then GPS.LSP_Module.Get_Language_Server
           (Lang).Is_Configuration_Supported
             (GPS.LSP_Client.Configurations.Rename_In_Comments));

      if Dialog = null then
         return Failure;
      end if;

      Show_All (Dialog);

      if Run (Dialog) = Gtk_Response_OK then
         declare
            Holder   : constant Controlled_Editor_Buffer_Holder :=
              Kernel.Get_Buffer_Factory.Get_Holder
                (File_Information (Context.Context));
            Location : constant GPS.Editors.Editor_Location'Class :=
              Holder.Editor.New_Location
                ((if Has_Entity_Line_Information (Context.Context)
                 then Integer (Entity_Line_Information (Context.Context))
                 else Line_Information (Context.Context)),
                 (if Has_Entity_Column_Information (Context.Context)
                  then Entity_Column_Information (Context.Context)
                  else Column_Information (Context.Context)));
         begin
            Request := new Rename_Request'
              (GPS.LSP_Client.Requests.LSP_Request with
               Kernel               => Kernel,
               File                 => File_Information (Context.Context),
               Position             =>
                 GPS.LSP_Client.Utilities.Location_To_LSP_Position (Location),
               New_Name             =>
                 VSS.Strings.Conversions.To_Virtual_String
                   (Get_Text (Dialog.New_Name)),
               Old_Name             =>
                 VSS.Strings.Conversions.To_Virtual_String (Entity),
               Make_Writable        => Get_Active (Dialog.Make_Writable),
               Auto_Save            => Get_Active (Dialog.Auto_Save),
               Allow_File_Renaming  =>
                 Get_Active (Dialog.Allow_File_Renaming));

            if Dialog.In_Comments /= null then
               Set_Rename_In_Comments_Option
                 (Kernel, Lang, Get_Active (Dialog.In_Comments));
            end if;

            if not GPS.LSP_Client.Requests.Execute
              (Lang, GPS.LSP_Client.Requests.Request_Access (Request))
            then
               --  Call old implementation
               Standard.Refactoring.Rename.Rename
                 (Kernel, Context,
                  Old_Name      => To_Unbounded_String (Entity),
                  New_Name      => To_Unbounded_String
                    (Get_Text (Dialog.New_Name)),
                  Auto_Save     => Get_Active (Dialog.Auto_Save),
                  Overridden    => True,
                  Make_Writable => Get_Active (Dialog.Make_Writable));
            else
               Holder.Editor.Current_View.Set_Activity_Progress_Bar_Visibility
                 (True);
            end if;
         end;

         Destroy (Dialog);
      else
         Destroy (Dialog);
      end if;

      return Success;

   exception
      when E : others =>
         Trace (Me, E);
         Destroy (Dialog);
         return Failure;
   end Execute;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Rename_Request;
      Result : LSP.Messages.WorkspaceEdit)
   is
      use type VSS.Strings.Virtual_String;

      On_Error : Boolean;
      Holder   : constant Controlled_Editor_Buffer_Holder :=
        Self.Kernel.Get_Buffer_Factory.Get_Holder
          (Self.File);
   begin
      Holder.Editor.Current_View.Set_Activity_Progress_Bar_Visibility
        (False);
      GPS.LSP_Client.Edit_Workspace.Edit
        (Kernel                   => Self.Kernel,
         Workspace_Edit           => Result,
         Title                    =>
           "Refactoring - rename "
             & Self.Old_Name & " to " & Self.New_Name,
         Make_Writable            => Self.Make_Writable,
         Auto_Save                => Self.Auto_Save,
         Allow_File_Renaming      => Self.Allow_File_Renaming,
         Locations_Message_Markup =>
           "<b>"
         & Escape_Text
             (VSS.Strings.Conversions.To_UTF_8_String (Self.Old_Name))
         & "</b>"
         & " renamed to <b>"
         & Escape_Text
             (VSS.Strings.Conversions.To_UTF_8_String (Self.New_Name))
         & "</b>",
         Error                    => On_Error);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Rename_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value)
   is
      Holder : constant Controlled_Editor_Buffer_Holder :=
        Self.Kernel.Get_Buffer_Factory.Get_Holder
          (Self.File);
   begin
      Holder.Editor.Current_View.Set_Activity_Progress_Bar_Visibility
        (False);
      Trace (Me, "Error when renaming: " & Message);
   end On_Error_Message;

   ----------------------------------
   -- Refactoring_Rename_Procedure --
   ----------------------------------

   procedure Refactoring_Rename_Procedure
     (Kernel             : Kernel_Handle;
      File               : GNATCOLL.VFS.Virtual_File;
      Location           : Editor_Location'Class;
      Name               : String;
      New_Name           : String;
      Make_Writable      : Boolean;
      Auto_Save          : Boolean;
      Rename_Files       : Boolean;
      Rename_In_Comments : Boolean)
   is
      Lang : constant Language.Language_Access :=
        Kernel.Get_Language_Handler.Get_Language_From_File (File);
      Request : Rename_Request_Access;
   begin
      Request := new Rename_Request'
        (GPS.LSP_Client.Requests.LSP_Request with
         Kernel        => Kernel,
         File          => File,
         Position      =>
           GPS.LSP_Client.Utilities.Location_To_LSP_Position (Location),
         New_Name      =>
           VSS.Strings.Conversions.To_Virtual_String (New_Name),
         Old_Name      =>
           VSS.Strings.Conversions.To_Virtual_String (Name),
         Make_Writable => Make_Writable,
         Auto_Save     => Auto_Save,
         Allow_File_Renaming  => Rename_Files);

      Set_Rename_In_Comments_Option (Kernel, Lang, Rename_In_Comments);

      if GPS.LSP_Client.Requests.Execute
        (Lang, GPS.LSP_Client.Requests.Request_Access (Request))
      then
         --  executed
         return;
      end if;

      --  Call old implementation
      declare
         Context     : Selection_Context := New_Context
           (Kernel, Refactoring_Module);
         Interactive : Interactive_Command_Context :=
           Create_Null_Context (Context);

      begin
         Set_File_Information
           (Context,
            Files           => (1 => File),
            Project         => Kernel.Get_Project_Tree.Root_Project,
            Publish_Project => False,
            Line            => Location.Line,
            Column          => Location.Column);

         Set_Entity_Information
           (Context,
            Name,
            Basic_Types.Editable_Line_Type (Location.Line),
            Location.Column);

         Standard.Refactoring.Rename.Rename
           (Kernel,
            Interactive,
            Old_Name      => To_Unbounded_String (Name),
            New_Name      => To_Unbounded_String (New_Name),
            Auto_Save     => Auto_Save,
            Overridden    => True,
            Make_Writable => Make_Writable);

         Free (Interactive);
      end;

   exception
      when E : others =>
         Trace (Me, E);
   end Refactoring_Rename_Procedure;

   -----------------------------------
   -- Set_Rename_In_Comments_Option --
   -----------------------------------

   procedure Set_Rename_In_Comments_Option
     (Kernel : access Kernel_Handle_Record'Class;
      Lang   : Language.Language_Access;
      Value  : Boolean) is
   begin
      if Lang.Get_Name = "ada" then
         GPS.Kernel.Preferences.LSP_Ada_Rename_In_Comment.Set_Pref
           (Kernel.Get_Preferences,
            Value);
      end if;
   end Set_Rename_In_Comments_Option;

   --------------
   -- Register --
   --------------

   procedure Register
     (Kernel : Kernel_Handle;
      Id     : GPS.Kernel.Modules.Module_ID) is
   begin
      Refactoring_Module := Id;

      Src_Editor_Module.Shell.Refactoring_Rename_Handler :=
        Refactoring_Rename_Procedure'Access;

      Register_Contextual_Submenu
        (Kernel,
         Name  => "Refactoring",
         Group => Editing_Contextual_Group);

      Register_Action
        (Kernel, "rename entity",
         Command      => new Rename_Entity_Command,
         Description  => "Rename an entity, including its references",
         Category     => "Refactoring",
         Filter       => Lookup_Filter (Kernel, "Entity"),
         For_Learning => True);

      Register_Contextual_Menu
        (Kernel,
         Label  => "Refactoring/Rename %s",
         Action => "rename entity",
         Group  => Editing_Contextual_Group);
   end Register;

end GPS.LSP_Client.Refactoring.Rename;
