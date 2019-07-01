------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with Ada.Strings.Unbounded;        use Ada.Strings.Unbounded;

with GNATCOLL.Traces;              use GNATCOLL.Traces;
with GNATCOLL.VFS;                 use GNATCOLL.VFS;

with Glib;                         use Glib;
with Gtk.Box;                      use Gtk.Box;
with Gtk.Check_Button;             use Gtk.Check_Button;
with Gtk.Dialog;                   use Gtk.Dialog;
with Gtk.GEntry;                   use Gtk.GEntry;
with Gtk.Label;                    use Gtk.Label;
with Gtk.Stock;                    use Gtk.Stock;
with Gtk.Widget;                   use Gtk.Widget;

with GPS.Dialogs;                  use GPS.Dialogs;
with GPS.Editors;                  use GPS.Editors;
with GPS.Kernel.Actions;           use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;          use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;               use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;        use GPS.Kernel.Modules.UI;
with GPS.LSP_Module;

with Basic_Types;
with Commands;                     use Commands;
with Commands.Interactive;         use Commands.Interactive;
with Histories;                    use Histories;
with Language;

with Refactoring.Rename;
with Src_Editor_Module.Shell;

with GPS.LSP_Client.Edit_Workspace;
with GPS.LSP_Client.Requests.Rename;
with LSP.Messages;
with LSP.Types;

package body GPS.LSP_Client.Rename is

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
         Kernel        : Kernel_Handle;
         Old_Name      : Unbounded_String;
         Make_Writable : Boolean;
         Auto_Save     : Boolean;
      end record;
   type Rename_Request_Access is access all Rename_Request;
   --  Used for communicate with LSP

   overriding procedure On_Result_Message
     (Self   : in out Rename_Request;
      Result : LSP.Messages.WorkspaceEdit);

   -- Entity_Renaming_Dialog_Record --

   type Entity_Renaming_Dialog_Record is new GPS_Dialog_Record with record
      New_Name          : Gtk_GEntry;
      Auto_Save         : Gtk_Check_Button;
      Rename_Primitives : Gtk_Check_Button;
      Make_Writable     : Gtk_Check_Button;
   end record;
   type Entity_Renaming_Dialog is access all
     Entity_Renaming_Dialog_Record'Class;

   procedure Gtk_New
     (Dialog : out Entity_Renaming_Dialog;
      Kernel : access Kernel_Handle_Record'Class;
      Entity : String);
   --  Create a new dialog for renaming entities

   procedure Refactoring_Rename_Procedure
     (Kernel            : Kernel_Handle;
      File              : GNATCOLL.VFS.Virtual_File;
      Line              : Integer;
      Column            : Basic_Types.Visible_Column_Type;
      Name              : String;
      New_Name          : String;
      Make_Writable     : Boolean;
      Auto_Save         : Boolean;
      Rename_Primitives : Boolean);

   Auto_Save_Hist         : constant History_Key := "refactor_auto_save";
   Rename_Primitives_Hist : constant History_Key := "refactor_primitives";
   Make_Writable_Hist     : constant History_Key := "refactor_make_writable";

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Dialog : out Entity_Renaming_Dialog;
      Kernel : access Kernel_Handle_Record'Class;
      Entity : String)
   is
      Label  : Gtk_Label;
      Box    : Gtk_Box;
      Button : Gtk_Widget;
      pragma Unreferenced (Button);
   begin
      if not Save_MDI_Children (Kernel, Force => False) then
         return;
      end if;

      Dialog := new Entity_Renaming_Dialog_Record;
      GPS.Dialogs.Initialize
        (Dialog,
         Title  => "Renaming entity",
         Kernel => Kernel);

      Gtk_New (Label, "Renaming " & Entity);
      Set_Alignment (Label, 0.0, 0.0);
      Pack_Start (Get_Content_Area (Dialog), Label, Expand => False);

      Gtk_New_Hbox (Box);
      Pack_Start (Get_Content_Area (Dialog), Box, Expand => False);

      Gtk_New (Label, "New name: ");
      Pack_Start (Box, Label, Expand => False);

      Gtk_New (Dialog.New_Name);
      Dialog.New_Name.Set_Name ("new_name");
      Set_Text (Dialog.New_Name, Entity);
      Select_Region (Dialog.New_Name, 0, -1);
      Set_Activates_Default (Dialog.New_Name, True);
      Pack_Start (Box, Dialog.New_Name);

      Gtk_New (Dialog.Auto_Save, "Automatically save modified files");
      Associate (Get_History (Kernel).all, Auto_Save_Hist, Dialog.Auto_Save);
      Pack_Start
        (Get_Content_Area (Dialog), Dialog.Auto_Save, Expand => False);

      Gtk_New (Dialog.Rename_Primitives,
               "Rename overriding and overridden entities");
      Set_Tooltip_Text
        (Dialog.Rename_Primitives,
         "If the entity is a subprogram, also rename subprograms that"
         & " override or are overridden by it." & ASCII.LF
         & "If the entity is a subprogram parameter, also rename parameters"
         & " with the same name in overriding or overridden subprograms.");
      Create_New_Boolean_Key_If_Necessary
        (Hist          => Get_History (Kernel).all,
         Key           => Rename_Primitives_Hist,
         Default_Value => True);
      Associate (Get_History (Kernel).all,
                 Rename_Primitives_Hist,
                 Dialog.Rename_Primitives);
      Pack_Start (Get_Content_Area (Dialog), Dialog.Rename_Primitives);

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
      Pack_Start (Get_Content_Area (Dialog), Dialog.Make_Writable);

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
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      Entity  : constant String := Entity_Name_Information (Context.Context);
      Dialog  : Entity_Renaming_Dialog;
      Lang    : Language.Language_Access;

   begin
      if Entity /= "" then
         Gtk_New (Dialog, Get_Kernel (Context.Context), Entity);
         if Dialog = null then
            return Failure;
         end if;

         Show_All (Dialog);

         if Run (Dialog) = Gtk_Response_OK then
            Lang := Kernel.Get_Language_Handler.Get_Language_From_File
              (File_Information (Context.Context));

            if GPS.LSP_Module.LSP_Is_Enabled (Lang) then
               declare
                  Request : Rename_Request_Access := new Rename_Request;
               begin
                  Request.Kernel        := Kernel;
                  Request.Text_Document := File_Information (Context.Context);
                  Request.Line          := Line_Information
                    (Context.Context);
                  Request.Column        := Column_Information
                    (Context.Context);
                  Request.New_Name      := LSP.Types.To_LSP_String
                    (Get_Text (Dialog.New_Name));
                  Request.Old_Name      := To_Unbounded_String (Entity);
                  Request.Make_Writable := Get_Active (Dialog.Make_Writable);
                  Request.Auto_Save     := Get_Active (Dialog.Auto_Save);

                  GPS.LSP_Client.Requests.Execute
                    (Lang, GPS.LSP_Client.Requests.Request_Access (Request));
               end;

            else
               --  Call old implementation
               Refactoring.Rename.Rename
                 (Kernel, Context,
                  To_Unbounded_String (Entity),
                  To_Unbounded_String (Get_Text (Dialog.New_Name)),
                  Get_Active (Dialog.Auto_Save),
                  Get_Active (Dialog.Rename_Primitives),
                  Get_Active (Dialog.Make_Writable));
            end if;
         end if;

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
      Result : LSP.Messages.WorkspaceEdit) is
   begin
      GPS.LSP_Client.Edit_Workspace.Edit
        (Self.Kernel, Result, To_String (Self.Old_Name),
         "Refactoring - rename", Self.Make_Writable, Self.Auto_Save);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Result_Message;

   ----------------------------------
   -- Refactoring_Rename_Procedure --
   ----------------------------------

   procedure Refactoring_Rename_Procedure
     (Kernel            : Kernel_Handle;
      File              : GNATCOLL.VFS.Virtual_File;
      Line              : Integer;
      Column            : Basic_Types.Visible_Column_Type;
      Name              : String;
      New_Name          : String;
      Make_Writable     : Boolean;
      Auto_Save         : Boolean;
      Rename_Primitives : Boolean)
   is
      Lang : constant Language.Language_Access :=
        Kernel.Get_Language_Handler.Get_Language_From_File (File);
   begin
      if GPS.LSP_Module.LSP_Is_Enabled (Lang) then
         declare
            Request : Rename_Request_Access := new Rename_Request;
         begin
            Request.Kernel        := Kernel;
            Request.Text_Document := File;
            Request.Line          := Line;
            Request.Column        := Column;
            Request.New_Name      := LSP.Types.To_LSP_String (New_Name);
            Request.Old_Name      := To_Unbounded_String (Name);
            Request.Make_Writable := Make_Writable;
            Request.Auto_Save     := Auto_Save;

            GPS.LSP_Client.Requests.Execute
              (Lang, GPS.LSP_Client.Requests.Request_Access (Request));
         end;

      else
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
               Line            => Line,
               Column          => Column);

            Set_Entity_Information
              (Context,
               Name,
               Basic_Types.Editable_Line_Type (Line),
               Column);

            Refactoring.Rename.Rename
              (Kernel,
               Interactive,
               To_Unbounded_String (Name),
               To_Unbounded_String (New_Name),
               Auto_Save,
               Rename_Primitives,
               Make_Writable);

            Free (Interactive);
         end;
      end if;

   exception
      when E : others =>
         Trace (Me, E);
   end Refactoring_Rename_Procedure;

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

end GPS.LSP_Client.Rename;
