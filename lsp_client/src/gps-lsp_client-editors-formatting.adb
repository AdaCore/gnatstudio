------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
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

with GNATCOLL.Traces;                     use GNATCOLL.Traces;
with GNATCOLL.VFS;                        use GNATCOLL.VFS;

with Gtkada.MDI;

with GPS.Editors;                         use GPS.Editors;
with GPS.LSP_Client.Edit_Workspace;
with GPS.LSP_Client.Utilities;
with GPS.LSP_Clients;
with GPS.LSP_Module;
with GPS.Kernel.Actions;
with GPS.Kernel.MDI;
with GPS.Kernel.Modules;                  use GPS.Kernel.Modules;

with GUI_Utils;
with Language;                            use Language;
with Src_Editor_Box;                      use Src_Editor_Box;
with Src_Editor_Buffer;                   use Src_Editor_Buffer;
with Src_Editor_Module;
with Src_Editor_View;                     use Src_Editor_View;
with Commands;                            use Commands;
with Commands.Interactive;                use Commands.Interactive;

with GPS.LSP_Client.Requests.Range_Formatting;
with GPS.LSP_Client.Requests.Document_Formatting;

package body GPS.LSP_Client.Editors.Formatting is

   Me : constant Trace_Handle := Create ("GPS.EDITORS.LSP_FORMATTING");

   LSP_FORMATTING_ON : constant Trace_Handle := Create
     ("GPS.EDITORS.LSP_FORMATTING", On);
   --  Enable/disable LSP formatting

   -- Range_Formatting_Request --

   type Range_Formatting_Request is
     new GPS.LSP_Client.Requests.Range_Formatting.
       Abstract_Range_Formatting_Request with
      record
         Kernel : Kernel_Handle;
      end record;
   type Range_Formatting_Request_Access is access all Range_Formatting_Request;
   --  Used for communicate with LSP

   overriding procedure On_Result_Message
     (Self   : in out Range_Formatting_Request;
      Result : LSP.Messages.TextEdit_Vector);

   -- Document_Formatting_Request --

   type Document_Formatting_Request is
     new GPS.LSP_Client.Requests.Document_Formatting.
       Abstract_Document_Formatting_Request with
      record
         Kernel : Kernel_Handle;
         Editor : Gtkada.MDI.MDI_Child;
      end record;
   type Document_Formatting_Request_Access is
     access all Document_Formatting_Request;
   --  Used for communicate with LSP

   overriding procedure On_Result_Message
     (Self   : in out Document_Formatting_Request;
      Result : LSP.Messages.TextEdit_Vector);

   -- LSP_Editor_Formatting_Provider --

   type LSP_Editor_Formatting_Provider is
     new GPS.Editors.Editor_Formatting_Provider with record
      Kernel : Kernel_Handle;
   end record;

   overriding function Format_Section
     (Self     : in out LSP_Editor_Formatting_Provider;
      From, To : Editor_Location'Class;
      Force    : Boolean := False)
      return Boolean;

   type LSP_Formatting_Module_Id_Record is
     new Module_ID_Record with null record;
   type LSP_Formatting_Module_Id_Access is
     access all LSP_Formatting_Module_Id_Record'Class;

   overriding procedure Destroy
     (Module : in out LSP_Formatting_Module_Id_Record);

   type Indentation_File_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Indentation_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   Module_Id : LSP_Formatting_Module_Id_Access;
   Provider  : aliased LSP_Editor_Formatting_Provider;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Range_Formatting_Request;
      Result : LSP.Messages.TextEdit_Vector)
   is
      Editor : constant Editor_Buffer'Class :=
        Self.Kernel.Get_Buffer_Factory.Get
          (File      => Self.Text_Document,
           Open_View => False,
           Focus     => False);
      Map    : LSP.Messages.TextDocumentEdit_Maps.Map;
      Dummy  : Boolean;
   begin

      if Editor = Nil_Editor_Buffer then
         --  Buffer can be closed
         return;
      end if;

      Map.Include
        (GPS.LSP_Client.Utilities.To_URI (Self.Text_Document), Result);

      GPS.LSP_Client.Edit_Workspace.Edit
        (Kernel         => Self.Kernel,
         Workspace_Edit => LSP.Messages.WorkspaceEdit'
           (changes         => Map,
            documentChanges => <>),
         Title          => "Formatting",
         Make_Writable  => False,
         Auto_Save      => False,
         Show_Messages  => False,
         Error          => Dummy);

      Editor.Unselect;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Result_Message;

   overriding procedure On_Result_Message
     (Self   : in out Document_Formatting_Request;
      Result : LSP.Messages.TextEdit_Vector)
   is
      Buffer : constant Editor_Buffer'Class :=
        Self.Kernel.Get_Buffer_Factory.Get
          (File      => Self.Text_Document,
           Open_View => False,
           Focus     => False);
      Map    : LSP.Messages.TextDocumentEdit_Maps.Map;
      Dummy  : Boolean;
   begin

      if Buffer = Nil_Editor_Buffer then
         --  Buffer can be closed
         return;
      end if;

      Map.Include
        (GPS.LSP_Client.Utilities.To_URI (Self.Text_Document), Result);

      GPS.LSP_Client.Edit_Workspace.Edit
        (Kernel         => Self.Kernel,
         Workspace_Edit => LSP.Messages.WorkspaceEdit'
           (changes         => Map,
            documentChanges => <>),
         Title          => "Formatting",
         Make_Writable  => False,
         Auto_Save      => False,
         Show_Messages  => False,
         Error          => Dummy);

      GUI_Utils.Grab_Toplevel_Focus
        (MDI     => GPS.Kernel.MDI.Get_MDI (Self.Kernel),
         Widget  => Self.Editor,
         Present => True);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Result_Message;

   --------------------
   -- Format_Section --
   --------------------

   overriding function Format_Section
     (Self     : in out LSP_Editor_Formatting_Provider;
      From, To : Editor_Location'Class;
      Force    : Boolean := False)
      return Boolean
   is
      File    : constant Virtual_File := From.Buffer.File;
      Lang    : Language.Language_Access;
      Client  : GPS.LSP_Clients.LSP_Client_Access;
      Option  : LSP.Messages.DocumentRangeFormattingOptions;

      Params       : Indent_Parameters;
      Indent_Style : Indentation_Kind;

      Request : Range_Formatting_Request_Access;

   begin
      if Self.Kernel.Is_In_Destruction then
         return True;
      end if;

      Lang := Self.Kernel.Get_Language_Handler.Get_Language_From_File (File);

      if not LSP_FORMATTING_ON.Is_Active
        or else Lang = null
        or else not GPS.LSP_Module.LSP_Is_Enabled (Lang)
      then
         return False;
      end if;

      Client := GPS.LSP_Module.Get_Language_Server (Lang).Get_Client;
      if not Client.Is_Ready then
         return False;
      end if;

      Option := Client.Capabilities.documentRangeFormattingProvider;
      if not Option.Is_Set then
         return False;
      end if;

      declare
         Buffer : constant GPS.Editors.Editor_Buffer'Class :=
           Self.Kernel.Get_Buffer_Factory.Get
           (File        => File,
            Open_Buffer => False,
            Open_View   => False);
      begin
         if Buffer = Nil_Editor_Buffer
           or else not Buffer.Is_Opened_On_LSP_Server
         then
            return False;
         end if;
      end;

      Request := new Range_Formatting_Request;
      Request.Kernel := Self.Kernel;
      Request.Text_Document := File;
      Request.Span :=
        (first =>
           (line      => LSP.Types.Line_Number (From.Line - 1),
            character =>
              GPS.LSP_Client.Utilities.Visible_Column_To_UTF_16_Offset
                (From.Column)),
         last  =>
           (line      => LSP.Types.Line_Number (To.Line - 1),
            character =>
              GPS.LSP_Client.Utilities.Visible_Column_To_UTF_16_Offset
                (To.Column)));

      Get_Indentation_Parameters (Lang, Params, Indent_Style);

      Request.Indentation_Level := Params.Indent_Level;
      Request.Use_Tabs          := Params.Use_Tabs;

      GPS.LSP_Client.Requests.Execute
        (Lang, GPS.LSP_Client.Requests.Request_Access (Request));

      return True;
   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Format_Section;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Module : in out LSP_Formatting_Module_Id_Record)
   is
      pragma Unreferenced (Module);
   begin
      Src_Editor_Buffer.Set_Formatting_Provider (null);
   end Destroy;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Indentation_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel       : constant Kernel_Handle := Get_Kernel (Context.Context);
      Editor       : Gtkada.MDI.MDI_Child;
      Box          : Source_Editor_Box;
      View         : Source_View;
      Buffer       : Source_Buffer;
      File         : Virtual_File;

      Lang         : Language.Language_Access;
      Client       : GPS.LSP_Clients.LSP_Client_Access;
      Option       : LSP.Messages.DocumentFormattingOptions;

      Params       : Indent_Parameters;
      Indent_Style : Indentation_Kind;

      Request      : Document_Formatting_Request_Access;

   begin
      if Kernel.Is_In_Destruction
        or else not LSP_FORMATTING_ON.Is_Active
      then
         return Success;
      end if;

      Editor := Src_Editor_Module.Find_Current_Editor (Kernel);
      Box    := Src_Editor_Module.Get_Source_Box_From_MDI (Editor);
      View   := Get_View (Box);
      Buffer := Get_Buffer (Box);

      if not Get_Editable (View) then
         return Failure;
      end if;

      File := Buffer.Get_Filename;
      Lang := Kernel.Get_Language_Handler.Get_Language_From_File (File);

      if Lang = null
        or else not GPS.LSP_Module.LSP_Is_Enabled (Lang)
      then
         return Failure;
      end if;

      Client := GPS.LSP_Module.Get_Language_Server (Lang).Get_Client;
      if not Client.Is_Ready then
         return Failure;
      end if;

      Option := Client.Capabilities.documentFormattingProvider;
      if not Option.Is_Set then
         return Failure;
      end if;

      Request := new Document_Formatting_Request;
      Request.Kernel := Kernel;
      Request.Editor := Editor;
      Request.Text_Document := File;

      Get_Indentation_Parameters (Lang, Params, Indent_Style);

      Request.Indentation_Level := Params.Indent_Level;
      Request.Use_Tabs          := Params.Use_Tabs;

      GPS.LSP_Client.Requests.Execute
        (Lang, GPS.LSP_Client.Requests.Request_Access (Request));

      return Success;
   exception
      when E : others =>
         Trace (Me, E);
         return Failure;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : Kernel_Handle) is
   begin
      Module_Id := new LSP_Formatting_Module_Id_Record;
      Register_Module
        (Module      => Modules.Module_ID (Formatting.Module_Id),
         Kernel      => Kernel,
         Module_Name => "LSP_Formatting",
         Priority    => Default_Priority);

      Provider.Kernel := Kernel;
      Src_Editor_Buffer.Set_Formatting_Provider (Provider'Access);

      if LSP_FORMATTING_ON.Is_Active then
         GPS.Kernel.Actions.Register_Action
           (Kernel       => Kernel,
            Name         =>  "autoindent file",
            Command      => new Indentation_File_Command,
            Description  => "Automatically indent the current file",
            Category     => "Editor",
            Filter       =>
              Lookup_Filter (Kernel, "Writable source editor") and
                Lookup_Filter (Kernel, "Is not Makefile"));
      end if;
   end Register_Module;

end GPS.LSP_Client.Editors.Formatting;
