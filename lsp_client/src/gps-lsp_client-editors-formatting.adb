------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2020-2025, AdaCore                  --
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

with Ada.Containers.Indefinite_Ordered_Maps;
with Ada.Exceptions;               use Ada.Exceptions;
with Ada.Strings.Wide_Wide_Maps;   use Ada.Strings.Wide_Wide_Maps;

with GNATCOLL.JSON;
with GNATCOLL.Traces;               use GNATCOLL.Traces;
with GNATCOLL.VFS;                  use GNATCOLL.VFS;

with GPS.Editors.GtkAda;
with VSS.Characters;
with VSS.Strings.Character_Iterators;
pragma Unreferenced (VSS.Strings.Character_Iterators);
--  GNAT 20211114 generates incorrect warning, with clause is necessary to
--  make visible Virtual_String.First_Character.Element subprogram.
with VSS.Strings.Conversions;

with Gtkada.MDI;

with GPS.Default_Styles;
with GPS.Editors;                   use GPS.Editors;
with GPS.Editors.Line_Information;  use GPS.Editors.Line_Information;
with GPS.LSP_Client.Edit_Workspace;
with GPS.LSP_Client.Utilities;
with GPS.Kernel.Actions;
with GPS.Kernel.MDI;
with GPS.Kernel.Modules;            use GPS.Kernel.Modules;
with GPS.Kernel.Messages;
with GPS.Kernel.Messages.Tools_Output;

with GUI_Utils;
with Language;                      use Language;
with Src_Editor_Box;                use Src_Editor_Box;
with Src_Editor_Buffer;             use Src_Editor_Buffer;
with Src_Editor_Module;
with Src_Editor_View;               use Src_Editor_View;
with Commands;                      use Commands;
with Commands.Interactive;          use Commands.Interactive;

with GPS.LSP_Client.Language_Servers;
with GPS.LSP_Module;

with GPS.LSP_Client.Requests;
with GPS.LSP_Client.Requests.Document_Formatting;
with GPS.LSP_Client.Requests.Range_Formatting;
with GPS.LSP_Client.Requests.On_Type_Formatting;

package body GPS.LSP_Client.Editors.Formatting is

   Me : constant Trace_Handle := Create ("GPS.LSP.FORMATTING.ADVANCED", Off);
   --  Logging trace

   package LSP_Trigger_Characters_Maps is new
     Ada.Containers.Indefinite_Ordered_Maps
       (String,
        Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Set);
   --  Map of language to trigger character set

   type LSP_Editor_Formatting_Provider is
     new GPS.Editors.Editor_Formatting_Provider
   with record
      Kernel                : Kernel_Handle;
      Lang_To_Trigger_Chars : LSP_Trigger_Characters_Maps.Map;
   end record;

   overriding function On_Range_Formatting
     (Self        : in out LSP_Editor_Formatting_Provider;
      From, To    : Editor_Location'Class;
      Cursor_Line : Natural;
      Cursor_Move : in out Integer)
      return Boolean;

   overriding function On_Type_Formatting
     (Self        : in out LSP_Editor_Formatting_Provider;
      From, To    : Editor_Location'Class;
      Cursor_Line : Natural)
      return Boolean;

   overriding function Get_Name
     (Self : LSP_Editor_Formatting_Provider) return String;

   -- Document_Formatting_Request --

   type Document_Formatting_Request is
     new GPS.LSP_Client.Requests.Document_Formatting.
       Abstract_Document_Formatting_Request with
      record
         Editor : Gtkada.MDI.MDI_Child;
      end record;
   type Document_Formatting_Request_Access is
     access all Document_Formatting_Request;
   --  Used for communicate with LSP

   overriding function Get_Task_Label
     (Self : Document_Formatting_Request) return String
   is
     ("format file");

   overriding procedure On_Result_Message
     (Self   : in out Document_Formatting_Request;
      Result : LSP.Messages.TextEdit_Vector);

   overriding procedure On_Error_Message
     (Self    : in out Document_Formatting_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : VSS.Strings.Virtual_String;
      Data    : GNATCOLL.JSON.JSON_Value);

   overriding procedure On_Rejected
     (Self   : in out Document_Formatting_Request;
      Reason : GPS.LSP_Client.Requests.Reject_Reason);

   -- Range_Formatting_Request --

   type Range_Formatting_Request is
     new GPS.LSP_Client.Requests.Range_Formatting.
       Abstract_Range_Formatting_Request with null record;
   type Range_Formatting_Request_Access is access all Range_Formatting_Request;
   --  Corresponding LSP request

   overriding function Get_Task_Label
     (Self : Range_Formatting_Request) return String
   is
     ("format range");

   overriding procedure On_Result_Message
     (Self   : in out Range_Formatting_Request;
      Result : LSP.Messages.TextEdit_Vector);

   overriding procedure On_Error_Message
     (Self    : in out Range_Formatting_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : VSS.Strings.Virtual_String;
      Data    : GNATCOLL.JSON.JSON_Value);

   overriding procedure On_Rejected
     (Self   : in out Range_Formatting_Request;
      Reason : GPS.LSP_Client.Requests.Reject_Reason);

   -- On_Type_Formatting_Request --

   type On_Type_Formatting_Request is
     new GPS.LSP_Client.Requests.On_Type_Formatting.
       Abstract_On_Type_Formatting_Request with null record;
   type On_Type_Formatting_Request_Access is
     access all On_Type_Formatting_Request;
   --  Used for communicate with LSP

   overriding procedure On_Result_Message
     (Self   : in out On_Type_Formatting_Request;
      Result : LSP.Messages.TextEdit_Vector);

   -- LSP_Formatting_Module_Id_Record --

   type LSP_Formatting_Module_Id_Record is
     new Module_ID_Record with null record;
   type LSP_Formatting_Module_Id_Access is
     access all LSP_Formatting_Module_Id_Record'Class;

   type Indentation_File_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Indentation_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   Module_Id    : LSP_Formatting_Module_Id_Access;
   LSP_Provider : aliased LSP_Editor_Formatting_Provider;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Document_Formatting_Request;
      Result : LSP.Messages.TextEdit_Vector)
   is
      Buffer : constant Editor_Buffer'Class :=
        Self.Kernel.Get_Buffer_Factory.Get
          (File => Self.File, Open_View => False, Focus => False);
      Map    : LSP.Messages.TextDocumentEdit_Maps.Map;
      Dummy  : Boolean;
   begin

      if Buffer = Nil_Editor_Buffer then
         --  Buffer can be closed
         return;
      end if;

      --  Cancel the activity progress bar when formatting succeeds.
      Src_Editor_Module.Cancel_Activity_Bar (Self.Kernel, Self.File);

      if Buffer.Version /= Self.Document_Version then
         Trace
           (Me,
            "Document_Formatting canceled for "
            & (+Base_Name (Buffer.File))
            & " ver."
            & Integer'Image (Buffer.Version)
            & ", data ver."
            & Integer'Image (Self.Document_Version));
         return;
      end if;

      Map.Include (GPS.LSP_Client.Utilities.To_URI (Self.File), Result);

      GPS.LSP_Client.Edit_Workspace.Edit
        (Kernel                   => Self.Kernel,
         Workspace_Edit           =>
           LSP.Messages.WorkspaceEdit'
             (changes => Map, documentChanges => <>, changeAnnotations => <>),
         Title                    => "Formatting",
         Make_Writable            => False,
         Auto_Save                => False,
         Allow_File_Renaming      => False,
         Locations_Message_Markup => "",
         Error                    => Dummy);

      GUI_Utils.Grab_Toplevel_Focus
        (MDI     => GPS.Kernel.MDI.Get_MDI (Self.Kernel),
         Widget  => Self.Editor,
         Present => True);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Result_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self   : in out Document_Formatting_Request;
      Reason : GPS.LSP_Client.Requests.Reject_Reason) is
   begin
      Trace
        (Me,
         "Document_Formatting rejected: "
         & GPS.LSP_Client.Requests.Reject_Reason'Image (Reason));

      --  Cancel the activity bar when formatting failed.
      Src_Editor_Module.Cancel_Activity_Bar (Self.Kernel, Self.File);
   end On_Rejected;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Document_Formatting_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : VSS.Strings.Virtual_String;
      Data    : GNATCOLL.JSON.JSON_Value) is
   begin
      --  Cancel the activity bar when formatting failed.
      Src_Editor_Module.Cancel_Activity_Bar (Self.Kernel, Self.File);

      --  Try to parse the formatter's output, in case the format is recognized
      --  (which is the case for the ALS).
      GPS.Kernel.Messages.Tools_Output.Parse_File_Locations
        (Self.Kernel,
         Text              =>
           VSS.Strings.Conversions.To_UTF_8_String (Message),
         Category          => "Formatting",
         Highlight         => True,
         Styles            => GPS.Default_Styles.Messages_Styles,
         Show_In_Locations => True);

      --  Display the message in the Messages view
      Self.Kernel.Insert
        (VSS.Strings.Conversions.To_UTF_8_String (Message));

   exception
      when E : others =>
         Trace
           (Me,
            "Exception found when parsing message: "
            & VSS.Strings.Conversions.To_UTF_8_String (Message));
         Trace (Me, Exception_Message (E));
   end On_Error_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Range_Formatting_Request;
      Result : LSP.Messages.TextEdit_Vector)
   is
      Editor : Editor_Buffer'Class :=
        Self.Kernel.Get_Buffer_Factory.Get
          (File      => Self.File,
           Open_View => False,
           Focus     => False);
      Map    : LSP.Messages.TextDocumentEdit_Maps.Map;
      Dummy  : Boolean;
   begin

      if Editor = Nil_Editor_Buffer then
         --  Buffer can be closed
         return;
      end if;

      --  Cancel the activity bar when range formatting succeeds.
      Src_Editor_Module.Cancel_Activity_Bar (Self.Kernel, Self.File);

      if Editor.Version /= Self.Document_Version then
         Trace
           (Me,
            "Range_Formatting canceled for " & (+Base_Name (Editor.File)) &
              " ver." & Integer'Image (Editor.Version) & ", data ver." &
              Integer'Image (Self.Document_Version));
         return;
      end if;

      declare
         Controller : constant Cursor_Movement_Controller'Class :=
           GPS_Editor_Buffer'Class (Editor).Freeze_Cursor;
         pragma Unreferenced (Controller);
      begin
         Map.Include (GPS.LSP_Client.Utilities.To_URI (Self.File), Result);

         GPS.LSP_Client.Edit_Workspace.Edit
           (Kernel                   => Self.Kernel,
            Workspace_Edit           => LSP.Messages.WorkspaceEdit'
              (changes           => Map,
               documentChanges   => <>,
               changeAnnotations => <>),
            Title                    => "Formatting",
            Make_Writable            => False,
            Auto_Save                => False,
            Allow_File_Renaming      => False,
            Locations_Message_Markup => "",
            Limit_Span               =>
              (if Src_Editor_Module.Get_Limit_LSP_Formatting
                   (Editor.Get_Language)
               then Self.Span
               else LSP.Messages.Empty_Span),
            Compute_Minimal_Edits    =>  True,
            Error                    => Dummy);
      end;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Range_Formatting_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : VSS.Strings.Virtual_String;
      Data    : GNATCOLL.JSON.JSON_Value) is
   begin
      --  Cancel the activity bar when range formatting fails.
      Src_Editor_Module.Cancel_Activity_Bar (Self.Kernel, Self.File);

      --  Try to parse the formatter's output, in case the format is recognized
      --  (which is the case for the ALS).
      GPS.Kernel.Messages.Tools_Output.Parse_File_Locations
        (Self.Kernel,
         Text              =>
           VSS.Strings.Conversions.To_UTF_8_String (Message),
         Category          => "Formatting",
         Highlight         => True,
         Styles            => GPS.Default_Styles.Messages_Styles,
         Show_In_Locations => True);

      --  Display the message in the Messages view
      Self.Kernel.Insert (VSS.Strings.Conversions.To_UTF_8_String (Message));

   exception
      when E : others =>
         Trace
           (Me,
            "Exception found when parsing message: "
            & VSS.Strings.Conversions.To_UTF_8_String (Message));
         Trace (Me, Exception_Message (E));
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self   : in out Range_Formatting_Request;
      Reason : GPS.LSP_Client.Requests.Reject_Reason) is
   begin
      Trace
        (Me,
         "Range_Formatting rejected: "
         & GPS.LSP_Client.Requests.Reject_Reason'Image (Reason));

      --  Cancel the activity bar when range formatting request gets
      --  rejected.
      Src_Editor_Module.Cancel_Activity_Bar (Self.Kernel, Self.File);
   end On_Rejected;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out On_Type_Formatting_Request;
      Result : LSP.Messages.TextEdit_Vector)
   is
      Editor : Editor_Buffer'Class :=
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

      if Editor.Version /= Self.Document_Version then
         Trace
           (Me,
            "On_Type_Formatting canceled for " & (+Base_Name (Editor.File)) &
              " ver." & Integer'Image (Editor.Version) & ", data ver." &
              Integer'Image (Self.Document_Version));
         return;
      end if;

      declare
         Controller : constant Cursor_Movement_Controller'Class :=
           GPS_Editor_Buffer'Class (Editor).Freeze_Cursor;
         pragma Unreferenced (Controller);
      begin
         Map.Include (GPS.LSP_Client.Utilities.To_URI (Self.File), Result);

         GPS.LSP_Client.Edit_Workspace.Edit
           (Kernel                   => Self.Kernel,
            Workspace_Edit           => LSP.Messages.WorkspaceEdit'
              (changes           => Map,
               documentChanges   => <>,
               changeAnnotations => <>),
            Title          => "Format",
            Make_Writable            => False,
            Auto_Save                => False,
            Allow_File_Renaming      => False,
            Locations_Message_Markup => "",
            Error                    => Dummy);
      end;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Result_Message;

   -------------------------
   -- On_Range_Formatting --
   -------------------------

   overriding function On_Range_Formatting
     (Self        : in out LSP_Editor_Formatting_Provider;
      From, To    : Editor_Location'Class;
      Cursor_Line : Natural;
      Cursor_Move : in out Integer)
      return Boolean
   is
      pragma Unreferenced (Cursor_Line, Cursor_Move);
      File    : constant Virtual_File := From.Buffer.File;
      Lang    : Language.Language_Access;
      Request : Range_Formatting_Request_Access;
      Editor  : Gtkada.MDI.MDI_Child;
      Box     : Source_Editor_Box;

   begin
      Editor := GPS.Editors.GtkAda.Get_MDI_Child
        (From.Buffer.Current_View);
      Box := Src_Editor_Module.Get_Source_Box_From_MDI (Editor);
      Lang := Self.Kernel.Get_Language_Handler.Get_Language_From_File (File);

      Box.Set_Activity_Progress_Bar_Visibility (True);

      Request := new Range_Formatting_Request'
        (GPS.LSP_Client.Requests.LSP_Request with
           Kernel          => Self.Kernel,
         File            => File,
         Span            =>
           (first => GPS.LSP_Client.Utilities.Location_To_LSP_Position (From),
            last  => GPS.LSP_Client.Utilities.Location_To_LSP_Position (To)),
         Document_Version  => From.Buffer.Version);

      return GPS.LSP_Client.Requests.Execute
        (Lang, GPS.LSP_Client.Requests.Request_Access (Request));

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end On_Range_Formatting;

   ------------------------
   -- On_Type_Formatting --
   ------------------------

   overriding
   function On_Type_Formatting
     (Self        : in out LSP_Editor_Formatting_Provider;
      From, To    : Editor_Location'Class;
      Cursor_Line : Natural) return Boolean
   is
      pragma Unreferenced (Cursor_Line);
      use LSP_Trigger_Characters_Maps;

      Buffer       : constant Editor_Buffer'Class := From.Buffer;
      Loc          : Editor_Location'Class :=
        New_Location (Buffer, Line (From), Column (From));
      File         : constant Virtual_File := Buffer.File;
      Lang         : Language.Language_Access;
      C            : LSP_Trigger_Characters_Maps.Cursor;
      Params       : Indent_Parameters;
      Indent_Style : Indentation_Kind;
      Request      : On_Type_Formatting_Request_Access;

   begin
      --  XXX Implementation of this subprogram is suitable only when
      --  trigger characters are single character. Need to be checked
      --  with LSP specification and enchanced to support multicharacter
      --  triggers, which is degenerate case for Ada/C/C++.

      Lang := Self.Kernel.Get_Language_Handler.Get_Language_From_File (File);

      C := Self.Lang_To_Trigger_Chars.Find (Lang.Get_Name);

      --  Retrieve the list of known trigger characters from the server
      --  capabilities for Lang
      if not Has_Element (C) then
         declare
            use type GPS.LSP_Client.Language_Servers.Language_Server_Access;

            Server  : GPS.LSP_Client.Language_Servers.Language_Server_Access;
            Options : LSP.Messages.Optional_DocumentOnTypeFormattingOptions;
            Set     : Ada.Strings.Wide_Wide_Maps.Wide_Wide_Character_Set;
            Dummy   : Boolean;

            function First_Character
              (Str : VSS.Strings.Virtual_String) return Wide_Wide_Character;

            ---------------------
            -- First_Character --
            ---------------------

            function First_Character
              (Str : VSS.Strings.Virtual_String) return Wide_Wide_Character
            is
               use type VSS.Strings.Character_Count;

            begin
               pragma Assert (Str.Character_Length = 1);
               return Wide_Wide_Character (Str.At_First_Character.Element);
            end First_Character;

         begin
            Server := GPS.LSP_Module.Get_Language_Server (Lang);
            --  No server for Lang, stop here
            if Server = null then
               return False;
            end if;

            Options :=
              Server.Get_Client.Capabilities.documentOnTypeFormattingProvider;

            if Options.Is_Set then
               Set :=
                 Set
                 or To_Set
                   (First_Character (Options.Value.firstTriggerCharacter));
               if Options.Value.moreTriggerCharacter.Is_Set then
                  for Index
                  in 1 .. Options.Value.moreTriggerCharacter.Value.Length
                  loop
                     Set :=
                       Set
                       or To_Set
                         (First_Character
                            (Options.Value.moreTriggerCharacter.Value.Element
                               (Index)));
                  end loop;
               end if;
            end if;
            Self.Lang_To_Trigger_Chars.Insert (Lang.Get_Name, Set, C, Dummy);
         end;
      end if;

      --  No known trigger characters for Lang, stop here
      if Element (C) = Null_Set then
         return False;
      end if;

      while To > Loc loop
         declare
            Ch   : constant VSS.Characters.Virtual_Character'Base :=
              Loc.Get_Char;
            Text : VSS.Strings.Virtual_String;

         begin
            --  Check whether we have trigger character typed in a line

            if Is_In (Wide_Wide_Character (Ch), Element (C)) then
               Loc := Forward_Char (Loc, 1);
               Get_Indentation_Parameters (Lang, Params, Indent_Style);
               Text.Append (Ch);

               Request :=
                 new On_Type_Formatting_Request'
                   (GPS.LSP_Client.Requests.LSP_Request
                    with
                    Kernel           => Self.Kernel,
                    File             => File,
                    Position         =>
                      GPS.LSP_Client.Utilities.Location_To_LSP_Position (Loc),
                    Text             => Text,
                    Document_Version => Buffer.Version);

               return
                 GPS.LSP_Client.Requests.Execute
                   (Lang, GPS.LSP_Client.Requests.Request_Access (Request));
            end if;

         exception
            when others =>
               null;
         end;

         Loc := Forward_Char (Loc, 1);
      end loop;

      return False;

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end On_Type_Formatting;

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Self : LSP_Editor_Formatting_Provider) return String
   is
      pragma Unreferenced (Self);
   begin
      return Src_Editor_Module.LSP_Formatter_Value;
   end Get_Name;

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
      Request      : Document_Formatting_Request_Access;

   begin
      Editor := Src_Editor_Module.Find_Current_Editor (Kernel);
      Box    := Src_Editor_Module.Get_Source_Box_From_MDI (Editor);
      View   := Get_View (Box);
      Buffer := Get_Buffer (Box);

      if not Get_Editable (View) then
         return Failure;
      end if;

      File := Buffer.Get_Filename;
      Lang := Kernel.Get_Language_Handler.Get_Language_From_File (File);

      Request := new Document_Formatting_Request'
        (GPS.LSP_Client.Requests.LSP_Request with
           Kernel            => Kernel,
         File              => File,
         Editor            => Editor,
         Document_Version  => Buffer.Get_Editor_Buffer.Version);

      if GPS.LSP_Client.Requests.Execute
        (Lang, GPS.LSP_Client.Requests.Request_Access (Request))
      then
         Box.Set_Activity_Progress_Bar_Visibility (True);
         return Success;
      else
         return Failure;
      end if;

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

      GPS.Kernel.Actions.Register_Action
        (Kernel      => Kernel,
         Name        => "format file",
         Command     => new Indentation_File_Command,
         Description => "Format the current file",
         Category    => "Editor",
         Filter      =>
           Lookup_Filter (Kernel, "Writable source editor")
         and Lookup_Filter (Kernel, "Is not Makefile"),
         Icon_Name   => "gps-case-sensitive-symbolic");

      LSP_Provider.Kernel := Kernel;
      Src_Editor_Module.Register_Formatter (LSP_Provider'Access);
   end Register_Module;

end GPS.LSP_Client.Editors.Formatting;
