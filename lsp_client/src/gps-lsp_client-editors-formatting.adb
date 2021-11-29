------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2020-2021, AdaCore                  --
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
with Ada.Strings.Maps;                        use Ada.Strings.Maps;

with GNATCOLL.Traces;                         use GNATCOLL.Traces;
with GNATCOLL.VFS;                            use GNATCOLL.VFS;

with Gtkada.MDI;

with GPS.Editors;                             use GPS.Editors;
with GPS.Editors.Line_Information;            use GPS.Editors.Line_Information;
with GPS.LSP_Client.Edit_Workspace;
with GPS.LSP_Client.Utilities;
with GPS.Kernel.Actions;
with GPS.Kernel.MDI;
with GPS.Kernel.Modules;                      use GPS.Kernel.Modules;

with GUI_Utils;
with Language;                                use Language;
with Src_Editor_Box;                          use Src_Editor_Box;
with Src_Editor_Buffer;                       use Src_Editor_Buffer;
with Src_Editor_Module;
with Src_Editor_View;                         use Src_Editor_View;
with Commands;                                use Commands;
with Commands.Interactive;                    use Commands.Interactive;

with GPS.LSP_Client.Language_Servers;
with GPS.LSP_Module;
with LSP.Types;

with GPS.LSP_Client.Requests.Document_Formatting;
with GPS.LSP_Client.Requests.Range_Formatting;
with GPS.LSP_Client.Requests.On_Type_Formatting;

package body GPS.LSP_Client.Editors.Formatting is

   Me : constant Trace_Handle := Create ("GPS.LSP.FORMATTING.ADVANCED", Off);
   --  Logging trace

   LSP_FORMATTING_ON : constant Trace_Handle := Create
     ("GPS.LSP.FORMATTING", On);
   --  Enable/disable LSP formatting

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

   overriding procedure On_Result_Message
     (Self   : in out Document_Formatting_Request;
      Result : LSP.Messages.TextEdit_Vector);

   -- Range_Formatting_Request --

   type Range_Formatting_Request is
     new GPS.LSP_Client.Requests.Range_Formatting.
       Abstract_Range_Formatting_Request with null record;
   type Range_Formatting_Request_Access is access all Range_Formatting_Request;
   --  Corresponding LSP request

   overriding procedure On_Result_Message
     (Self   : in out Range_Formatting_Request;
      Result : LSP.Messages.TextEdit_Vector);

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

   -- LSP_Editor_Formatting_Provider --

   package Triggers_Maps is
     new Ada.Containers.Indefinite_Ordered_Maps
       (String, Ada.Strings.Maps.Character_Set);

   type LSP_Editor_Formatting_Provider is
     new GPS.Editors.Editor_Formatting_Provider with record
      Kernel   : Kernel_Handle;
      Triggers : Triggers_Maps.Map;
   end record;

   overriding function Format_Section
     (Self     : in out LSP_Editor_Formatting_Provider;
      From, To : Editor_Location'Class;
      Force    : Boolean := False)
      return Boolean;

   overriding function On_Type_Formatting
     (Self     : in out LSP_Editor_Formatting_Provider;
      From, To : Editor_Location'Class)
      return Boolean;

   -- LSP_Formatting_Module_Id_Record --

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
     (Self   : in out Document_Formatting_Request;
      Result : LSP.Messages.TextEdit_Vector)
   is
      Buffer : constant Editor_Buffer'Class :=
        Self.Kernel.Get_Buffer_Factory.Get
          (File      => Self.File,
           Open_View => False,
           Focus     => False);
      Map    : LSP.Messages.TextDocumentEdit_Maps.Map;
      Dummy  : Boolean;
   begin

      if Buffer = Nil_Editor_Buffer then
         --  Buffer can be closed
         return;
      end if;

      if Buffer.Version /= Self.Document_Version then
         Trace
           (Me,
            "Document_Formatting canceled for " & (+Base_Name (Buffer.File)) &
              " ver." & Integer'Image (Buffer.Version) & ", data ver." &
              Integer'Image (Self.Document_Version));
         return;
      end if;

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
            Locations_Message_Markup => "",
            Error                    => Dummy);
      end;

   exception
      when E : others =>
         Trace (Me, E);
   end On_Result_Message;

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
            Locations_Message_Markup => "",
            Error                    => Dummy);
      end;

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
      File         : constant Virtual_File := From.Buffer.File;
      Lang         : Language.Language_Access;
      Params       : Indent_Parameters;
      Indent_Style : Indentation_Kind;
      Request      : Range_Formatting_Request_Access;

   begin
      if not LSP_FORMATTING_ON.Is_Active then
         return False;
      end if;

      Lang := Self.Kernel.Get_Language_Handler.Get_Language_From_File (File);
      Get_Indentation_Parameters (Lang, Params, Indent_Style);

      Request := new Range_Formatting_Request'
        (GPS.LSP_Client.Requests.LSP_Request with
           Kernel          => Self.Kernel,
           File            => File,
           Span            =>
           (first => GPS.LSP_Client.Utilities.Location_To_LSP_Position (From),
            last  => GPS.LSP_Client.Utilities.Location_To_LSP_Position (To)),
         Indentation_Level => Params.Indent_Level,
         Use_Tabs          => Params.Use_Tabs,
         Document_Version  => From.Buffer.Version);

      return GPS.LSP_Client.Requests.Execute
        (Lang, GPS.LSP_Client.Requests.Request_Access (Request));

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Format_Section;

   ------------------------
   -- On_Type_Formatting --
   ------------------------

   overriding function On_Type_Formatting
     (Self     : in out LSP_Editor_Formatting_Provider;
      From, To : Editor_Location'Class)
      return Boolean
   is
      use Triggers_Maps;

      Buffer       : constant Editor_Buffer'Class := From.Buffer;
      Loc          : Editor_Location'Class :=
        New_Location (Buffer, Line (From), Column (From));
      File         : constant Virtual_File := Buffer.File;
      Lang         : Language.Language_Access;
      C            : Triggers_Maps.Cursor;
      Params       : Indent_Parameters;
      Indent_Style : Indentation_Kind;
      Request      : On_Type_Formatting_Request_Access;

   begin
      if not LSP_FORMATTING_ON.Is_Active then
         return False;
      end if;

      Lang := Self.Kernel.Get_Language_Handler.Get_Language_From_File (File);
      C    := Self.Triggers.Find (Lang.Get_Name);

      if not Has_Element (C) then
         declare
            use type GPS.LSP_Client.Language_Servers.Language_Server_Access;

            Server  : GPS.LSP_Client.Language_Servers.Language_Server_Access;
            Options : LSP.Messages.Optional_DocumentOnTypeFormattingOptions;
            Set     : Ada.Strings.Maps.Character_Set :=
              Ada.Strings.Maps.Null_Set;
            Dummy   : Boolean;

            function To_Char (Str : LSP.Types.LSP_String) return Character;

            function To_Char (Str : LSP.Types.LSP_String) return Character is
               S : constant String := LSP.Types.To_UTF_8_String (Str);
            begin
               pragma Assert (S'Length = 1);
               return S (S'First);
            end To_Char;

         begin
            Server := GPS.LSP_Module.Get_Language_Server (Lang);
            if Server = null then
               return False;
            end if;

            Options := Server.Get_Client.Capabilities.
              documentOnTypeFormattingProvider;

            if Options.Is_Set then
               Set := Set or To_Set
                 (To_Char (Options.Value.firstTriggerCharacter));
               if Options.Value.moreTriggerCharacter.Is_Set then
                  for Index in 1 .. Natural
                    (Options.Value.moreTriggerCharacter.Value.Length)
                  loop
                     Set := Set or To_Set
                       (To_Char (Options.Value.moreTriggerCharacter.
                            Value.Element (Index)));
                  end loop;
               end if;
            end if;
            Self.Triggers.Insert (Lang.Get_Name, Set, C, Dummy);
         end;
      end if;

      if Element (C) = Null_Set then
         return False;
      end if;

      while To > Loc loop
         declare
            Ch : Integer;
         begin
            Ch := Get_Char (Loc);
            --  Check whether we have trigger character typed in a line
            if Ch < 256 and then Is_In (Character'Val (Ch), Element (C)) then
               Loc := Forward_Char (Loc, 1);
               Get_Indentation_Parameters (Lang, Params, Indent_Style);

               Request := new On_Type_Formatting_Request'
                 (GPS.LSP_Client.Requests.LSP_Request with
                  Kernel          => Self.Kernel,
                  File            => File,
                  Position        =>
                    GPS.LSP_Client.Utilities.Location_To_LSP_Position (Loc),
                  Text            =>
                    VSS.Strings.To_Virtual_String
                      ("" & Wide_Wide_Character'Val (Ch)),
                  Indentation_Level => Params.Indent_Level,
                  Use_Tabs          => Params.Use_Tabs,
                  Document_Version  => Buffer.Version);

               return GPS.LSP_Client.Requests.Execute
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
      Params       : Indent_Parameters;
      Indent_Style : Indentation_Kind;

      Request      : Document_Formatting_Request_Access;

   begin
      if not LSP_FORMATTING_ON.Is_Active then
         return Failure;
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
      Get_Indentation_Parameters (Lang, Params, Indent_Style);

      Request := new Document_Formatting_Request'
        (GPS.LSP_Client.Requests.LSP_Request with
           Kernel            => Kernel,
           File              => File,
           Editor            => Editor,
           Indentation_Level => Params.Indent_Level,
           Use_Tabs          => Params.Use_Tabs,
           Document_Version  => Buffer.Get_Editor_Buffer.Version);

      if GPS.LSP_Client.Requests.Execute
        (Lang, GPS.LSP_Client.Requests.Request_Access (Request))
      then
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

      Provider.Kernel := Kernel;
      Src_Editor_Buffer.Set_Formatting_Provider (Provider'Access);

      if LSP_FORMATTING_ON.Is_Active then
         GPS.Kernel.Actions.Register_Action
           (Kernel       => Kernel,
            Name         =>  "format file",
            Command      => new Indentation_File_Command,
            Description  => "Format the current file",
            Category     => "Editor",
            Filter       =>
              Lookup_Filter (Kernel, "Writable source editor") and
                Lookup_Filter (Kernel, "Is not Makefile"),
            Icon_Name    => "gps-case-sensitive-symbolic");
      end if;
   end Register_Module;

end GPS.LSP_Client.Editors.Formatting;
