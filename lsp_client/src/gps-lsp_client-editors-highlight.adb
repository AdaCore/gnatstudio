------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2023, AdaCore                  --
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

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Basic_Types;                use Basic_Types;
with Glib.Main;                  use Glib.Main;
with Glib;                       use Glib;
with GNATCOLL.JSON;
with GNATCOLL.Projects;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GPS.Default_Styles;         use GPS.Default_Styles;
with GPS.Kernel.Contexts;        use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Messages.Simple; use GPS.Kernel.Messages.Simple;
with GPS.Kernel.Messages;        use GPS.Kernel.Messages;
with GPS.Kernel.Modules;         use GPS.Kernel.Modules;
with GPS.LSP_Client.Requests.Document_Highlight;
use GPS.LSP_Client.Requests.Document_Highlight;
with GPS.LSP_Client.Requests;    use GPS.LSP_Client.Requests;
with GPS.LSP_Client.Utilities;   use GPS.LSP_Client.Utilities;
with GUI_Utils;                  use GUI_Utils;
with Language;                   use Language;
with Language_Handlers;          use Language_Handlers;
with GPS.Editors; use GPS.Editors;

package body GPS.LSP_Client.Editors.Highlight is

   Me          : constant Trace_Handle :=
     Create ("GPS.LSP.DOCUMENT_HIGHLIGHT");
   Me_Advanced : constant Trace_Handle :=
     Create ("GPS.LSP.DOCUMENT_HIGHLIGHT.ADVANCED", Off);

   type GPS_LSP_Document_Highlight_Request is
     new Abstract_Document_Highlight_Request with null record;
   type GPS_LSP_Document_Highlight_Request_Access is
     access all GPS_LSP_Document_Highlight_Request'Class;

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Document_Highlight_Request;
      Result : LSP.Messages.DocumentHighlight_Vector);

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Document_Highlight_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   overriding procedure On_Rejected
     (Self : in out GPS_LSP_Document_Highlight_Request);

   type On_Location_Changed is new File_Location_Hooks_Function with
     null record;
   overriding procedure Execute
     (Self         : On_Location_Changed;
      Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Line, Column : Integer;
      Project      : GNATCOLL.Projects.Project_Type);
   --  Called when the current editor location changes.
   --  Check if the cursor is on an entity: if yes, try to auto-highlight it,
   --  unless we were already highlighting it.

   type On_File_Edited is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when the focused editor changes.
   --  Used to cleanup all the auto-highlight messages.

   Document_Highlight_Message_Category : constant String := "Autohighlight";
   Document_Highlight_Message_Flags    : constant Message_Flags :=
     (Editor_Side => True,
      Locations   => False,
      Editor_Line => True);

   type Highlighting_Context_Type is record
      Line   : Integer := 0;
      Column : Visible_Column_Type := 0;
      Word   : Unbounded_String := Null_Unbounded_String;
   end record;

   Null_Highlighting_Context : constant Highlighting_Context_Type :=
     Highlighting_Context_Type'
       (Line   => 0,
        Column => 0,
        Word   => Null_Unbounded_String);

   type Document_Highlight_Module_ID_Record is
     new Module_ID_Record with record
      Highlighting_Context      : Highlighting_Context_Type;
      --  The current highlighting context.

      Locations                 : LSP.Messages.DocumentHighlight_Vector;
      --  The locations to highlight.

      Loc_Index                 : Positive := 1;
      --  The index of the location to highlight. Used by the Idle/Timeout
      --  function.

      Highlighter_Source_ID     : Glib.Main.G_Source_Id :=
        Glib.Main.No_Source_Id;
      --  The ID of the Idle/Timeout function that highlights the locations
      --  in a non-blocking way.

      Textual_Search_Start_Line : Integer := 1;
      --  The line from where the textual search should start to find the
      --  next occurrence.

      Textual_Search_Start_Col  : Visible_Column_Type := 1;
      --  The column from where the textual search should start to find the
      --  next occurrence.
   end record;
   type Document_Highlight_Module_ID is
     access all Document_Highlight_Module_ID_Record'Class;

   Module : Document_Highlight_Module_ID;

   type Highlight_Timeout_Parameter_Type is record
      Kernel : Kernel_Handle;
      File   : Virtual_File;
   end record;
   package LSP_Highlight_Sources is
     new Glib.Main.Generic_Sources (Highlight_Timeout_Parameter_Type);

   function LSP_Highlighting_Source
     (Params : Highlight_Timeout_Parameter_Type) return Boolean;
   --  Idle/Timeout function that highlights the locations received through
   --  the LSP.

   function Textual_Highlighting_Source
     (Params : Highlight_Timeout_Parameter_Type) return Boolean;
   --  Idle/Timeout function that searches textually and highlights the next
   --  occurrence of word to highlight.
   --  This function is used as fallback when LSP is not supported for the
   --  current file's language.

   procedure Cleanup_Document_Highlight_Messages
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Cleanup all the auto-highlighting messages.

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Document_Highlight_Request;
      Result : LSP.Messages.DocumentHighlight_Vector)
   is
   begin
      Module.Locations := Result;
      Module.Loc_Index := 1;

      if Result.Is_Empty then
         Trace (Me_Advanced, "No locations to highlight");
         return;
      end if;

      declare
         Timeout_Params : constant Highlight_Timeout_Parameter_Type :=
           Highlight_Timeout_Parameter_Type'
             (Kernel => Self.Kernel,
              File   => Self.File);
      begin
         Module.Highlighter_Source_ID := LSP_Highlight_Sources.Idle_Add
           (LSP_Highlighting_Source'Access, Timeout_Params);
      end;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Document_Highlight_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value)
   is
      pragma Unreferenced (Code, Self);
   begin
      Trace (Me, "Error received on hover request: " & Message);
      Trace (Me, "Data: " & GNATCOLL.JSON.Write (Data));
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out GPS_LSP_Document_Highlight_Request) is null;

   -----------------------------------------
   -- Cleanup_Document_Highlight_Messages --
   -----------------------------------------

   procedure Cleanup_Document_Highlight_Messages
     (Kernel : not null access Kernel_Handle_Record'Class) is
   begin
      Kernel.Get_Messages_Container.Remove_Category
        (Category => Document_Highlight_Message_Category,
         Flags    => Document_Highlight_Message_Flags);
   end Cleanup_Document_Highlight_Messages;

   --------------------------
   -- On_Highlight_Timeout --
   --------------------------

   function LSP_Highlighting_Source
     (Params : Highlight_Timeout_Parameter_Type) return Boolean is
   begin
      if Module.Loc_Index > Integer (Module.Locations.Length) then
         Module.Highlighter_Source_ID := Glib.Main.No_Source_Id;
         return False;
      end if;

      declare
         use type LSP.Messages.DocumentHighlightKind;
         use type Basic_Types.Visible_Column_Type;

         Loc     : constant LSP.Messages.DocumentHighlight :=
           Module.Locations (Module.Loc_Index);
         Kind    : constant LSP.Messages.DocumentHighlightKind :=
           (if Loc.kind.Is_Set then Loc.kind.Value else LSP.Messages.Read);

         Holder  : constant GPS.Editors.Controlled_Editor_Buffer_Holder :=
           Params.Kernel.Get_Buffer_Factory.Get_Holder (File => Params.File);
         From    : constant GPS.Editors.Editor_Location'Class :=
           GPS.LSP_Client.Utilities.LSP_Position_To_Location
             (Holder.Editor, Loc.span.first);
         To      : constant GPS.Editors.Editor_Location'Class :=
           GPS.LSP_Client.Utilities.LSP_Position_To_Location
             (Holder.Editor, Loc.span.last);

         Message : Simple_Message_Access;
      begin
         Message := Create_Simple_Message
           (Get_Messages_Container (Params.Kernel),
            Category                 => Document_Highlight_Message_Category,
            File                     => Params.File,
            Line                     => From.Line,
            Column                   => From.Column,
            Text                     => "",
            Importance               => Unspecified,
            Flags                    => Document_Highlight_Message_Flags,
            Allow_Auto_Jump_To_First => False);

         GPS.Kernel.Messages.Set_Highlighting
           (Self   => Message,
            Style  => (if Kind = LSP.Messages.Write then
                          Editor_Ephemeral_Highlighting_Smart
                       else
                          Editor_Ephemeral_Highlighting_Simple),
            Length => Highlight_Length (To.Column - From.Column));

         Module.Loc_Index := Module.Loc_Index + 1;

         return True;
      end;
   end LSP_Highlighting_Source;

   ---------------------------------
   -- Textual_Highlighting_Source --
   ---------------------------------

   function Textual_Highlighting_Source
     (Params : Highlight_Timeout_Parameter_Type) return Boolean
   is
      Buffer               : constant Editor_Buffer'Class :=
        Params.Kernel.Get_Buffer_Factory.Get
          (File => Params.File, Open_View => False, Open_Buffer => False);
      Search_Start_Loc     : constant Editor_Location'Class :=
        Buffer.New_Location
          (Line   => Module.Textual_Search_Start_Line,
           Column => Module.Textual_Search_Start_Col);
      Occurrence_Start_Loc : Editor_Location'Class := Search_Start_Loc;
      Occurrence_End_Loc   : Editor_Location'Class := Search_Start_Loc;
      Success              : Boolean := False;
   begin
      --  Search the next occurrence
      Search_Start_Loc.Search
        (Pattern           => To_String (Module.Highlighting_Context.Word),
         Whole_Word        => True,
         Dialog_On_Failure => False,
         Starts            => Occurrence_Start_Loc,
         Ends              => Occurrence_End_Loc,
         Success           => Success);

      --  If an occurrence has been found, highlight it and return True to
      --  call that function later. When not found, return False to stop
      --  searching.

      if Success then
         declare
            use type Basic_Types.Visible_Column_Type;

            Message      : Simple_Message_Access;
            Start_Line   : constant Natural := Occurrence_Start_Loc.Line;
            End_Line     : constant Natural := Occurrence_End_Loc.Line;
            Start_Column : constant Visible_Column_Type :=
              Occurrence_Start_Loc.Column;
            End_Column   : constant Visible_Column_Type :=
              Occurrence_End_Loc.Column;
         begin
            Message := Create_Simple_Message
              (Get_Messages_Container (Params.Kernel),
               Category                 => Document_Highlight_Message_Category,
               File                     => Params.File,
               Line                     => Start_Line,
               Column                   => Start_Column,
               Text                     => "",
               Importance               => Unspecified,
               Flags                    => Document_Highlight_Message_Flags,
               Allow_Auto_Jump_To_First => False);

            GPS.Kernel.Messages.Set_Highlighting
              (Self   => Message,
               Style  => Editor_Ephemeral_Highlighting_Simple,
               Length => Highlight_Length (End_Column - Start_Column));

            Module.Textual_Search_Start_Line := End_Line;
            Module.Textual_Search_Start_Col := End_Column;

            return True;
         end;
      else
         Module.Highlighter_Source_ID := Glib.Main.No_Source_Id;
         return False;
      end if;
   end Textual_Highlighting_Source;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self         : On_Location_Changed;
      Kernel       : not null access Kernel_Handle_Record'Class;
      File         : Virtual_File;
      Line, Column : Integer;
      Project      : GNATCOLL.Projects.Project_Type)
   is
      Request                  : GPS_LSP_Document_Highlight_Request_Access;
      Context                  : constant Selection_Context :=
        Kernel.Get_Current_Context;
      Lang                     : constant Language_Access :=
        Kernel.Get_Language_Handler.Get_Language_From_File (File);
      Current_Word             : constant String := Entity_Name_Information
        (Context);
      Request_Success          : Boolean := False;
      New_Highlighting_Context : constant Highlighting_Context_Type :=
        Highlighting_Context_Type'
          (Line   => Line,
           Column => Entity_Column_Information (Context),
           Word   => To_Unbounded_String (Current_Word));
   begin
      if Line = 0 then
         --  This is a special line => do nothing
         return;
      end if;

      --  If we are not an entity, cleanup the previous highlighting messages
      --  and return immediately.

      if Current_Word = "" then
         Cleanup_Document_Highlight_Messages (Kernel);
         Module.Highlighting_Context := Null_Highlighting_Context;
         return;
      end if;

      --  If we are on an new entity, try to highlight it using the LSP first,
      --  and, if not available, using a simple textual search.

      if Module.Highlighting_Context /= New_Highlighting_Context then
         Cleanup_Document_Highlight_Messages (Kernel);

         --  Remove the auto-highlight idle/timeout function if any.
         if Module.Highlighter_Source_ID /= Glib.Main.No_Source_Id then
            Glib.Main.Remove (Module.Highlighter_Source_ID);
            Module.Highlighter_Source_ID := Glib.Main.No_Source_Id;
         end if;

         Module.Highlighting_Context := New_Highlighting_Context;

         declare
            Holder   : constant Controlled_Editor_Buffer_Holder :=
              Kernel.Get_Buffer_Factory.Get_Holder (File);
            Location : constant GPS.Editors.Editor_Location'Class :=
              Holder.Editor.New_Location (Line, Visible_Column_Type (Column));
         begin
            --  Return immediately if the location is not valid
            if Location = Nil_Editor_Location then
               Trace
                 (Me_Advanced,
                  "Location is invalid: avoid auto-highlighting");
               return;
            end if;

            Request := new GPS_LSP_Document_Highlight_Request'
              (LSP_Request with
               Kernel   => Kernel_Handle (Kernel),
               File     => File,
               Position => Location_To_LSP_Position (Location));
         end;

         Request_Success := GPS.LSP_Client.Requests.Execute
           (Lang, Request_Access (Request));

         --  The request failed to execute (no LSP, or request not supported):
         --  perform the highlighting based on a textual search.

         if not Request_Success then
            declare
               Timeout_Params : constant Highlight_Timeout_Parameter_Type :=
                 Highlight_Timeout_Parameter_Type'
                   (Kernel => Kernel_Handle (Kernel),
                    File   => File);
            begin
               Module.Highlighter_Source_ID := LSP_Highlight_Sources.Idle_Add
                 (Textual_Highlighting_Source'Access, Timeout_Params);
               Module.Textual_Search_Start_Line := 1;
               Module.Textual_Search_Start_Col := 1;
            end;
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File) is
   begin
      Cleanup_Document_Highlight_Messages (Kernel);
   end Execute;

   --------------
   -- Register --
   --------------

   procedure Register (Kernel : Kernel_Handle) is
   begin
      if Me.Is_Active then
         Module := new Document_Highlight_Module_ID_Record;
         Register_Module
           (Module      => Module,
            Kernel      => Kernel,
            Module_Name => "LSP_Document_Highlight");
         Location_Changed_Hook.Add_Debounce (new On_Location_Changed);
         File_Edited_Hook.Add (new On_File_Edited);
      end if;
   end Register;

end GPS.LSP_Client.Editors.Highlight;
