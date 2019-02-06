------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2019, AdaCore                       --
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

with Basic_Types;
with GPS.LSP_Client.Utilities;

package body GPS.LSP_Client.Editors is

   use type GPS.LSP_Client.Text_Documents.Text_Document_Server_Proxy_Access;

   -----------------------
   -- After_Insert_Text --
   -----------------------

   overriding procedure After_Insert_Text
     (Self            : in out Src_Editor_Handler;
      Cursor_Location : GPS.Editors.Editor_Location'Class;
      From_User       : Boolean)
   is
      pragma Unreferenced (Cursor_Location, From_User);

   begin
      if Self.Server /= null then
         Self.Server.Send_Text_Document_Did_Change (Self'Unchecked_Access);
      end if;
   end After_Insert_Text;

   ------------------------
   -- Before_Insert_Text --
   ------------------------

   overriding procedure Before_Insert_Text
     (Self      : in out Src_Editor_Handler;
      Location  : GPS.Editors.Editor_Location'Class;
      Text      : String := "";
      From_User : Boolean)
   is
      pragma Unreferenced (From_User);

   begin
      if Self.Server /= null then
         Self.Actions.Append
           ((Kind           => Insert,
             Start_Location =>
               (LSP.Types.Line_Number (Location.Line - 1),
                GPS.LSP_Client.Utilities.Visible_Column_To_UTF_16_Offset
                  (Location.Column)),
             End_Location   =>
               (LSP.Types.Line_Number (Location.Line - 1),
                GPS.LSP_Client.Utilities.Visible_Column_To_UTF_16_Offset
                  (Location.Column)),
             Text           => LSP.Types.To_LSP_String (Text)));
      end if;
   end Before_Insert_Text;

   -------------------------
   -- Before_Delete_Range --
   -------------------------

   overriding procedure Before_Delete_Range
     (Self           : in out Src_Editor_Handler;
      Start_Location : GPS.Editors.Editor_Location'Class;
      End_Location   : GPS.Editors.Editor_Location'Class;
      Offset         : Integer;
      From_User      : Boolean)
   is
      pragma Unreferenced (Offset, From_User);

      use type Basic_Types.Visible_Column_Type;

   begin
      if Self.Server /= null then
         Self.Actions.Append
           ((Kind           => Remove,
             Start_Location =>
               (LSP.Types.Line_Number (Start_Location.Line - 1),
                GPS.LSP_Client.Utilities.Visible_Column_To_UTF_16_Offset
                  (Start_Location.Column)),
             End_Location   =>
               (LSP.Types.Line_Number (End_Location.Line - 1),
                GPS.LSP_Client.Utilities.Visible_Column_To_UTF_16_Offset
                  (End_Location.Column + 1))));  --  ??? Need to be checked
         --  after correct implementation of Visible_Column_To_UTF_16_Offset
      end if;
   end Before_Delete_Range;

   ----------
   -- File --
   ----------

   overriding function File
     (Self : Src_Editor_Handler) return GNATCOLL.VFS.Virtual_File is
   begin
      return Self.Buffer.Element.File;
   end File;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Src_Editor_Handler) is
   begin
      if not Self.Buffer.Is_Empty then
         Self.Set_Server (null);
         Self.Manager.Unregister (Self'Unchecked_Access);
         Self.Buffer.Clear;
      end if;
   end Finalize;

   ----------------------------
   -- Get_Did_Change_Message --
   ----------------------------

   overriding function Get_Did_Change_Message
     (Self : in out Src_Editor_Handler;
      Mode : GPS.LSP_Client.Text_Documents.Text_Document_Sync_Kind_Type)
      return LSP.Messages.DidChangeTextDocumentParams
   is
      Changes : LSP.Messages.TextDocumentContentChangeEvent_Vector;

   begin
      case Mode is
      when GPS.LSP_Client.Text_Documents.Full =>
         Changes.Append
           (LSP.Messages.TextDocumentContentChangeEvent'
              (text   =>
                    LSP.Types.To_LSP_String
                 (Self.Buffer.Element.Get_Chars
                      (Self.Buffer.Element.Beginning_Of_Buffer,
                       Self.Buffer.Element.End_Of_Buffer)),
               others => <>));

      when GPS.LSP_Client.Text_Documents.Incremental =>
         for Action of Self.Actions loop
            case Action.Kind is
            when Insert =>
               Changes.Append
                 (LSP.Messages.TextDocumentContentChangeEvent'
                    (span   => (Is_Set => True,
                                Value  =>
                                  (first => Action.Start_Location,
                                   last  => Action.End_Location)),
                     text   => Action.Text,
                     others => <>));

            when Remove =>
               Changes.Append
                 (LSP.Messages.TextDocumentContentChangeEvent'
                    (span   => (Is_Set => True,
                                Value  =>
                                  (first => Action.Start_Location,
                                   last  => Action.End_Location)),
                     others => <>));
            end case;
         end loop;
      end case;

      Self.Actions.Clear;

      return
        (textDocument   =>
           (uri     => GPS.LSP_Client.Utilities.To_URI (Self.File),
            version => LSP.Types.Version_Id (Self.Buffer.Element.Version)),
         contentChanges => Changes);
   end Get_Did_Change_Message;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : in out Src_Editor_Handler'Class;
      Buffer : GPS.Editors.Editor_Buffer'Class) is
   begin
      Self.Buffer.Replace_Element (Buffer);
   end Initialize;

   ----------------
   -- Set_Server --
   ----------------

   overriding procedure Set_Server
     (Self   : in out Src_Editor_Handler;
      Server :
        GPS.LSP_Client.Text_Documents.Text_Document_Server_Proxy_Access) is
   begin
      if Self.Server /= Server then
         if Self.Server /= null then
            Self.Server.Send_Text_Document_Did_Close (Self'Unchecked_Access);
         end if;

         Self.Server := Server;

         if Self.Server /= null then
            Self.Server.Send_Text_Document_Did_Open (Self'Unchecked_Access);
         end if;
      end if;
   end Set_Server;

end GPS.LSP_Client.Editors;
