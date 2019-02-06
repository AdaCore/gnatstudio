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

with LSP.Types;

with GPS.LSP_Client.Utilities;

package body GPS.LSP_Client.Editors is

   ------------------------
   -- Before_Insert_Text --
   ------------------------

   overriding procedure Before_Insert_Text
     (Self      : in out Src_Editor_Handler;
      Location  : GPS.Editors.Editor_Location'Class;
      Text      : String := "";
      From_User : Boolean) is
      pragma Unreferenced (Self, Location, Text, From_User);
   begin
      null;
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
      pragma Unreferenced (Self, Start_Location, End_Location, Offset);
      pragma Unreferenced (From_User);
   begin
      null;
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
         Self.Buffer.Clear;
      end if;
   end Finalize;

   ----------------------------
   -- Get_Did_Change_Message --
   ----------------------------

   overriding function Get_Did_Change_Message
     (Self : in out Src_Editor_Handler)
      return LSP.Messages.DidChangeTextDocumentParams
   is
      Buffer  : constant GPS.Editors.Editor_Buffer'Class :=
                  Self.Kernel.Get_Buffer_Factory.Get
                    (File        => Self.File,
                     Open_Buffer => True,
                     Open_View   => False);
      Changes : LSP.Messages.TextDocumentContentChangeEvent_Vector;

   begin
      Changes.Append
        (LSP.Messages.TextDocumentContentChangeEvent'
           (text   =>
                LSP.Types.To_LSP_String
                  (Buffer.Get_Chars
                     (Buffer.Beginning_Of_Buffer, Buffer.End_Of_Buffer)),
            others => <>));

      return
        (textDocument   =>
           (uri     => GPS.LSP_Client.Utilities.To_URI (Self.File),
            version => LSP.Types.Version_Id (Buffer.Version)),
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
      GPS.LSP_Client.Text_Document_Handlers.Text_Document_Server_Proxy_Access)
   is
      use type
        GPS.LSP_Client.Text_Document_Handlers
          .Text_Document_Server_Proxy_Access;

   begin
      if Self.Server /= Server then
         if Self.Server /= null then
            Self.Server.Text_Document_Did_Close (Self'Unchecked_Access);
         end if;

         Self.Server := Server;

         if Self.Server /= null then
            Self.Server.Text_Document_Did_Open (Self'Unchecked_Access);
         end if;
      end if;
   end Set_Server;

   -------------------
   -- Set_Sync_Kind --
   -------------------

   overriding procedure Set_Sync_Kind
     (Self : in out Src_Editor_Handler;
      To   :
        GPS.LSP_Client.Text_Document_Handlers.Text_Document_Sync_Kinds) is
   begin
      null;
   end Set_Sync_Kind;

end GPS.LSP_Client.Editors;
