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

with GPS.Editors;
with GPS.LSP_Client.Utilities;

package body GPS.LSP_Client.Editors is

   ----------
   -- File --
   ----------

   overriding function File
     (Self : Src_Editor_Handler) return GNATCOLL.VFS.Virtual_File is
   begin
      return Self.File;
   end File;

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
