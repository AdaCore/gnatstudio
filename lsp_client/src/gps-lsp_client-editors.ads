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
--  Integration with GPS's source editor

private with Ada.Containers.Indefinite_Vectors;

with GNATCOLL.VFS;

private with LSP.Messages;
private with LSP.Types;

with GPS.Editors;
with GPS.Kernel;

with GPS.LSP_Client.Text_Documents;

package GPS.LSP_Client.Editors is

   type Src_Editor_Handler
     (Kernel  : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Manager : not null access
        GPS.LSP_Client.Text_Documents.Text_Document_Manager'Class) is
   limited new GPS.LSP_Client.Text_Documents.Text_Document_Handler
     and GPS.Editors.Editor_Listener with private;

   type Src_Editor_Handler_Access is access all Src_Editor_Handler'Class;

   procedure Initialize
     (Self   : in out Src_Editor_Handler'Class;
      File   : GNATCOLL.VFS.Virtual_File);
   --  Initialize handler and register it in the module.

private

   type Action_Kind_Type is (Insert, Remove);

   type Action (Kind : Action_Kind_Type) is record
      Start_Location : LSP.Messages.Position;
      End_Location   : LSP.Messages.Position;

      case Kind is
         when Insert =>
            Text : LSP.Types.LSP_String;

         when Remove =>
            null;
      end case;
   end record;

   package Action_Vectors is
     new Ada.Containers.Indefinite_Vectors (Positive, Action);

   type Src_Editor_Handler
     (Kernel  : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Manager : not null access
        GPS.LSP_Client.Text_Documents.Text_Document_Manager'Class) is
   limited new GPS.LSP_Client.Text_Documents.Text_Document_Handler
     and GPS.Editors.Editor_Listener
   with record
      File    : GNATCOLL.VFS.Virtual_File;
      Actions : Action_Vectors.Vector;
   end record;

   overriding function File
     (Self : Src_Editor_Handler) return GNATCOLL.VFS.Virtual_File;
   --  Returns name of the file processed by this handler.

   overriding function Get_Did_Change_Message
     (Self : in out Src_Editor_Handler;
      Mode : GPS.LSP_Client.Text_Documents.Text_Document_Sync_Kind_Type)
      return LSP.Messages.DidChangeTextDocumentParams;
   --  Returns message to be send to the server. Called by server manager
   --  when it is ready to send update to the server. Mode is active text
   --  synchronization mode.

   overriding procedure Before_Insert_Text
     (Self      : in out Src_Editor_Handler;
      Location  : GPS.Editors.Editor_Location'Class;
      Text      : String := "";
      From_User : Boolean);
   --  Append edit command into internal buffer, but without sending a
   --  notification to the language server. Notification will be sent later
   --  by After_Insert_Text subprogram to have complete text of the document
   --  in case of full text document synchronization mode.

   overriding procedure Before_Delete_Range
     (Self           : in out Src_Editor_Handler;
      Start_Location : GPS.Editors.Editor_Location'Class;
      End_Location   : GPS.Editors.Editor_Location'Class;
      Offset         : Integer;
      From_User      : Boolean);
   --  Append edit command into internal buffer, but without sending a
   --  notification to the language server. Notification will be sent later
   --  by After_Delete_Range subprogram to have complete text of the document
   --  in case of full text document synchronization mode.

   overriding procedure After_Insert_Text
     (Self            : in out Src_Editor_Handler;
      Cursor_Location : GPS.Editors.Editor_Location'Class;
      From_User       : Boolean);
   --  Send DidChangeTextDocument notification to language server.

   overriding procedure After_Delete_Range
     (Self            : in out Src_Editor_Handler;
      Cursor_Location : GPS.Editors.Editor_Location'Class;
      From_User       : Boolean) renames After_Insert_Text;
   --  Send DidChangeTextDocument notification to language server.

   overriding procedure Finalize (Self : in out Src_Editor_Handler);

   overriding procedure Destroy (Self : in out Src_Editor_Handler)
                                 renames Finalize;

   overriding procedure File_Closed
     (Self : in out Src_Editor_Handler;
      File : GNATCOLL.VFS.Virtual_File);
   --  Called when the file has been closed

   overriding procedure File_Renamed
     (Self : in out Src_Editor_Handler;
      From : GNATCOLL.VFS.Virtual_File;
      To   : GNATCOLL.VFS.Virtual_File);
   --  Called when the file has been renamed

end GPS.LSP_Client.Editors;
