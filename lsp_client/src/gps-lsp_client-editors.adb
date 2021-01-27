------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2019-2021, AdaCore                  --
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

with GNATCOLL.VFS; use GNATCOLL.VFS;

with Gtk.Widget;                    use Gtk.Widget;

with LSP.Types;
with VSS.Strings.Conversions;

with Language;  use Language;
with GPS.LSP_Client.Utilities;
with GPS.Kernel;   use GPS.Kernel;
with GPS.Editors;  use GPS.Editors;
with GPS.LSP_Module;
with GPS.LSP_Clients; use GPS.LSP_Clients;
with GPS.LSP_Client.Language_Servers; use GPS.LSP_Client.Language_Servers;

package body GPS.LSP_Client.Editors is

   function Get_Buffer (Self : Src_Editor_Handler) return Editor_Buffer'Class;
   --  Convenience function to retrieve the editor buffer;
   --  performs a lookup in the Kernel's Editor_Factory.

   function Get_Server
     (Self : Src_Editor_Handler) return Language_Server_Access;
   --  Return the language server that should be associated with Self if it
   --  exists, null if not.

   function Get_Client
     (Self : Src_Editor_Handler)
      return GPS.LSP_Clients.LSP_Client_Access;
   --  Return the  client associated to Self, or null

   ----------------
   -- Get_Buffer --
   ----------------

   function Get_Buffer
     (Self : Src_Editor_Handler) return Editor_Buffer'Class is
   begin
      return Self.Kernel.Get_Buffer_Factory.Get
        (File        => Self.File,
         Open_Buffer => False,
         Open_View   => False);
   end Get_Buffer;

   ----------------
   -- Get_Server --
   ----------------

   function Get_Server
     (Self : Src_Editor_Handler) return Language_Server_Access
   is
      Lang : Language_Access;
   begin
      if Self.File = No_File then
         return null;
      end if;
      Lang := Get_Buffer (Self).Get_Language;
      if Lang = null then
         return null;
      end if;
      return GPS.LSP_Module.Get_Language_Server (Language => Lang);
   end Get_Server;

   ----------------
   -- Get_Client --
   ----------------

   function Get_Client
     (Self : Src_Editor_Handler)
      return GPS.LSP_Clients.LSP_Client_Access
   is
      Server : constant Language_Server_Access := Get_Server (Self);
   begin
      if Server = null then
         return null;
      else
         return Server.Get_Client;
      end if;
   end Get_Client;

   ------------------------
   -- After_Delete_Range --
   ------------------------

   overriding procedure After_Delete_Range
     (Self      : in out Src_Editor_Handler;
      From_User : Boolean)
   is
      Server : constant Language_Server_Access := Get_Server (Self);
      Client : constant LSP_Client_Access := Get_Client (Self);
   begin
      if Server = null then
         return;
      end if;

      if Client /= null then
         Client.Send_Text_Document_Did_Change (Self'Unchecked_Access);
      end if;
   end After_Delete_Range;

   -----------------------
   -- After_Insert_Text --
   -----------------------

   overriding procedure After_Insert_Text
     (Self      : in out Src_Editor_Handler;
      Location  : Editor_Location'Class;
      Text      : String := "";
      From_User : Boolean)
   is
      pragma Unreferenced (From_User);
      Client : constant LSP_Client_Access := Get_Client (Self);
   begin
      if Get_Server (Self) = null then
         return;
      end if;

      Self.Actions.Append
        ((Kind           => Insert,
          Start_Location =>
            GPS.LSP_Client.Utilities.Location_To_LSP_Position (Location),
          End_Location   =>
            GPS.LSP_Client.Utilities.Location_To_LSP_Position (Location),
          Text           => VSS.Strings.Conversions.To_Virtual_String (Text)));

      if Client /= null then
         Client.Send_Text_Document_Did_Change (Self'Unchecked_Access);
      end if;
   end After_Insert_Text;

   -------------------------
   -- Before_Delete_Range --
   -------------------------

   overriding procedure Before_Delete_Range
     (Self           : in out Src_Editor_Handler;
      Start_Location : GPS.Editors.Editor_Location'Class;
      End_Location   : GPS.Editors.Editor_Location'Class;
      From_User      : Boolean)
   is
      pragma Unreferenced (From_User);
   begin
      if Get_Server (Self) = null then
         return;
      end if;

      Self.Actions.Append
        ((Kind           => Remove,
          Start_Location =>
            GPS.LSP_Client.Utilities.Location_To_LSP_Position (Start_Location),
          End_Location   =>
            GPS.LSP_Client.Utilities.Location_To_LSP_Position (End_Location)));

      --  We send the notification in After_Delete_Range, since it needs
      --  the actual content in case the "full" mode is used.
   end Before_Delete_Range;

   ----------
   -- File --
   ----------

   overriding function File
     (Self : Src_Editor_Handler) return GNATCOLL.VFS.Virtual_File is
   begin
      return Self.File;
   end File;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Src_Editor_Handler) is
   begin
      null;
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

      Buffer : constant Editor_Buffer'Class := Get_Buffer (Self);
   begin
      case Mode is
      when GPS.LSP_Client.Text_Documents.Full =>
         Changes.Append
           (LSP.Messages.TextDocumentContentChangeEvent'
              (text   =>
                    LSP.Types.To_LSP_String
                 (Buffer.Get_Chars_U
                      (Buffer.Beginning_Of_Buffer,
                       Buffer.End_Of_Buffer)),
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
                     text   => LSP.Types.To_LSP_String (Action.Text),
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
            version => (True, LSP.Types.LSP_Number (Buffer.Version))),
         contentChanges => Changes);
   end Get_Did_Change_Message;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self   : in out Src_Editor_Handler'Class;
      File   : GNATCOLL.VFS.Virtual_File) is
   begin
      Self.File := File;
   end Initialize;

   ------------------
   -- File_Renamed --
   ------------------

   overriding procedure File_Renamed
     (Self : in out Src_Editor_Handler;
      From : GNATCOLL.VFS.Virtual_File;
      To   : GNATCOLL.VFS.Virtual_File)
   is
      pragma Unreferenced (From);
   begin
      Self.File := To;
      --  File_Edited_Hook is always called after File_Renamed, so the
      --  Did_Open will be sent there.
   end File_Renamed;

end GPS.LSP_Client.Editors;
