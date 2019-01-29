------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with Ada.Containers.Hashed_Maps;
with Ada.Unchecked_Deallocation;
with System.Storage_Elements;

with GNATCOLL.VFS;       use GNATCOLL.VFS;

with GPS.Kernel.Hooks;   use GPS.Kernel.Hooks;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with Language;           use Language;
with GPS.LSP_Client.Editors;
with GPS.LSP_Client.Text_Document_Handlers;
with GPS.LSP_Clients;

package body GPS.LSP_Module is

   function Hash (Value : Language_Access) return Ada.Containers.Hash_Type;

   package Client_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Language_Access,
      Element_Type    => LSP_Clients.LSP_Client_Access,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => LSP_Clients."=");

   package Text_Document_Handler_Maps is
     new Ada.Containers.Hashed_Maps
       (GNATCOLL.VFS.Virtual_File,
        GPS.LSP_Client.Text_Document_Handlers.Text_Document_Handler_Access,
        GNATCOLL.VFS.Full_Name_Hash,
        GNATCOLL.VFS."=",
        GPS.LSP_Client.Text_Document_Handlers."=");

   type Module_Id_Record is
     new GPS.Kernel.Modules.Module_ID_Record with
      record
         Clients       : Client_Maps.Map;  --  Map from language to LSP client
         Text_Handlers : Text_Document_Handler_Maps.Map;
         --  Set of text handlers for currently opened files.
      end record;

   type LSP_Module_Id is access all Module_Id_Record'Class;

   Module : LSP_Module_Id;

   type On_File_Edited is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when a file has been opened

   type On_Buffer_Modified is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Buffer_Modified;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when a buffer has been modified

   type On_File_Closed is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Closed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called before closing a buffer.

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Buffer_Modified;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);

      Lang   : constant not null Language.Language_Access :=
        Kernel.Get_Language_Handler.Get_Language_From_File (File);
      Cursor : constant Client_Maps.Cursor := Module.Clients.Find (Lang);
      Client : LSP_Clients.LSP_Client_Access;

   begin
      if Lang.Has_LSP
        and then Client_Maps.Has_Element (Cursor)
      then
         Client := Client_Maps.Element (Cursor);
         Client.Did_Change_Text_Document (File);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Closed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);

      procedure Free is
        new Ada.Unchecked_Deallocation
          (GPS.LSP_Client.Text_Document_Handlers.Text_Document_Handler'Class,
           GPS.LSP_Client.Text_Document_Handlers.Text_Document_Handler_Access);

      Lang    : constant not null Language.Language_Access :=
        Kernel.Get_Language_Handler.Get_Language_From_File (File);
      Cursor  : constant Client_Maps.Cursor := Module.Clients.Find (Lang);
      Client  : LSP_Clients.LSP_Client_Access;
      Handler :
        GPS.LSP_Client.Text_Document_Handlers.Text_Document_Handler_Access;

   begin
      if Lang.Has_LSP
        and then Client_Maps.Has_Element (Cursor)
      then
         Client := Client_Maps.Element (Cursor);
         Client.Did_Close_Text_Document (File);

         if Module.Text_Handlers.Contains (File) then
            Handler := Module.Text_Handlers (File);
            Handler.Finalize;
            Free (Handler);
         end if;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      Lang    : constant not null Language.Language_Access :=
        Kernel.Get_Language_Handler.Get_Language_From_File (File);
      Cursor  : constant Client_Maps.Cursor := Module.Clients.Find (Lang);
      Client  : LSP_Clients.LSP_Client_Access;
      Handler :
        GPS.LSP_Client.Text_Document_Handlers.Text_Document_Handler_Access;

   begin
      if Lang.Has_LSP then
         if Client_Maps.Has_Element (Cursor) then
            Client := Client_Maps.Element (Cursor);
         else
            Client := new LSP_Clients.LSP_Client (Kernel);
            Module.Clients.Insert (Lang, Client);
            Client.Start (Lang.Get_LSP_Args);
         end if;

         if not Module.Text_Handlers.Contains (File) then
            --  New file is open. Sometimes GPS calls this hook multiple times.

            Handler :=
              new GPS.LSP_Client.Editors.Src_Editor_Handler'
                (File => File);

            Module.Text_Handlers.Insert (Handler.File, Handler);
            Client.Did_Open_Text_Document (Handler);
         end if;
      end if;
   end Execute;

   ----------
   -- Hash --
   ----------

   function Hash (Value : Language_Access) return Ada.Containers.Hash_Type is
      X : constant System.Storage_Elements.Integer_Address :=
        System.Storage_Elements.To_Integer (Value.all'Address);
   begin
      return Ada.Containers.Hash_Type'Mod (X);
   end Hash;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : Kernel_Handle) is
   begin
      Module := new Module_Id_Record;
      Register_Module (Module_ID (Module), Kernel, "LSP_Client");
      File_Edited_Hook.Add (new On_File_Edited);
      Buffer_Edited_Hook.Add (new On_Buffer_Modified);
      File_Closed_Hook.Add (new On_File_Closed);
   end Register_Module;

end GPS.LSP_Module;
