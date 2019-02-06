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
with Ada.Containers.Indefinite_Holders;
with Ada.Containers.Vectors;
with System.Storage_Elements;

with GNATCOLL.VFS;       use GNATCOLL.VFS;

with GPS.Core_Kernels;
with GPS.Editors;
with GPS.Kernel.Hooks;   use GPS.Kernel.Hooks;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with GPS.LSP_Client.Editors;
with GPS.LSP_Client.Text_Document_Handlers;
with GPS.LSP_Clients;
with Language;           use Language;
with Src_Editor_Buffer;

package body GPS.LSP_Module is

   type Listener_Factory is
     new GPS.Core_Kernels.Editor_Listener_Factory with null record;

   overriding function Create
     (Self    : Listener_Factory;
      Editor  : GPS.Editors.Editor_Buffer'Class;
      Factory : GPS.Editors.Editor_Buffer_Factory'Class;
      Kernel  : GPS.Core_Kernels.Core_Kernel)
      return GPS.Editors.Editor_Listener_Access;

   package Editor_Buffer_Holders is
     new Ada.Containers.Indefinite_Holders
       (GPS.Editors.Editor_Buffer'Class, GPS.Editors."=");

   type Buffer_Handler_Record is record
      Buffer  : Editor_Buffer_Holders.Holder;
      Handler :
        GPS.LSP_Client.Text_Document_Handlers.Text_Document_Handler_Access;
   end record;

   package Buffer_Handler_Vectors is
     new Ada.Containers.Vectors (Positive, Buffer_Handler_Record);

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
         Buffers       : Buffer_Handler_Vectors.Vector;
         --  Set of editor buffers with allocated handlers.
         Text_Handlers : Text_Document_Handler_Maps.Map;
         --  Set of text handlers for currently opened files.
         --  ??? Is this necessary ???
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

   ------------
   -- Create --
   ------------

   overriding function Create
     (Self    : Listener_Factory;
      Editor  : GPS.Editors.Editor_Buffer'Class;
      Factory : GPS.Editors.Editor_Buffer_Factory'Class;
      Kernel  : GPS.Core_Kernels.Core_Kernel)
      return GPS.Editors.Editor_Listener_Access
   is
      pragma Unreferenced (Self, Factory);

   begin
      --  Create, initialize and return object to integrate source editor and
      --  language server.

      return Result : constant GPS.Editors.Editor_Listener_Access :=
        new GPS.LSP_Client.Editors.Src_Editor_Handler
          (GPS.Kernel.Kernel_Handle (Kernel))
      do
         declare
            Handler : GPS.LSP_Client.Editors.Src_Editor_Handler'Class
            renames GPS.LSP_Client.Editors.Src_Editor_Handler'Class
              (Result.all);
         begin
            Handler.Initialize (Editor);
            Module.Buffers.Append
              ((Editor_Buffer_Holders.To_Holder (Editor),
               Handler'Unchecked_Access));
         end;
      end return;
   end Create;

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
      pragma Unreferenced (Self, Kernel);

   begin
      for J in Module.Buffers.First_Index .. Module.Buffers.Last_Index loop
         if Module.Buffers (J).Buffer.Element.File = File then
            Module.Buffers (J).Handler.Set_Server (null);
            Module.Buffers (J).Handler.Finalize;
            Module.Buffers.Delete (J);

            exit;
         end if;
      end loop;
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

      Lang   : constant not null Language.Language_Access :=
        Kernel.Get_Language_Handler.Get_Language_From_File (File);
      Cursor : constant Client_Maps.Cursor := Module.Clients.Find (Lang);
      Client : LSP_Clients.LSP_Client_Access;

   begin
      if Lang.Has_LSP then
         if Client_Maps.Has_Element (Cursor) then
            Client := Client_Maps.Element (Cursor);
         else
            Client := new LSP_Clients.LSP_Client (Kernel);
            Module.Clients.Insert (Lang, Client);
            Client.Start (Lang.Get_LSP_Args);
         end if;

         for R of Module.Buffers loop
            if R.Buffer.Element.File = File then
               R.Handler.Set_Server
                 (GPS.LSP_Client.Text_Document_Handlers
                  .Text_Document_Server_Proxy_Access
                    (Client));

               exit;
            end if;
         end loop;
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

      Src_Editor_Buffer.Add_Listener_Factory (new Listener_Factory);
   end Register_Module;

end GPS.LSP_Module;
