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
with GNATCOLL.VFS;       use GNATCOLL.VFS;
with GPS.Kernel.Hooks;   use GPS.Kernel.Hooks;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;
with Language;           use Language;
with System.Storage_Elements;
with GPS.LSP_Clients;

package body GPS.LSP_Module is

   function Hash (Value : Language_Access) return Ada.Containers.Hash_Type;

   package Client_Maps is new Ada.Containers.Hashed_Maps
     (Key_Type        => Language_Access,
      Element_Type    => LSP_Clients.LSP_Client_Access,
      Hash            => Hash,
      Equivalent_Keys => "=",
      "="             => LSP_Clients."=");

   type Module_Id_Record is new GPS.Kernel.Modules.Module_ID_Record with
   record
      Clients : Client_Maps.Map;  --  Map from language to LSP client
   end record;

   type LSP_Module_Id is access all Module_Id_Record'Class;

   Module : LSP_Module_Id;

   type On_File_Edited is new File_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File);
   --  Called when a file has been opened

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_File_Edited;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File)
   is
      pragma Unreferenced (Self);
      Lang : constant not null Language.Language_Access :=
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

         Client.Open_File (File);
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
   end Register_Module;

end GPS.LSP_Module;
