------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2020-2026, AdaCore                  --
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

with GNATCOLL.Traces;                     use GNATCOLL.Traces;
with GNATCOLL.VFS;                        use GNATCOLL.VFS;

with LSP.Types;

with Basic_Types;                         use Basic_Types;
with Language;
with Src_Editor_Buffer;                   use Src_Editor_Buffer;
with Src_Editor_Buffer.Blocks;            use Src_Editor_Buffer.Blocks;

with GPS.Editors;                         use GPS.Editors;
with GPS.Kernel.Modules;                  use GPS.Kernel.Modules;

with GPS.LSP_Client.Language_Servers;
with GPS.LSP_Client.Requests.Folding_Range;
with GPS.LSP_Module;

package body GPS.LSP_Client.Editors.Folding is

   Me : constant Trace_Handle := Create ("GPS.LSP.FOLDING.ADVANCED");

   LSP_FOLDING_ON : constant Trace_Handle := Create ("GPS.LSP.FOLDING", On);
   --  Enable/disable LSP folding

   -- Folding_Request --

   type Folding_Request is
     new GPS.LSP_Client.Requests.Folding_Range.
       Abstract_Folding_Range_Request with null record;
   type Folding_Request_Access is access all Folding_Request;
   --  Used for communicate with LSP

   overriding procedure On_Result_Message
     (Self   : in out Folding_Request;
      Result : LSP.Messages.FoldingRange_Vector);

   overriding function Auto_Cancel
     (Self : in out Folding_Request) return Boolean is (True);

   -- LSP_Editor_Folding_Provider --

   type LSP_Editor_Folding_Provider is
     new GPS.Editors.Editor_Folding_Provider with record
      Kernel : Kernel_Handle;
   end record;

   overriding function Compute_Blocks
     (Self : in out LSP_Editor_Folding_Provider;
      File : GNATCOLL.VFS.Virtual_File) return Boolean;

   type LSP_Folding_Module_Id_Record is
     new Module_ID_Record with null record;
   type LSP_Folding_Module_Id_Access is
     access all LSP_Folding_Module_Id_Record'Class;

   overriding procedure Destroy
     (Module : in out LSP_Folding_Module_Id_Record);

   Module_Id : LSP_Folding_Module_Id_Access;
   Provider  : aliased LSP_Editor_Folding_Provider;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Folding_Request;
      Result : LSP.Messages.FoldingRange_Vector)
   is
      Buffer : Source_Buffer;
      Data   : Blocks_Vector.Vector;

      function Get_Kind
        (kind : LSP.Types.Optional_Virtual_String) return Block_Kind;

      --------------
      -- Get_Kind --
      --------------

      function Get_Kind
        (kind : LSP.Types.Optional_Virtual_String) return Block_Kind
      is
         use type LSP.Messages.FoldingRangeKind;

      begin
         if kind.Is_Set then
            if kind.Value = "region" then
               return Region;
            elsif kind.Value = "imports" then
               return Imports;
            elsif kind.Value = "comment" then
               return Comment;
            end if;
         end if;

         return Unknown;
      end Get_Kind;

   begin
      declare
         Bufs : constant Source_Buffer_Array := Buffer_List (Self.Kernel);
      begin
         for Idx in Bufs'Range loop
            if Bufs (Idx).Get_Filename = Self.File then
               Buffer := Bufs (Idx);
               exit;
            end if;
         end loop;
      end;

      if Buffer = null then
         --  Buffer can be closed
         return;
      end if;

      for FoldingRange of Result loop
         Data.Append
           (Block'
              (Editable_Line_Type (FoldingRange.startLine) + 1,
               Editable_Line_Type (FoldingRange.endLine) + 1,
               Get_Kind (FoldingRange.kind)));
      end loop;

      Set_Blocks (Buffer, Data);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Result_Message;

   --------------------
   -- Compute_Blocks --
   --------------------

   overriding function Compute_Blocks
     (Self : in out LSP_Editor_Folding_Provider;
      File : GNATCOLL.VFS.Virtual_File) return Boolean
   is
      use type GPS.LSP_Client.Language_Servers.Language_Server_Access;

      Lang    : constant Language.Language_Access :=
        Self.Kernel.Get_Language_Handler.Get_Language_From_File (File);
      Server  : GPS.LSP_Client.Language_Servers.Language_Server_Access;
      Request : Folding_Request_Access;
   begin
      if not LSP_FOLDING_ON.Is_Active then
         return False;
      end if;

      Server := GPS.LSP_Module.Get_Language_Server (Lang);
      if Server = null then
         return False;
      end if;

      Request := new Folding_Request'
        (GPS.LSP_Client.Requests.LSP_Request
         with Kernel => Self.Kernel,
              File   => File);

      return GPS.LSP_Client.Requests.Execute
        (Lang, GPS.LSP_Client.Requests.Request_Access (Request));

   exception
      when E : others =>
         Trace (Me, E);
         return False;
   end Compute_Blocks;

   -------------
   -- Destroy --
   -------------

   overriding procedure Destroy
     (Module : in out LSP_Folding_Module_Id_Record)
   is
      pragma Unreferenced (Module);
   begin
      Src_Editor_Buffer.Set_Folding_Provider (null);
   end Destroy;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : Kernel_Handle) is
   begin
      Module_Id := new LSP_Folding_Module_Id_Record;
      Register_Module
        (Module      => Modules.Module_ID (Folding.Module_Id),
         Kernel      => Kernel,
         Module_Name => "LSP_Folding",
         Priority    => Default_Priority);

      Provider.Kernel := Kernel;
      Src_Editor_Buffer.Set_Folding_Provider (Provider'Access);
   end Register_Module;

end GPS.LSP_Client.Editors.Folding;
