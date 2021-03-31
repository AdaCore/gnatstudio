------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2021, AdaCore                   --
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

with Ada.Unchecked_Deallocation;

with GPS.Editors;
with GPS.LSP_Client.Language_Servers;
with GPS.LSP_Clients;
with GPS.LSP_Module;

package body GPS.LSP_Client.Requests is

   ------------
   -- Adjust --
   ------------

   overriding procedure Adjust (Self : in out Abstract_Reference) is
   begin
      if Self.Request /= null then
         Self.Request.References.Append (Self'Unchecked_Access);
         Self.Position := Self.Request.References.Last;
      end if;
   end Adjust;

   ------------
   -- Cancel --
   ------------

   procedure Cancel (Self : in out Reference) is
   begin
      if Self.Request /= null then
         if Self.Server /= null then
            Self.Server.Cancel (Self.Request);

         else
            Self.Request.On_Rejected;
            Destroy (Self.Request);
         end if;
      end if;
   end Cancel;

   --------
   -- Id --
   --------

   function Id (Self : LSP_Request) return LSP.Types.LSP_Number_Or_String is
   begin
      return Self.Id;
   end Id;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Item : in out Request_Access) is
      procedure Free is
        new Ada.Unchecked_Deallocation (LSP_Request'Class, Request_Access);

   begin
      if Item /= null then
         for Reference of Item.References loop
            Reference.Request := null;
            Reference.Server  := null;
            Reference.Position := Reference_Lists.No_Element;
         end loop;

         Item.References.Clear;
         Item.Finalize;
         Free (Item);
      end if;
   end Destroy;

   -------------
   -- Execute --
   -------------

   function Execute
     (Language : not null Standard.Language.Language_Access;
      Request  : in out Request_Access) return Boolean is
   begin
      return Execute (Language, Request).Has_Request;
   end Execute;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Language : not null Standard.Language.Language_Access;
      Request  : in out Request_Access)
   is
      Dummy : Boolean;
   begin
      Dummy := Execute (Language, Request);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Language : not null Standard.Language.Language_Access;
      Request  : in out Request_Access) return Reference
   is
      use type GPS.LSP_Client.Language_Servers.Language_Server_Access;

      Server : GPS.LSP_Client.Language_Servers.Language_Server_Access;
      Client : GPS.LSP_Clients.LSP_Client_Access;

      On_Checks_Passed : Boolean := True;

   begin
      return Result : Reference do
         if Request = null then
            return;
         end if;

         if Request.Kernel /= null
           and then Request.Kernel.Is_In_Destruction
         then
            --  exiting GNAT Studio
            On_Checks_Passed := False;
         end if;

         if On_Checks_Passed then
            Server := GPS.LSP_Module.Get_Language_Server (Language);

            if Server = null then
               --  Reject the request when there is no language server
               --  configured
               On_Checks_Passed := False;
            end if;
         end if;

         if On_Checks_Passed then
            Client := Server.Get_Client;

            if not Client.Is_Ready
              or else not Request.Is_Request_Supported (Client.Capabilities)
            then
               --  Not ready or not supported
               On_Checks_Passed := False;
            end if;
         end if;

         if On_Checks_Passed
           and then Request.Text_Document /= GNATCOLL.VFS.No_File
         then
            declare
               use GPS.Editors;

               Buffer : constant GPS.Editors.Editor_Buffer'Class :=
                 Request.Kernel.Get_Buffer_Factory.Get
                   (File        => Request.Text_Document,
                    Open_Buffer => False,
                    Open_View   => False);

            begin
               if Buffer /= Nil_Editor_Buffer
                 and then not Buffer.Is_Opened_On_LSP_Server
               then
                  --  Not opened on the server side yet
                  On_Checks_Passed := False;
               end if;
            end;
         end if;

         if On_Checks_Passed then
            Result.Initialize (Request, Server);
            Server.Execute (Request);

         else
            Request.On_Rejected;
            Destroy (Request);
         end if;
      end return;
   end Execute;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Abstract_Reference) is
   begin
      if Self.Request /= null then
         Self.Request.References.Delete (Self.Position);
         Self.Request := null;
         Self.Server  := null;
      end if;
   end Finalize;

   -----------------
   -- Has_Request --
   -----------------

   function Has_Request (Self : Reference) return Boolean is
   begin
      return Self.Request /= null;
   end Has_Request;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self    : in out Abstract_Reference'Class;
      Request : Request_Access;
      Server  : GPS.LSP_Client.Language_Servers.Language_Server_Access) is
   begin
      Self.Request := Request;
      Self.Server  := Language_Server_Access (Server);

      if Self.Request /= null then
         Self.Request.References.Append (Self'Unchecked_Access);
         Self.Position := Self.Request.References.Last;
      end if;
   end Initialize;

   -------------
   -- Request --
   -------------

   function Request (Self : Reference) return Request_Access is
   begin
      return Self.Request;
   end Request;

   -------------------
   -- Text_Document --
   -------------------

   function Text_Document
     (Self : LSP_Request) return GNATCOLL.VFS.Virtual_File is
   begin
      return GNATCOLL.VFS.No_File;
   end Text_Document;

end GPS.LSP_Client.Requests;
