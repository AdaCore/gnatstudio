------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2019, AdaCore                        --
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

with GNATCOLL.JSON;
with GNATCOLL.Projects;

package body GPS.LSP_Client.Language_Servers.Real is

   procedure Initialize (Self : in out Real_Language_Server'Class);
   --  Initialize language server object. Doesn't start server.

   ---------------
   -- Associate --
   ---------------

   overriding procedure Associate
     (Self     : in out Real_Language_Server;
      Document :
        not null GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access) is
   begin
      Abstract_Language_Server (Self).Associate (Document);

      if Self.Client.Is_Ready then
         Document.Set_Server (Self.Client'Unchecked_Access);
      end if;
   end Associate;

   ------------
   -- Create --
   ------------

   function Create
     (Kernel        : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Manager       : not null access
        GPS.LSP_Client.Text_Documents.Text_Document_Manager'Class;
      Configuration : not null access
        GPS.LSP_Client.Configurations.Server_Configuration'Class)
      return not null Language_Server_Access is
   begin
      return Result : constant not null Language_Server_Access :=
        new Real_Language_Server (Kernel, Manager, Configuration)
      do
         Real_Language_Server'Class (Result.all).Initialize;
      end return;
   end Create;

   ----------------
   -- Dissociate --
   ----------------

   overriding procedure Dissociate
     (Self     : in out Real_Language_Server;
      Document :
        not null GPS.LSP_Client.Text_Documents.Text_Document_Handler_Access) is
   begin
      if Self.Client.Is_Ready then
         Document.Set_Server (null);
      end if;

      Abstract_Language_Server (Self).Dissociate (Document);
   end Dissociate;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Self : in out Real_Language_Server'Class) is
   begin
      Self.Client.Initialize;
   end Initialize;

   --------------------
   -- Server_Started --
   --------------------

   overriding procedure Server_Started (Self : in out Real_Language_Server) is
      Settings : constant GNATCOLL.JSON.JSON_Value :=
                   Self.Configuration.Configuration_Settings;

   begin
      if not Settings.Is_Empty then
         --  Send WorkspaceDidChangeConfiguration notification to complete
         --  initialization of the language server.

         Self.Client.Workspace_Did_Change_Configuration
           ((settings => Settings));
      end if;

      for Document of Self.Text_Documents loop
         Document.Set_Server (Self.Client'Unchecked_Access);
      end loop;
   end Server_Started;

   -----------
   -- Start --
   -----------

   procedure Start (Self : in out Real_Language_Server'Class) is
   begin
      Self.Client.Start
        (Self.Configuration.Full_Server_Executable_Path,
         Self.Configuration.Server_Arguments);
   end Start;

end GPS.LSP_Client.Language_Servers.Real;
