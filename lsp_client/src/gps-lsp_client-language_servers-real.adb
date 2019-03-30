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

with GPS.Kernel.Project;

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
     (Kernel  : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Manager : not null access
        GPS.LSP_Client.Text_Documents.Text_Document_Manager'Class)
      return not null Language_Server_Access is
   begin
      return Result : constant not null Language_Server_Access :=
        new Real_Language_Server (Kernel, Manager)
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
      Variables : constant GNATCOLL.Projects.Scenario_Variable_Array :=
                    GPS.Kernel.Project.Scenario_Variables (Self.Kernel);
      Settings  : constant GNATCOLL.JSON.JSON_Value :=
                    GNATCOLL.JSON.Create_Object;
      Scenarios : constant GNATCOLL.JSON.JSON_Value :=
                    GNATCOLL.JSON.Create_Object;

   begin
      --  First, send WorkspaceDidChangeConfiguration notification to complete
      --  initialization of ALS.

      --  ??? Other servers may require another settings object, to be
      --  implemented.

      Settings.Set_Field
        ("ada.projectFile",
         GPS.Kernel.Project.Get_Project
           (Self.Kernel).Project_Path.Display_Base_Name);
      --  ??? Mush be synchronized with rootPath of Initialize request.

      for Variable of Variables loop
         Scenarios.Set_Field
           (GNATCOLL.Projects.External_Name (Variable),
            GNATCOLL.Projects.Value (Variable));
      end loop;

      Settings.Set_Field ("ada.scenarioVariables", Scenarios);

      Self.Client.Workspace_Did_Change_Configuration
        ((settings => Settings));

      for Document of Self.Text_Documents loop
         Document.Set_Server (Self.Client'Unchecked_Access);
      end loop;
   end Server_Started;

end GPS.LSP_Client.Language_Servers.Real;
