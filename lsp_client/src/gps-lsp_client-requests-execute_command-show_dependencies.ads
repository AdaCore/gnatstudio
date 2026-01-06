------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2024-2026, AdaCore                  --
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

with Ada.Containers.Vectors;

with LSP.Types;

package GPS.LSP_Client.Requests.Execute_Command.Show_Dependencies is

   type ALS_ShowDependenciesKind is (Show_Imported, Show_Importing);

   type Abstract_Show_Dependencies_Command_Request is
     abstract new Abstract_Execute_Command_Request with record
      File          : GNATCOLL.VFS.Virtual_File;
      Kind          : ALS_ShowDependenciesKind;
      Show_Implicit : Boolean := False;
   end record;

   overriding function Params
     (Self : Abstract_Show_Dependencies_Command_Request)
      return LSP.Messages.ExecuteCommandParams;
   --  Return parameters of the request to be sent to the server.

   overriding function Text_Document
     (Self : Abstract_Show_Dependencies_Command_Request)
      return GNATCOLL.VFS.Virtual_File is  (Self.File);

   overriding function Command_Name
     (Self : Abstract_Show_Dependencies_Command_Request)
        return VSS.Strings.Virtual_String is ("als-show-dependencies");

   overriding procedure On_Result_Message
     (Self : in out Abstract_Show_Dependencies_Command_Request) is null;

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Show_Dependencies_Command_Request;
      JS     : not null access LSP.JSON_Streams.JSON_Stream'Class);

   type Unit_Description is record
      uri        : LSP.Types.LSP_URI;
      projectUri : LSP.Types.LSP_URI;
   end record;

   package Unit_Description_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Positive,
      Element_Type => Unit_Description,
      "="          => "=");

   procedure On_Result_Message
     (Self   : in out Abstract_Show_Dependencies_Command_Request;
      Result : Unit_Description_Vectors.Vector) is abstract;

end GPS.LSP_Client.Requests.Execute_Command.Show_Dependencies;
