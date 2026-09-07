------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2020-2026, AdaCore                   --
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

with GNATCOLL.JSON; use GNATCOLL.JSON;

with VSS.Strings;
with VSS.Strings.Conversions;

with GPS.LSP_Client.Utilities; use GPS.LSP_Client.Utilities;

with URIs;

package body GPS.LSP_Client.Requests.Execute_Command.Named_Parameters is

   ------------
   -- Params --
   ------------

   overriding
   function Params
     (Self : Abstract_Named_Parameters_Command_Request)
      return LSP.Structures.ExecuteCommandParams
   is
      Arguments : JSON_Array;

      Argument : constant JSON_Value := Create_Object;
      Where    : constant JSON_Value := Create_Object;
      Document : constant JSON_Value := Create_Object;
      Position : constant JSON_Value := Create_Object;

   begin
      Document.Set_Field
        ("uri",
         Create
           (VSS.Strings.Conversions.To_UTF_8_String
              (VSS.Strings.Virtual_String (To_URI (Self.Text_Document)))));

      Position.Set_Field ("line", Create (Integer (Self.Position.line)));
      Position.Set_Field
        ("character", Create (Integer (Self.Position.character)));

      Where.Set_Field ("textDocument", Document);
      Where.Set_Field ("position", Position);

      Argument.Set_Field
        ("context",
         Create
           (URIs.Conversions.From_File
              (Self.Project.Project_Path.Display_Full_Name)));
      Argument.Set_Field ("where", Where);

      Arguments.Append (Argument);

      return
        (workDoneToken => (Is_Set => False),
         command       => Self.Command_Name,
         arguments     =>
           GPS.LSP_Client.Utilities.To_LSP_Any (Create (Arguments)));
   end Params;

   -------------------
   -- Text_Document --
   -------------------

   overriding
   function Text_Document
     (Self : Abstract_Named_Parameters_Command_Request)
      return GNATCOLL.VFS.Virtual_File is
   begin
      return Self.File;
   end Text_Document;

end GPS.LSP_Client.Requests.Execute_Command.Named_Parameters;
