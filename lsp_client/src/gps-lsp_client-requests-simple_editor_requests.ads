------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2020, AdaCore                   --
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

--  This package provides the abstract base type used to implement simple
--  textdocument requests which have the same interface, ie they take
--  as parameter a TextDocumentPositionParams and their return type is
--  Location | Location[] | LocationLink[] | null
with GNATCOLL.VFS;

with Basic_Types;

package GPS.LSP_Client.Requests.Simple_Editor_Requests is

   type Command_Kind is (Goto_Body, Goto_Spec, Goto_Type_Decl);
   --  The command kinds that we support

   type Abstract_Simple_Request is abstract new LSP_Request with record
      Command       : Command_Kind;
      Text_Document : GNATCOLL.VFS.Virtual_File;
      Line          : Positive;
      Column        : Basic_Types.Visible_Column_Type;
   end record;

   procedure On_Result_Message
     (Self   : in out Abstract_Simple_Request;
      Result : LSP.Messages.Location_Vector) is abstract;
   --  Children need to override this, this is what takes care of the actual
   --  processing.

   overriding procedure Params
     (Self   : Abstract_Simple_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Simple_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class);

   overriding function Method
     (Self : Abstract_Simple_Request) return String is
     (case Self.Command is
         when Goto_Body => "textDocument/implementation",
         when Goto_Spec => "textDocument/definition",
         when Goto_Type_Decl => "textDocument/typeDefinition");

end GPS.LSP_Client.Requests.Simple_Editor_Requests;
