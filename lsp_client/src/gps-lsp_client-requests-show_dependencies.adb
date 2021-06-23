------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2020-2021, AdaCore                  --
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

with LSP.JSON_Streams;

with GPS.LSP_Client.Utilities;

package body GPS.LSP_Client.Requests.Show_Dependencies is

   ------------
   -- Method --
   ------------

   overriding function Method
     (Self : Abstract_Show_Dependencies_Request)
      return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);

   begin
      return "textDocument/alsShowDependencies";
   end Method;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Show_Dependencies_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      Results : LSP.Messages.ALS_Unit_Description_Vector;
   begin
      LSP.Messages.ALS_Unit_Description_Vector'Read
        (Stream, Results);

      if not Self.Kernel.Is_In_Destruction then
         Abstract_Show_Dependencies_Request'Class (Self).On_Result_Message
           (Results);
      end if;
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   function Params
     (Self : Abstract_Show_Dependencies_Request)
      return LSP.Messages.ALS_ShowDependenciesParams is
   begin
      return
        (textDocument =>
           (uri => GPS.LSP_Client.Utilities.To_URI (Self.File)),
         kind         => Self.Kind,
         showImplicit => Self.Show_Implicit);
   end Params;

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding function Is_Request_Supported
     (Self    : Abstract_Show_Dependencies_Request;
      Options : LSP.Messages.ServerCapabilities)
      return Boolean is
   begin
      return True;
   end Is_Request_Supported;

   ------------
   -- Params --
   ------------

   overriding procedure Params
     (Self   : Abstract_Show_Dependencies_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is
   begin
      LSP.Messages.ALS_ShowDependenciesParams'Write (Stream, Self.Params);
   end Params;

end GPS.LSP_Client.Requests.Show_Dependencies;
