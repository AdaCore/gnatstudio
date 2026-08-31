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

with LSP.Inputs;
with LSP.Outputs;

with GPS.LSP_Client.Utilities;

package body GPS.LSP_Client.Requests.Folding_Range is

   ------------
   -- Method --
   ------------

   overriding
   function Method
     (Self : Abstract_Folding_Range_Request) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);

   begin
      return "textDocument/foldingRange";
   end Method;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding
   procedure On_Result_Message
     (Self    : in out Abstract_Folding_Range_Request;
      Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class)
   is
      Response : LSP.Structures.FoldingRange_Vector;

   begin
      --  Result is "FoldingRange[] | null" per the LSP spec.
      LSP.Inputs.Read_FoldingRange_Vector_Or_Null (Handler, Response);
      Abstract_Folding_Range_Request'Class (Self).On_Result_Message (Response);
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   function Params
     (Self : Abstract_Folding_Range_Request)
      return LSP.Structures.FoldingRangeParams is
   begin
      return
        (workDoneToken      => <>,
         partialResultToken => <>,
         textDocument       =>
           (uri => GPS.LSP_Client.Utilities.To_URI (Self.Text_Document)));
   end Params;

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding
   function Is_Request_Supported
     (Self    : Abstract_Folding_Range_Request;
      Options : LSP.Structures.ServerCapabilities) return Boolean
   is
      use type LSP
                 .Structures
                 .foldingRangeProvider_OfServerCapabilities_Variant;

      Option :
        LSP
          .Structures
          .foldingRangeProvider_OfServerCapabilities_Optional renames
          Options.foldingRangeProvider;
   begin
      if not Option.Is_Set
        or else
          (Option.Value.Kind = LSP.Structures.Variant_1
           and then not Option.Value.Variant_1)
      then
         return False;
      else
         return True;
      end if;
   end Is_Request_Supported;

   ------------
   -- Params --
   ------------

   overriding
   procedure Params
     (Self    : Abstract_Folding_Range_Request;
      Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      LSP.Outputs.Write_FoldingRangeParams (Handler, Self.Params);
   end Params;

end GPS.LSP_Client.Requests.Folding_Range;
