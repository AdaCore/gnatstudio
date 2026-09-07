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

with VSS.JSON.Pull_Readers.Buffered;

with GPS.LSP_Client.Utilities;

package body GPS.LSP_Client.Requests.Document_Symbols is

   ------------
   -- Method --
   ------------

   overriding
   function Method
     (Self : Document_Symbols_Request) return VSS.Strings.Virtual_String
   is
      pragma Unreferenced (Self);
   begin
      return "textDocument/documentSymbol";
   end Method;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding
   procedure On_Result_Message
     (Self    : in out Document_Symbols_Request;
      Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class)
   is
      Symbols : LSP.Structures.DocumentSymbol_Result;

      Parent :
        constant not null access
          VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class := Handler'Access;
      Peek   :
        VSS.JSON.Pull_Readers.Buffered.JSON_Buffered_Pull_Reader (Parent);

      Is_Empty_Array : Boolean := False;

   begin
      --  LSP.Inputs.Read_DocumentSymbol_Result (generated from
      --  metaModel.json) distinguishes the SymbolInformation[] vs
      --  DocumentSymbol[] variants by peeking at the first array element's
      --  field names. It only handles a JSON "null" or a non-empty array:
      --  an empty array "[]" (a common, valid "no symbols" response, e.g.
      --  for an empty file) falls through to its "raise Program_Error"
      --  fallback branch. Detect that case ourselves first and short
      --  circuit it, since either variant is equally valid (and equally
      --  empty) in that case.
      --
      --  Peek.Reset only rewinds Peek's own replay cursor: it cannot
      --  "un-consume" the tokens Peek.Read_Next already pulled from the
      --  underlying Handler. So the real decode below must keep reading
      --  through Peek (which replays its buffered tokens first, then
      --  forwards to Handler once the buffer is exhausted), never
      --  through Handler directly, or those tokens would be lost.

      Peek.Mark;

      if Peek.Is_Start_Array then
         Peek.Read_Next;
         Is_Empty_Array := Peek.Is_End_Array;
      end if;

      Peek.Reset;

      if Is_Empty_Array then
         Symbols := (Kind => LSP.Structures.Variant_1, others => <>);
      else
         Peek.Unmark;
         LSP.Inputs.Read_DocumentSymbol_Result (Peek, Symbols);
      end if;

      Document_Symbols_Request'Class (Self).On_Result_Message (Symbols);
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   overriding
   procedure Params
     (Self    : Document_Symbols_Request;
      Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      LSP.Outputs.Write_DocumentSymbolParams
        (Handler,
         (workDoneToken      => <>,
          partialResultToken => <>,
          textDocument       =>
            (uri => GPS.LSP_Client.Utilities.To_URI (Self.Text_Document)),
          query              => Self.Query,
          case_sensitive     => Self.Case_Sensitive,
          whole_word         => Self.Whole_Word,
          negate             => Self.Negate,
          kind               => Self.Kind));
   end Params;

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding
   function Is_Request_Supported
     (Self    : Document_Symbols_Request;
      Options : LSP.Structures.ServerCapabilities) return Boolean is
   begin
      return Options.documentSymbolProvider.Is_Set;
   end Is_Request_Supported;

end GPS.LSP_Client.Requests.Document_Symbols;
