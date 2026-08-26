------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                       Copyright (C) 2019-2026, AdaCore                   --
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

package body GPS.LSP_Client.Requests.Simple_Editor_Requests is

   function To_Location_Or_Link_Vector
     (Result : LSP.Structures.Declaration_Result)
      return Location_Or_Link_Vector;

   function To_Location_Or_Link_Vector
     (Result : LSP.Structures.Definition_Result)
      return Location_Or_Link_Vector;

   function To_LocationLink_Vector
     (Links : LSP.Structures.DeclarationLink_Vector)
      return LocationLink_Vector;
   function To_LocationLink_Vector
     (Links : LSP.Structures.DefinitionLink_Vector) return LocationLink_Vector;
   --  LSP.Structures generates a distinct vector type per request kind for
   --  what is structurally the same LocationLink element type -- copy
   --  element-by-element into our own common LocationLink_Vector.

   ------------
   -- Method --
   ------------

   overriding
   function Method
     (Self : Abstract_Simple_Request) return VSS.Strings.Virtual_String is
   begin
      case Self.Command is
         when Goto_Body         =>
            return "textDocument/implementation";

         when Goto_Spec         =>
            return "textDocument/declaration";

         when Goto_Spec_Or_Body =>
            return "textDocument/definition";

         when Goto_Type_Decl    =>
            return "textDocument/typeDefinition";
      end case;
   end Method;

   -----------------------------
   -- To_LocationLink_Vector --
   -----------------------------

   function To_LocationLink_Vector
     (Links : LSP.Structures.DeclarationLink_Vector) return LocationLink_Vector
   is
      Result : LocationLink_Vector;

   begin
      for Link of Links loop
         Result.Append (LSP.Structures.LocationLink (Link));
      end loop;

      return Result;
   end To_LocationLink_Vector;

   function To_LocationLink_Vector
     (Links : LSP.Structures.DefinitionLink_Vector) return LocationLink_Vector
   is
      Result : LocationLink_Vector;

   begin
      for Link of Links loop
         Result.Append (LSP.Structures.LocationLink (Link));
      end loop;

      return Result;
   end To_LocationLink_Vector;

   ---------------------------------
   -- To_Location_Or_Link_Vector --
   ---------------------------------

   function To_Location_Or_Link_Vector
     (Result : LSP.Structures.Declaration_Result)
      return Location_Or_Link_Vector is
   begin
      case Result.Kind is
         when LSP.Structures.Variant_1 =>
            return
              (Kind => Location_Vector_Kind, Locations => Result.Variant_1);

         when LSP.Structures.Variant_2 =>
            return
              (Kind  => LocationLink_Vector_Kind,
               Links => To_LocationLink_Vector (Result.Variant_2));

         when LSP.Structures.Variant_3 =>
            return (Kind => Location_Vector_Kind, others => <>);
      end case;
   end To_Location_Or_Link_Vector;

   function To_Location_Or_Link_Vector
     (Result : LSP.Structures.Definition_Result) return Location_Or_Link_Vector
   is
   begin
      case Result.Kind is
         when LSP.Structures.Variant_1 =>
            return
              (Kind => Location_Vector_Kind, Locations => Result.Variant_1);

         when LSP.Structures.Variant_2 =>
            return
              (Kind  => LocationLink_Vector_Kind,
               Links => To_LocationLink_Vector (Result.Variant_2));

         when LSP.Structures.Variant_3 =>
            return (Kind => Location_Vector_Kind, others => <>);
      end case;
   end To_Location_Or_Link_Vector;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding
   procedure On_Result_Message
     (Self    : in out Abstract_Simple_Request;
      Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class) is
   begin
      case Self.Command is
         when Goto_Spec =>
            declare
               Result : LSP.Structures.Declaration_Result;

            begin
               LSP.Inputs.Read_Declaration_Result (Handler, Result);
               Abstract_Simple_Request'Class (Self).On_Result_Message
                 (To_Location_Or_Link_Vector (Result));
            end;

         when others    =>
            declare
               Result : LSP.Structures.Definition_Result;

            begin
               LSP.Inputs.Read_Definition_Result (Handler, Result);
               Abstract_Simple_Request'Class (Self).On_Result_Message
                 (To_Location_Or_Link_Vector (Result));
            end;
      end case;
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   overriding
   procedure Params
     (Self    : Abstract_Simple_Request;
      Handler : in out VSS.JSON.Content_Handlers.JSON_Content_Handler'Class) is
   begin
      case Self.Command is
         when Goto_Type_Decl    =>
            LSP.Outputs.Write_TypeDefinitionParams
              (Handler,
               (textDocument       =>
                  (uri =>
                     GPS.LSP_Client.Utilities.To_URI (Self.Text_Document)),
                position           => Self.Position,
                workDoneToken      => (Is_Set => False),
                partialResultToken => (Is_Set => False)));

         when Goto_Spec         =>
            LSP.Outputs.Write_DeclarationParams
              (Handler,
               (textDocument                         =>
                  (uri =>
                     GPS.LSP_Client.Utilities.To_URI (Self.Text_Document)),
                position                             => Self.Position,
                alsDisplayMethodAncestryOnNavigation =>
                  (Is_Set => True,
                   Value  => Self.Display_Ancestry_On_Navigation),
                workDoneToken                        => (Is_Set => False),
                partialResultToken                   => (Is_Set => False)));

         when Goto_Body         =>
            LSP.Outputs.Write_ImplementationParams
              (Handler,
               (textDocument                         =>
                  (uri =>
                     GPS.LSP_Client.Utilities.To_URI (Self.Text_Document)),
                position                             => Self.Position,
                alsDisplayMethodAncestryOnNavigation =>
                  (Is_Set => True,
                   Value  => Self.Display_Ancestry_On_Navigation),
                workDoneToken                        => (Is_Set => False),
                partialResultToken                   => (Is_Set => False)));

         when Goto_Spec_Or_Body =>
            LSP.Outputs.Write_DefinitionParams
              (Handler,
               (textDocument                         =>
                  (uri =>
                     GPS.LSP_Client.Utilities.To_URI (Self.Text_Document)),
                position                             => Self.Position,
                alsDisplayMethodAncestryOnNavigation =>
                  (Is_Set => True,
                   Value  => Self.Display_Ancestry_On_Navigation),
                workDoneToken                        => (Is_Set => False),
                partialResultToken                   => (Is_Set => False)));
      end case;
   end Params;

   --------------------------
   -- Is_Request_Supported --
   --------------------------

   overriding
   function Is_Request_Supported
     (Self    : Abstract_Simple_Request;
      Options : LSP.Structures.ServerCapabilities) return Boolean is
   begin
      case Self.Command is
         when Goto_Body         =>
            return Options.implementationProvider.Is_Set;

         when Goto_Spec         =>
            return Options.declarationProvider.Is_Set;

         when Goto_Spec_Or_Body =>
            return Options.definitionProvider.Is_Set;

         when Goto_Type_Decl    =>
            return Options.typeDefinitionProvider.Is_Set;
      end case;
   end Is_Request_Supported;

end GPS.LSP_Client.Requests.Simple_Editor_Requests;
