------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2020, AdaCore                       --
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

with Ada.Calendar;                    use Ada.Calendar;
with GNATCOLL.JSON;
with GNATCOLL.Traces;                 use GNATCOLL.Traces;
with GNATCOLL.VFS;                    use GNATCOLL.VFS;
with GPS.LSP_Client.Requests;         use GPS.LSP_Client.Requests;
with GPS.LSP_Client.Requests.Document_Symbols;
use GPS.LSP_Client.Requests.Document_Symbols;
with GPS.LSP_Client.Language_Servers; use GPS.LSP_Client.Language_Servers;
with GPS.LSP_Client.Utilities;        use GPS.LSP_Client.Utilities;
with GPS.LSP_Module;                  use GPS.LSP_Module;
with Language;                        use Language;
with LSP.Messages;                    use LSP.Messages;
with LSP.Types;                       use LSP.Types;
with Outline_View;                    use Outline_View;

package body GPS.LSP_Client.Outline is

   Me        : constant Trace_Handle :=
     Create ("GPS.LSP.OUTLINE.ADVANCED", On);
   Me_Active : constant Trace_Handle :=
     Create ("GPS.LSP.OUTLINE", Off);

   type Outline_LSP_Provider is new Outline_View.Outline_Provider with record
      Kernel   : Kernel_Handle;
      Canceled : Boolean := False;
   end record;
   type Outline_LSP_Provider_Access is access all Outline_LSP_Provider;

   overriding procedure Start_Fill
     (Self : access Outline_LSP_Provider; File : Virtual_File);

   overriding procedure Stop_Fill (Self : access Outline_LSP_Provider);

   overriding function Support_Language
     (Self : access Outline_LSP_Provider;
      Lang : Language_Access)
      return Boolean;

   type GPS_LSP_Outline_Request is
     new Document_Symbols_Request with record
      Kernel : Kernel_Handle;
   end record;
   type GPS_LSP_Outline_Request_Access is access all
     GPS_LSP_Outline_Request'Class;

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Outline_Request;
      Result : LSP.Messages.Symbol_Vector);

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Outline_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value);

   overriding procedure On_Rejected (Self : in out GPS_LSP_Outline_Request);

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out GPS_LSP_Outline_Request;
      Result : LSP.Messages.Symbol_Vector)
   is
      use DocumentSymbol_Trees;

      function Get_Optional_String (S : Optional_String) return String;

      function Get_Optional_Boolean (B : Optional_Boolean) return Boolean;

      function Get_Optional_Visibility
        (V : Optional_Als_Visibility) return Construct_Visibility;

      -------------------------
      -- Get_Optional_String --
      -------------------------

      function Get_Optional_String (S : Optional_String) return String is
      begin
         if S.Is_Set then
            return To_UTF_8_String (S.Value);
         else
            return "";
         end if;
      end Get_Optional_String;

      --------------------------
      -- Get_Optional_Boolean --
      --------------------------

      function Get_Optional_Boolean (B : Optional_Boolean) return Boolean is
      begin
         if B.Is_Set then
            return B.Value;
         else
            return True;
         end if;
      end Get_Optional_Boolean;

      -----------------------------
      -- Get_Optional_Visibility --
      -----------------------------

      function Get_Optional_Visibility
        (V : Optional_Als_Visibility) return Construct_Visibility is
      begin
         if V.Is_Set then
            return To_Construct_Visibility (V.Value);
         else
            return Visibility_Public;
         end if;
      end Get_Optional_Visibility;

   begin
      declare
         Now   : constant Time := Clock;
         Model : Outline_View.Outline_Model_Access :=
           Outline_View.Get_Outline_Model (Self.Kernel, Self.Text_Document);

         procedure Parse_Tree (Position : Cursor);

         ----------------
         -- Parse_Tree --
         ----------------

         procedure Parse_Tree (Position : Cursor) is
            Symbol  : constant DocumentSymbol := Element (Position);
            Visible : Boolean;
         begin
            Outline_View.Add_Row
              (Self           => Model,
               Name           => To_UTF_8_String (Symbol.name),
               Profile        => Get_Optional_String (Symbol.detail),
               Category       => To_Language_Category (Symbol.kind),
               Is_Declaration =>
                 Get_Optional_Boolean (Symbol.alsIsDeclaration),
               Visibility     =>
                 Get_Optional_Visibility (Symbol.alsVisibility),
               Def_Line       =>
                 Integer (Symbol.selectionRange.first.line + 1),
               Def_Col        =>
                 Integer
                   (UTF_16_Offset_To_Visible_Column
                        (Symbol.selectionRange.first.character)),
               End_Line       => Integer (Symbol.span.last.line + 1),
               Id             => "",
               Visible        => Visible);
            if Visible then
               Iterate_Children (Position, Parse_Tree'Access);
               Outline_View.Move_Cursor (Model, Outline_View.Up);
            end if;
         end Parse_Tree;
      begin
         if Model = null then
            return;
         end if;

         Outline_View.Clear_Outline_Model (Model);
         if Result.Is_Tree then
            Iterate_Children (Result.Tree.Root, Parse_Tree'Access);
         else
            declare
               Dummy : Boolean;
            begin
               for Symbol of Result.Vector loop
                  Outline_View.Add_Row
                    (Self           => Model,
                     Name           => To_UTF_8_String (Symbol.name),
                     Profile        => "",
                     Category       => To_Language_Category (Symbol.kind),
                     Is_Declaration => False,
                     Visibility     => Visibility_Public,
                     Def_Line       =>
                       Integer (Symbol.location.span.first.line + 1),
                     Def_Col        =>
                       Integer
                         (UTF_16_Offset_To_Visible_Column
                              (Symbol.location.span.first.character)),
                     End_Line       =>
                       Integer (Symbol.location.span.last.line + 1),
                     Id             => "",
                     Visible        => Dummy);
                  Outline_View.Move_Cursor (Model, Outline_View.Up);
               end loop;
            end;
         end if;
         Outline_View.Free (Model);
         Trace
           (Me,
            "Time elapsed to compute outline:" & Duration'Image (Clock - Now));
         Outline_View.Finished_Computing (Self.Kernel);
      end;
   end On_Result_Message;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out GPS_LSP_Outline_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value) is
   begin
      Trace (Me, "Error received after sending " & Self.Method);
      Outline_View.Finished_Computing (Self.Kernel);
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected
     (Self : in out GPS_LSP_Outline_Request) is
   begin
      Trace (Me, Self.Method & " has been rejected");
      Outline_View.Finished_Computing (Self.Kernel);
   end On_Rejected;

   ----------------
   -- Start_Fill --
   ----------------

   overriding procedure Start_Fill
     (Self : access Outline_LSP_Provider; File : Virtual_File)
   is
      R : GPS_LSP_Outline_Request_Access;
   begin
      Trace (Me, "Sending documentSymbols Request");
      R :=
        new GPS_LSP_Outline_Request'
          (LSP_Request with Kernel => Self.Kernel, Text_Document => File);

      GPS.LSP_Client.Requests.Execute
        (Self.Kernel.Get_Language_Handler.Get_Language_From_File (File),
         Request_Access (R));
   end Start_Fill;

   ---------------
   -- Stop_Fill --
   ---------------

   overriding procedure Stop_Fill (Self : access Outline_LSP_Provider)
   is
      pragma Unreferenced (Self);
   begin
      null;
   end Stop_Fill;

   ----------------------
   -- Support_Language --
   ----------------------

   overriding function Support_Language
     (Self : access Outline_LSP_Provider;
      Lang : Language_Access)
      return Boolean
   is
      pragma Unreferenced (Self);
   begin
      return Get_Language_Server (Lang) /= null;
   end Support_Language;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module (Kernel : Kernel_Handle) is
   begin
      if Me_Active.Active then
         declare
            Provider : constant Outline_LSP_Provider_Access :=
              new Outline_LSP_Provider'(Kernel => Kernel, Canceled => <>);
         begin
            Outline_View.Set_LSP_Provider
              (Outline_View.Outline_Provider_Access (Provider));
         end;
      end if;
   end Register_Module;

end GPS.LSP_Client.Outline;
