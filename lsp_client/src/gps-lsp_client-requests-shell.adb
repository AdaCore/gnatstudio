------------------------------------------------------------------------------
--                               GNAT Studio                                --
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

with GNATCOLL.Any_Types;
with GNATCOLL.Scripts; use GNATCOLL.Scripts;

package body GPS.LSP_Client.Requests.Shell is

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Shell_Request) is
   begin
      Free (Self.On_Result_Message);
      Free (Self.On_Error_Message);
      Free (Self.On_Rejected);

      LSP_Request (Self).Finalize;
   end Finalize;

   ------------
   -- Method --
   ------------

   overriding function Method (Self : Shell_Request) return String is
   begin
      return Ada.Strings.Unbounded.To_String (Self.Method);
   end Method;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Shell_Request;
      Code    : LSP.Messages.ErrorCodes;
      Message : String;
      Data    : GNATCOLL.JSON.JSON_Value) is
   begin
      if Self.On_Error_Message /= null then
         declare
            Arguments : Callback_Data'Class :=
                          Self.On_Error_Message.Get_Script.Create (3);

         begin
            Set_Nth_Arg (Arguments, 1, LSP.Messages.ErrorCodes'Pos (Code));
            Set_Nth_Arg (Arguments, 2, Message);
            Set_Nth_Arg (Arguments, 3, GNATCOLL.JSON.Write (Data));

            declare
               Dummy : GNATCOLL.Any_Types.Any_Type :=
                         Self.On_Error_Message.Execute (Arguments);

            begin
               null;
            end;

            Free (Arguments);
         end;
      end if;
   end On_Error_Message;

   -----------------
   -- On_Rejected --
   -----------------

   overriding procedure On_Rejected (Self : in out Shell_Request) is
   begin
      if Self.On_Rejected /= null then
         declare
            Arguments : Callback_Data'Class :=
                          Self.On_Rejected.Get_Script.Create (0);
            Dummy     : GNATCOLL.Any_Types.Any_Type :=
                          Self.On_Rejected.Execute (Arguments);

         begin
            Free (Arguments);
         end;
      end if;
   end On_Rejected;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Shell_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      Arguments : Callback_Data'Class :=
                    Self.On_Result_Message.Get_Script.Create (1);

   begin
      Set_Nth_Arg (Arguments, 1, GNATCOLL.JSON.Write (Stream.Read));

      declare
         Dummy : GNATCOLL.Any_Types.Any_Type :=
                   Self.On_Result_Message.Execute (Arguments);

      begin
         null;
      end;

      Free (Arguments);
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   overriding procedure Params
     (Self   : Shell_Request;
      Stream : not null access LSP.JSON_Streams.JSON_Stream'Class) is
   begin
      Stream.Write (Self.Params);
   end Params;

end GPS.LSP_Client.Requests.Shell;
