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

with GNATCOLL.JSON; use GNATCOLL.JSON;

with VSS.Strings.Conversions;

with GPS.LSP_Client.Utilities; use GPS.LSP_Client.Utilities;

package body GPS.LSP_Client.Requests.Execute_Command.Show_Dependencies is

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding
   procedure On_Result_Message
     (Self    : in out Abstract_Show_Dependencies_Command_Request;
      Handler : in out VSS.JSON.Pull_Readers.JSON_Pull_Reader'Class)
   is
      procedure Read_Item (Item : out Unit_Description);

      ---------------
      -- Read_Item --
      ---------------

      procedure Read_Item (Item : out Unit_Description) is
      begin
         pragma Assert (Handler.Is_Start_Object);
         Handler.Read_Next;

         while not Handler.Is_End_Object loop
            pragma Assert (Handler.Is_Key_Name);
            declare
               use type VSS.Strings.Virtual_String;

               Key : constant VSS.Strings.Virtual_String := Handler.Key_Name;
            begin
               Handler.Read_Next;
               if Key = "uri" then
                  Item.uri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               elsif Key = "projectUri" then
                  Item.projectUri := (Handler.String_Value with null record);
                  Handler.Read_Next;
               else
                  Handler.Skip_Current_Value;
               end if;
            end;
         end loop;
         Handler.Read_Next;
      end Read_Item;

      Result : Unit_Description_Vectors.Vector;
   begin
      pragma Assert (Handler.Is_Start_Array);
      Handler.Read_Next;

      while not Handler.Is_End_Array loop
         declare
            Next : Unit_Description;
         begin
            Read_Item (Next);
            Result.Append (Next);
         end;
      end loop;
      Handler.Read_Next;

      Abstract_Show_Dependencies_Command_Request'Class (Self).On_Result_Message
        (Result);
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   overriding
   function Params
     (Self : Abstract_Show_Dependencies_Command_Request)
      return LSP.Structures.ExecuteCommandParams
   is
      Arguments : JSON_Array;

      Argument : constant JSON_Value := Create_Object;

      URI : constant String :=
        VSS.Strings.Conversions.To_UTF_8_String
          (VSS.Strings.Virtual_String (To_URI (Self.Text_Document)));

      Kind : constant Positive := ALS_ShowDependenciesKind'Pos (Self.Kind) + 1;
   begin

      Argument.Set_Field ("uri", Create (URI));
      Argument.Set_Field ("kind", Create (Kind));
      Argument.Set_Field ("showImplicit", Create (Self.Show_Implicit));

      Arguments.Append (Argument);

      return
        (workDoneToken => (Is_Set => False),
         command       => Self.Command_Name,
         arguments     =>
           GPS.LSP_Client.Utilities.To_LSP_Any (Create (Arguments)));
   end Params;

end GPS.LSP_Client.Requests.Execute_Command.Show_Dependencies;
