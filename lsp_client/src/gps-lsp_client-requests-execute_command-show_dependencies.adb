------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2024, AdaCore                       --
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

with LSP.Types;                use LSP.Types;
with LSP.Messages;             use LSP.Messages;

with GPS.LSP_Client.Utilities; use GPS.LSP_Client.Utilities;

package body GPS.LSP_Client.Requests.Execute_Command.Show_Dependencies is

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self   : in out Abstract_Show_Dependencies_Command_Request;
      JS     : not null access LSP.JSON_Streams.JSON_Stream'Class)
   is
      procedure Read_Item (Item : out Unit_Description);

      ---------------
      -- Read_Item --
      ---------------

      procedure Read_Item (Item : out Unit_Description) is
      begin
         pragma Assert (JS.R.Is_Start_Object);
         JS.R.Read_Next;

         while not JS.R.Is_End_Object loop
            pragma Assert (JS.R.Is_Key_Name);
            declare
               use type VSS.Strings.Virtual_String;

               Key : constant VSS.Strings.Virtual_String := JS.R.Key_Name;
            begin
               JS.R.Read_Next;
               if Key = "uri" then
                  LSP.Types.Read_LSP_URI (JS, Item.uri);
               elsif Key = "projectUri" then
                  LSP.Types.Read_LSP_URI (JS, Item.projectUri);
               else
                  JS.Skip_Value;
               end if;
            end;
         end loop;
         JS.R.Read_Next;
      end Read_Item;

      Result : Unit_Description_Vectors.Vector;
   begin
      pragma Assert (JS.R.Is_Start_Array);
      JS.R.Read_Next;

      while not JS.R.Is_End_Array loop
         declare
            Next : Unit_Description;
         begin
            Read_Item (Next);
            Result.Append (Next);
         end;
      end loop;
      JS.R.Read_Next;

      Abstract_Show_Dependencies_Command_Request'Class (Self)
        .On_Result_Message (Result);
   end On_Result_Message;

   ------------
   -- Params --
   ------------

   overriding function Params
     (Self : Abstract_Show_Dependencies_Command_Request)
      return LSP.Messages.ExecuteCommandParams
   is
      Arguments : Any_Vector;

      Argument  : constant LSP.Types.LSP_Any := Create_Object;

      URI : constant String := To_UTF_8_String (To_URI (Self.Text_Document));

      Kind : constant Positive := ALS_ShowDependenciesKind'Pos (Self.Kind) + 1;
   begin

      Argument.Set_Field ("uri", Create (URI));
      Argument.Set_Field ("kind", Create (Kind));
      Argument.Set_Field ("showImplicit", Create (Self.Show_Implicit));

      Arguments.Append (Argument);

      return
        (Is_Unknown => True,
         Base       => <>,
         command    => Self.Command_Name,
         arguments  => (Is_Set => True, Value => Arguments));
   end Params;

end GPS.LSP_Client.Requests.Execute_Command.Show_Dependencies;
