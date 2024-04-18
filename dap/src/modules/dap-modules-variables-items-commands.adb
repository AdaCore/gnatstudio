------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

with VSS.Strings.Conversions;

with DAP.Clients.Variables;
with DAP.Utils;

package body DAP.Modules.Variables.Items.Commands is

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Self : Command_Item_Info) return Virtual_String is
   begin
      return Self.Cmd;
   end Get_Name;

   ----------------
   -- Is_Command --
   ----------------

   overriding function Is_Command (Info : Command_Item_Info) return Boolean is
   begin
      return True;
   end Is_Command;

   -------------------
   -- Find_DAP_Item --
   -------------------

   overriding procedure Find_DAP_Item
     (Info  : Command_Item_Info;
      C     : in out DAP.Types.Variables_References_Trees.Cursor;
      Found : out Boolean)
   is
      use DAP.Types.Variables_References_Trees;
   begin
      DAP.Clients.Variables.Find_By_Name (Info.Cmd, C);
      Found := C /= No_Element;
   end Find_DAP_Item;

   -----------
   -- Store --
   -----------

   overriding procedure Store
     (Info  : Command_Item_Info;
      Value : in out GNATCOLL.JSON.JSON_Value) is
   begin
      Value.Set_Field ("tag", "cmd");
      Value.Set_Field ("value", DAP.Utils.UTF8 (Info.Cmd));
      Value.Set_Field ("split", Info.Split_Lines);
   end Store;

   ----------
   -- Load --
   ----------

   function Load (Value : GNATCOLL.JSON.JSON_Value) return Item_Info'Class is
   begin
      return Create
        (Command     => VSS.Strings.Conversions.To_Virtual_String
           (String'(Value.Get ("value"))),
         Split_Lines => Value.Get ("split"));
   end Load;

   ------------
   -- Create --
   ------------

   function Create
     (Command     : VSS.Strings.Virtual_String;
      Split_Lines : Boolean;
      Format      : DAP.Tools.ValueFormat)
      return Item_Info'Class is
   begin
      return Item_Info'Class
        (Command_Item_Info'
           (Cmd         => Command,
            Split_Lines => Split_Lines,
            Format      => Format,
            others      => <>));
   end Create;

end DAP.Modules.Variables.Items.Commands;
