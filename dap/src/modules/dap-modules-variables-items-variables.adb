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

package body DAP.Modules.Variables.Items.Variables is

   --------------
   -- Get_Name --
   --------------

   overriding function Get_Name
     (Self : Variable_Item_Info) return Virtual_String is
   begin
      return Self.Varname;
   end Get_Name;

   -------------------
   -- Find_DAP_Item --
   -------------------

   overriding procedure Find_DAP_Item
     (Info  : Variable_Item_Info;
      C     : in out DAP.Types.Variables_References_Trees.Cursor;
      Found : out Boolean) is
   begin
      DAP.Clients.Variables.Find_Name_Or_Parent (Info.Varname, C, Found);
   end Find_DAP_Item;

   -----------
   -- Store --
   -----------

   overriding procedure Store
     (Info  : Variable_Item_Info;
      Value : in out GNATCOLL.JSON.JSON_Value) is
   begin
      Value.Set_Field ("tag", "variable");
      Value.Set_Field ("value", DAP.Utils.To_UTF8 (Info.Varname));
      Value.Set_Field
        ("format", Value_Format'Image (Convert (Info.Format)));
   end Store;

   ----------
   -- Load --
   ----------

   function Load (Value : GNATCOLL.JSON.JSON_Value) return Item_Info'Class is
   begin
      return Variables.Create
        (Variable => VSS.Strings.Conversions.To_Virtual_String
           (String'(Value.Get ("value"))),
         Format   => Convert (Value_Format'Value (Value.Get ("format"))));
   end Load;

   ------------
   -- Create --
   ------------

   function Create
     (Variable : VSS.Strings.Virtual_String;
      Format   : DAP.Tools.ValueFormat)
      return Item_Info'Class is
   begin
      return Item_Info'Class
        (Variable_Item_Info'
           (Varname => Variable, Format => Format, others => <>));
   end Create;

end DAP.Modules.Variables.Items.Variables;
