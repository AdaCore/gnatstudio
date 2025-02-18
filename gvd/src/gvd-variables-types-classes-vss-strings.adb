------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2025, AdaCore                          --
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

with GNAT.Regpat;             use GNAT.Regpat;

with VSS.Strings;             use VSS.Strings;
with VSS.Strings.Conversions;

with Debugger.Base_Gdb.Ada;

package body GVD.Variables.Types.Classes.VSS.Strings is

   Value_Pattern : constant Pattern_Matcher :=
     Compile ("~""\$\d+\s=\s(.+)");

   -----------
   -- Clear --
   -----------

   overriding procedure Clear
     (Self : not null access GVD_VSS_String_Type) is
   begin
      Self.Value := Empty_Virtual_String;
      GVD_Class_Type (Self.all).Clear;
   end Clear;

   -----------
   -- Clone --
   -----------

   overriding procedure Clone
     (Self : not null access GVD_VSS_String_Type;
      Item : not null GVD_Generic_Type_Access) is
   begin
      GVD_Class_Type (Self.all).Clone (Item);
      Self.Value := GVD_VSS_String_Type_Access (Item).Value;
   end Clone;

   ------------
   -- Create --
   ------------

   function Create return GVD_Type_Holder
   is
      Data : constant GVD_Type_Holder_Data_Access :=
        new GVD_Type_Holder_Data'
          (Count    => 1,
           Instance => new GVD_VSS_String_Type (1));
   begin
      return GVD_Type_Holder'(Standard.Ada.Finalization.Controlled with Data);
   end Create;

   ----------------------
   -- Get_Simple_Value --
   ----------------------

   overriding function Get_Simple_Value
     (Self : not null access GVD_VSS_String_Type) return String is
   begin
      return Standard.VSS.Strings.Conversions.To_UTF_8_String (Self.Value);
   end Get_Simple_Value;

   -----------------------
   -- Get_Value_Command --
   -----------------------

   overriding function Get_Value_Command
     (Self   : not null access GVD_VSS_String_Type;
      Entity : String)
      return String
   is
      pragma Unreferenced (Self);
   begin
      return "p " & Entity;
   end Get_Value_Command;

   ---------------
   -- Set_Value --
   ---------------

   overriding procedure Set_Value
     (Self  : not null access GVD_VSS_String_Type;
      Value : String)
   is
      Matched : Match_Array (0 .. 1);
   begin
      Match (Value_Pattern, Value, Matched);

      if Matched (1) = No_Match then
         Self.Value :=
           Standard.VSS.Strings.Conversions.To_Virtual_String (Value);
      else
         declare
            S : constant String :=
              Value (Matched (1).First .. Matched (1).Last - 3);
         begin
            Self.Value :=
              Standard.VSS.Strings.Conversions.To_Virtual_String (S);
         end;
      end if;
   end Set_Value;

begin
   Debugger.Base_Gdb.Ada.Predefined_Type_Reestr.Insert
     ("vss.strings.virtual_string", Create'Access);

end GVD.Variables.Types.Classes.VSS.Strings;
