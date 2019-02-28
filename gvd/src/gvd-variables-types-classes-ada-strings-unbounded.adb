------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with GNAT.Regpat;           use GNAT.Regpat;

with Debugger.Base_Gdb.Ada;

package body GVD.Variables.Types.Classes.Ada.Strings.Unbounded is

   Value_Pattern : constant Pattern_Matcher :=
     Compile ("^\(max_length => (\d+), counter => \(value => (\d+)\)," &
                " last => (\d+), data => ""([\s\S]+)""");

   -----------
   -- Clear --
   -----------

   overriding procedure Clear
     (Self : not null access GVD_Ada_Unbounded_String_Type) is
   begin
      Self.Value := Null_Unbounded_String;
      GVD_Class_Type (Self.all).Clear;
   end Clear;

   -----------
   -- Clone --
   -----------

   overriding procedure Clone
     (Self : not null access GVD_Ada_Unbounded_String_Type;
      Item : not null GVD_Generic_Type_Access) is
   begin
      GVD_Class_Type (Self.all).Clone (Item);
      Self.Value := GVD_Ada_Unbounded_String_Type_Access (Item).Value;
   end Clone;

   ------------
   -- Create --
   ------------

   function Create return GVD_Type_Holder
   is
      Data : constant GVD_Type_Holder_Data_Access :=
        new GVD_Type_Holder_Data'
          (Count    => 1,
           Instance => new GVD_Ada_Unbounded_String_Type (1));
   begin
      return GVD_Type_Holder'(Standard.Ada.Finalization.Controlled with Data);
   end Create;

   ----------------------
   -- Get_Simple_Value --
   ----------------------

   overriding function Get_Simple_Value
     (Self : not null access GVD_Ada_Unbounded_String_Type) return String is
   begin
      return To_String (Self.Value);
   end Get_Simple_Value;

   -----------------------
   -- Get_Value_Command --
   -----------------------

   overriding function Get_Value_Command
     (Self   : not null access GVD_Ada_Unbounded_String_Type;
      Entity : String)
      return String
   is
      pragma Unreferenced (Self);
   begin
      return Entity & ".reference.all";
   end Get_Value_Command;

   ---------------
   -- Set_Value --
   ---------------

   overriding procedure Set_Value
     (Self  : not null access GVD_Ada_Unbounded_String_Type;
      Value : String)
   is
      Matched : Match_Array (0 .. 4);
   begin
      Match (Value_Pattern, Value, Matched);
      if Matched (3) /= No_Match
        and then Matched (4) /= No_Match
      then
         declare
            S : constant String :=
              Value (Matched (4).First .. Matched (4).Last);
            L : constant Integer := Integer'Value
              (Value (Matched (3).First .. Matched (3).Last));
         begin
            Self.Value := To_Unbounded_String
              ('"' & S (S'First .. Integer'Min (S'First + L - 1, S'Last)) &
              '"');
         end;
      end if;
   end Set_Value;

begin
   Debugger.Base_Gdb.Ada.Predefined_Type_Reestr.Insert
     ("ada.strings.unbounded.unbounded_string", Create'Access);

end GVD.Variables.Types.Classes.Ada.Strings.Unbounded;
