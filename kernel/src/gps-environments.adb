------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2014-2018, AdaCore                  --
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

with Ada.Environment_Variables; use Ada.Environment_Variables;

package body GPS.Environments is

   ------------
   -- Append --
   ------------

   procedure Append
     (Self        : in out Environment_Record;
      Name        : String;
      Users_Value : String;
      GPS_Value   : String)
   is
      Value : constant Environment_Values :=
        (Users_Value => To_Unbounded_String (Users_Value),
         GPS_Value   => To_Unbounded_String (GPS_Value));
   begin
      Self.Map.Include (To_Unbounded_String (Name), Value);
   end Append;

   ---------------------------
   -- Apply_GPS_Environment --
   ---------------------------

   procedure Apply_GPS_Environment (Self : Environment_Record) is
      Cursor : Maps.Cursor := Self.Map.First;
   begin
      while Maps.Has_Element (Cursor) loop
         declare
            Name  : constant String := To_String (Maps.Key (Cursor));
            Item  : Environment_Values renames Maps.Element (Cursor);
         begin
            if Item.Users_Value = Item.GPS_Value then
               --  Nothing to do here
               null;
            elsif Length (Item.GPS_Value) = 0 then
               Clear (Name);
            else
               Set (Name, To_String (Item.GPS_Value));
            end if;

            Maps.Next (Cursor);
         end;
      end loop;
   end Apply_GPS_Environment;

   -----------------------------
   -- Apply_Users_Environment --
   -----------------------------

   procedure Apply_Users_Environment (Self : Environment_Record) is
      Cursor : Maps.Cursor := Self.Map.First;
   begin
      while Maps.Has_Element (Cursor) loop
         declare
            Name  : constant String := To_String (Maps.Key (Cursor));
            Item  : Environment_Values renames Maps.Element (Cursor);
         begin
            if Item.Users_Value = Item.GPS_Value then
               --  Nothing to do here
               null;
            elsif Length (Maps.Element (Cursor).Users_Value) = 0 then
               Clear (Name);
            else
               Set (Name, To_String (Maps.Element (Cursor).Users_Value));
            end if;

            Maps.Next (Cursor);
         end;
      end loop;
   end Apply_Users_Environment;

   -----------------
   -- Has_Element --
   -----------------

   function Has_Element
     (Self : Environment_Record;
      Name : String) return Boolean is
   begin
      return Self.Map.Contains (To_Unbounded_String (Name));
   end Has_Element;

   -----------
   -- Value --
   -----------

   function Value
     (Self : Environment_Record;
      Name : String) return String is
   begin
      return To_String (Self.Map (To_Unbounded_String (Name)).Users_Value);
   end Value;

end GPS.Environments;
