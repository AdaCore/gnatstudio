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
      Self.Map.Insert (To_Unbounded_String (Name), Value);
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
            Value : constant String :=
              To_String (Maps.Element (Cursor).GPS_Value);
         begin
            if Value = "" then
               Clear (Name);
            else
               Set (Name, Value);
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
            Value : constant String :=
              To_String (Maps.Element (Cursor).Users_Value);
         begin
            if Value = "" then
               Clear (Name);
            else
               Set (Name, Value);
            end if;

            Maps.Next (Cursor);
         end;
      end loop;
   end Apply_Users_Environment;

end GPS.Environments;
