------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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

with Ada.Text_IO;           use Ada.Text_IO;

package body Parse_Support is

   -----------
   -- Print --
   -----------

   procedure Print
      (Self : access Generic_Type'Class;
       Lang : not null access Language_Root'Class;
       Name : String)
   is
      procedure Display
        (Entity : not null access Generic_Type'Class;
         Name   : String;
         Indent : Natural);
      --  Display recursively Entity and its children

      -------------
      -- Display --
      -------------

      procedure Display
        (Entity : not null access Generic_Type'Class;
         Name   : String;
         Indent : Natural)
      is
         Iter : Generic_Iterator'Class := Entity.Start;
         Ent  : Generic_Type_Access;
      begin
         Put_Line
           ((1 .. Indent => ' ') & Name & ": "
            & Entity.Get_Type_Name (Language_Access (Lang))
            & " := " & Entity.Get_Simple_Value
            & " [" & Entity.Get_Type_Descr & "]");

         while not Iter.At_End loop
            Ent := Iter.Data;
            if Ent /= null then
               Display (Ent, Iter.Field_Name (Lang, ""), Indent + 3);
            end if;
            Iter.Next;
         end loop;
      end Display;

   begin
      if Self = null then
         Put_Line (Name & ": no type info");
      else
         Display (Self, Name, 0);
      end if;
   end Print;

end Parse_Support;
