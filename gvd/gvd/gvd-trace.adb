-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with GNAT.IO; use GNAT.IO;

package body Odd.Trace is

   procedure Output_String (Str : String);

   procedure Output_String (Str : String) is
   begin
      for J in Str'Range loop 
         case Str (J) is
            when ASCII.LF =>
               Put ("\n");

               if J < Str'Last then
                  Put_Line ("""");
                  Put ("   """);
               end if;
            when ASCII.HT =>
               Put ("\t");
            when others =>
               Put (Str (J));
         end case;
      end loop;
   end Output_String;

   ------------------
   -- Input_Filter --
   ------------------

   procedure Input_Filter
     (Descriptor : Process_Descriptor;
      Str        : String;
      User_Data  : System.Address := System.Null_Address) is
   begin
      Put ("-> """);
      Output_String (Str);
      Put_Line ("""");
   end Input_Filter;

   -------------------
   -- Output_Filter --
   -------------------

   procedure Output_Filter
     (Descriptor : Process_Descriptor;
      Str        : String;
      User_Data  : System.Address := System.Null_Address) is
   begin
      Put ("<- """);
      Output_String (Str);
      Put_Line ("""");
   end Output_Filter;

end Odd.Trace;
