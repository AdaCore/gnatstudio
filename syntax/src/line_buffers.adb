-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with GNAT.IO;      use GNAT.IO;
with String_Utils; use String_Utils;

package body Line_Buffers is

   use Basic_Types;

   --------------------
   -- To_Line_Buffer --
   --------------------

   function To_Line_Buffer (Buffer : String) return Extended_Line_Buffer is
      B     : Extended_Line_Buffer;
      Index : Natural := Buffer'First;
      First : Natural;
      Tmp   : Line_Buffer;
      Prev  : Line_Buffer;
      pragma Warnings (Off, Prev);
      --  GNAT will issue a "warning: "Prev" may be null" which cannot occur
      --  since Prev is set to Tmp at the end of each iteration.

   begin
      loop
         exit when Index >= Buffer'Length;

         First := Index;
         Skip_To_Char (Buffer, Index, ASCII.LF);
         Tmp := new Line_Buffer_Record;

         if First = Buffer'First then
            B.First   := Tmp;
            B.Current := B.First;

         else
            Prev.Next := Tmp;
         end if;

         if Index < Buffer'Length and then Buffer (Index + 1) = ASCII.CR then
            Index := Index + 1;
         end if;

         Tmp.Line := new String (1 .. Index - First + 1);
         Tmp.Line.all := Buffer (First .. Index);
         Index := Index + 1;
         Prev := Tmp;
      end loop;

      return B;
   end To_Line_Buffer;

   -----------
   -- Print --
   -----------

   procedure Print (Buffer : Extended_Line_Buffer) is
      Tmp : Line_Buffer := Buffer.First;
   begin
      loop
         exit when Tmp = null;
         Put (Tmp.Line.all);
         Tmp := Tmp.Next;
      end loop;
   end Print;

   ----------
   -- Free --
   ----------

   procedure Free (Buffer : in out Extended_Line_Buffer) is
      Tmp  : Line_Buffer := Buffer.First;
      Prev : Line_Buffer;

   begin
      loop
         exit when Tmp = null;
         Prev := Tmp;
         Tmp := Tmp.Next;
         Free (Prev.Line);
         Free (Prev);
      end loop;
   end Free;

   ------------------
   -- Replace_Text --
   ------------------

   procedure Replace_Text
     (Buffer  : in out Extended_Line_Buffer;
      Line    : Natural;
      First   : Natural;
      Last    : Natural;
      Replace : String)
   is
      S : String_Access;
   begin
      if Buffer.First = null then
         --  No replacing actually requested
         return;
      end if;

      if Buffer.Current_Line <= Line then
         for J in 1 .. Line - Buffer.Current_Line loop
            Buffer.Current := Buffer.Current.Next;
         end loop;
      else
         Buffer.Current := Buffer.First;

         for J in 1 .. Line - 1 loop
            Buffer.Current := Buffer.Current.Next;
         end loop;
      end if;

      Buffer.Current_Line := Line;

      if Last - First = Replace'Length then
         --  Simple case, no need to reallocate buffer

         Buffer.Current.Line (First .. Last - 1) := Replace;

      else
         S := new String
           (1 .. Buffer.Current.Line'Last - ((Last - First) - Replace'Length));
         S (1 .. First - 1) := Buffer.Current.Line (1 .. First - 1);
         S (First .. First + Replace'Length - 1) := Replace;
         S (First + Replace'Length .. S'Last) :=
           Buffer.Current.Line (Last .. Buffer.Current.Line'Last);

         Free (Buffer.Current.Line);
         Buffer.Current.Line := S;
      end if;
   end Replace_Text;

end Line_Buffers;
