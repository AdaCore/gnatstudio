-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

with Language;    use Language;
with Basic_Types; use Basic_Types;
with Interfaces.C;
with Interfaces.C.Strings;

package body Src_Editor_Buffer.Blocks is

   use Block_List;

   function Strlen
     (Str : Gtkada.Types.Chars_Ptr) return Interfaces.C.size_t;
   pragma Import (C, Strlen);

   --------------------
   -- Compute_Blocks --
   --------------------

   procedure Compute_Blocks (Buffer : access Source_Buffer_Record'Class) is
      Constructs    : Construct_List;
      Current       : Construct_Access;
      Line_Start    : Integer;
      Line_End      : Integer;
      Column        : Integer;
      C_Str         : Gtkada.Types.Chars_Ptr := Gtkada.Types.Null_Ptr;
      Slice_Length  : Natural;
      Slice         : Unchecked_String_Access;
      Block         : Block_Access;
      pragma Suppress (Access_Check, Slice);

      use type Interfaces.C.Strings.chars_ptr;

   begin
      if not Buffer.Parse_Blocks then
         return;
      end if;

      if Buffer.Lang = null then
         Buffer.Parse_Blocks := False;
         Buffer_Information_Changed (Buffer);
         return;
      end if;

      --  Free the previous block information.

      Free (Buffer.Blocks);

      for Line in Buffer.Line_Data'Range loop
         Buffer.Line_Data (Line).Block := null;
      end loop;

      C_Str := Get_Slice (Buffer, 0, 0);

      if C_Str = Gtkada.Types.Null_Ptr then
         return;
      end if;

      Slice        := To_Unchecked_String (C_Str);
      Slice_Length := Natural (Strlen (C_Str));

      Parse_Constructs (Buffer.Lang, Slice (1 .. Slice_Length), Constructs);
      Current := Constructs.First;

      while Current /= null loop
         if Current.Category in Construct_Category
           or else Current.Category in Enclosing_Entity_Category
         then
            Line_Start := Current.Sloc_Start.Line;
            Line_End   := Current.Sloc_End.Line;
            Column     := Integer'Min
              (Current.Sloc_Start.Column, Current.Sloc_End.Column);

            Block := new Block_Record'
              (Indentation_Level => 0,
               Offset            => Column,
               First_Line        => Line_Start,
               Last_Line         => Line_End,
               Block_Type        => Current.Category,
               GC                => null);

            Buffer.Line_Data (Line_Start).Block := Block;

            Append (Buffer.Blocks, Block);

            for J in Line_Start + 1 .. Line_End loop
               if Buffer.Line_Data (J).Block = null then
                  Buffer.Line_Data (J).Block := Block;
               end if;
            end loop;
         end if;

         Current := Current.Next;
      end loop;

      Free (Constructs);
   end Compute_Blocks;

end Src_Editor_Buffer.Blocks;
