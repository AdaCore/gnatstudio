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

with Src_Editor_Buffer.Line_Information;
use Src_Editor_Buffer.Line_Information;

with Src_Editor_Module; use Src_Editor_Module;

with Commands.Editor; use Commands.Editor;
with Gtk.Text_Iter;   use Gtk.Text_Iter;

package body Src_Editor_Buffer.Blocks is

   use Block_List;

   --------------------
   -- Compute_Blocks --
   --------------------

   procedure Compute_Blocks (Buffer : access Source_Buffer_Record'Class) is
      Constructs    : Construct_List;
      Current       : Construct_Access;
      Line_Start    : Editable_Line_Type;
      Line_End      : Editable_Line_Type;
      Text          : GNAT.OS_Lib.String_Access;
      Block         : Block_Access;
      Buffer_Line   : Buffer_Line_Type;
      First_Buffer_Line : Buffer_Line_Type;

   begin
      Buffer.Blocks_Need_Parsing := False;

      if Buffer.Lang = null then
         Buffer.Parse_Blocks := False;
         Buffer_Information_Changed (Buffer);
         return;
      end if;

      --  Free the previous block information.

      Free (Buffer.Blocks);

      if Buffer.Line_Data /= null then
         for Line in Buffer.Line_Data'Range loop
            Buffer.Line_Data (Line).Block := null;
         end loop;
      end if;

      if Buffer.Block_Folding then
         Remove_Block_Folding_Commands (Buffer, False);
      end if;

      Text := Get_String (Source_Buffer (Buffer));

      --  ??? See if we could have a line-by-line version of Parse_Constructs.

      Parse_Constructs (Buffer.Lang, Text.all, Constructs);
      GNAT.OS_Lib.Free (Text);

      Current := Constructs.First;

      while Current /= null loop
         if Buffer.Block_Highlighting
           and then (Current.Category in Construct_Category
                     or else Current.Category in Enclosing_Entity_Category)
         then
            Line_Start := Editable_Line_Type (Current.Sloc_Start.Line);
            Line_End   := Editable_Line_Type (Current.Sloc_End.Line);

            Block := new Block_Record'
              (Indentation_Level => 0,
               Offset_Start      => Current.Sloc_Start.Column,
               Offset_End        => Current.Sloc_End.Column,
               First_Line        => Line_Start,
               Last_Line         => Line_End,
               Block_Type        => Current.Category,
               GC                => null);

            First_Buffer_Line := Get_Buffer_Line (Buffer, Line_Start);

            if First_Buffer_Line /= 0 then
               for J in Line_Start .. Line_End loop
                  Buffer_Line := Get_Buffer_Line (Buffer, J);

                  if Buffer_Line /= 0 then
                     if Buffer.Line_Data (Buffer_Line).Block = null then
                        Buffer.Line_Data (Buffer_Line).Block := Block;
                     end if;
                  end if;
               end loop;

               Buffer.Line_Data (First_Buffer_Line).Block := Block;
               Append (Buffer.Blocks, Block);
            else
               Unchecked_Free (Block);
            end if;
         end if;

         --  Fill the folding information.
         --  ??? This needs to be optimized.

         --  ??? Packages should also be foldable. Right now they are left
         --  unfoldable so that the fold/unfold menus have a more interesting
         --  effect than simply folding the whole file. This will be solved
         --  when the notion of block nesting level is introduced.

         if Buffer.Block_Folding
           and then Current.Category in Subprogram_Category
           and then Current.Sloc_End.Line /= Current.Sloc_Start.Line
         then
            --  Do nothing if the block is folded.

            if Get_Buffer_Line
              (Buffer, Editable_Line_Type (Current.Sloc_Start.Line + 1)) /= 0
            then
               declare
                  Command : Hide_Editable_Lines_Command;
                  Iter    : Gtk_Text_Iter;
                  Buffer_Line : Buffer_Line_Type;
               begin
                  Buffer_Line := Get_Buffer_Line
                    (Buffer, Editable_Line_Type (Current.Sloc_Start.Line));

                  if Buffer_Line /= 0 then
                     Command := new Hide_Editable_Lines_Type;
                     Command.Buffer := Source_Buffer (Buffer);
                     Get_Iter_At_Line
                       (Buffer, Iter, Gint (Buffer_Line - 1) + 1);
                     Command.Mark   := Create_Mark (Buffer, "", Iter);
                     Command.Number := Editable_Line_Type
                       (Current.Sloc_End.Line - Current.Sloc_Start.Line) - 1;

                     Add_Block_Command
                       (Buffer,
                        Buffer_Line,
                        Command_Access (Command),
                        Hide_Block_Pixbuf);
                  end if;
               end;
            end if;
         end if;

         Current := Current.Next;
      end loop;

      Free (Constructs);
   end Compute_Blocks;

   -----------------------
   -- Get_Screen_Offset --
   -----------------------

   function Get_Screen_Offset
     (Buffer : access Source_Buffer_Record'Class;
      Block  : Block_Record) return Integer
   is
      Iter             : Gtk_Text_Iter;
      Line, Col1, Col2 : Gint;
   begin
      --  The heuristics to determine the right offset for a block is to look
      --  at the offsets of the first and last lines of the block, and to hope
      --  that no line of the block will be indented less than either of those
      --  lines. This is in order to get a good compromise between aesthetics
      --  and speed.

      Get_Iter_At_Line_Offset
        (Buffer,
         Iter,
         Gint (Get_Buffer_Line (Buffer, Block.First_Line) - 1),
         Gint (Block.Offset_Start));

      Get_Screen_Position (Buffer, Iter, Line, Col1);

      Get_Iter_At_Line_Offset
        (Buffer,
         Iter,
         Gint (Get_Buffer_Line (Buffer, Block.Last_Line) - 1),
         Gint (Block.Offset_End));

      Get_Screen_Position (Buffer, Iter, Line, Col2);

      return Integer'Min (Integer (Col1), Integer (Col2));
   end Get_Screen_Offset;

end Src_Editor_Buffer.Blocks;
