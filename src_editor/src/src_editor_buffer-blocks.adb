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
      Column        : Integer;
      Text          : GNAT.OS_Lib.String_Access;
      Block         : Block_Access;
      Buffer_Line   : Buffer_Line_Type;
      Block_Folded  : Boolean;

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

      Text := Get_String (Source_Buffer (Buffer));

      --  ??? See if we could have a line-by-line version of Parse_Constructs.

      Parse_Constructs (Buffer.Lang, Text.all, Constructs);
      GNAT.OS_Lib.Free (Text);

      Current := Constructs.First;

      while Current /= null loop
         Block_Folded := False;

         if Current.Category in Construct_Category
           or else Current.Category in Enclosing_Entity_Category
         then
            Line_Start := Editable_Line_Type (Current.Sloc_Start.Line);
            Line_End   := Editable_Line_Type (Current.Sloc_End.Line);
            Column     := Integer'Min
              (Current.Sloc_Start.Column, Current.Sloc_End.Column);

            Block := new Block_Record'
              (Indentation_Level => 0,
               Offset            => Column,
               First_Line        => Line_Start,
               Last_Line         => Line_End,
               Block_Type        => Current.Category,
               GC                => null);

            Buffer_Line := Get_Buffer_Line (Buffer, Line_Start);

            if Buffer_Line = 0 then
               Block_Folded := True;
            else
               Buffer.Line_Data (Buffer_Line).Block := Block;
               Append (Buffer.Blocks, Block);

               for J in Line_Start + 1 .. Line_End loop

                  Buffer_Line := Get_Buffer_Line (Buffer, J);

                  if Buffer_Line = 0 then
                     Block_Folded := True;
                     exit;
                  end if;

                  if Buffer.Line_Data (Buffer_Line).Block = null then
                     Buffer.Line_Data (Buffer_Line).Block := Block;
                  end if;
               end loop;
            end if;
         end if;

         --  Fill the folding information.
         --  ??? This needs to be optimized.

         --  ??? Packages should also be foldable. Right now they are left
         --  unfoldable so that the fold/unfold menus have a more interesting
         --  effect than simply folding the whole file. This will be solved
         --  when the notion of block nesting level is introduced.

         if (not Block_Folded)
           and then Current.Category in Subprogram_Category
           and then Current.Sloc_End.Line /= Current.Sloc_Start.Line
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
                  Get_Iter_At_Line (Buffer, Iter, Gint (Buffer_Line - 1) + 1);
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

         Current := Current.Next;
      end loop;

      Free (Constructs);
   end Compute_Blocks;

end Src_Editor_Buffer.Blocks;
