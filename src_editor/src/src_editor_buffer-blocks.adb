------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2018, AdaCore                     --
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

with Language;          use Language;
with Src_Editor_Buffer.Line_Information;
with Src_Editor_Module; use Src_Editor_Module;

with Commands.Editor;   use Commands.Editor;
with Glib.Unicode;      use Glib.Unicode;
with Gtk.Text_Iter;     use Gtk.Text_Iter;

package body Src_Editor_Buffer.Blocks is

   use Src_Editor_Buffer.Line_Information;

   --------------------
   -- Compute_Blocks --
   --------------------

   procedure Compute_Blocks
     (Buffer    : access Source_Buffer_Record'Class;
      Immediate : Boolean) is
   begin
      if Buffer.Lang = null then
         Buffer.Parse_Blocks := False;
         Buffer_Information_Changed (Buffer);
         return;
      end if;

      if Buffer.Blocks_Exact then
         return;
      end if;

      if not Immediate
        and then not Buffer.Get_Tree.Is_Ready
      then
         return;
      end if;

      if Immediate then
         Buffer.Get_Tree.Update;
         Buffer.Blocks_Exact := True;
      end if;

      if Buffer.Block_Folding then
         Remove_Block_Folding_Commands (Buffer, False);
      end if;

      declare
         Current : Semantic_Tree_Iterator'Class
           := Buffer.Get_Tree.Root_Iterator;
      begin
         while Has_Element (Current) loop
            declare
               Node : constant Semantic_Node'Class := Element (Current);
            begin
               --  Fill the folding information
               --  ??? This needs to be optimized.

               if Buffer.Block_Folding
                 and then Node.Category not in Cat_Subtype .. Cat_Include
                 and then Node.Sloc_End.Line /= Node.Sloc_Start.Line
               then
                  --  Do nothing if the block is folded

                  if Get_Buffer_Line
                    (Buffer,
                     Editable_Line_Type (Node.Sloc_Start.Line + 1)) /= 0
                  then
                     declare
                        Command     : Hide_Editable_Lines_Command;
                        Buffer_Line : Buffer_Line_Type;
                     begin
                        Buffer_Line := Get_Buffer_Line
                          (Buffer, Editable_Line_Type (Node.Sloc_Start.Line));

                        if Buffer_Line /= 0 then
                           Command        := new Hide_Editable_Lines_Type;
                           Command.Buffer := Source_Buffer (Buffer);
                           Command.Number := Editable_Line_Type
                             (Node.Sloc_End.Line - Node.Sloc_Start.Line);

                           Add_Block_Command
                             (Buffer,
                              Editable_Line_Type (Node.Sloc_Start.Line),
                              Command_Access (Command),
                              Hide_Block_Pixbuf);
                        end if;
                     end;
                  end if;
               end if;
            end;
            Next (Current);
         end loop;
      end;

      Buffer_Information_Changed (Buffer);
   end Compute_Blocks;

   -----------------------------
   -- Calculate_Screen_Offset --
   -----------------------------

   procedure Calculate_Screen_Offset
     (Buffer : access Source_Buffer_Record'Class;
      Block  : in out Block_Record)
   is
      Iter      : Gtk_Text_Iter;
      Line, Col : Gint;
      Result    : Boolean;

   begin
      if Block.Stored_Offset /= 0
        and then Blocks_Are_Exact (Buffer)
      then
         return;
      end if;

      --  The heuristic to determine the right offset for a block is to look
      --  for the first non blank character on the line.

      Get_Iter_At_Line_Offset
        (Buffer,
         Iter,
         Gint (Get_Buffer_Line (Buffer, Block.First_Line) - 1), 0);

      while Is_Space (Get_Char (Iter)) and then not Ends_Line (Iter) loop
         Forward_Char (Iter, Result);
      end loop;

      Get_Screen_Position (Buffer, Iter, Line, Col);

      Block.Stored_Offset := Integer (Col) + 1;
   end Calculate_Screen_Offset;

end Src_Editor_Buffer.Blocks;
