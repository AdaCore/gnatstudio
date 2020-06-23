------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2020, AdaCore                     --
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

with GNAT.Regpat;

with Glib.Unicode;      use Glib.Unicode;
with Gtk.Text_Iter;     use Gtk.Text_Iter;

with Language;          use Language;
with Src_Editor_Buffer.Line_Information;
with Src_Editor_Module; use Src_Editor_Module;
with Commands.Editor;   use Commands.Editor;

with GPS.Kernel.Preferences;

package body Src_Editor_Buffer.Blocks is

   use Src_Editor_Buffer.Line_Information;

   -----------------------------
   -- Is_Block_Already_Folded --
   -----------------------------

   function Is_Block_Already_Folded
     (Buffer     : access Source_Buffer_Record'Class;
      Start_Line : Natural) return Boolean
   is
     (for some Folded_Block of Buffer.Folded_Blocks =>
         Folded_Block.Start_Mark.Element.Line = Start_Line);

   --------------------
   -- Compute_Blocks --
   --------------------

   procedure Compute_Blocks
     (Buffer    : access Source_Buffer_Record'Class;
      Immediate : Boolean) is
   begin
      if not Buffer.Block_Folding then
         --  Folding is not allowed
         return;
      end if;

      if Buffer.Folding_Provider /= null
        and then Buffer.Folding_Provider.Compute_Blocks (Buffer.Filename)
      then
         --  Provider will do/already done all work
         return;
      end if;

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
         declare
            Tree : Semantic_Tree'Class := Buffer.Get_Tree;

         begin
            Tree.Update;
            Buffer.Blocks_Exact := True;
         end;
      end if;

      Remove_Block_Folding_Commands (Buffer, False);

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

               if Buffer.Get_Language.Is_Foldable_Block (Node.Category)
                 and then Node.Sloc_End.Line /= Node.Sloc_Start.Line
               then
                  --  Do nothing if the block is already folded

                  if not Is_Block_Already_Folded
                    (Buffer, Node.Sloc_Start.Line)
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

   ----------------
   -- Set_Blocks --
   ----------------

   procedure Set_Blocks
     (Buffer : access Source_Buffer_Record'Class;
      Blocks : Blocks_Vector.Vector)
   is
      use GPS.Kernel.Preferences;

      function Match (First, Last : Editable_Line_Type) return Boolean;

      -----------
      -- Match --
      -----------

      function Match (First, Last : Editable_Line_Type) return Boolean is
         Text : constant String := To_String
           (Buffer.Get_Text (First, 1, Last, 0));
      begin
         return (Fold_Comment_Reg1.Get_Pref /= ""
             and then GNAT.Regpat.Match (Fold_Comment_Reg1.Get_Pref, Text))
           or else (Fold_Comment_Reg2.Get_Pref /= ""
                    and then GNAT.Regpat.Match
                      (Fold_Comment_Reg2.Get_Pref, Text))
           or else (Fold_Comment_Reg3.Get_Pref /= ""
                    and then GNAT.Regpat.Match
                      (Fold_Comment_Reg3.Get_Pref, Text));
      end Match;

   begin
      Remove_Block_Folding_Commands (Buffer, False);

      for Block of Blocks loop
         if not Is_Block_Already_Folded
           (Buffer, Natural (Block.First_Line))
         then
            declare
               Command     : Hide_Editable_Lines_Command;
               Buffer_Line : Buffer_Line_Type;
               Dummy       : Boolean;
            begin
               Buffer_Line := Get_Buffer_Line (Buffer, Block.First_Line);

               if Buffer_Line /= 0 then
                  Command        := new Hide_Editable_Lines_Type;
                  Command.Buffer := Source_Buffer (Buffer);
                  Command.Number := Block.Last_Line - Block.First_Line;

                  Add_Block_Command
                    (Buffer,
                     Block.First_Line,
                     Command_Access (Command),
                     Hide_Block_Pixbuf);

                  if not Buffer.Auto_Folded then
                     case Block.Kind is
                     when Imports =>
                        if Fold_With_Use_Blocks.Get_Pref /= 0
                          and then Integer (Command.Number) >=
                          Fold_With_Use_Blocks.Get_Pref
                        then
                           Dummy := Fold_Unfold_Line
                             (Buffer, Block.First_Line, True);
                        end if;

                     when Comment =>
                        if (Autofold_Comment_Blocks.Get_Pref /= 0
                            and then Integer (Command.Number) >=
                              Autofold_Comment_Blocks.Get_Pref)
                          or else Match (Block.First_Line, Block.Last_Line)
                        then
                           Dummy := Fold_Unfold_Line
                             (Buffer, Block.First_Line, True);
                        end if;
                     when others =>
                        null;
                     end case;
                  end if;
               end if;
            end;
         end if;
      end loop;

      Buffer.Auto_Folded := True;
      Buffer_Information_Changed (Buffer);
   end Set_Blocks;

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
