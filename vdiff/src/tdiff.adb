-----------------------------------------------------------------------
--                               GPS                                 --
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

with Diff_Utils2;      use Diff_Utils2;
with Ada.Command_Line; use Ada.Command_Line;
with Ada.Text_IO;      use Ada.Text_IO;
with VFS;              use VFS;
procedure TDiff is
   use Diff_Chunk_List;

   procedure Print (R : Diff_Range);
   --  Print a given line range on standard output.

   -----------
   -- Print --
   -----------

   procedure Print (R : Diff_Range) is
   begin
      if R.First = R.Last then
         Put (Positive'Image (R.First));
      else
         Put (Positive'Image (R.First));
         Put (" ->");
         Put (Positive'Image (R.Last));
      end if;
   end Print;

   Result : Diff_List;
   Tmp_Node : Diff_List_Node;
   Tmp : Diff_Chunk_Access;

begin
   if Argument_Count /= 2 then
      Put_Line ("incorrect number of parameters. exiting.");
      return;
   end if;

   Result := Diff (Create (Argument (1)), Create (Argument (2)));
   Tmp_Node := First (Result);
   loop
      exit when Tmp_Node = Null_Node;
      Tmp := Data (Tmp_Node);
      Put (Diff_Action'Image (Tmp.Range2.Action));
      Put (" from");
      Print (Tmp.Range1);
      Put (" to");
      Print (Tmp.Range2);
      New_Line;

      Tmp_Node := Next (Tmp_Node);
   end loop;

   Free (Result);
end TDiff;