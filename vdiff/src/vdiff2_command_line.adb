------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Basic_Types;                  use Basic_Types;
with Vdiff2_Module.Utils;          use Vdiff2_Module.Utils;
with Vdiff2_Module.Utils.Shell_Command; use Vdiff2_Module.Utils.Shell_Command;
with Vdiff2_Module.Utils.Text;     use Vdiff2_Module.Utils.Text;
with GPS.Editors;                  use GPS.Editors;
with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;

package body Vdiff2_Command_Line is

   use Diff_Head_List;
   use Diff_Chunk_List;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Diff_Command_Line;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
   pragma Unreferenced (Context);
   begin
      return Execute (Command);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Diff_Command_Line) return Command_Return_Type
   is
      use Diff_Head_List.Std_Vectors;

      Diff : Diff_Head_Access;
   begin

      if Has_Element (Command.Head)
        and Command.Line > 0
        and Command.Action /= null
      then
         Diff := Element (Command.Head);
         Diff.Current_Node := Is_In_Diff_Chunk_List
           (Command.File, Diff.all, Command.Line);

         if Diff_Chunk_List.Std_Vectors.Has_Element (Diff.Current_Node) then
            Command.Action
              (Command.Kernel, Diff,
               Command.Line, Command.File);
         end if;
      end if;

      return Commands.Success;

   exception
      when others =>
         return Failure;
   end Execute;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item      : out Diff_Command_Line_Access;
      Kernel    : Kernel_Handle;
      List_Diff : Diff_Head_List_Access;
      File      : Virtual_File;
      Line      : Natural;
      Action    : Vdiff2_Command_Line.Handler_Action_Line) is
   begin

      Item           := new Diff_Command_Line;
      Item.Kernel    := Kernel;
      Item.List_Diff := List_Diff;
      Item.File      := File;
      Item.Line      := Line;
      Item.Action    := Action;
      Item.Head      := Get_Diff_Node (File, List_Diff.all);
   end Create;

   ---------------------------
   -- Is_In_Diff_Chunk_List --
   ---------------------------

   function Is_In_Diff_Chunk_List
     (Selected_File : Virtual_File;
      Item          : Diff_Head;
      Line          : Natural)
      return Diff_Chunk_List.Std_Vectors.Cursor
   is
      use Diff_Chunk_List.Std_Vectors;

      List      : constant Diff_Chunk_List.Vector := Item.List;
      Curr_Node : Diff_Chunk_List.Std_Vectors.Cursor := List.First;
      Diff      : Diff_Chunk_Access;

   begin
      while Has_Element (Curr_Node) loop
         Diff := Element (Curr_Node);

         if Selected_File = Item.Files (1) then
            exit when Diff.Range1.First <= Line
              and then Diff.Range1.Last >= Line;

         elsif Selected_File = Item.Files (2) then
            exit when Diff.Range2.First <= Line
              and then Diff.Range2.Last >= Line;

         elsif Item.Files (3) /= GNATCOLL.VFS.No_File
           and then Selected_File = Item.Files (3)
         then
            exit when Diff.Range3.First <= Line
              and then Diff.Range3.Last >= Line;
         end if;

         Next (Curr_Node);
      end loop;

      return Curr_Node;
   end Is_In_Diff_Chunk_List;

   ----------------------
   -- Move_On_Ref_File --
   ----------------------

   procedure Move_On_Ref_File
     (Kernel : Kernel_Handle;
      Diff   : Diff_Head_Access;
      Line   : Natural := 0;
      File   : Virtual_File := GNATCOLL.VFS.No_File)
   is
      Diff1     : Diff_Chunk_Access;
      VFile    : T_VFile;
      VRange   : T_VRange;
      Num_File : T_Loc := 0;
      pragma Unreferenced (Line);

   begin
      Diff1 := Diff_Chunk_List.Std_Vectors.Element (Diff.Current_Node);

      VRange (1) := Diff1.Range1;
      VRange (2) := Diff1.Range2;
      VRange (3) := Diff1.Range3;
      VFile      := Diff.Files;

      for J in VFile'Range loop
         if File = VFile (J) then
            Num_File := J;
            exit;
         end if;
      end loop;

      if Num_File = Diff.Ref_File or Num_File = 0 then
         return;
      end if;

      Move_Block (Kernel, VFile (Num_File), VFile (Diff.Ref_File),
                  VRange (Num_File), VRange (Diff.Ref_File));

      declare
         Info : Line_Information_Array
           (Editable_Line_Type
              (VRange (Num_File).First - 1) ..
              Editable_Line_Type (VRange (Num_File).First - 1));
         Null_Line_Info : Line_Information_Record;

      begin
         Info (Info'First) := Null_Line_Info;
         Add_Line_Information (Kernel, File, "Vdiff2_Col_Merge", Info => Info);
         Unhighlight_Block
           (Kernel, VFile (Num_File), VRange (Num_File), "Change_diff");
         Unhighlight_Block
           (Kernel, VFile (Num_File), VRange (Num_File), "Append_diff");
      end;

      Diff1.Range1 := VRange (1);
      Diff1.Range2 := VRange (2);
      Diff1.Range3 := VRange (3);
   end Move_On_Ref_File;

   --------------------------
   -- Delete_From_Ref_File --
   --------------------------

   procedure Delete_From_Ref_File
     (Kernel : Kernel_Handle;
      Diff   : Diff_Head_Access;
      Line   : Natural := 0;
      File   : Virtual_File := GNATCOLL.VFS.No_File)
   is
      Diff1     : Diff_Chunk_Access;
      VFile    : T_VFile;
      VRange   : T_VRange;
      Num_File : T_Loc := 0;
      Other    : T_Loc := 0;
      pragma Unreferenced (Line);

   begin
      Diff1 := Diff_Chunk_List.Std_Vectors.Element (Diff.Current_Node);

      VRange (1) := Diff1.Range1;
      VRange (2) := Diff1.Range2;
      VRange (3) := Diff1.Range3;
      VFile      := Diff.Files;

      for J in VFile'Range loop
         if File = VFile (J) then
            Num_File := J;
            exit;
         end if;
      end loop;

      for J in VFile'Range loop
         if J /= Diff.Ref_File and J /= Num_File then
            Other := J;
            exit;
         end if;
      end loop;

      if Num_File = Diff.Ref_File or Num_File = 0 then
         return;
      end if;

      Delete_Block (Kernel, VFile (Diff.Ref_File),
                    VRange (Num_File), VRange (Diff.Ref_File));

      declare
         Info           : Line_Information_Array
           (Editable_Line_Type
              (VRange (Num_File).First - 1) ..
              Editable_Line_Type (VRange (Num_File).First - 1));
         Null_Line_Info : Line_Information_Record;

      begin
         Info (Info'First) := Null_Line_Info;
         Add_Line_Information (Kernel, File, "Vdiff2_Col_Merge", Info => Info);
         Unhighlight_Block
           (Kernel, VFile (Num_File), VRange (Num_File), "Change_diff");
         Unhighlight_Block
           (Kernel, VFile (Num_File), VRange (Num_File), "Append_diff");
      end;

      VRange (Num_File).Blank_Lines_Mark.Replace_Element
        (Add_Line
           (Kernel,
            GPS_Editor_Buffer'Class
              (Get_Buffer_Factory (Kernel).Get (VFile (Num_File))),
            Editable_Line_Type (VRange (Num_File).First),
            "Default_diff",
            (VRange (Other).Last - VRange (Other).First)));

      VRange (Diff.Ref_File).Blank_Lines_Mark.Replace_Element
        (Add_Line
           (Kernel,
            GPS_Editor_Buffer'Class
              (Get_Buffer_Factory (Kernel).Get (VFile (Diff.Ref_File))),
            Editable_Line_Type (VRange (Diff.Ref_File).First),
            "Default_diff",
            (VRange (Other).Last - VRange (Other).First)));

      Diff1.Range1 := VRange (1);
      Diff1.Range2 := VRange (2);
      Diff1.Range3 := VRange (3);
   end Delete_From_Ref_File;
end Vdiff2_Command_Line;
