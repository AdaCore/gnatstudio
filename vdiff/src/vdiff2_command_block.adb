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

with GNAT.OS_Lib;         use GNAT.OS_Lib;
with GNATCOLL.Utils;      use GNATCOLL.Utils;
with GPS.Intl;            use GPS.Intl;
with GPS.Kernel.Contexts; use GPS.Kernel.Contexts;
with GPS.Kernel.Scripts;  use GPS.Kernel.Scripts;
with GNATCOLL.Traces;              use GNATCOLL.Traces;
with GNATCOLL.Arg_Lists;       use GNATCOLL.Arg_Lists;
with GNATCOLL.VFS;                 use GNATCOLL.VFS;
with Vdiff2_Module.Utils; use Vdiff2_Module.Utils;
with Vdiff2_Module;       use Vdiff2_Module;

package body Vdiff2_Command_Block is

   use Diff_Head_List;
   use Diff_Chunk_List;

   Me : constant Trace_Handle := Create ("VDiff2_Command");
   Id : Diff_Head_List_Access;

   ------------
   -- Create --
   ------------

   procedure Create
     (Item      : out Diff_Command_Access;
      Kernel    : Kernel_Handle;
      List_Diff : Diff_Head_List_Access;
      Action    : Handler_Action) is

   begin
      Item := new Diff_Command_Block;
      Item.Kernel := Kernel;
      Item.List_Diff := List_Diff;

      if Id = null then
         Id := List_Diff;
      end if;

      Item.Action := Action;
   end Create;

   -----------------------
   -- Unchecked_Execute --
   -----------------------

   procedure Unchecked_Execute
     (Command : access Diff_Command_Block;
      Diff    : access Diff_Head) is
   begin
      Command.Action (Command.Kernel, Diff);
   end Unchecked_Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Diff_Command_Block;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Context);
   begin
      Trace
        (Me,
         "Files (1): " &
         Display_Full_Name (Command.Last_Active_Diff.Files (1)));
      return Execute (Command);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Diff_Command_Block) return Command_Return_Type
   is
      use Diff_Head_List.Std_Vectors;

      Context       : constant Selection_Context :=
                        Get_Current_Context (Command.Kernel);
      Curr_Node     : Diff_Head_List.Std_Vectors.Cursor;
      Diff          : Diff_Head_Access;
      Selected_File : Virtual_File;
   begin
      if Has_File_Information (Context)
        and then Has_Directory_Information (Context)
      then
         Selected_File := File_Information (Context);
         Curr_Node := Get_Diff_Node (Selected_File, Command.List_Diff.all);

         if Has_Element (Curr_Node) then
            Diff := Element (Curr_Node);
         else
            Diff := Command.Last_Active_Diff;
         end if;
      else
         Diff := Command.Last_Active_Diff;
      end if;

      if Diff /= null then
         Command.Action (Command.Kernel, Diff);

         if Diff.List.Is_Empty then
            Command.List_Diff.Delete (Curr_Node);
         end if;
      end if;

      return Commands.Success;
   exception
      when others =>
         return Failure;
   end Execute;

   -----------------------
   -- Reload_Difference --
   -----------------------

   procedure Reload_Difference
     (Kernel : Kernel_Handle;
      Item   : access Diff_Head)
   is
      Tmp : Diff_List;
   begin
      Unhighlight_Difference (Kernel, Item);

      if Item.Files (3) = GNATCOLL.VFS.No_File then
         Tmp := Diff (Kernel, Item.Files (1), Item.Files (2));
      else
         Tmp := Diff3 (Kernel, Item.Files (1), Item.Files (2), Item.Files (3));
      end if;

      if Tmp.Is_Empty then
         Kernel.Insert (-"No differences found.", Mode => Info);
      end if;

      Free_List (Item.List);
      Item.List := Tmp;
      Item.Current_Node := First (Item.List);
      Show_Differences3 (Kernel, Item);
   end Reload_Difference;

   ----------------------
   -- Close_Difference --
   ----------------------

   procedure Close_Difference
     (Kernel : Kernel_Handle;
      Diff   : access Diff_Head)
   is
      Files : constant T_VFile := Diff.Files;
      CL    : Arg_List;
      Args1 : Argument_List :=
                (1 => new String'(+Full_Name (Files (1))));
      Args2 : Argument_List :=
                (1 => new String'(+Full_Name (Files (2))));
      Args3 : Argument_List (1 .. 1);

   begin
      if Files (1) /= GNATCOLL.VFS.No_File then
         CL := Create ("Editor.close");
         Append_Argument (CL, +Full_Name (Files (1)), One_Arg);
         Execute_GPS_Shell_Command (Kernel, CL);
      end if;

      if Files (2) /= GNATCOLL.VFS.No_File then
         CL := Create ("Editor.close");
         Append_Argument (CL, +Full_Name (Files (2)), One_Arg);
         Execute_GPS_Shell_Command (Kernel, CL);
      end if;

      if Files (3) /= GNATCOLL.VFS.No_File then
         CL := Create ("Editor.close");
         Append_Argument (CL, +Full_Name (Files (3)), One_Arg);
         Execute_GPS_Shell_Command (Kernel, CL);
         --  At this point all the memory associated with Diff is freed
      end if;

      Free (Args1);
      Free (Args2);
      Free (Args3);
   end Close_Difference;

   ----------------------------
   -- Unhighlight_Difference --
   ----------------------------

   procedure Unhighlight_Difference
     (Kernel : Kernel_Handle;
      Diff   : access Diff_Head) is
   begin
      Hide_Differences (Kernel, Diff);
   end Unhighlight_Difference;

   -----------------------
   -- Remove_Difference --
   -----------------------

   procedure Remove_Difference
     (Kernel : Kernel_Handle;
      Diff   : access Diff_Head) is
   begin
      Unhighlight_Difference (Kernel, Diff);
      Diff.List.Clear;
   end Remove_Difference;

   ---------------------
   -- Change_Ref_File --
   ---------------------

   procedure Change_Ref_File
     (Kernel : Kernel_Handle;
      Diff   : access Diff_Head) is
   begin
      Unhighlight_Difference (Kernel, Diff);
      Show_Differences3 (Kernel, Diff);
   end Change_Ref_File;

end Vdiff2_Command_Block;
