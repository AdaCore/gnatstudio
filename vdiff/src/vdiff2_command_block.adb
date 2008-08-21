-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2008, AdaCore              --
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

with GNAT.OS_Lib;         use GNAT.OS_Lib;
with GNATCOLL.Utils;      use GNATCOLL.Utils;
with GPS.Intl;            use GPS.Intl;
with GPS.Kernel.Console;  use GPS.Kernel.Console;
with GPS.Kernel.Contexts; use GPS.Kernel.Contexts;
with GPS.Kernel.Scripts;  use GPS.Kernel.Scripts;
with Traces;              use Traces;
with GNATCOLL.VFS;                 use GNATCOLL.VFS;
with Vdiff2_Module.Utils; use Vdiff2_Module.Utils;
with Vdiff2_Module;       use Vdiff2_Module;

package body Vdiff2_Command_Block is

   use Diff_Head_List;
   use Diff_Chunk_List;

   Me : constant Debug_Handle := Create ("VDiff2_Command");
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

   -------------------------
   --  Unchecked_Execute  --
   -------------------------

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
         "Files (1): " & Full_Name (Command.Last_Active_Diff.Files (1)).all);
      return Execute (Command);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Diff_Command_Block) return Command_Return_Type
   is
      Context       : constant Selection_Context :=
                        Get_Current_Context (Command.Kernel);
      Curr_Node     : Diff_Head_List.List_Node;
      Diff          : Diff_Head_Access;
      Selected_File : Virtual_File;
   begin
      if Has_File_Information (Context)
        and then Has_Directory_Information (Context)
      then
         Selected_File := File_Information (Context);
         Curr_Node := Get_Diff_Node (Selected_File, Command.List_Diff.all);

         if Curr_Node /= Diff_Head_List.Null_Node then
            Diff := Diff_Head_List.Data (Curr_Node);
         else
            Diff := Command.Last_Active_Diff;
         end if;
      else
         Diff := Command.Last_Active_Diff;
      end if;

      if Diff /= null then
         Command.Action (Command.Kernel, Diff);

         if Diff.List = Diff_Chunk_List.Null_List then
            Remove_Nodes
              (Command.List_Diff.all,
               Prev (Command.List_Diff.all, Curr_Node),
               Curr_Node);
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

      if Tmp = Diff_Chunk_List.Null_List then
         Console.Insert (Kernel, -"No differences found.", Mode => Info);
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
      Args1 : Argument_List :=
                (1 => new String'(Full_Name (Files (1)).all));
      Args2 : Argument_List :=
                (1 => new String'(Full_Name (Files (2)).all));
      Args3 : Argument_List (1 .. 1);

   begin
      if Files (1) /= GNATCOLL.VFS.No_File then
         Execute_GPS_Shell_Command (Kernel, "Editor.close", Args1);
      end if;

      if Files (2) /= GNATCOLL.VFS.No_File then
         Execute_GPS_Shell_Command (Kernel, "Editor.close", Args2);
      end if;

      if Files (3) /= GNATCOLL.VFS.No_File then
         Args3 := (1 => new String'(Full_Name (Files (3)).all));
         Execute_GPS_Shell_Command (Kernel, "Editor.close", Args3);
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
      Diff   : access Diff_Head)is
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
      Free (Diff.List);
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
