-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2001-2003                    --
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

with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Glide_Kernel.Scripts; use Glide_Kernel.Scripts;
with Basic_Types;          use Basic_Types;
with GNAT.OS_Lib;          use GNAT.OS_Lib;
with Diff_Utils2;          use Diff_Utils2;
with Vdiff2_Module.Utils;  use Vdiff2_Module.Utils;
with Traces; use Traces;
with VFS;                  use VFS;

package body Vdiff2_Command is
   Me                  : constant Debug_Handle := Create ("VDiff2_Command");
   use Diff_Head_List;
   use Diff_Chunk_List;
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
      Item           := new Diff_Command;
      Item.Kernel    := Kernel;
      Item.List_Diff := List_Diff;

      if Id = null then
         Id := List_Diff;
      end if;

      Item.Action    := Action;
   end Create;

   -------------------------
   --  Unchecked_Execute  --
   -------------------------

   procedure Unchecked_Execute
     (Command : access Diff_Command;
      Diff    : in out Diff_Head_List.List_Node)
   is
      Tmp : Diff_Head_Access := new Diff_Head;
   begin
      if Diff /= Diff_Head_List.Null_Node then
         Tmp.all := Data (Diff);
         Command.Action (Command.Kernel, Tmp);
      end if;

      Free (Tmp);
   end Unchecked_Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Diff_Command;
      Event   : Gdk.Event.Gdk_Event) return Command_Return_Type
   is
      pragma Unreferenced (Event);
   begin
      return Execute (Command);
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Diff_Command)
      return Command_Return_Type
   is
      Context       : constant Selection_Context_Access
        := Get_Current_Context (Command.Kernel);
      Curr_Node     : Diff_Head_List.List_Node;
      Diff          : Diff_Head_Access := new Diff_Head;
      Selected_File : Virtual_File;

   begin
      if Has_File_Information (File_Selection_Context_Access (Context))
        and then
          Has_Directory_Information (File_Selection_Context_Access (Context))
      then
         Selected_File :=
           File_Information (File_Selection_Context_Access (Context));

         Curr_Node := Is_In_Diff_List (Selected_File, Command.List_Diff.all);

         if Curr_Node /= Diff_Head_List.Null_Node then
            Diff.all := Data (Curr_Node);
            Trace (Me, "Execute Action");
            Command.Action (Command.Kernel, Diff);
            if Diff /= null then
               Set_Data (Curr_Node, Diff.all);
            else
               Remove_Nodes (Command.List_Diff.all,
                             Prev (Command.List_Diff.all, Curr_Node),
                             Curr_Node);
            end if;
            Free (Diff);
         end if;
      end if;
      return Commands.Success;
   exception
      when others =>
         return Failure;
   end Execute;

   ---------------------
   -- Next_Difference --
   ---------------------

   procedure Next_Difference
     (Kernel : Kernel_Handle;
      Diff   : in out  Diff_Head_Access)
   is
      Link      : Diff_List_Node;
      Curr_Data : Diff_Chunk_Access;
   begin
      if Next (Diff.Current_Node) /= Diff_Chunk_List.Null_Node then
         Diff.Current_Node := Next (Diff.Current_Node);
         Link := Diff.Current_Node;
         Curr_Data := Data (Link);
         Trace (Me, "Execute Action Next");
         Goto_Difference (Kernel, Curr_Data);
      end if;
   end Next_Difference;

   ---------------------
   -- Prev_Difference --
   ---------------------

   procedure Prev_Difference
     (Kernel : Kernel_Handle;
      Diff   : in out Diff_Head_Access)
   is
      Link      : Diff_List_Node;
      Curr_Data : Diff_Chunk_Access;
   begin
      Link := Prev (Diff.List, Diff.Current_Node);
      if  Link /= Diff_Chunk_List.Null_Node then
         Diff.Current_Node := Link;
         Curr_Data := Data (Link);
         Trace (Me, "Execute Action Prev");
         Goto_Difference (Kernel, Curr_Data);
      end if;
   end Prev_Difference;

   ---------------------
   -- First_Difference --
   ---------------------

   procedure First_Difference
     (Kernel : Kernel_Handle;
      Diff   : in out Diff_Head_Access)
   is
      Link      : Diff_List_Node;
      Curr_Data : Diff_Chunk_Access;
   begin
      if Diff.Current_Node /= First (Diff.List) then
         Diff.Current_Node := First (Diff.List);
         Link := Diff.Current_Node;
         Curr_Data := Data (Link);
         Trace (Me, "Execute Action First");
         Goto_Difference (Kernel, Curr_Data);
      end if;
   end First_Difference;

   ---------------------
   -- Last_Difference --
   ---------------------

   procedure Last_Difference
     (Kernel : Kernel_Handle;
      Diff   : in out Diff_Head_Access)
   is
      Link     : Diff_List_Node;
      Curr_Data : Diff_Chunk_Access;
   begin
      if Diff.Current_Node /= Last (Diff.List) then
         Diff.Current_Node := Last (Diff.List);
         Link := Diff.Current_Node;
         Curr_Data := Data (Link);
         Trace (Me, "Execute Action Last");
         Goto_Difference (Kernel, Curr_Data);
      end if;
   end Last_Difference;

   -----------------------
   -- Reload_Difference --
   -----------------------

   procedure Reload_Difference
     (Kernel : Kernel_Handle;
      Item   : in out Diff_Head_Access)
   is
      Tmp : Diff_List;
   begin
      Unhighlight_Difference (Kernel, Item);
      if Item.File3 = VFS.No_File then
         Trace (Me, "Execute Action Diff");
         Tmp := Diff
           (Kernel, Item.File1,
            Item.File2);
      else
         Trace (Me, "Execute Action Diff3");
         Tmp := Diff3
           (Kernel, Item.File1,
            Item.File2, Item.File3);
      end if;

      if Tmp = Diff_Chunk_List.Null_List then
         Trace (Me, "no difference found it's not normal");
      else
         Free_List (Item.List);
         Item.List := Tmp;
      end if;
      --  ??? Just For the Moment
         Item.Current_Node := First (Item.List);
         Modify_Differences (Kernel, Item.all, Id);
   end Reload_Difference;


   ----------------------
   -- Close_Difference --
   ----------------------

   procedure Close_Difference
     (Kernel : Kernel_Handle;
      Diff   : in out Diff_Head_Access)
   is
      Args1 : Argument_List := (1 => new String'(Full_Name (Diff.File1)));
      Args2 : Argument_List := (1 => new String'(Full_Name (Diff.File2)));
      Args3 : Argument_List (1 .. 1);
   begin

      if Diff.File3 /= VFS.No_File then
         Args3 := (1 => new String'(Full_Name (Diff.File3)));
         --  After this call all memory associated when the Diff is Free
         Execute_GPS_Shell_Command (Kernel, "close", Args3);
      end if;

      if Diff.File1 /= VFS.No_File then
         Execute_GPS_Shell_Command (Kernel, "close", Args1);
      end if;

      if Diff.File2 /= VFS.No_File then
         Execute_GPS_Shell_Command (Kernel, "close", Args2);
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
      Diff   : in out Diff_Head_Access)is
   begin
      Hide_Differences (Kernel, Diff.all);
   end Unhighlight_Difference;

   -----------------------
   -- Remove_Difference --
   -----------------------

   procedure Remove_Difference
     (Kernel : Kernel_Handle;
      Diff   : in out Diff_Head_Access) is
   begin
      Unhighlight_Difference (Kernel, Diff);
      Free (Diff.List);
   end Remove_Difference;

   ---------------------
   -- Change_Ref_File --
   ---------------------

   procedure Change_Ref_File
     (Kernel : Kernel_Handle;
      Diff   : in out Diff_Head_Access) is
   begin
      Unhighlight_Difference (Kernel, Diff);
      Modify_Differences (Kernel, Diff.all, Id);
   end Change_Ref_File;

end Vdiff2_Command;
