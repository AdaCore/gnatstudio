-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2005                       --
--                              AdaCore                              --
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

with Ada.Exceptions;                    use Ada.Exceptions;

with GNAT.OS_Lib;                       use GNAT.OS_Lib;

with Gtk.Window;                        use Gtk.Window;
with Gtkada.Dialogs;                    use Gtkada.Dialogs;
with Gtkada.File_Selector;              use Gtkada.File_Selector;
with Gtkada.MDI;                        use Gtkada.MDI;

with GPS.Kernel.Contexts;               use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;                    use GPS.Kernel.MDI;
with GPS.Kernel.Modules;                use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;            use GPS.Kernel.Preferences;
with GPS.Kernel.Standard_Hooks;         use GPS.Kernel.Standard_Hooks;
with GPS.Intl;                          use GPS.Intl;

with Commands;                          use Commands;
with Diff_Utils2;                       use Diff_Utils2;
with Vdiff2_Command_Block;              use Vdiff2_Command_Block;
with Vdiff2_Module.Utils;               use Vdiff2_Module.Utils;
with Vdiff2_Module.Utils.Shell_Command; use Vdiff2_Module.Utils.Shell_Command;
with OS_Utils;                          use OS_Utils;
with Traces;                            use Traces;
with VFS;                               use VFS;

package body Vdiff2_Module.Callback is

   use Diff_Head_List;

   function Get_Ref_Filename (File : Virtual_File) return String;
   pragma Inline (Get_Ref_Filename);
   --  Returns the ref filename for the give file

   ----------------------
   -- Get_Ref_Filename --
   ----------------------

   function Get_Ref_Filename (File : Virtual_File) return String is
   begin
      return Get_Tmp_Dir & "ref$" & Base_Name (File);
   end Get_Ref_Filename;

   ---------------------------
   -- On_Compare_Tree_Files --
   ---------------------------

   procedure On_Compare_Three_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      File1  : constant Virtual_File :=
        Select_File
          (Title             => -"Select Common Ancestor",
           Parent            => Get_Current_Window (Kernel),
           Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
           Kind              => Open_File,
           File_Pattern      => "*.*;{*.ads,*.adb};{*.c,*.h,*.cpp}",
           Pattern_Name      => -"All files;GNAT Ada default;C/C++ files",
           History           => Get_History (Kernel));
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Widget, Button);

   begin
      if File1 = VFS.No_File then
         return;
      end if;

      declare
         File2 : constant Virtual_File :=
           Select_File
             (Title             => -"Select First Changes",
              Base_Directory    => VFS.Get_Current_Dir,
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
              Kind              => Open_File,
              File_Pattern      => "*.*;{*.ads,*.adb};{*.c,*.h,*.cpp}",
              Pattern_Name      => -"All files;GNAT Ada default;C/C++ files",
              History           => Get_History (Kernel));
         Dummy : Command_Return_Type;
         pragma Unreferenced (Dummy);

      begin
         if File2 = VFS.No_File then
            return;
         end if;

         declare
            File3 : constant Virtual_File :=
              Select_File
                (Title             => -"Select Second Changes",
                 Base_Directory    => VFS.Get_Current_Dir,
                 Parent            => Get_Current_Window (Kernel),
                 Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
                 Kind              => Open_File,
                 File_Pattern      => "*.*;{*.ads,*.adb};{*.c,*.h,*.cpp}",
                 Pattern_Name      =>
                   -"All files;GNAT Ada default;C/C++ files",
                 History           => Get_History (Kernel));
         begin
            if File3 = VFS.No_File then
               Visual_Diff (File1, File2);
               return;
            end if;

            Visual_Diff (File2, File1, File3);
         end;
      end;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Compare_Three_Files;

   --------------------------
   -- On_Compare_Two_Files --
   --------------------------

   procedure On_Compare_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      File1  : constant Virtual_File :=
        Select_File
          (Title             => -"Select First File",
           Base_Directory    => VFS.Get_Current_Dir,
           Parent            => Get_Current_Window (Kernel),
           Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
           Kind              => Open_File,
           File_Pattern      => "*.*;{*.ads,*.adb};{*.c,*.h,*.cpp}",
           Pattern_Name      => -"All files;GNAT Ada default;C/C++ files",
           History           => Get_History (Kernel));
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Widget, Button);

   begin
      if File1 = VFS.No_File then
         return;
      end if;

      declare
         File2 : constant Virtual_File :=
           Select_File
             (Title             => -"Select Second File",
              Base_Directory    => VFS.Get_Current_Dir,
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
              Kind              => Open_File,
              File_Pattern      => "*.*;{*.ads,*.adb};{*.c,*.h,*.cpp}",
              Pattern_Name      => -"All files;GNAT Ada default;C/C++ files",
              History           => Get_History (Kernel));

      begin
         if File2 = VFS.No_File then
            return;
         end if;

         Visual_Diff (File1, File2);
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Compare_Two_Files;

   -------------------------
   -- On_Merge_Tree_Files --
   -------------------------

   procedure On_Merge_Three_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Item   : Diff_Head;
      File1  : constant Virtual_File :=
        Select_File
          (Title             => -"Select Common Ancestor",
           Base_Directory    => VFS.No_File,
           Parent            => Get_Current_Window (Kernel),
           Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
           Kind              => Open_File,
           File_Pattern      => "*.*;{*.ads,*.adb};{*.c,*.h,*.cpp}",
           Pattern_Name      => -"All files;GNAT Ada default;C/C++ files",
           History           => Get_History (Kernel));
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Widget, Button);

   begin
      if File1 = VFS.No_File then
         return;
      end if;

      declare
         File2 : constant Virtual_File :=
           Select_File
             (Title             => -"Select First Changes",
              Base_Directory    => VFS.No_File,
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
              Kind              => Open_File,
              File_Pattern      => "*.*;{*.ads,*.adb};{*.c,*.h,*.cpp}",
              Pattern_Name      => -"All files;GNAT Ada default;C/C++ files",
              History           => Get_History (Kernel));

      begin
         if File2 = VFS.No_File then
            return;
         end if;

         declare
            File3 : constant Virtual_File :=
              Select_File
                (Title             => -"Select Second Changes",
                 Base_Directory    => VFS.No_File,
                 Parent            => Get_Current_Window (Kernel),
                 Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
                 Kind              => Open_File,
                 File_Pattern      => "*.*;{*.ads,*.adb};{*.c,*.h,*.cpp}",
                 Pattern_Name      =>
                   -"All files;GNAT Ada default;C/C++ files",
                 History           => Get_History (Kernel));

         begin
            if File3 = VFS.No_File then
               Visual_Diff (File1, File2);
               return;
            end if;

            Visual_Diff (File2, File1, File3);

            declare
               Merge     : constant Virtual_File :=
                 Select_File
                   (Title             => -"Select Merge File",
                    Base_Directory    => VFS.No_File,
                    Parent            => Get_Current_Window (Kernel),
                    Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
                    Kind              => Open_File,
                    File_Pattern      => "*.*;{*.ads,*.adb};{*.c,*.h,*.cpp}",
                    Pattern_Name      =>
                      -"All files;GNAT Ada default;C/C++ files",
                    History           => Get_History (Kernel));

            begin
               if Merge /= VFS.No_File then
                  Show_Merge (Kernel, Full_Name (Merge).all, Item);
               end if;
            end;
         end;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Merge_Three_Files;

   ------------------------
   -- On_Merge_Two_Files --
   ------------------------

   procedure On_Merge_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Item   : Diff_Head;
      File1  : constant Virtual_File :=
        Select_File
          (Title             => -"Select First File",
           Parent            => Get_Current_Window (Kernel),
           Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
           Kind              => Open_File,
           File_Pattern      => "*.*;{*.ads,*.adb};{*.c,*.h,*.cpp}",
           Pattern_Name      => -"All files;GNAT Ada default;C/C++ files",
           History           => Get_History (Kernel));
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Widget, Button);

   begin
      if File1 = VFS.No_File then
         return;
      end if;

      declare
         File2 : constant Virtual_File :=
           Select_File
             (Title             => -"Select Second File",
              Base_Directory    => VFS.No_File,
              Parent            => Get_Current_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
              Kind              => Open_File,
              File_Pattern      => "*.*;{*.ads,*.adb};{*.c,*.h,*.cpp}",
              Pattern_Name      => -"All files;GNAT Ada default;C/C++ files",
              History           => Get_History (Kernel));

      begin
         if File2 = VFS.No_File then
            return;
         end if;

         Visual_Diff (File1, File2);

         declare
            Merge     : constant Virtual_File :=
              Select_File
                (Title             => -"Select Merge File",
                 Base_Directory    => VFS.No_File,
                 Parent            => Get_Current_Window (Kernel),
                 Use_Native_Dialog => Get_Pref (Use_Native_Dialogs),
                 Kind              => Open_File,
                 File_Pattern      => "*.*;{*.ads,*.adb};{*.c,*.h,*.cpp}",
                 Pattern_Name      =>
                   -"All files;GNAT Ada default;C/C++ files",
                 History           => Get_History (Kernel));

         begin
            if Merge /= VFS.No_File then
               Show_Merge (Kernel, Full_Name (Merge).all, Item);
            end if;
         end;
      end;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Merge_Two_Files;

   ---------------
   -- Diff_Hook --
   ---------------

   function Diff_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
      return Boolean
   is
      pragma Unreferenced (Kernel);
      D : Diff_Hooks_Args := Diff_Hooks_Args (Data.all);
      Success : Boolean;
   begin
      if D.Orig_File = VFS.No_File then
         if D.New_File = VFS.No_File then
            return False;
         end if;

         declare
            Ref_F : constant Virtual_File :=
                      Create (Full_Filename => Get_Ref_Filename (D.New_File));
            Res   : Boolean;
         begin
            Res := Visual_Patch (Ref_F, D.New_File, D.Diff_File, True);
            Delete (Ref_F, Success);
            return Res;
         end;

      elsif D.New_File = VFS.No_File then
         if D.Orig_File = VFS.No_File then
            return False;
         end if;

         declare
            Ref_F : constant Virtual_File :=
                      Create (Full_Filename => Get_Ref_Filename (D.Orig_File));
            Res   : Boolean;
         begin
            Res := Visual_Patch (D.Orig_File, Ref_F, D.Diff_File, False);
            Delete (Ref_F, Success);
            return Res;
         end;

      else
         return Visual_Patch (D.Orig_File, D.New_File, D.Diff_File);
      end if;
   end Diff_Hook;

   --------------------
   -- File_Closed_Cb --
   --------------------

   procedure File_Closed_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D         : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Diff      : Diff_Head_Access := new Diff_Head;
      Curr_Node : Diff_Head_List.List_Node;
      Ref_File  : Virtual_File := VFS.No_File;
   begin
      if Vdiff_Module_ID = null then
         return;
      end if;

      Curr_Node := First (VDiff2_Module (Vdiff_Module_ID).List_Diff.all);

      while Curr_Node /= Diff_Head_List.Null_Node loop
         Diff.all := Diff_Head_List.Data (Curr_Node);
         exit when Diff.On_Destruction
           or else Diff.File1 = D.File
           or else Diff.File2 = D.File
           or else Diff.File3 = D.File;
         Curr_Node := Next (Curr_Node);
      end loop;

      if Curr_Node /= Diff_Head_List.Null_Node
        and then not Diff.On_Destruction
      then
         Diff.On_Destruction := True;

         Hide_Differences (Kernel, Diff.all);

         Remove_Nodes
           (VDiff2_Module (Vdiff_Module_ID).List_Diff.all,
            Prev (VDiff2_Module (Vdiff_Module_ID).List_Diff.all, Curr_Node),
            Curr_Node);

         if Diff.Ref_File /= 0 then
            if Diff.Ref_File = 1 then
               Ref_File := Diff.File1;
            elsif Diff.Ref_File = 2 then
               Ref_File := Diff.File2;
            elsif Diff.Ref_File = 3 then
               Ref_File := Diff.File3;
            end if;

            if Ref_File /= VFS.No_File
              and then Ref_File /= D.File
            then
               Close_Child (Get_File_Editor (Kernel, Ref_File));
            end if;
         end if;

         Free_All (Diff.all);
      end if;

      Free (Diff);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end File_Closed_Cb;

   --------------------------
   -- Diff_Command_Handler --
   --------------------------

   procedure Diff_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
   begin
      if Command = "visual_diff" then
         declare
            File1 : constant Virtual_File :=
                      Create (Nth_Arg (Data, 1), Kernel,
                              Use_Source_Path => True);
            File2 : constant Virtual_File :=
                      Create (Nth_Arg (Data, 2), Kernel,
                              Use_Source_Path => True);
            File3 : constant Virtual_File :=
                      Create (Nth_Arg (Data, 3, Default => ""),
                              Kernel, Use_Source_Path => True);
         begin
            Visual_Diff (File1, File2, File3);
         end;
      end if;
   end Diff_Command_Handler;

   ------------------------------
   --  On_Preferences_Changed  --
   ------------------------------

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Diff      : Diff_Head;
      Curr_Node : Diff_Head_List.List_Node :=
        First (VDiff2_Module (Vdiff_Module_ID).List_Diff.all);

   begin
      Register_Highlighting (Kernel);

      while Curr_Node /= Diff_Head_List.Null_Node loop
         Diff := Data (Curr_Node);
         Hide_Differences (Kernel, Diff);
         Show_Differences3 (Kernel, Diff);
         Set_Data (Curr_Node, Diff);
         Curr_Node := Next (Curr_Node);
      end loop;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Preferences_Changed;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Change_Ref_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Node          : Diff_Head_List.List_Node;
      Selected_File : Virtual_File;
      Cmd           : Diff_Command_Access;
      Diff          : Diff_Head;
      Ref_File      : constant T_Loc := Diff.Ref_File;
   begin
      Create
        (Cmd,
         Get_Kernel (Vdiff_Module_ID.all),
         VDiff2_Module (Vdiff_Module_ID).List_Diff,
         Change_Ref_File'Access);

      Selected_File :=
        Create (Get_Ref_Filename
                  (File_Information
                     (File_Selection_Context_Access (Context.Context))));

      Node := Is_In_Diff_List
        (Selected_File,
         VDiff2_Module (Vdiff_Module_ID).List_Diff.all);
      Diff := Data (Node);

      if Diff.File1 = Selected_File then
         Diff.Ref_File := 1;
      elsif Diff.File2 = Selected_File then
         Diff.Ref_File := 2;
      elsif Diff.File3 = Selected_File then
         Diff.Ref_File := 3;
      end if;

      if Diff.Ref_File /= Ref_File then
         Set_Data (Node, Diff);
         Unchecked_Execute (Cmd, Node);
      end if;

      Free (Root_Command (Cmd.all));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Hide_Difference_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Node          : Diff_Head_List.List_Node;
      Selected_File : Virtual_File;
      Cmd           : Diff_Command_Access;

   begin
      Create
        (Cmd,
         Get_Kernel (Vdiff_Module_ID.all),
         VDiff2_Module (Vdiff_Module_ID).List_Diff,
         Unhighlight_Difference'Access);

      Selected_File :=
        Create (Get_Ref_Filename
                  (File_Information
                     (File_Selection_Context_Access (Context.Context))));

      Node := Is_In_Diff_List
        (Selected_File,
         VDiff2_Module (Vdiff_Module_ID).List_Diff.all);

      Unchecked_Execute (Cmd, Node);
      Free (Root_Command (Cmd.all));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Recompute_Diff_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Node          : Diff_Head_List.List_Node;
      Selected_File : Virtual_File;
      Cmd           : Diff_Command_Access;
      Arg           : String_Access;
      Success       : Boolean;

   begin
      Create
        (Cmd,
         Get_Kernel (Vdiff_Module_ID.all),
         VDiff2_Module (Vdiff_Module_ID).List_Diff,
         Reload_Difference'Access);

      Selected_File :=
        Create (Get_Ref_Filename
                  (File_Information
                     (File_Selection_Context_Access (Context.Context))));

      Node := Is_In_Diff_List
        (Selected_File,
         VDiff2_Module (Vdiff_Module_ID).List_Diff.all);

      Arg := new String'(Full_Name (Data (Node).File1).all);
      Execute_GPS_Shell_Command
        (Get_Kernel (Vdiff_Module_ID.all),
         "Editor.save_buffer", (1 => Arg));
      Free (Arg);

      Unchecked_Execute (Cmd, Node);

      Delete (Data (Node).File1, Success);

      Free (Root_Command (Cmd.all));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Close_Difference_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Node          : Diff_Head_List.List_Node;
      Selected_File : Virtual_File;
      Cmd           : Diff_Command_Access;

   begin
      Create
        (Cmd,
         Get_Kernel (Vdiff_Module_ID.all),
         VDiff2_Module (Vdiff_Module_ID).List_Diff,
         Close_Difference'Access);

      Selected_File :=
        Create (Get_Ref_Filename
                  (File_Information
                     (File_Selection_Context_Access (Context.Context))));

      Node := Is_In_Diff_List
        (Selected_File,
         VDiff2_Module (Vdiff_Module_ID).List_Diff.all);

      Unchecked_Execute (Cmd, Node);
      Free (Root_Command (Cmd.all));
      return Commands.Success;
   end Execute;

end Vdiff2_Module.Callback;
