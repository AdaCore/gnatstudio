-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2001-2008, AdaCore                --
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

with GNAT.OS_Lib;                       use GNAT.OS_Lib;
with GNATCOLL.Filesystem;               use GNATCOLL.Filesystem;
with GNATCOLL.Utils;                    use GNATCOLL.Utils;
with GNATCOLL.VFS;                      use GNATCOLL.VFS;

with Gtk.Window;                        use Gtk.Window;

with Gtkada.Dialogs;                    use Gtkada.Dialogs;
with Gtkada.File_Selector;              use Gtkada.File_Selector;
with Gtkada.MDI;                        use Gtkada.MDI;

with GPS.Intl;                          use GPS.Intl;
with GPS.Kernel.Contexts;               use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;                    use GPS.Kernel.MDI;
with GPS.Kernel.Modules;                use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;            use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;                use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks;         use GPS.Kernel.Standard_Hooks;
with Traces;                            use Traces;
with Vdiff2_Command_Block;              use Vdiff2_Command_Block;
with Vdiff2_Module.Utils.Shell_Command; use Vdiff2_Module.Utils.Shell_Command;
with Vdiff2_Module.Utils;               use Vdiff2_Module.Utils;

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
      return Get_Local_Filesystem.Get_Tmp_Directory
        & "ref$" & Base_Name (File);
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
                    Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                    Kind              => Open_File,
                    File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
                    Pattern_Name      => -"All files;Ada files;C/C++ files",
                    History           => Get_History (Kernel));
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Widget, Button);

   begin
      if File1 = GNATCOLL.VFS.No_File then
         return;
      end if;

      declare
         File2 : constant Virtual_File :=
                   Select_File
                     (Title             => -"Select First Changes",
                      Base_Directory    => GNATCOLL.VFS.Get_Current_Dir,
                      Parent            => Get_Current_Window (Kernel),
                      Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                      Kind              => Open_File,
                      File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
                      Pattern_Name      => -"All files;Ada files;C/C++ files",
                      History           => Get_History (Kernel));
         Dummy : Command_Return_Type;
         pragma Unreferenced (Dummy);

      begin
         if File2 = GNATCOLL.VFS.No_File then
            return;
         end if;

         declare
            File3 : constant Virtual_File :=
              Select_File
                (Title             => -"Select Second Changes",
                 Base_Directory    => GNATCOLL.VFS.Get_Current_Dir,
                 Parent            => Get_Current_Window (Kernel),
                 Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                 Kind              => Open_File,
                 File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
                 Pattern_Name      => -"All files;Ada files;C/C++ files",
                 History           => Get_History (Kernel));
         begin
            Visual_Diff (File1, File2, File3);
         end;
      end;
   exception
      when E : others => Trace (Exception_Handle, E);
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
                    Base_Directory    => GNATCOLL.VFS.Get_Current_Dir,
                    Parent            => Get_Current_Window (Kernel),
                    Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                    Kind              => Open_File,
                    File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
                    Pattern_Name      => -"All files;Ada files;C/C++ files",
                    History           => Get_History (Kernel));
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Widget, Button);

   begin
      if File1 = GNATCOLL.VFS.No_File then
         return;
      end if;

      declare
         File2 : constant Virtual_File :=
                   Select_File
                     (Title             => -"Select Second File",
                      Base_Directory    => GNATCOLL.VFS.Get_Current_Dir,
                      Parent            => Get_Current_Window (Kernel),
                      Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                      Kind              => Open_File,
                      File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
                      Pattern_Name      => -"All files;Ada files;C/C++ files",
                      History           => Get_History (Kernel));

      begin
         if File2 = GNATCOLL.VFS.No_File then
            return;
         end if;

         Visual_Diff (File1, File2);
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Compare_Two_Files;

   -------------------------
   -- On_Merge_Tree_Files --
   -------------------------

   procedure On_Merge_Three_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      File1  : constant Virtual_File :=
                 Select_File
                   (Title             => -"Select Common Ancestor",
                    Base_Directory    => GNATCOLL.VFS.No_File,
                    Parent            => Get_Current_Window (Kernel),
                    Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                    Kind              => Open_File,
                    File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
                    Pattern_Name      => -"All files;Ada files;C/C++ files",
                    History           => Get_History (Kernel));
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Widget, Button);

   begin
      if File1 = GNATCOLL.VFS.No_File then
         return;
      end if;

      declare
         File2 : constant Virtual_File :=
                   Select_File
                     (Title             => -"Select First Changes",
                      Base_Directory    => GNATCOLL.VFS.No_File,
                      Parent            => Get_Current_Window (Kernel),
                      Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                      Kind              => Open_File,
                      File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
                      Pattern_Name      => -"All files;Ada files;C/C++ files",
                      History           => Get_History (Kernel));

      begin
         if File2 = GNATCOLL.VFS.No_File then
            return;
         end if;

         declare
            File3 : constant Virtual_File :=
              Select_File
                (Title             => -"Select Second Changes",
                 Base_Directory    => GNATCOLL.VFS.No_File,
                 Parent            => Get_Current_Window (Kernel),
                 Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                 Kind              => Open_File,
                 File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
                 Pattern_Name      => -"All files;Ada files;C/C++ files",
                 History           => Get_History (Kernel));

         begin
            if File3 = GNATCOLL.VFS.No_File then
               Visual_Diff (File1, File2);
               return;
            end if;

            Visual_Diff (File1 => File2, File2 => File1, File3 => File3);

            declare
               Merge : constant Virtual_File :=
                 Select_File
                   (Title             => -"Select Merge File",
                    Base_Directory    => GNATCOLL.VFS.No_File,
                    Parent            => Get_Current_Window (Kernel),
                    Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                    Kind              => Open_File,
                    File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
                    Pattern_Name      => -"All files;Ada files;C/C++ files",
                    History           => Get_History (Kernel));

            begin
               if Merge /= GNATCOLL.VFS.No_File then
                  Show_Merge (Kernel, Full_Name (Merge).all);
               end if;
            end;
         end;
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Merge_Three_Files;

   ------------------------
   -- On_Merge_Two_Files --
   ------------------------

   procedure On_Merge_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      File1  : constant Virtual_File :=
                 Select_File
                   (Title             => -"Select First File",
                    Parent            => Get_Current_Window (Kernel),
                    Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                    Kind              => Open_File,
                    File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
                    Pattern_Name      => -"All files;Ada files;C/C++ files",
                    History           => Get_History (Kernel));
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Widget, Button);

   begin
      if File1 = GNATCOLL.VFS.No_File then
         return;
      end if;

      declare
         File2 : constant Virtual_File :=
                   Select_File
                     (Title             => -"Select Second File",
                      Base_Directory    => GNATCOLL.VFS.No_File,
                      Parent            => Get_Current_Window (Kernel),
                      Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                      Kind              => Open_File,
                      File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
                      Pattern_Name      => -"All files;Ada files;C/C++ files",
                      History           => Get_History (Kernel));

      begin
         if File2 = GNATCOLL.VFS.No_File then
            return;
         end if;

         Visual_Diff (File1, File2);

         declare
            Merge : constant Virtual_File :=
              Select_File
                (Title             => -"Select Merge File",
                 Base_Directory    => GNATCOLL.VFS.No_File,
                 Parent            => Get_Current_Window (Kernel),
                 Use_Native_Dialog => Use_Native_Dialogs.Get_Pref,
                 Kind              => Open_File,
                 File_Pattern      => "*;*.ad?;{*.c,*.h,*.cpp,*.cc,*.C}",
                 Pattern_Name      => -"All files;Ada files;C/C++ files",
                 History           => Get_History (Kernel));

         begin
            if Merge /= GNATCOLL.VFS.No_File then
               Show_Merge (Kernel, Full_Name (Merge).all);
            end if;
         end;
      end;

   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Merge_Two_Files;

   ---------------
   -- Diff_Hook --
   ---------------

   function Diff_Hook
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class) return Boolean
   is
      D       : constant Diff_Hooks_Args := Diff_Hooks_Args (Data.all);
      Success : Boolean;
   begin
      if D.Orig_File = GNATCOLL.VFS.No_File then
         if D.New_File = GNATCOLL.VFS.No_File then
            return False;
         end if;

         declare
            Ref_F : constant Virtual_File :=
                      Create (Full_Filename => Get_Ref_Filename (D.New_File));
            Res   : Diff_Head_Access;
         begin
            Res := Visual_Patch (Ref_F, D.New_File, D.Diff_File, True);

            if Res /= null then
               declare
                  Args : Argument_List :=
                           (1 => new String'(Full_Name (Ref_F).all),
                            2 => new String'(Boolean'Image (False)));
               begin
                  Execute_GPS_Shell_Command
                    (Kernel, "Editor.set_writable", Args);
                  Free (Args);
               end;
            end if;

            Delete (Ref_F, Success);

            return Res /= null;
         end;

      elsif D.New_File = GNATCOLL.VFS.No_File then
         if D.Orig_File = GNATCOLL.VFS.No_File then
            return False;
         end if;

         declare
            Ref_F : constant Virtual_File :=
                      Create (Full_Filename => Get_Ref_Filename (D.Orig_File));
            Res   : Diff_Head_Access;
         begin
            Res := Visual_Patch (D.Orig_File, Ref_F, D.Diff_File, False);

            if Res /= null then
               declare
                  Args : Argument_List :=
                           (1 => new String'(Full_Name (Ref_F).all),
                            2 => new String'(Boolean'Image (False)));
               begin
                  Execute_GPS_Shell_Command
                    (Kernel, "Editor.set_writable", Args);
                  Free (Args);
               end;
            end if;

            Delete (Ref_F, Success);
            return Res /= null;
         end;

      else
         return Visual_Patch (D.Orig_File, D.New_File, D.Diff_File) /= null;
      end if;
   end Diff_Hook;

   --------------------
   -- File_Closed_Cb --
   --------------------

   procedure File_Closed_Cb
     (Kernel : access Kernel_Handle_Record'Class;
      Data   : access Hooks_Data'Class)
   is
      D    : constant File_Hooks_Args := File_Hooks_Args (Data.all);
      Diff : Diff_Head_Access;
      Node : Diff_Head_List.List_Node;
   begin
      if Vdiff_Module_ID = null then
         return;
      end if;

      Node :=
        Get_Diff_Node
          (D.File,
           VDiff2_Module (Vdiff_Module_ID).List_Diff.all);

      if Node = Diff_Head_List.Null_Node then
         return;
      end if;

      Diff := Diff_Head_List.Data (Node);

      if not Diff.In_Destruction then
         Diff.In_Destruction := True;

         Hide_Differences (Kernel, Diff);

         --  Close all temporary files used in the visual diff

         for J in Diff.Files'Range loop
            if Diff.Files (J) /= No_File
              and then Diff.Files (J) /= D.File
              and then not Is_Regular_File (Diff.Files (J))
            then
               declare
                  Child : constant MDI_Child :=
                            Get_File_Editor
                              (Kernel, Diff.Files (J));
               begin
                  if Child /= null then
                     Close_Child (Child);
                  end if;
               end;
            end if;
         end loop;

         Remove_Nodes
           (VDiff2_Module (Vdiff_Module_ID).List_Diff.all,
            Prev (VDiff2_Module (Vdiff_Module_ID).List_Diff.all, Node),
            Node);
      end if;

   exception
      when E : others => Trace (Exception_Handle, E);
   end File_Closed_Cb;

   ------------------------------
   --  On_Preferences_Changed  --
   ------------------------------

   procedure On_Preferences_Changed
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Diff      : Diff_Head_Access;
      Curr_Node : Diff_Head_List.List_Node :=
                    First (VDiff2_Module (Vdiff_Module_ID).List_Diff.all);
   begin
      Register_Highlighting (Kernel);

      while Curr_Node /= Diff_Head_List.Null_Node loop
         Diff := Data (Curr_Node);
         Hide_Differences (Kernel, Diff);
         Show_Differences3 (Kernel, Diff);
         Curr_Node := Next (Curr_Node);
      end loop;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Preferences_Changed;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Change_Ref_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Node          : Diff_Head_List.List_Node;
      Selected_File : Virtual_File;
      Cmd           : Diff_Command_Access;
      Diff          : Diff_Head_Access;
      Ref_File      : T_VFile_Index;
   begin
      Create
        (Cmd,
         Get_Kernel (Vdiff_Module_ID.all),
         VDiff2_Module (Vdiff_Module_ID).List_Diff,
         Change_Ref_File'Access);

      Selected_File :=
        Create (Get_Ref_Filename (File_Information (Context.Context)));

      Node := Get_Diff_Node
        (Selected_File,
         VDiff2_Module (Vdiff_Module_ID).List_Diff.all);
      Diff := Data (Node);
      Ref_File := Diff.Ref_File;

      for J in T_VFile_Index loop
         if Diff.Files (J) = Selected_File then
            Diff.Ref_File := J;
            exit;
         end if;
      end loop;

      if Diff.Ref_File /= Ref_File then
         Unchecked_Execute (Cmd, Diff);
      end if;

      Free (Root_Command (Cmd.all));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
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
        Create (Get_Ref_Filename (File_Information (Context.Context)));

      Node := Get_Diff_Node
        (Selected_File,
         VDiff2_Module (Vdiff_Module_ID).List_Diff.all);

      Unchecked_Execute (Cmd, Data (Node));
      Free (Root_Command (Cmd.all));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Recompute_Diff_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Node          : Diff_Head_List.List_Node;
      Selected_File : Virtual_File;
      Cmd           : Diff_Command_Access;
      Arg           : String_Access;
      Success       : Boolean;
      To_Delete     : array (T_VFile'Range) of Boolean := (others => False);

   begin
      Create
        (Cmd,
         Get_Kernel (Vdiff_Module_ID.all),
         VDiff2_Module (Vdiff_Module_ID).List_Diff,
         Reload_Difference'Access);

      Selected_File := File_Information (Context.Context);

      Node := Get_Diff_Node
        (Selected_File,
         VDiff2_Module (Vdiff_Module_ID).List_Diff.all);

      for J in Data (Node).Files'Range loop
         if Data (Node).Files (J) /= GNATCOLL.VFS.No_File then
            declare
               Filename : constant String :=
                            Full_Name (Data (Node).Files (J)).all;
            begin
               if not Is_Regular_File (Filename) then
                  To_Delete (J) := True;
               end if;

               Arg := new String'(Filename);
               Execute_GPS_Shell_Command
                 (Get_Kernel (Vdiff_Module_ID.all),
                  "Editor.save_buffer", (1 => Arg));
               Free (Arg);
            end;
         end if;
      end loop;

      Unchecked_Execute (Cmd, Data (Node));

      for J in To_Delete'Range loop
         if To_Delete (J) then
            GNATCOLL.VFS.Delete (Data (Node).Files (J), Success);
         end if;
      end loop;

      Free (Root_Command (Cmd.all));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
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
        Create (Get_Ref_Filename (File_Information (Context.Context)));

      Node := Get_Diff_Node
        (Selected_File,
         VDiff2_Module (Vdiff_Module_ID).List_Diff.all);

      Unchecked_Execute (Cmd, Data (Node));
      Free (Root_Command (Cmd.all));

      return Commands.Success;
   end Execute;

end Vdiff2_Module.Callback;
