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

with GNATCOLL.Arg_Lists;                use GNATCOLL.Arg_Lists;
with GNATCOLL.Utils;                    use GNATCOLL.Utils;
with Gtk.Window;                        use Gtk.Window;
with Gtkada.Dialogs;                    use Gtkada.Dialogs;
with Gtkada.File_Selector;              use Gtkada.File_Selector;
with Gtkada.MDI;                        use Gtkada.MDI;
with GPS.Editors;                       use GPS.Editors;
with GPS.Intl;                          use GPS.Intl;
with GPS.Kernel.Contexts;               use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;                    use GPS.Kernel.MDI;
with GPS.Kernel.Modules;                use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;            use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;                use GPS.Kernel.Scripts;
with Vdiff2_Command_Block;              use Vdiff2_Command_Block;
with Vdiff2_Module.Utils.Shell_Command; use Vdiff2_Module.Utils.Shell_Command;
with Vdiff2_Module.Utils;               use Vdiff2_Module.Utils;

package body Vdiff2_Module.Callback is

   use Diff_Head_List;

   Ref_Prefix : constant Filesystem_String := "ref$";

   function Get_Ref_Filename (File : Virtual_File) return Virtual_File;
   pragma Inline (Get_Ref_Filename);
   --  Returns the ref filename for the give file

   ----------------------
   -- Get_Ref_Filename --
   ----------------------

   function Get_Ref_Filename (File : Virtual_File) return Virtual_File is
   begin
      return Create_From_Dir
        (Get_Tmp_Directory, Ref_Prefix & Base_Name (File));
   end Get_Ref_Filename;

   -------------
   -- Execute --
   -------------

   overriding function Execute
      (Self : access Compare_Three_Files;
       Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
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
      pragma Unreferenced (Button, Self);
   begin
      if File1 = GNATCOLL.VFS.No_File then
         return Commands.Failure;
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
            return Commands.Failure;
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
            Visual_Diff (Side_By_Side, File1, File2, File3);
         end;
      end;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
      (Self : access Compare_Two_Files;
       Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
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
      pragma Unreferenced (Self, Button);

   begin
      if File1 = GNATCOLL.VFS.No_File then
         return Commands.Failure;
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
            return Commands.Failure;
         end if;

         Visual_Diff (Side_By_Side, File1, File2);
      end;
      return Commands.Success;
   end Execute;

   --------------------------
   -- On_Merge_Three_Files --
   --------------------------

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
               Visual_Diff (Side_By_Side, File1, File2);
               return;
            end if;

            Visual_Diff
              (Side_By_Side, File1 => File2, File2 => File1, File3 => File3);

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
                  Show_Merge (Kernel, Merge);
               end if;
            end;
         end;
      end;
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

         Visual_Diff (Side_By_Side, File1, File2);

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
               Show_Merge (Kernel, Merge);
            end if;
         end;
      end;
   end On_Merge_Two_Files;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self   : On_Diff;
      Kernel : not null access Kernel_Handle_Record'Class;
      Vcs_File, Orig_File, New_File, Diff_File : Virtual_File;
      Title  : String)
      return Boolean
   is
      pragma Unreferenced (Self);
      Success : Boolean;

      procedure Setup_Ref (Base, Ref_File : Virtual_File);
      --  Setup filename titles and writable permission

      ---------------
      -- Setup_Ref --
      ---------------

      procedure Setup_Ref (Base, Ref_File : Virtual_File) is
         Filename : constant Filesystem_String := Full_Name (Ref_File);
      begin
         Set_Writable : declare
            CL : Arg_List := Create ("Editor.set_writable");
         begin
            Append_Argument (CL, +Filename, One_Arg);
            Append_Argument (CL, "FALSE", One_Arg);
            Execute_GPS_Shell_Command (Kernel, CL);
         end Set_Writable;

         Set_Title : declare
            CL : Arg_List := Create ("Editor.set_title");

         begin
            Append_Argument (CL, +Filename, One_Arg);
            if Title = "" then
               Append_Argument (CL, +Base_Name (Ref_File), One_Arg);
               Append_Argument (CL, +Base_Name (Ref_File), One_Arg);
            else
               Append_Argument (CL, Title, One_Arg);
               Append_Argument (CL, Title, One_Arg);
            end if;

            Execute_GPS_Shell_Command (Kernel, CL);
         end Set_Title;

         if Vcs_File /= No_File then
            Set_Reference : declare
               CL : Arg_List := Create ("VCS.set_reference");
            begin
               Append_Argument (CL, +Filename, One_Arg);
               Append_Argument (CL, +Full_Name (Vcs_File), One_Arg);
               Execute_GPS_Shell_Command (Kernel, CL);
            end Set_Reference;

            if Base /= No_File and then Base /= Vcs_File then
               Set_Base_Reference : declare
                  CL : Arg_List := Create ("VCS.set_reference");
               begin
                  Append_Argument (CL, +Full_Name (Base), One_Arg);
                  Append_Argument (CL, +Full_Name (Vcs_File), One_Arg);
                  Execute_GPS_Shell_Command (Kernel, CL);
               end Set_Base_Reference;
            end if;
         end if;
      end Setup_Ref;

      Res   : Diff_Head_Access;

   begin
      if Orig_File = GNATCOLL.VFS.No_File then
         if New_File = GNATCOLL.VFS.No_File then
            return False;
         end if;

         declare
            Ref_F : Virtual_File renames Get_Ref_Filename (New_File);
         begin
            Res := Visual_Patch
              (Diff_Mode.Get_Pref, Ref_F, New_File, Diff_File, True);

            if Res /= null then
               Setup_Ref (New_File, Ref_F);
            end if;

            Delete (Ref_F, Success);

         end;

      elsif New_File = GNATCOLL.VFS.No_File then
         declare
            Ref_F : Virtual_File renames Get_Ref_Filename (Orig_File);
         begin
            Res := Visual_Patch
              (Diff_Mode.Get_Pref, Orig_File, Ref_F, Diff_File, False);

            if Res /= null then
               Setup_Ref (No_File, Ref_F);
            end if;

            Delete (Ref_F, Success);
         end;

      else
         Res := Visual_Patch
           (Diff_Mode.Get_Pref, Orig_File, New_File, Diff_File);
         if Res /= null then
            Setup_Ref (New_File, Orig_File);
         end if;
      end if;

      return Res /= null;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self    : On_File_Closed;
      Kernel  : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      File    : Virtual_File)
   is
      pragma Unreferenced (Self);
      use Diff_Head_List.Std_Vectors;

      Diff : Diff_Head_Access;
      Node : Diff_Head_List.Std_Vectors.Cursor;
   begin
      if Vdiff_Module_ID = null then
         return;
      end if;

      Node :=
        Get_Diff_Node (File, VDiff2_Module (Vdiff_Module_ID).List_Diff.all);

      if not Has_Element (Node) then
         return;
      end if;

      Diff := Element (Node);

      if not Diff.In_Destruction then
         Diff.In_Destruction := True;

         Hide_Differences (Kernel, Diff);

         --  Close all temporary files used in the visual diff

         for J in Diff.Files'Range loop
            if Diff.Files (J) /= No_File
              and then Diff.Files (J) /= File
              and then
                (not Is_Regular_File (Diff.Files (J))
                 or else Starts_With
                   (Diff.Files (J).Display_Base_Name, String (Ref_Prefix)))
            then
               declare
                  Child : constant MDI_Child :=
                            Get_File_Editor (Kernel, Diff.Files (J));
               begin
                  if Child /= null then
                     Close_Child (Child);
                  end if;
               end;
            end if;
         end loop;

         VDiff2_Module (Vdiff_Module_ID).List_Diff.Delete (Node);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Pref, Self);
   begin
      Register_Highlighting (Kernel);

      for Diff of VDiff2_Module (Vdiff_Module_ID).List_Diff.all loop
         Hide_Differences (Kernel, Diff);
         Show_Differences3 (Kernel, Diff);
      end loop;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Change_Ref_File_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);

      Node          : Diff_Head_List.Std_Vectors.Cursor;
      Selected_File : Virtual_File;
      Cmd           : Diff_Command_Access;
      Diff          : Diff_Head_Access;
      Ref_File      : T_VFile_Index;
   begin
      Create   --  Freed below
        (Cmd,
         Get_Kernel (Vdiff_Module_ID.all),
         VDiff2_Module (Vdiff_Module_ID).List_Diff,
         Change_Ref_File'Access);

      Selected_File := Get_Ref_Filename (File_Information (Context.Context));

      Node := Get_Diff_Node
        (Selected_File,
         VDiff2_Module (Vdiff_Module_ID).List_Diff.all);
      Diff := Diff_Head_List.Std_Vectors.Element (Node);
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

      Unref (Command_Access (Cmd));
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
      Node          : Diff_Head_List.Std_Vectors.Cursor;
      Selected_File : Virtual_File;
      Cmd           : Diff_Command_Access;

   begin
      Create
        (Cmd,
         Get_Kernel (Vdiff_Module_ID.all),
         VDiff2_Module (Vdiff_Module_ID).List_Diff,
         Unhighlight_Difference'Access);

      Selected_File := Get_Ref_Filename (File_Information (Context.Context));

      Node := Get_Diff_Node
        (Selected_File,
         VDiff2_Module (Vdiff_Module_ID).List_Diff.all);

      Unchecked_Execute (Cmd, Diff_Head_List.Std_Vectors.Element (Node));
      Unref (Command_Access (Cmd));
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Remove_Difference_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      use Diff_Head_List.Std_Vectors;

      Kernel : constant Kernel_Handle := Get_Kernel (Vdiff_Module_ID.all);
      Node          : Diff_Head_List.Std_Vectors.Cursor;
      Selected_File : Virtual_File;
      Cmd           : Diff_Command_Access;
   begin
      Create
        (Cmd,
         Kernel,
         VDiff2_Module (Vdiff_Module_ID).List_Diff,
         Unhighlight_Difference'Access);

      Selected_File := Get_Ref_Filename (File_Information (Context.Context));

      Node := Get_Diff_Node
        (Selected_File,
         VDiff2_Module (Vdiff_Module_ID).List_Diff.all);

      Unchecked_Execute (Cmd, Element (Node));

      --  Remove all virtual buffers created during diff operations
      --  ??? This never closes the ref file in side-by-side diffs.

      for J of Element (Node).Files loop
         declare
            Editor : constant Editor_Buffer'Class :=
              Get_Buffer_Factory (Kernel).Get (J, Open_View => False);
         begin
            if Editor.Current_View = Nil_Editor_View then
               Editor.Close;
            end if;
         end;
      end loop;

      VDiff2_Module (Vdiff_Module_ID).List_Diff.Delete (Node);

      Unref (Command_Access (Cmd));
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
      use Diff_Head_List.Std_Vectors;

      Node          : Diff_Head_List.Std_Vectors.Cursor;
      Selected_File : Virtual_File;
      Cmd           : Diff_Command_Access;
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

      for J in Element (Node).Files'Range loop
         if Element (Node).Files (J) /= GNATCOLL.VFS.No_File then
            declare
               File : constant Virtual_File := Element (Node).Files (J);
            begin
               if not Is_Regular_File (File) then
                  To_Delete (J) := True;
               end if;

               declare
                  Editor : constant Editor_Buffer'Class :=
                    Get_Buffer_Factory
                      (Get_Kernel (Vdiff_Module_ID.all)).Get
                    (File, Open_View => False);
               begin
                  Editor.Save (Interactive => False,
                               File        => File);
               end;
            end;
         end if;
      end loop;

      Unchecked_Execute (Cmd, Element (Node));

      for J in To_Delete'Range loop
         if To_Delete (J) then
            GNATCOLL.VFS.Delete (Element (Node).Files (J), Success);
         end if;
      end loop;

      Unref (Command_Access (Cmd));
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
      Node          : Diff_Head_List.Std_Vectors.Cursor;
      Selected_File : Virtual_File;
      Cmd           : Diff_Command_Access;

   begin
      Create
        (Cmd,
         Get_Kernel (Vdiff_Module_ID.all),
         VDiff2_Module (Vdiff_Module_ID).List_Diff,
         Close_Difference'Access);

      Selected_File := Get_Ref_Filename (File_Information (Context.Context));

      Node := Get_Diff_Node
        (Selected_File,
         VDiff2_Module (Vdiff_Module_ID).List_Diff.all);

      Unchecked_Execute (Cmd, Diff_Head_List.Std_Vectors.Element (Node));
      Unref (Command_Access (Cmd));

      return Commands.Success;
   end Execute;

end Vdiff2_Module.Callback;
