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

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Glib.Values;               use Glib.Values;

with Gtkada.File_Selector;      use Gtkada.File_Selector;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtk.Window;                use Gtk.Window;

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Intl;                use Glide_Intl;

with Basic_Types;               use Basic_Types;
with Traces;                    use Traces;
with Commands;                  use Commands;

with Diff_Utils2;               use Diff_Utils2;
with Vdiff2_Utils;              use Vdiff2_Utils;
with Vdiff2_Command;            use Vdiff2_Command;
with Vdiff2_Module;             use Vdiff2_Module;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with OS_Utils;                  use OS_Utils;

with Ada.Exceptions;            use Ada.Exceptions;


package body Vdiff2_Module.Callback is

   use Diff_Head_List;
   use Diff_Chunk_List;

   Me : constant Debug_Handle := Create (Vdiff_Module_Name);

   ---------------------------
   -- On_Compare_Tree_Files --
   ---------------------------

   procedure On_Compare_Three_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Id     : constant VDiff2_Module := VDiff2_Module (Vdiff_Module_ID);
      Item   : Diff_Head;
      Result : Diff_List;
      File1  : constant String :=
        Select_File
          (Title             => -"Select Common Ancestor",
           Parent            => Get_Main_Window (Kernel),
           Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
           Kind              => Unspecified,
           History           => Get_History (Kernel));
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Widget, Button);

   begin
      if File1 = "" then
         return;
      end if;
      Change_Dir (Dir_Name (File1));
      declare
         File2 : constant String :=
           Select_File
             (Title             => -"Select First Changes",
              Base_Directory    => "",
              Parent            => Get_Main_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Unspecified,
              History           => Get_History (Kernel));
         Dummy : Command_Return_Type;
         pragma Unreferenced (Dummy);
      begin
         if File2 = "" then
            return;
         end if;
         Change_Dir (Dir_Name (File2));
         declare
            File3 : constant String :=
              Select_File
                (Title             => -"Select Second Changes",
                 Base_Directory    => "",
                 Parent            => Get_Main_Window (Kernel),
                 Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
                 Kind              => Unspecified,
                 History           => Get_History (Kernel));

         begin
            if File3 = "" then
               return;
            end if;
            Trace (Me, "begin Diff3");
            Result := Diff3 (Kernel, File1, File2, File3);
            Trace (Me, "end Diff3 ");

            if Result = Diff_Chunk_List.Null_List then
               Button := Message_Dialog
                 (Msg         => -"No differences found.",
                  Buttons     => Button_OK,
                  Parent      => Get_Main_Window (Kernel));
               return;
            end if;
            Item := (List => Result,
                     File1 => new String'(File1),
                     File2 => new String'(File2),
                     File3 => new String'(File3),
                     Current_Node => First (Result),
                     Ref_File => 2);
            Append (Id.List_Diff.all, Item);

            Trace (Me, "begin Show_Differences3");
            Show_Differences3 (Kernel, Item);
            Trace (Me, "end Show_Differences3");
            Dummy := Execute (Id.Command_First);
            --  Free (Result);
         end;
      end;
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Compare_Three_Files;

   --------------------------
   -- On_Compare_Two_Files --
   --------------------------

   procedure On_Compare_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Id     : constant VDiff2_Module := VDiff2_Module (Vdiff_Module_ID);
      Item   : Diff_Head;
      Result : Diff_List;
      File1  : constant String :=
        Select_File
          (Title             => -"Select First File",
           Base_Directory    => "",
           Parent            => Get_Main_Window (Kernel),
           Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
           Kind              => Unspecified,
           History           => Get_History (Kernel));
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Widget, Button);

   begin
      if File1 = "" then
         return;
      end if;
      Change_Dir (Dir_Name (File1));
      declare
         File2 : constant String :=
           Select_File
             (Title             => -"Select Second File",
              Base_Directory    => "",
              Parent            => Get_Main_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Unspecified,
              History           => Get_History (Kernel));
         Dummy : Command_Return_Type;
         pragma Unreferenced (Dummy);

      begin
         if File2 = "" then
            return;
         end if;

         Result := Diff (Kernel, File1, File2);

         if Result = Diff_Chunk_List.Null_List then
            Button := Message_Dialog
              (Msg         => -"No differences found.",
               Buttons     => Button_OK,
               Parent      => Get_Main_Window (Kernel));
            return;
         end if;

         Item := (List => Result,
                  File1 => new String'(File1),
                  File2 => new String'(File2),
                  File3 => null,
                  Current_Node => First (Result),
                  Ref_File => 2);
         Append (Id.List_Diff.all, Item);
         Show_Differences3 (Kernel, Item);
         Dummy := Execute (Id.Command_First);
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Compare_Two_Files;

   -------------------------
   -- On_Merge_Tree_Files --
   -------------------------

   procedure On_Merge_Three_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Id     : constant VDiff2_Module := VDiff2_Module (Vdiff_Module_ID);
      Item   : Diff_Head;
      Result : Diff_List;
      File1  : constant String :=
        Select_File
          (Title             => -"Select Common Ancestor",
           Base_Directory    => "",
           Parent            => Get_Main_Window (Kernel),
           Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
           Kind              => Unspecified,
           History           => Get_History (Kernel));
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Widget, Button);

   begin
      if File1 = "" then
         return;
      end if;
      Change_Dir (Dir_Name (File1));
      declare
         File2 : constant String :=
           Select_File
             (Title             => -"Select First Changes",
              Base_Directory    => "",
              Parent            => Get_Main_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Unspecified,
              History           => Get_History (Kernel));

      begin
         if File2 = "" then
            return;
         end if;
         Change_Dir (Dir_Name (File2));
         declare
            File3 : constant String :=
              Select_File
                (Title             => -"Select Second Changes",
                 Base_Directory    => "",
                 Parent            => Get_Main_Window (Kernel),
                 Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
                 Kind              => Unspecified,
                 History           => Get_History (Kernel));
            Dummy : Command_Return_Type;
            pragma Unreferenced (Dummy);

         begin
            if File3 = "" then
               return;
            end if;
            Change_Dir (Dir_Name (File3));
            Result := Diff3 (Kernel, File1, File2, File3);

            if Result = Diff_Chunk_List.Null_List then
               Button := Message_Dialog
                 (Msg         => -"No differences found.",
                  Buttons     => Button_OK,
                  Parent      => Get_Main_Window (Kernel));
               return;
            end if;

            Item := (List => Result,
                     File1 => new String'(File1),
                     File2 => new String'(File2),
                     File3 => new String'(File3),
                     Current_Node => First (Result),
                     Ref_File => 2);
            Append (Id.List_Diff.all, Item);
            Show_Differences3 (Kernel, Item);
            Dummy := Execute (Id.Command_First);

            declare
               Merge     : constant String :=
                 Select_File
                   (Title             => -"Select Merge File",
                    Base_Directory    => "",
                    Parent            => Get_Main_Window (Kernel),
                    Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
                    Kind              => Unspecified,
                    History           => Get_History (Kernel));
               Args_edit : Argument_List := (1 => new String'(Merge));

            begin
               if Merge /= "" then
                  Show_Merge (Kernel, Merge, Item);
               end if;

               Free (Args_edit);
            end;
         end;
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Merge_Three_Files;

   ------------------------
   -- On_Merge_Two_Files --
   ------------------------

   procedure On_Merge_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Id     : constant VDiff2_Module := VDiff2_Module (Vdiff_Module_ID);
      Item   : Diff_Head;
      Result : Diff_List;
      File1  : constant String :=
        Select_File
          (Title             => -"Select First File",
           Parent            => Get_Main_Window (Kernel),
           Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
           Kind              => Unspecified,
           History           => Get_History (Kernel));
      Button : Message_Dialog_Buttons;
      pragma Unreferenced (Widget, Button);

   begin
      if File1 = "" then
         return;
      end if;
      Change_Dir (Dir_Name (File1));
      declare
         File2 : constant String :=
           Select_File
             (Title             => -"Select Second File",
              Base_Directory    => "",
              Parent            => Get_Main_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Unspecified,
              History           => Get_History (Kernel));
         Dummy : Command_Return_Type;
         pragma Unreferenced (Dummy);

      begin
         if File2 = "" then
            return;
         end if;
         Change_Dir (Dir_Name (File2));
         Result := Diff (Kernel, File1, File2);

         if Result = Diff_Chunk_List.Null_List then
            Button := Message_Dialog
              (Msg         => -"No differences found.",
               Buttons     => Button_OK,
               Parent      => Get_Main_Window (Kernel));
            return;
         end if;

         Item := (List => Result,
                  File1 => new String'(File1),
                  File2 => new String'(File2),
                  File3 => null,
                  Current_Node => First (Result),
                  Ref_File => 2);
         Append (Id.List_Diff.all, Item);
         Show_Differences3 (Kernel, Item);
         Dummy := Execute (Id.Command_First);

         declare
            Merge     : constant String :=
              Select_File
                (Title             => -"Select Merge File",
                 Base_Directory    => "",
                 Parent            => Get_Main_Window (Kernel),
                 Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
                 Kind              => Unspecified,
                 History           => Get_History (Kernel));
            Args_edit : Argument_List := (1 => new String'(Merge));

         begin
            if Merge /= "" then
               Show_Merge (Kernel, Merge, Item);
            end if;

            Free (Args_edit);
         end;
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Merge_Two_Files;

   -----------------
   -- Mime_Action --
   -----------------

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean
   is
      Id      : constant VDiff2_Module := VDiff2_Module (Vdiff_Module_ID);
      Item    : Diff_Head;
      Result  : Diff_List;
      Success : Boolean;
      Button  : Message_Dialog_Buttons;
      Dummy : Command_Return_Type;
      pragma Unreferenced (Mode, Button, Dummy);

   begin

      if Mime_Type = Mime_Diff_File then
         declare
            Orig_File : constant String := Get_String (Data (Data'First));
            New_File  : constant String := Get_String (Data (Data'First + 1));
            Diff_File : constant String := Get_String (Data (Data'First + 2));

         begin
            if Orig_File = "" then
               if New_File = "" then
                  return False;
               end if;

               declare
                  Base     : constant String := Base_Name (New_File);
                  Ref_File : constant String := Get_Tmp_Dir & Base & "$ref";

               begin
                  Result := Diff
                    (Kernel, Ref_File, New_File, Diff_File, Revert => True);

                  if Result = Diff_Chunk_List.Null_List then
                     Button := Message_Dialog
                       (Msg         => -"No differences found.",
                        Buttons     => Button_OK,
                        Parent      => Get_Main_Window (Kernel));
                     return False;
                  end if;

                  Item :=
                    (List => Result,
                     File1 => new String'(Ref_File),
                     File2 => new String'(New_File),
                     File3 => null,
                     Current_Node => First (Result),
                     Ref_File => 2);
                  Append (Id.List_Diff.all, Item);
                  Show_Differences3 (Kernel, Item);
                  Delete_File (Ref_File, Success);
               end;

            elsif New_File = "" then
               if Orig_File = "" then
                  return False;
               end if;

               declare
                  Base     : constant String := Base_Name (Orig_File);
                  Ref_File : constant String := Get_Tmp_Dir & Base & "$ref";

               begin
                  Result := Diff (Kernel, Orig_File, Ref_File, Diff_File);

                  if Result = Diff_Chunk_List.Null_List then
                     Button := Message_Dialog
                       (Msg         => -"No differences found.",
                        Buttons     => Button_OK,
                        Parent      => Get_Main_Window (Kernel));
                     return False;
                  end if;
                  Item := (List => Result,
                                 File1 => new String'(Orig_File),
                                 File2 => new String'(Ref_File),
                                 File3 => null,
                                 Current_Node => First (Result),
                                 Ref_File => 2);
                  Append (Id.List_Diff.all, Item);
                  Show_Differences3 (Kernel, Item);
                  Delete_File (Ref_File, Success);
               end;

            else
               --  All arguments are specified

               Result := Diff (Kernel, Orig_File, New_File, Diff_File);

               if Result = Diff_Chunk_List.Null_List then
                  Button := Message_Dialog
                    (Msg         => -"No differences found.",
                     Buttons     => Button_OK,
                     Parent      => Get_Main_Window (Kernel));
                  return False;
               end if;

               Item :=
                 (List => Result,
                  File1 => new String'(Orig_File),
                  File2 => new String'(New_File),
                  File3 => null,
                  Current_Node => First (Result),
                  Ref_File => 2);
               Append (Id.List_Diff.all, Item);
               Show_Differences3 (Kernel, Item);
            end if;

            Dummy := Execute (Id.Command_First);
            return True;
         end;
      end if;

      return False;
   end Mime_Action;


   --------------------
   -- File_Closed_Cb --
   --------------------

   procedure File_Closed_Cb
     (Widget  : access Glib.Object.GObject_Record'Class;
      Args    : GValues;
      Kernel  : Kernel_Handle)
   is
      Diff     : Diff_Head_Access := new Diff_Head;
      File     : constant String := Get_String (Nth (Args, 1));
      CurrNode : Diff_Head_List.List_Node :=
        First (VDiff2_Module (Vdiff_Module_ID).List_Diff.all);
      pragma Unreferenced (Widget);

   begin
      while CurrNode /= Diff_Head_List.Null_Node loop
         Diff.all := Data (CurrNode);
         exit when (Diff.File1 /= null and then Diff.File1.all = File)
           or else (Diff.File2 /= null and then Diff.File2.all = File)
           or else (Diff.File3 /= null and then Diff.File3.all = File);
         CurrNode := Next (CurrNode);
      end loop;

      if CurrNode /= Diff_Head_List.Null_Node then
         Hide_Differences (Kernel, Diff.all);
         Remove_Nodes (VDiff2_Module (Vdiff_Module_ID).List_Diff.all,
                          Prev (VDiff2_Module (Vdiff_Module_ID).List_Diff.all,
                             CurrNode),
                          CurrNode);
      end if;
      Free_All (Diff.all);
      Free (Diff);
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end File_Closed_Cb;

   ------------------------------
   --  On_Preferences_Changed  --
   ------------------------------

   procedure On_Preferences_Changed
     (Kernel : access GObject_Record'Class; K : Kernel_Handle) is
      pragma Unreferenced (Kernel);
   begin
      Register_Highlighting (K);
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Preferences_Changed;

   ---------------------
   --  On_Ref_Change  --
   ---------------------

   procedure On_Ref_Change
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access) is
      pragma Unreferenced (Widget);
      Node          : Diff_Head_List.List_Node;
      Selected_File : GNAT.OS_Lib.String_Access;
      Cmd           : Diff_Command_Access;
      Diff          : Diff_Head;
   begin
      Create
        (Cmd,
         VDiff2_Module (Vdiff_Module_ID).Kernel,
         VDiff2_Module (Vdiff_Module_ID).List_Diff,
         Change_Ref_File'Access);

      Selected_File := new String'
        (Directory_Information
           (File_Selection_Context_Access (Context)) &
         File_Information
           (File_Selection_Context_Access (Context)));
      Node := Is_In_Diff_List
        (Selected_File,
         VDiff2_Module (Vdiff_Module_ID).List_Diff.all);
      Diff := Data (Node);

      if Diff.File1.all = Selected_File.all then
         Diff.Ref_File := 1;
      elsif Diff.File2.all = Selected_File.all then
         Diff.Ref_File := 2;
      elsif Diff.File3.all = Selected_File.all then
         Diff.Ref_File := 3;
      end if;

      Set_Data (Node, Diff);
      Free (Selected_File);
      Unchecked_Execute (Cmd, Node);
      Free (Root_Command (Cmd.all));
   end On_Ref_Change;

   ---------------------------
   --  On_Hide_Differences  --
   ---------------------------

   procedure On_Hide_Differences
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
   pragma Unreferenced (Widget);
      Node          : Diff_Head_List.List_Node;
      Selected_File : GNAT.OS_Lib.String_Access;
      Cmd           : Diff_Command_Access;

   begin
      Create
        (Cmd,
         VDiff2_Module (Vdiff_Module_ID).Kernel,
         VDiff2_Module (Vdiff_Module_ID).List_Diff,
         Unhighlight_Difference'Access);

      Selected_File := new String'
        (Directory_Information
           (File_Selection_Context_Access (Context)) &
         File_Information
           (File_Selection_Context_Access (Context)));

      Node := Is_In_Diff_List
        (Selected_File,
         VDiff2_Module (Vdiff_Module_ID).List_Diff.all);

      Free (Selected_File);
      Unchecked_Execute (Cmd, Node);
      Free (Root_Command (Cmd.all));
   end On_Hide_Differences;

   ----------------------
   --  On_Recalculate  --
   ----------------------

   procedure On_Recalculate
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
   pragma Unreferenced (Widget);
      Node          : Diff_Head_List.List_Node;
      Selected_File : GNAT.OS_Lib.String_Access;
      Cmd           : Diff_Command_Access;

   begin
      Create
        (Cmd,
         VDiff2_Module (Vdiff_Module_ID).Kernel,
         VDiff2_Module (Vdiff_Module_ID).List_Diff,
         Reload_Difference'Access);

      Selected_File := new String'
        (Directory_Information
           (File_Selection_Context_Access (Context)) &
         File_Information
           (File_Selection_Context_Access (Context)));

      Node := Is_In_Diff_List
        (Selected_File,
         VDiff2_Module (Vdiff_Module_ID).List_Diff.all);

      Free (Selected_File);
      Unchecked_Execute (Cmd, Node);
      Free (Root_Command (Cmd.all));
   end On_Recalculate;

   ---------------------------
   --  On_Close_Difference  --
   ---------------------------

   procedure On_Close_Difference
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
   pragma Unreferenced (Widget);
      Node          : Diff_Head_List.List_Node;
      Selected_File : GNAT.OS_Lib.String_Access;
      Cmd           : Diff_Command_Access;

   begin
      Create
        (Cmd,
         VDiff2_Module (Vdiff_Module_ID).Kernel,
         VDiff2_Module (Vdiff_Module_ID).List_Diff,
         Close_Difference'Access);

      Selected_File := new String'
        (Directory_Information
           (File_Selection_Context_Access (Context)) &
         File_Information
           (File_Selection_Context_Access (Context)));

      Node := Is_In_Diff_List
        (Selected_File,
         VDiff2_Module (Vdiff_Module_ID).List_Diff.all);

      Free (Selected_File);
      Unchecked_Execute (Cmd, Node);
      Free (Root_Command (Cmd.all));
   end On_Close_Difference;

end Vdiff2_Module.Callback;