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

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Intl;                use Glide_Intl;
with Diff_Utils;                use Diff_Utils;

with Vdiff2_Utils;              use Vdiff2_Utils;
with OS_Utils;                  use OS_Utils;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with Ada.Exceptions;            use Ada.Exceptions;
with Traces;                    use Traces;

package body Vdiff2_Module is

   Me : constant Debug_Handle := Create (Vdiff_Module_Name);

   function Mime_Action
     (Kernel    : access Kernel_Handle_Record'Class;
      Mime_Type : String;
      Data      : GValue_Array;
      Mode      : Mime_Mode := Read_Write) return Boolean;
   --  Process, if possible, the data sent by the kernel


   procedure On_Compare_Tree_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Tools->VDiff->Compare Two Files...
   procedure On_Compare_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Tools->VDiff->Compare Tree Files...
   procedure On_Merge_Tree_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Tools->VDiff->Merge Two Files...
   procedure On_Merge_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Callback for Tools->VDiff->Merge Tree Files...
   procedure Free (Ar : in out String_List);


   ---------------------------
   -- On_Compare_Tree_Files --
   ---------------------------
   procedure On_Compare_Tree_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is

      Result : Diff_Occurrence_Link;
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

      declare
         File2           : constant String :=
           Select_File
             (Title             => -"Select First Changes",
              Parent            => Get_Main_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Unspecified,
              History           => Get_History (Kernel));

      begin
         if File2 = "" then
            return;
         end if;

         declare
            File3           : constant String :=
              Select_File
                (Title             => -"Select Second Changes",
                 Parent            => Get_Main_Window (Kernel),
                 Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
                 Kind              => Unspecified,
                 History           => Get_History (Kernel));
         begin
            if File3 = "" then
               return;
            end if;
            Result := Diff (Kernel, File1, File2, File3);

            if Result = null then
               Button := Message_Dialog
                 (Msg         => -"No differences found.",
                  Buttons     => Button_OK,
                  Parent      => Get_Main_Window (Kernel));
               return;
            end if;
            Show_Differences (Kernel, Result, File1, File2, File3);
            Free (Result);
         end;
      end;
   end On_Compare_Tree_Files;

   --------------------------
   -- On_Compare_Two_Files --
   --------------------------

   procedure On_Compare_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Result : Diff_Occurrence_Link;
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

      declare
         File2           : constant String :=
           Select_File
             (Title             => -"Select Second File",
              Parent            => Get_Main_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Unspecified,
              History           => Get_History (Kernel));

      begin
         if File2 = "" then
            return;
         end if;

         Result := Diff (Kernel, File1, File2);

         if Result = null then
            Button := Message_Dialog
              (Msg         => -"No differences found.",
               Buttons     => Button_OK,
               Parent      => Get_Main_Window (Kernel));
            return;
         end if;
         Show_Differences (Kernel, Result, File1, File2);
         Free (Result);
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Compare_Two_Files;

   -------------------------
   -- On_Merge_Tree_Files --
   -------------------------

   procedure On_Merge_Tree_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Result : Diff_Occurrence_Link;
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

      declare
         File2           : constant String :=
           Select_File
             (Title             => -"Select First Changes",
              Parent            => Get_Main_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Unspecified,
              History           => Get_History (Kernel));

      begin
         if File2 = "" then
            return;
         end if;

         declare
            File3           : constant String :=
              Select_File
                (Title             => -"Select Second Changes",
                 Parent            => Get_Main_Window (Kernel),
                 Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
                 Kind              => Unspecified,
                 History           => Get_History (Kernel));
         begin
            if File3 = "" then
               return;
            end if;
            Result := Diff (Kernel, File1, File2, File3);

            if Result = null then
               Button := Message_Dialog
                 (Msg         => -"No differences found.",
                  Buttons     => Button_OK,
                  Parent      => Get_Main_Window (Kernel));
               return;
            end if;
            Show_Differences (Kernel, Result, File1, File2, File3);
            declare
               Merge : constant String :=
                 Select_File
                   (Title             => -"Select Merge File",
                    Parent            => Get_Main_Window (Kernel),
                    Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
                    Kind              => Unspecified,
                    History           => Get_History (Kernel));
               Args_edit       : Argument_List := (1 => new String'(Merge));
            begin
               if Merge /= "" then
                  Show_Merge (Kernel, Result, Merge, File1, File2, File3);
               end if;
               Free (Args_edit);
            end;

            Free (Result);
         end;
      end;
   end On_Merge_Tree_Files;

   ------------------------
   -- On_Merge_Two_Files --
   ------------------------

   procedure On_Merge_Two_Files
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Result : Diff_Occurrence_Link;
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

      declare
         File2 : constant String :=
           Select_File
             (Title             => -"Select Second File",
              Parent            => Get_Main_Window (Kernel),
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Unspecified,
              History           => Get_History (Kernel));

      begin
         if File2 = "" then
            return;
         end if;

         Result := Diff (Kernel, File1, File2);

         if Result = null then
            Button := Message_Dialog
              (Msg         => -"No differences found.",
               Buttons     => Button_OK,
               Parent      => Get_Main_Window (Kernel));
            return;
         end if;
         Show_Differences (Kernel, Result, File1, File2);
         declare
            Merge : constant String :=
              Select_File
                (Title             => -"Select Merge File",
                 Parent            => Get_Main_Window (Kernel),
                 Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
                 Kind              => Unspecified,
                 History           => Get_History (Kernel));
            Args_edit       : Argument_List := (1 => new String'(Merge));
         begin
            if Merge /= "" then
               Show_Merge (Kernel, Result, Merge, File1, File2);
            end if;
            Free (Args_edit);
         end;
         Free (Result);
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
      Result  : Diff_Occurrence_Link;
      Success : Boolean;
      Button  : Message_Dialog_Buttons;
      pragma Unreferenced (Mode, Button);
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

                  if Result = null then
                     Button := Message_Dialog
                       (Msg         => -"No differences found.",
                        Buttons     => Button_OK,
                        Parent      => Get_Main_Window (Kernel));
                     return False;
                  end if;

                  Show_Differences
                    (Kernel, Result, Ref_File,
                     New_File);
                  Delete_File (Ref_File, Success);
                  Free (Result);
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

                  if Result = null then
                     Button := Message_Dialog
                       (Msg         => -"No differences found.",
                        Buttons     => Button_OK,
                        Parent      => Get_Main_Window (Kernel));
                     return False;
                  end if;

                  Show_Differences
                    (Kernel, Result, Orig_File,
                     Ref_File);
                  Delete_File (Ref_File, Success);
                  Free (Result);
               end;

            else
               --  All arguments are specified

               Result := Diff (Kernel, Orig_File, New_File, Diff_File);

               if Result = null then
                  Button := Message_Dialog
                    (Msg         => -"No differences found.",
                     Buttons     => Button_OK,
                     Parent      => Get_Main_Window (Kernel));
                  return False;
               end if;

               Show_Differences
                 (Kernel, Result, Orig_File,
                  New_File);
               Free (Result);
            end if;
            return True;
         end;
      end if;

      return False;
   end Mime_Action;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Tools : constant String := '/' & (-"Tools") & '/'
        & (-"Visual Diff") & '/';
   begin
      Register_Module
        (Module       => Vdiff_Module_ID,
         Kernel       => Kernel,
         Module_Name  => Vdiff_Module_Name,
         Priority     => Default_Priority,
         Mime_Handler => Mime_Action'Access);
      Register_Menu
        (Kernel, Tools, -"Compare Two Files...", "",
         On_Compare_Two_Files'Access);
      Register_Menu
        (Kernel, Tools, -"Compare Tree Files...", "",
         On_Compare_Tree_Files'Access);
      Register_Menu
        (Kernel, Tools, -"Merge Two Files...", "",
         On_Merge_Two_Files'Access);
      Register_Menu
        (Kernel, Tools, -"Merge Tree Files...", "",
         On_Merge_Tree_Files'Access);
   end Register_Module;
   ----------
   -- Free --
   ----------

   procedure Free (Ar : in out String_List) is
   begin
      for A in Ar'Range loop
         Free (Ar (A));
      end loop;
   end Free;
end Vdiff2_Module;
