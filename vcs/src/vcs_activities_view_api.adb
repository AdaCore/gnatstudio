-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                         Copyright (C) 2005                        --
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

with Ada.Exceptions;            use Ada.Exceptions;

with Gtk.Enums;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Check_Menu_Item;       use Gtk.Check_Menu_Item;
with Gtkada.MDI;                use Gtkada.MDI;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with String_List_Utils;         use String_List_Utils;
with VCS;                       use VCS;
with VCS_Activities;            use VCS_Activities;
with VCS_Activities_View;       use VCS_Activities_View;
with VCS_Module;                use VCS_Module;
with VCS_View_Pkg;
with Traces;                    use Traces;
with VFS;                       use VFS;
with Log_Utils;                 use Log_Utils;

package body VCS_Activities_View_API is

   procedure Query_Activities_Files
     (Explorer   : VCS_Activities_View_Access;
      Kernel     : Kernel_Handle;
      Real_Query : Boolean);
   --  Query/List the status of files belonging to activities.
   --  If Real_Query is True, a real VCS query will be made, otherwise
   --  the files will simply be listed.
   --  Calling this does NOT open the VCS Explorer.

   procedure On_Menu_Create_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Delete_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Commit_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Diff_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Remove_From_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Toggle_Group_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   ----------------------------
   -- On_Menu_Toggle_Group_Commit --
   ----------------------------

   procedure On_Menu_Toggle_Group_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      A_Context : constant Activity_Context_Access :=
                    Activity_Context_Access (Context);
      Activity  : Activity_Id;
   begin
      Activity := Value (Activity_Information (A_Context));
      Toggle_Group_Commit (Activity);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Toggle_Group_Commit;

   -----------------------------
   -- On_Menu_Create_Activity --
   -----------------------------

   procedure On_Menu_Create_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
   begin
      On_Create_Activity (Kernel);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Create_Activity;

   -----------------------------
   -- On_Menu_Delete_Activity --
   -----------------------------

   procedure On_Menu_Delete_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Kernel    : constant Kernel_Handle := Get_Kernel (Context);
      A_Context : constant Activity_Context_Access :=
                    Activity_Context_Access (Context);
   begin
      On_Delete_Activity (Kernel, Value (Activity_Information (A_Context)));
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Delete_Activity;

   -----------------------------
   -- On_Menu_Add_To_Activity --
   -----------------------------

   procedure On_Menu_Add_To_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Kernel    : constant Kernel_Handle := Get_Kernel (Context);
      A_Context : Activity_Context_Access;
      Activity  : Activity_Id;
   begin
      if Context.all in Activity_Context'Class then
         A_Context := Activity_Context_Access (Context);

         Activity := Value (Activity_Information (A_Context));

         if Has_File_Information (A_Context) then
            --  If we have a file information, then there is a single file to
            --  handle.
            Add_File (Kernel, Activity, File_Information (A_Context));

         else
            --  We have no file information, use the selected file in the VCS
            --  explorer.

            declare
               use type String_List.List_Node;
               Files    : String_List.List :=
                            VCS_View_Pkg.Get_Selected_Files (Kernel);
               File     : Virtual_File;
               Files_It : String_List.List_Node;
            begin
               Files_It := String_List.First (Files);

               while Files_It /= String_List.Null_Node loop
                  File := Create
                    (Full_Filename => String_List.Data (Files_It));
                  Add_File (Kernel, Activity, File);

                  Files_It := String_List.Next (Files_It);
               end loop;

               String_List.Free (Files);
            end;
         end if;

         Query_Activities_Files
           (Get_Activities_Explorer (Kernel, False, False),
            Kernel, Real_Query => False);
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Add_To_Activity;

   ----------------------------------
   -- On_Menu_Remove_From_Activity --
   ----------------------------------

   procedure On_Menu_Remove_From_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      use type String_List.List_Node;

      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      File     : Virtual_File;
      Files    : String_List.List;
      Files_It : String_List.List_Node;
   begin
      Files := Get_Selected_Files (Get_Activities_Explorer (Kernel, False));

      Files_It := String_List.First (Files);

      while Files_It /= String_List.Null_Node loop
         File := Create (Full_Filename => String_List.Data (Files_It));
         On_Remove_From_Activity (Kernel, Get_File_Activity (File), File);

         Files_It := String_List.Next (Files_It);
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Remove_From_Activity;

   -----------------------------
   -- On_Menu_Commit_Activity --
   -----------------------------

   procedure On_Menu_Commit_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Kernel         : constant Kernel_Handle := Get_Kernel (Context);
      A_Context      : constant Activity_Context_Access :=
                         Activity_Context_Access (Context);
      Activity       : constant Activity_Id :=
                         Value (Activity_Information (A_Context));
      Suffix         : constant String := Action_To_Log_Suffix (Commit);
      Files          : String_List.List;
      Files_Temp     : String_List.List_Node;
      All_Logs_Exist : Boolean := True;
      File           : Virtual_File;

      use String_List;
      use type String_List.List_Node;
   begin
      Files := Get_Files_In_Activity (Activity);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Kernel, -"VCS: No file in activity, cannot commit", Mode => Error);
         return;
      end if;

      Files_Temp := String_List.First (Files);

      --  Open log editors for files that don't have a log

      while Files_Temp /= String_List.Null_Node loop
         File := Create (Full_Filename => String_List.Data (Files_Temp));

         if Get_Log_From_File (Kernel, File, False) = VFS.No_File then
            --  There is some missing logs
            All_Logs_Exist := False;

            Open_File_Editor
              (Kernel,
               Get_Log_From_File (Kernel, File, True, Suffix),
               Position => Position_Bottom);
         end if;

         Files_Temp := String_List.Next (Files_Temp);
      end loop;

      --  If All files have a log, commit the whole lot

      if All_Logs_Exist then
         Log_Action_Files
           (Kernel, Get_VCS_For_Activity (Kernel, Activity),
            Commit, Files, Activity);
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Commit_Activity;

   ---------------------------
   -- On_Menu_Diff_Activity --
   ---------------------------

   procedure On_Menu_Diff_Activity
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Kernel    : constant Kernel_Handle := Get_Kernel (Context);
      A_Context : constant Activity_Context_Access :=
                    Activity_Context_Access (Context);
      Activity  : constant Activity_Id :=
                    Value (Activity_Information (A_Context));
      VCS       : constant VCS_Access :=
                    Get_VCS_For_Activity (Kernel, Activity);
      Files     : constant String_List.List :=
                    Get_Files_In_Activity (Activity);
      Iter      : String_List.List_Node;
   begin
      --  For each file we get the diff

      Iter := String_List.First (Files);

      for K in 1 .. String_List.Length (Files) loop
         Diff (VCS, Create (String_List.Data (Iter)));
         Iter := String_List.Next (Iter);
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Diff_Activity;

   ----------------------
   -- On_Menu_Edit_Log --
   ----------------------

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Kernel    : constant Kernel_Handle := Get_Kernel (Context);
      List      : String_List.List;

      A_Context : Activity_Context_Access :=
                    Activity_Context_Access (Context);
      Activity  : Activity_Id;

   begin
      if Context /= null then
         if Has_File_Information (A_Context) then
            --  This is a file

            List := Get_Selected_Files (Kernel);

            while not String_List.Is_Empty (List) loop
               declare
                  File : constant Virtual_File :=
                           Create (String_List.Head (List));
               begin
                  Get_Log_From_ChangeLog (Kernel, File);

                  declare
                     Log_File     : constant Virtual_File :=
                                      Get_Log_From_File (Kernel, File, True);
                     Already_Open : Boolean;
                  begin
                     Already_Open := Is_Open (Kernel, Log_File);
                     Open_File_Editor (Kernel, Log_File);

                     if not Already_Open then
                        Split
                          (Get_MDI (Kernel), Gtk.Enums.Orientation_Vertical,
                           Reuse_If_Possible => True, After => True);
                     end if;
                  end;
               end;

               String_List.Next (List);
            end loop;

         else
            --  This is an activity line

            A_Context := Activity_Context_Access (Context);
            Activity  := Value (Activity_Information (A_Context));

            declare
               Log_File : constant Virtual_File :=
                            Get_Log_File (Kernel, Activity);
               Already_Open : Boolean;
            begin
               Already_Open := Is_Open (Kernel, Log_File);
               Open_File_Editor (Kernel, Log_File);

               if not Already_Open then
                  Split
                    (Get_MDI (Kernel), Gtk.Enums.Orientation_Vertical,
                     Reuse_If_Possible => True, After => True);
               end if;
            end;

         end if;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Edit_Log;

   -------------------
   -- Open_Explorer --
   -------------------

   procedure Open_Activities_Explorer
     (Kernel  : Kernel_Handle;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Context);
      Explorer : VCS_Activities_View_Access;
   begin
      Explorer := Get_Activities_Explorer (Kernel, True, True);
      Clear (Explorer);
      Query_Activities_Files (Explorer, Kernel, Real_Query => False);
   end Open_Activities_Explorer;

   ----------------------------
   -- Query_Activities_Files --
   ----------------------------

   procedure Query_Activities_Files
     (Explorer   : VCS_Activities_View_Access;
      Kernel     : Kernel_Handle;
      Real_Query : Boolean)
   is
      pragma Unreferenced (Explorer);

      VCS      : VCS_Access;
      Activity : Activity_Id;
      Files    : String_List.List;
      Status   : File_Status_List.List;
   begin
      Activity := First;

      while Activity /= No_Activity loop
         Files := Get_Files_In_Activity (Activity);
         VCS   := Get_VCS_For_Activity (Kernel, Activity);

         if String_List.Is_Empty (Files) or else VCS = null then
            Display_File_Status
              (Kernel, Activity, File_Status_List.Null_List, VCS, True);

         else
            if Real_Query then
               Get_Status (VCS, Files);

            else
               Status := Local_Get_Status (VCS, Files);
               Display_File_Status
                 (Kernel, Activity, Status, VCS, False, True);
               File_Status_List.Free (Status);
            end if;
         end if;

         Activity := Next (Activity);
      end loop;
   end Query_Activities_Files;

   procedure Query_Activities_Files
     (Kernel     : Kernel_Handle;
      Real_Query : Boolean) is
   begin
      Query_Activities_Files
        (Get_Activities_Explorer (Kernel), Kernel, Real_Query);
   end Query_Activities_Files;

   ------------------
   -- Query_Status --
   ------------------

   procedure Query_Status
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Explorer : VCS_Activities_View_Access;
   begin
      Open_Activities_Explorer (Kernel, null);
      Explorer := Get_Activities_Explorer (Kernel);
      Clear (Explorer);
      Query_Activities_Files (Explorer, Kernel, True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Query_Status;

   ------------------------------------
   -- VCS_Activities_Contextual_Menu --
   ------------------------------------

   procedure VCS_Activities_Contextual_Menu
     (Kernel  : Kernel_Handle;
      Context : Selection_Context_Access;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      File_Section     : Boolean;
      Activity_Section : Boolean;
      Item             : Gtk_Menu_Item;
      Check            : Gtk_Check_Menu_Item;
      Activity         : Activity_Id;
      VCS              : VCS_Access;

   begin
      --  Determine which sections should be displayed

      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         Activity_Section := not Has_File_Information
           (File_Selection_Context_Access (Context));
         File_Section     := Has_File_Information
           (File_Selection_Context_Access (Context));
      else
         Activity_Section := False;
         File_Section     := False;
      end if;

      if Activity_Section then
         Activity := Value
           (Activity_Information (Activity_Context_Access (Context)));

         VCS := Get_VCS_For_Activity (Kernel, Activity);

         Gtk_New (Check, Label => -"Group commit");
         Set_Active (Check, Get_Group_Commit (Activity));
         Append (Menu, Check);
         Context_Callback.Connect
           (Check, "activate", On_Menu_Toggle_Group_Commit'Access, Context);
         Set_Sensitive
           (Check,
            VCS /= null
            and then Absolute_Filenames_Supported (VCS)
            and then Atomic_Commands_Supported (VCS));

         Gtk_New (Item);
         Append (Menu, Item);
      end if;

      Gtk_New (Item, Label => -"Create new activity");
      Append (Menu, Item);
      Context_Callback.Connect
        (Item, "activate", On_Menu_Create_Activity'Access, Context);
      Set_Sensitive (Item, True);

      if Activity_Section then
         Gtk_New (Item, Label => -"Delete activity");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate", On_Menu_Delete_Activity'Access, Context);
         Set_Sensitive (Item, True);

         Gtk_New (Item, Label => -"Commit activity");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate", On_Menu_Commit_Activity'Access, Context);
         Set_Sensitive (Item, True);

         Gtk_New (Item, Label => -"Diff against head revision");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate", On_Menu_Diff_Activity'Access, Context);
         Set_Sensitive (Item, True);
      end if;

      Gtk_New (Item);
      Append (Menu, Item);

      if File_Section then
         Gtk_New (Item, Label => -"Remove from activity");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate", On_Menu_Remove_From_Activity'Access, Context);
         Set_Sensitive (Item, File_Section);
      end if;

      if Context /= null then
         --  Fill the section relative to files

         Gtk_New (Item, Label => -"Edit revision log");
         Append (Menu, Item);
         Context_Callback.Connect
           (Item, "activate", On_Menu_Edit_Log'Access, Context);
         Set_Sensitive (Item, True);
      end if;
   end VCS_Activities_Contextual_Menu;

end VCS_Activities_View_API;
