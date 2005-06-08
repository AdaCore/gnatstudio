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

with Gtk.Accel_Group;           use Gtk.Accel_Group;
with Gtk.Widget;                use Gtk.Widget;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Enums;

with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.MDI;                use Gtkada.MDI;

with VCS;                       use VCS;
with VCS.Unknown_VCS;           use VCS.Unknown_VCS;
with VCS_View_Pkg;              use VCS_View_Pkg;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GPS.Location_View;         use GPS.Location_View;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;

with String_List_Utils;         use String_List_Utils;

with VCS_Module;                use VCS_Module;
with Log_Utils;                 use Log_Utils;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;

with Basic_Types;               use Basic_Types;

with Projects.Registry;         use Projects, Projects.Registry;

with Commands;                  use Commands;
with Commands.VCS;              use Commands.VCS;
with Commands.External;         use Commands.External;

with Traces;                    use Traces;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with VFS;                       use VFS;
with File_Utils;                use File_Utils;
with String_Utils;              use String_Utils;

package body VCS_View_API is

   Max_Rev_Length : constant := 10;
   --  The maximum length of a revision string, in characters. Revisions longer
   --  than this will be krunched when displayed in the editors.

   VCS_Menu_Prefix : constant String := "<gps>/VCS/";

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure List_Project_Files
     (Context   : Selection_Context_Access;
      Recursive : Boolean);
   --  List the files contained in the project relative to Context, if any.
   --  If recursive is True, files in imported subprojects will be listed
   --  as well.

   procedure Get_Status_Project
     (Context   : Selection_Context_Access;
      Recursive : Boolean);
   --  Display the status for the files contained in the project relative
   --  to Context, if any.
   --  If recursive is True, files in imported subprojects will be listed
   --  as well.

   procedure Update_Project
     (Context   : Selection_Context_Access;
      Recursive : Boolean);
   --  Update the files contained in the project relative to Context, if any.
   --  If recursive is True, files in imported subprojects will be listed
   --  as well.

   procedure Query_Project_Files
     (Explorer   : VCS_View_Access;
      Kernel     : Kernel_Handle;
      Project    : Project_Type;
      Real_Query : Boolean;
      Recursive  : Boolean);
   --  Query/List the status of files belonging to Project.
   --  If Recursive is True, files from sub-projects will also be queried.
   --  If Real_Query is True, a real VCS query will be made, otherwise
   --  the files will simply be listed.
   --  Calling this does NOT open the VCS Explorer.

   procedure Change_Context
     (Explorer : VCS_View_Access;
      Context  : Selection_Context_Access);
   --  Fill the explorer with files that correspond to Context.
   --  Context might be null, in which case the contents of the root project is
   --  shown.

   function Check_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Display Head in the console, then return True if List is not
   --  empty, otherwise display List in the console and return False.

   procedure Log_Action_Files
     (Kernel : Kernel_Handle;
      Ref    : VCS_Access;
      Action : VCS_Action;
      Files  : String_List.List);
   --  Perform Action on the list of files, assuming that they all belong to
   --  the VCS system identified by Ref.
   --  This subprogram will do all the necessary file/log checks before
   --  performing Action.

   function Get_Files_In_Project
     (Project   : Project_Type;
      Recursive : Boolean := True) return String_List.List;
   --  Return the list of source files in Project.
   --  If Recursive is True, then source files from all included
   --  subprojects will be returned as well.

   function Get_Dirs_In_Project
     (Project   : Project_Type;
      Recursive : Boolean := False) return String_List.List;
   --  Return the source directories contained in Project.

   function Get_Current_Ref (Kernel : Kernel_Handle) return VCS_Access;
   --  Return the VCS reference corresponding to the current context in Kernel.

   function Get_Selected_Files
     (Context : Selection_Context_Access) return String_List.List;
   --  Return the list of files that are selected, according to Context.

   procedure Process_Dirs
     (Context    : Selection_Context_Access;
      Recursive  : Boolean;
      Update     : Boolean;
      Get_Status : Boolean);
   --  Perform VCS operations on directories contained in Context.

   function Save_Files
     (Kernel : Kernel_Handle;
      Files  : String_List.List;
      Save_Logs : Boolean := False) return Boolean;
   --  Ask the user whether he wants to save the file editors for Files.
   --  Return False if the user has cancelled the action.

   procedure On_Log_Action
     (Context : Selection_Context_Access;
      Action  : VCS_Action);
   --  Generic callback for an action that requires associated logs.

   procedure Comparison
     (Context : Selection_Context_Access;
      One_Rev : Boolean);
   --  Factorize code between On_Menu_Diff_Specific and On_Menu_Diff2.

   function Action_To_Log_Suffix (Action : VCS_Action) return String;
   --  Return the suffix to be used in log files that correspond to Action.

   --------------------------
   -- Action_To_Log_Suffix --
   --------------------------

   function Action_To_Log_Suffix (Action : VCS_Action) return String is
   begin
      case Action is
         when Commit =>
            return "$log";

         when others =>
            return "$" & Action'Img & "$log";
      end case;
   end Action_To_Log_Suffix;

   ---------------------
   -- Get_Current_Ref --
   ---------------------

   function Get_Current_Ref (Project : Project_Type) return VCS_Access is
   begin
      --  ??? maybe we could cache this information.
      return Get_VCS_From_Id
        (Get_Attribute_Value (Project, Vcs_Kind_Attribute));
   end Get_Current_Ref;

   ---------------------
   -- Get_Current_Ref --
   ---------------------

   function Get_Current_Ref (Kernel : Kernel_Handle) return VCS_Access is
      C : constant Selection_Context_Access := Get_Current_Context (Kernel);
   begin
      if C = null then
         return Get_Current_Ref (Get_Project (Kernel));
      else
         return Get_Current_Ref (C);
      end if;
   end Get_Current_Ref;

   -------------------
   -- Check_Handler --
   -------------------

   function Check_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean
   is
      use String_List;

      List_Temp : String_List.List_Node := First (List);
      Head_Temp : String_List.List_Node := First (Head);
      Length    : Integer := 0;

   begin
      if not String_List.Is_Empty (List) then
         while Head_Temp /= Null_Node loop
            Push_Message (Kernel, Error, Data (Head_Temp));
            Head_Temp := Next (Head_Temp);
         end loop;
      end if;

      while List_Temp /= Null_Node loop
         declare
            S : constant String := Data (List_Temp);
         begin
            Push_Message (Kernel, Error, S);
            Length := Length + S'Length;
         end;

         List_Temp := Next (List_Temp);
      end loop;

      if Length /= 0 then
         declare
            S : String (1 .. Length);
         begin
            Length := 1;
            List_Temp := First (List);

            while List_Temp /= Null_Node loop
               declare
                  D : constant String := Data (List_Temp);
               begin
                  S (Length .. Length - 1 + D'Length) := D;
                  Length := Length + D'Length;
               end;

               List_Temp := Next (List_Temp);
            end loop;

            Parse_File_Locations (Kernel, S, -"Style/Log Check");
         end;
      end if;

      return String_List.Is_Empty (List);
   end Check_Handler;

   -------------------------
   -- VCS_Contextual_Menu --
   -------------------------

   procedure VCS_Contextual_Menu
     (Kernel          : Kernel_Handle;
      Context         : Selection_Context_Access;
      Menu            : access Gtk.Menu.Gtk_Menu_Record'Class;
      Show_Everything : Boolean)
   is
      Item      : Gtk_Menu_Item;
      Menu_Item : Gtk_Menu_Item;
      Submenu   : Gtk_Menu;
      File_Name : File_Selection_Context_Access;
      Ref       : VCS_Access;
      Actions   : Action_Array;

      File_Section    : Boolean;
      Dir_Section     : Boolean;
      Project_Section : Boolean;
      Section_Active  : Boolean;
      Items_Inserted  : Boolean := False;

      Group : constant Gtk_Accel_Group := Get_Default_Accelerators (Kernel);

      procedure Add_Action
        (Action   : VCS_Action;
         Callback : Context_Callback.Marshallers.Void_Marshaller.Handler;
         Via_Log  : Boolean := False);
      --  Add a menu item corresponding to Action.
      --  If Via_Log is True, ???

      procedure Add_Separator;
      --  Add a separator in the menu if needed.

      ----------------
      -- Add_Action --
      ----------------

      procedure Add_Action
        (Action   : VCS_Action;
         Callback : Context_Callback.Marshallers.Void_Marshaller.Handler;
         Via_Log  : Boolean := False) is
      begin
         if Actions (Action) /= null then
            if Via_Log then
               Gtk_New (Item, Actions (Action).all & (-" (via revision log)"));
            else
               Gtk_New (Item, Actions (Action).all);
            end if;

            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller (Callback),
               Context);

            if not Section_Active then
               Set_Sensitive (Item, False);
            end if;

            if Show_Everything then
               Set_Accel_Path
                 (Item, VCS_Menu_Prefix & Actions (Action).all, Group);
            end if;

            Items_Inserted := True;
         end if;
      end Add_Action;

      -------------------
      -- Add_Separator --
      -------------------

      procedure Add_Separator is
      begin
         if Items_Inserted then
            Gtk_New (Item);
            Append (Menu, Item);
            Items_Inserted := False;
         end if;
      end Add_Separator;

      Log_File   : Boolean := False;
      Log_Action : VCS_Action;
      Log_Exists : Boolean;

   begin
      if Context = null then
         Ref := Get_Current_Ref (Get_Project (Kernel));
      else
         Ref := Get_Current_Ref (Context);
      end if;

      if Ref = null then
         --  ??? Should add a menu item to add a VCS in the project
         return;
      end if;

      if VCS_Module_ID.Menu_Context /= null then
         GPS.Kernel.Unref (VCS_Module_ID.Menu_Context);
      end if;

      VCS_Module_ID.Menu_Context := Context;
      GPS.Kernel.Ref (VCS_Module_ID.Menu_Context);

      Actions := Get_Identified_Actions (Ref);

      if Context /= null
        and then Context.all in File_Selection_Context'Class
      then
         File_Name := File_Selection_Context_Access (Context);
      end if;

      --  Determine which sections should be displayed

      File_Section := File_Name /= null
        and then Has_File_Information (File_Name);
      Dir_Section := File_Name /= null
        and then Has_Directory_Information (File_Name);
      Project_Section := File_Name /= null
        and then Has_Project_Information (File_Name);

      --  Look for the special case for handling log files

      if File_Section then
         declare
            File_S : constant String :=
              Full_Name (File_Information (File_Name)).all;
         begin
            if File_S'Length > 5
              and then File_S (File_S'Last - 3 .. File_S'Last) = "$log"
            then
               --  By default, the log is a "commit" log.
               Log_Action := Commit;

               declare
                  Index : Natural;
               begin
                  --  Attempt to read Action from the name of the log file

                  Index := File_S'Last - 4;
                  Skip_To_Char (File_S, Index, '$', -1);

                  if Index - 1 in File_S'Range then
                     Log_Action := VCS_Action'Value
                       (File_S (Index + 1 .. File_S'Last - 4));
                  end if;

               exception
                  when others =>
                     --  If the log does not describe a valid action, leave
                     --  the log action to Commit.
                     null;
               end;

               Log_File := True;
            end if;
         end;
      end if;

      --  Fill the section relative to files.

      Section_Active := File_Section;

      if File_Section or else Show_Everything then
         if Log_File then
            declare
               Original : constant Virtual_File :=
                 Get_File_From_Log (Kernel, File_Information (File_Name));
            begin
               if Original /= VFS.No_File
                 and then Actions (Commit) /= null
               then
                  Set_File_Information
                    (File_Name,
                     Original,
                     Get_Project_From_File (Get_Registry (Kernel).all,
                                            Original));

                  Gtk_New (Item, Label => Actions (Log_Action).all & " ("
                           & Krunch (Base_Name (Original)) & ")");

                  Append (Menu, Item);

                  case Log_Action is
                     when Add =>
                        Context_Callback.Connect
                          (Item, "activate", On_Menu_Add'Access,
                           Selection_Context_Access (File_Name));

                     when Remove =>
                        Context_Callback.Connect
                          (Item, "activate", On_Menu_Remove'Access,
                           Selection_Context_Access (File_Name));

                     when others =>
                        Context_Callback.Connect
                          (Item, "activate", On_Menu_Commit'Access,
                           Selection_Context_Access (File_Name));
                  end case;
               end if;
            end;

         else
            Log_Exists :=
              File_Name /= null and then
              Get_Log_From_File
                (Kernel, File_Information (File_Name), False) /= VFS.No_File;
            Add_Action (Status_Files, On_Menu_Get_Status'Access);
            Add_Action (Update, On_Menu_Update'Access);
            Add_Action (Commit, On_Menu_Commit'Access, not Log_Exists);

            Add_Separator;

            Add_Action (Open, On_Menu_Open'Access);
            Add_Action (History, On_Menu_View_Log'Access);
            Add_Action (History_Revision, On_Menu_View_Log_Rev'Access);

            Add_Separator;

            Add_Action (Diff_Head, On_Menu_Diff'Access);
            Add_Action (Diff_Working, On_Menu_Diff_Working'Access);
            Add_Action (Diff, On_Menu_Diff_Specific'Access);
            Add_Action (Diff2, On_Menu_Diff2'Access);
            Add_Action (Diff_Base_Head, On_Menu_Diff_Base_Head'Access);

            Add_Separator;

            if Actions (Annotate) /= null then
               Gtk_New (Item, Label => -"Add " & Actions (Annotate).all);
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate", On_Menu_Annotate'Access, Context);
               Set_Sensitive (Item, Section_Active);

               Gtk_New (Item, Label => -"Remove " & Actions (Annotate).all);
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate", On_Menu_Remove_Annotate'Access, Context);

               Set_Sensitive (Item, Section_Active);
            end if;

            Gtk_New (Item, Label => -"Edit revision log");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate", On_Menu_Edit_Log'Access, Context);
            Set_Sensitive (Item, Section_Active);

            Gtk_New (Item, Label => -"Edit global ChangeLog");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate", On_Menu_Edit_ChangeLog'Access, Context);
            Set_Sensitive (Item, Section_Active);

            Gtk_New (Item, Label => -"Remove revision log");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate", On_Menu_Remove_Log'Access, Context);
            Set_Sensitive (Item, Section_Active);

            Add_Separator;

            Add_Action (Add, On_Menu_Add'Access, not Log_Exists);
            Add_Action (Remove, On_Menu_Remove'Access, not Log_Exists);
            Add_Action (Revert, On_Menu_Revert'Access);
            Add_Action (Resolved, On_Menu_Resolved'Access);
         end if;
      end if;

      if Show_Everything
        or else (File_Section and then (Project_Section or else Dir_Section))
      then
         Gtk_New (Item);
         Append (Menu, Item);
      end if;

      --  Fill the section relative to directory

      Section_Active := Dir_Section;

      if Show_Everything or else Dir_Section then
         if Show_Everything
           or else Project_Section
           or else File_Section
         then
            Gtk_New (Menu_Item, Label => -"Directory");
            Append (Menu, Menu_Item);
            Gtk_New (Submenu);
            Set_Submenu (Menu_Item, Gtk_Widget (Submenu));
            Set_Sensitive (Menu_Item, Section_Active);
         else
            Submenu := Gtk_Menu (Menu);
         end if;

         if Actions (Status_Files) /= null then
            Gtk_New
              (Item,
               Label => Actions (Status_Files).all & (-" for directory"));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, "activate", On_Menu_Get_Status_Dir'Access,
               Selection_Context_Access (File_Name));
            Set_Sensitive (Item, Section_Active);

            Items_Inserted := True;
         end if;

         if Actions (Update) /= null then
            Gtk_New
              (Item, Label => Actions (Update).all & (-" directory"));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, "activate", On_Menu_Update_Dir'Access,
               Selection_Context_Access (File_Name));
            Set_Sensitive (Item, Section_Active);

            Items_Inserted := True;
         end if;

         if Actions (Status_Files) /= null then
            Gtk_New
              (Item, Label => Actions (Status_Files).all
               & (-" for directory recursively"));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, "activate", On_Menu_Get_Status_Dir_Recursive'Access,
               Selection_Context_Access (File_Name));
            Set_Sensitive (Item, Section_Active);

            Items_Inserted := True;
         end if;

         if Actions (Update) /= null then
            Gtk_New
              (Item, Label => Actions (Update).all
               & (-" directory recursively"));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, "activate", On_Menu_Update_Dir_Recursive'Access,
               Selection_Context_Access (File_Name));
            Set_Sensitive (Item, Section_Active);

            Items_Inserted := True;
         end if;

         if Show_Everything
           or else Project_Section
           or else File_Section
         then
            Set_Sensitive (Menu_Item, Section_Active and then Items_Inserted);
         end if;
      end if;

      if Show_Everything
        or else ((File_Section or else Dir_Section) and then Project_Section)
      then
         Add_Separator;
      end if;

      --  Fill the section relative to project

      Section_Active := Project_Section;

      if Show_Everything or else Project_Section then
         if Show_Everything
           or else Dir_Section
           or else File_Section
         then
            Gtk_New (Menu_Item, Label => -"Project");
            Append (Menu, Menu_Item);
            Gtk_New (Submenu);
            Set_Submenu (Menu_Item, Gtk_Widget (Submenu));
         else
            Submenu := Gtk_Menu (Menu);
         end if;

         Gtk_New (Item, Label => -"List all files in project");
         Append (Submenu, Item);
         Context_Callback.Connect
           (Item, "activate", On_Menu_List_Project_Files'Access,
            Selection_Context_Access (File_Name));

         if Actions (Status_Files) /= null then
            Gtk_New
              (Item, Label => Actions (Status_Files).all & (-" for project"));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, "activate", On_Menu_Get_Status_Project'Access,
               Selection_Context_Access (File_Name));
            Set_Sensitive (Item, Section_Active);
         end if;

         if Actions (Update) /= null then
            Gtk_New (Item, Label => Actions (Update).all & (-" project"));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, "activate", On_Menu_Update_Project'Access,
               Selection_Context_Access (File_Name));
            Set_Sensitive (Item, Section_Active);
         end if;

         Gtk_New (Item, Label => -"List all files in project and subprojects");
         Append (Submenu, Item);
         Context_Callback.Connect
           (Item, "activate", On_Menu_List_Project_Files_Recursive'Access,
            Selection_Context_Access (File_Name));

         if Actions (Status_Files) /= null then
            Gtk_New
              (Item, Label => Actions (Status_Files).all &
                 (-" for project and subprojects"));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, "activate", On_Menu_Get_Status_Project_Recursive'Access,
               Selection_Context_Access (File_Name));
            Set_Sensitive (Item, Section_Active);
         end if;

         if Actions (Update) /= null then
            Gtk_New
              (Item, Label => Actions (Update).all
               & (-" project and subprojects"));
            Append (Submenu, Item);
            Context_Callback.Connect
              (Item, "activate", On_Menu_Update_Project_Recursive'Access,
               Selection_Context_Access (File_Name));
            Set_Sensitive (Item, Section_Active);
         end if;

         if Show_Everything
           or else Dir_Section
           or else File_Section
         then
            Set_Sensitive (Menu_Item, Section_Active);
         end if;
      end if;
   end VCS_Contextual_Menu;

   --------------------
   -- Change_Context --
   --------------------

   procedure Change_Context
     (Explorer : VCS_View_Access;
      Context  : Selection_Context_Access)
   is
      File   : File_Selection_Context_Access;
      Status : File_Status_List.List;
      Dirs   : String_List.List;
      Ref    : VCS_Access;

      use String_List;
   begin
      if Explorer = null then
         return;
      end if;

      if Context = null then
         Query_Project_Files
           (Explorer,
            Get_Kernel (Explorer),
            Get_Project (Get_Kernel (Explorer)),
            False, False);
         return;
      end if;

      Ref := Get_Current_Ref (Context);
      Set_Current_Context (Explorer, Context);
      Clear (Explorer);

      if Context.all in File_Selection_Context'Class then
         File := File_Selection_Context_Access (Context);

         if Has_Directory_Information (File)
           and then not Has_File_Information (File)
         then
            String_List.Append (Dirs, Directory_Information (File));
            Status :=  Local_Get_Status (Ref, Dirs);
            String_List.Free (Dirs);
            Display_File_Status
              (Get_Kernel (Context), Status, Ref, False, True);
            File_Status_List.Free (Status);
            String_List.Free (Dirs);

         elsif Has_Project_Information (File)
           and then not Has_Directory_Information (File)
         then
            Query_Project_Files
              (Explorer,
               Get_Kernel (Context),
               Project_Information (File),
               False, False);

         else
            Query_Project_Files
              (Explorer,
               Get_Kernel (Context),
               Get_Project (Get_Kernel (Context)),
               False, False);
         end if;
      end if;
   end Change_Context;

   -------------------
   -- Open_Explorer --
   -------------------

   procedure Open_Explorer
     (Kernel  : Kernel_Handle;
      Context : Selection_Context_Access)
   is
      Explorer : VCS_View_Access;
   begin
      Explorer := Get_Explorer (Kernel, True, True);
      Change_Context (Explorer, Context);
   end Open_Explorer;

   ------------------------
   -- Get_Selected_Files --
   ------------------------

   function Get_Selected_Files
     (Context : Selection_Context_Access) return String_List.List
   is
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      Explorer : VCS_View_Access;
      List     : String_List.List;
      File     : File_Selection_Context_Access;
   begin
      if Context.all in File_Selection_Context'Class then
         File := File_Selection_Context_Access (Context);

         if Get_Creator (Context) = Module_ID (VCS_Module_ID) then
            Explorer := Get_Explorer (Kernel, False);
            List := Get_Selected_Files (Explorer);
         else
            if Has_File_Information (File) then
               String_List.Append
                 (List, Full_Name (File_Information (File)).all);
            end if;
         end if;
      end if;

      return List;
   end Get_Selected_Files;

   ----------------------------
   -- On_Menu_Edit_ChangeLog --
   ----------------------------

   procedure On_Menu_Edit_ChangeLog
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      List   : String_List.List;
      Kernel : Kernel_Handle;

      procedure Get_Location
        (File, ChangeLog_File : Virtual_File;
         Line, Column         : out Natural);
      --  Returns the line/column where the cursor needs to be located.
      --  This function must be called only when the global ChangeLog file
      --  contains an entry for file.

      ------------------
      -- Get_Location --
      ------------------

      procedure Get_Location
        (File, ChangeLog_File : Virtual_File;
         Line, Column         : out Natural)
      is
         Filename           : constant String := Base_Name (File);
         --  The filename to look for in the ChangeLog file

         ChangeLog_Filename : aliased String := Full_Name (ChangeLog_File).all;
         --  The global ChangeLog file

         Last               : Natural;
         Entry_Found        : Boolean;

      begin
         Line   := 1;
         Column := 0;

         --  Get last line in the file

         Last := Natural'Value
           (Execute_GPS_Shell_Command
              (Kernel,
               "Editor.get_last_line",
               (1 => ChangeLog_Filename'Unchecked_Access)));

         --  First, look for the filename entry

         loop
            declare
               L_Img  : aliased String  := Image (Line);
               B_Line : constant String :=
                 Execute_GPS_Shell_Command
                   (Kernel,
                    "Editor.get_chars",
                    (ChangeLog_Filename'Unchecked_Access,
                     L_Img'Unchecked_Access));

            begin
               Entry_Found := Index (B_Line, Filename) /= 0;

               exit when Entry_Found or else Line = Last;

               Line := Line + 1;
            end;
         end loop;

         --  Look for the first empty line, this is where the cursor
         --  needs to be set.

         if not Entry_Found then
            --  No entry found, this should not happen, returns the first
            --  position in the file.
            Line   := 1;
            Column := 1;
            return;
         end if;

         Line := Line + 1;

         loop
            declare
               L_Img   : aliased String := Image (Line);
               B_Line  : constant String :=
                 Execute_GPS_Shell_Command
                   (Kernel,
                    "Editor.get_chars",
                    (ChangeLog_Filename'Unchecked_Access,
                     L_Img'Unchecked_Access));
               Is_Empty : Boolean := True;
            begin
               for K in B_Line'Range loop
                  if B_Line (K) /= ' '
                    and then B_Line (K) /= ASCII.HT
                    and then B_Line (K) /= ASCII.CR
                    and then B_Line (K) /= ASCII.LF
                  then
                     Is_Empty := False;
                     exit;
                  end if;
               end loop;

               if Is_Empty then
                  if B_Line'Length = 0
                    or else B_Line (B_Line'First) = ASCII.LF
                  then
                     --  An empty line, insert an HT

                     declare
                        L_Img : aliased String := Image (Line);
                        C_Img : aliased String := "1";
                        Text1 : aliased String :=
                          String'(1 => ASCII.HT);
                        Text2 : aliased String :=
                          ASCII.HT & ASCII.LF & ASCII.LF;
                        Args  : GNAT.OS_Lib.Argument_List (1 .. 4);

                     begin
                        Args (1) := ChangeLog_Filename'Unchecked_Access;
                        Args (2) := L_Img'Unchecked_Access;
                        Args (3) := C_Img'Unchecked_Access;

                        if Line >= Last then
                           --  This is the end of the file
                           Args (4) := Text1'Unchecked_Access;
                        else
                           Args (4) := Text2'Unchecked_Access;
                        end if;

                        Execute_GPS_Shell_Command
                          (Kernel, "Editor.replace_text", Args);
                     end;

                     Column := 2;

                  elsif B_Line (B_Line'First) = ASCII.HT then
                     --  A line with a single HT, place cursor just after
                     Column := 2;

                  else
                     --  Only spaces, put cursor at the end of the line
                     Column := B_Line'Last;
                  end if;

                  exit;
               end if;

               Line := Line + 1;
            end;
         end loop;
      end Get_Location;

   begin
      Kernel := Get_Kernel (Context);
      List   := Get_Selected_Files (Context);

      while not String_List.Is_Empty (List) loop
         declare
            File           : constant Virtual_File :=
              Create (String_List.Head (List));
            ChangeLog_File : constant Virtual_File :=
              Get_ChangeLog_From_File (Kernel, File);
            Already_Open   : Boolean;
            Line, Column   : Natural;
         begin
            Already_Open := Is_Open (Kernel, ChangeLog_File);
            Open_File_Editor (Kernel, ChangeLog_File);

            --  At this point we know that there is an entry for the current
            --  file for the current data into the ChangeLog file. Set the
            --  cursor location at the right position.

            Get_Location (File, ChangeLog_File, Line, Column);

            declare
               L_Img              : aliased String := Image (Line);
               C_Img              : aliased String := Image (Column);
               ChangeLog_Filename : aliased String :=
                 Full_Name (ChangeLog_File).all;

            begin
               Execute_GPS_Shell_Command
                 (Kernel,
                  "Editor.edit",
                  (ChangeLog_Filename'Unchecked_Access,
                   L_Img'Unchecked_Access, C_Img'Unchecked_Access));
            end;

            if not Already_Open then
               Split (Get_MDI (Kernel), Gtk.Enums.Orientation_Vertical,
                      Reuse_If_Possible => True, After => True);
            end if;
         end;

         String_List.Next (List);
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Edit_ChangeLog;

   ----------------------
   -- On_Menu_Edit_Log --
   ----------------------

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      List   : String_List.List;
      Kernel : Kernel_Handle;

   begin
      Kernel := Get_Kernel (Context);
      List   := Get_Selected_Files (Context);

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
                  Split (Get_MDI (Kernel), Gtk.Enums.Orientation_Vertical,
                         Reuse_If_Possible => True, After => True);
               end if;
            end;
         end;

         String_List.Next (List);
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Edit_Log;

   ----------------
   -- Save_Files --
   ----------------

   function Save_Files
     (Kernel    : Kernel_Handle;
      Files     : String_List.List;
      Save_Logs : Boolean := False) return Boolean
   is
      use String_List;
      Children   : MDI_Child_Array (1 .. Length (Files));
      Logs       : MDI_Child_Array (Children'Range);
      Files_Temp : List_Node := First (Files);
      File       : Virtual_File;
   begin
      for C in Children'Range loop
         File := Create (Full_Filename => Head (Files));
         Children (C) := Get_File_Editor (Kernel, File);

         if Save_Logs then
            Logs (C) := Get_File_Editor
              (Kernel, Get_Log_From_File (Kernel, File, False));
         end if;

         Files_Temp := Next (Files_Temp);
      end loop;

      if Save_Logs then
         return Save_MDI_Children
           (Kernel, Children & Logs, Force => Get_Pref (Kernel, Auto_Save));
      else
         return Save_MDI_Children
           (Kernel, Children, Force => Get_Pref (Kernel, Auto_Save));
      end if;
   end Save_Files;

   ----------------------
   -- Log_Action_Files --
   ----------------------

   procedure Log_Action_Files
     (Kernel : Kernel_Handle;
      Ref    : VCS_Access;
      Action : VCS_Action;
      Files  : String_List.List)
   is
      use String_List;

      Logs               : String_List.List;
      Files_Temp         : List_Node := First (Files);

      Commit_Command     : Log_Action_Command_Access;
      Get_Status_Command : Get_Status_Command_Access;

      Project            : Project_Type;

      Success            : Boolean;
      pragma Unreferenced (Success);

      Cancel_All         : Boolean := False;

      Log_Checks         : External_Command_Access;
      File_Checks        : External_Command_Access;
      File               : Virtual_File;

      First_Check, Last_Check : Command_Access := null;

   begin
      if not Save_Files (Kernel, Files, Save_Logs => True) then
         return;
      end if;

      while Files_Temp /= Null_Node loop
         --  Save any open log editors, and then get the corresponding logs.

         File := Create (Full_Filename => Data (Files_Temp));
         Append (Logs, Get_Log (Kernel, File));
         Files_Temp := Next (Files_Temp);
      end loop;

      --  Create the Commit command.
      Create (Commit_Command, Ref, Action, Files, Logs);

      --  Create the Get_Status command.
      Create (Get_Status_Command, Ref, Files);

      --  The Get_Status command is a consequence of the Commit command.
      Add_Consequence_Action
        (Command_Access (Commit_Command),
         Command_Access (Get_Status_Command));

      --  Create the file checks and the log checks.
      Files_Temp := First (Files);

      while Files_Temp /= Null_Node loop
         File := Create (Full_Filename => Data (Files_Temp));
         Project := Get_Project_From_File (Get_Registry (Kernel).all, File);

         if Project /= No_Project then
            declare
               File_Check_Script : constant String := Get_Attribute_Value
                 (Project, Vcs_File_Check);
               Log_Check_Script  : constant String := Get_Attribute_Value
                 (Project, Vcs_Log_Check);
               Log_File  : constant Virtual_File :=
                             Get_Log_From_File
                               (Kernel, File, True,
                                Suffix => Action_To_Log_Suffix (Action));
               File_Args         : String_List.List;
               Log_Args          : String_List.List;
               Head_List         : String_List.List;
               S                 : GNAT.OS_Lib.String_Access;

               use type GNAT.OS_Lib.String_Access;

            begin
               if File_Check_Script /= "" then
                  Append (File_Args, Data (Files_Temp));

                  Create (File_Checks,
                          Kernel,
                          File_Check_Script,
                          "",
                          File_Args,
                          Null_List,
                          Check_Handler'Access,
                          -"Version Control: Checking files");

                  if First_Check = null then
                     First_Check := Command_Access (File_Checks);
                  else
                     Add_Consequence_Action (Last_Check, File_Checks);
                  end if;

                  Last_Check := Command_Access (File_Checks);
               end if;

               if Log_Check_Script /= "" then
                  --  Check that the log file is not empty.

                  S := Read_File (Log_File);

                  if S = null then
                     Cancel_All := True;
                     Insert (Kernel,
                             (-"File could not be read: ")
                             & Full_Name (Log_File).all);

                     Free (File_Args);
                     Free (Log_Args);
                     Free (Head_List);
                     exit;

                  elsif S.all = "" then
                     if Message_Dialog
                       ((-"File: ") & Full_Name (File).all
                        & ASCII.LF & ASCII.LF &
                          (-"The revision log for this file is empty,")
                        & ASCII.LF &
                          (-"Commit anyway ?"),
                        Confirmation,
                        Button_Yes or Button_No,
                        Button_Yes,
                        "", -"Empty log detected",
                        Gtk.Enums.Justify_Left) = Button_No
                     then
                        Cancel_All := True;

                        GNAT.OS_Lib.Free (S);
                        Free (File_Args);
                        Free (Log_Args);
                        Free (Head_List);
                        exit;
                     end if;
                  end if;

                  GNAT.OS_Lib.Free (S);

                  Append (Log_Args, Full_Name (Log_File).all);
                  Append
                    (Head_List, -"File: " & Full_Name (File).all & ASCII.LF
                     & (-"The revision log does not pass the checks."));

                  Create
                    (Log_Checks,
                     Kernel,
                     Log_Check_Script,
                     "",
                     Log_Args,
                     Head_List,
                     Check_Handler'Access,
                     -"CVS: Checking file changelogs");

                  if First_Check = null then
                     First_Check := Command_Access (Log_Checks);
                  else
                     Add_Consequence_Action (Last_Check, Log_Checks);
                  end if;

                  Last_Check := Command_Access (Log_Checks);
               end if;

               Free (File_Args);
               Free (Log_Args);
               Free (Head_List);
            end;
         end if;

         Files_Temp := Next (Files_Temp);
      end loop;

      --  Execute the commit command after the last file check or log check
      --  command.

      if Last_Check /= null then
         Add_Consequence_Action (Last_Check, Commit_Command);
      else
         First_Check := Command_Access (Commit_Command);
      end if;

      if Cancel_All then
         Destroy (First_Check);
      else
         Launch_Background_Command
           (Kernel, First_Check, True, True, Name (Ref));
      end if;

      Free (Logs);
   end Log_Action_Files;

   ---------------------
   -- Get_Current_Ref --
   ---------------------

   function Get_Current_Ref
     (Context : Selection_Context_Access) return VCS_Access
   is
      File     : File_Selection_Context_Access;
      Explorer : VCS_View_Access;
      Kernel   : Kernel_Handle;

   begin
      if Context = null then
         return Get_VCS_From_Id ("");
      end if;

      Kernel := Get_Kernel (Context);

      if Get_Creator (Context) = Module_ID (VCS_Module_ID) then
         Explorer := Get_Explorer (Kernel, False);
         return Get_Current_Ref (Explorer);

      elsif Context.all in File_Selection_Context'Class then
         File := File_Selection_Context_Access (Context);

         if Has_Project_Information (File) then
            return Get_Current_Ref (Project_Information (File));
         end if;
      end if;

      return Get_Current_Ref (Get_Project (Kernel));
   end Get_Current_Ref;

   ------------------------
   -- On_Menu_Remove_Log --
   ------------------------

   procedure On_Menu_Remove_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      use String_List;
      Kernel     : constant Kernel_Handle := Get_Kernel (Context);
      Files      : String_List.List;
      Files_Temp : String_List.List_Node;
      Explorer   : VCS_View_Access;

   begin
      Files := Get_Selected_Files (Context);
      Explorer := Get_Explorer (Kernel);
      Files_Temp := First (Files);

      while Files_Temp /= Null_Node loop
         declare
            File  : constant Virtual_File :=
              Create (Full_Filename => Data (Files_Temp));
            Log   : constant Virtual_File :=
              Get_Log_From_File (Kernel, File, False);
         begin
            if Is_Regular_File (Log) then
               Delete (Log);
               Close_File_Editors (Kernel, Log);
            end if;

            Remove_File_From_Mapping (Kernel, File);

            if Explorer /= null then
               Refresh_Log (Explorer, File);
            end if;
         end;

         Files_Temp := Next (Files_Temp);
      end loop;

      Free (Files);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Remove_Log;

   --------------------
   -- On_Menu_Commit --
   --------------------

   procedure On_Menu_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      On_Log_Action (Context, Commit);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Commit;

   -------------------
   -- On_Log_Action --
   -------------------

   procedure On_Log_Action
     (Context : Selection_Context_Access;
      Action  : VCS_Action)
   is
      Kernel         : constant Kernel_Handle := Get_Kernel (Context);
      Files          : String_List.List;
      Real_Files     : String_List.List;
      Files_Temp     : String_List.List_Node;
      All_Logs_Exist : Boolean := True;
      File           : Virtual_File;
      Suffix         : constant String := Action_To_Log_Suffix (Action);

      use String_List;
      use type String_List.List_Node;
   begin
      Real_Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Real_Files) then
         Console.Insert
           (Kernel, -"VCS: No file selected, cannot commit", Mode => Error);
         return;
      end if;

      Files_Temp := String_List.First (Real_Files);

      while Files_Temp /= Null_Node loop
         declare
            S : constant String := Data (Files_Temp);
         begin
            if S'Length > 4
              and then S (S'Last - 3 .. S'Last) = "$log"
            then
               declare
                  L : constant Virtual_File :=
                    Get_File_From_Log (Kernel, Create (Full_Filename => S));
               begin
                  if L /= VFS.No_File then
                     Append (Files, Full_Name (L).all);
                  end if;
               end;
            else
               Append (Files, S);
            end if;
         end;

         Files_Temp := Next (Files_Temp);
      end loop;

      Free (Real_Files);

      Files_Temp := String_List.First (Files);

      --  Open log editors for files that don't have a log.

      while Files_Temp /= String_List.Null_Node loop
         File := Create (Full_Filename => String_List.Data (Files_Temp));

         if Get_Log_From_File (Kernel, File, False) = VFS.No_File then
            Get_Log_From_ChangeLog (Kernel, File, Suffix);
            All_Logs_Exist := False;

            Open_File_Editor
              (Kernel,
               Get_Log_From_File (Kernel, File, True, Suffix),
               Position => Position_Bottom);
         end if;

         Files_Temp := String_List.Next (Files_Temp);
      end loop;

      --  If All files have a log, commit the whole lot.

      if All_Logs_Exist then
         Log_Action_Files (Kernel, Get_Current_Ref (Context), Action, Files);
      end if;

      String_List.Free (Files);
   end On_Log_Action;

   ------------------
   -- On_Menu_Open --
   ------------------

   procedure On_Menu_Open
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      List : String_List.List;

      Ref  : VCS_Access;
   begin
      List := Get_Selected_Files (Context);

      if String_List.Is_Empty (List) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot open file",
            Mode => Error);
         return;
      end if;

      Ref := Get_Current_Ref (Context);

      Open (Ref, List);
      Get_Status (Ref, List);
      String_List.Free (List);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Open;

   -----------------
   -- On_Menu_Add --
   -----------------

   procedure On_Menu_Add
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      On_Log_Action (Context, Add);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Add;

   --------------------
   -- On_Menu_Revert --
   --------------------

   procedure On_Menu_Revert
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List;
      Ref   : VCS_Access;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot remove file",
            Mode => Error);
         return;
      end if;

      Ref := Get_Current_Ref (Context);

      Revert (Ref, Files);
      Get_Status (Ref, Files);

      String_List.Free (Files);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Revert;

   ----------------------
   -- On_Menu_Resolved --
   ----------------------

   procedure On_Menu_Resolved
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List;
      Ref   : VCS_Access;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context),
            -"VCS: No file selected, cannot mark file as resolved",
            Mode => Error);
         return;
      end if;

      Ref := Get_Current_Ref (Context);

      Resolved (Ref, Files);
      Get_Status (Ref, Files);

      String_List.Free (Files);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Resolved;

   --------------------
   -- On_Menu_Remove --
   --------------------

   procedure On_Menu_Remove
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      On_Log_Action (Context, Remove);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Remove;

   ----------------------
   -- On_Menu_Annotate --
   ----------------------

   procedure On_Menu_Annotate
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List;
      File  : Virtual_File;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot annotate",
            Mode => Error);
         return;
      end if;

      while not String_List.Is_Empty (Files) loop
         File := Create (Full_Filename => String_List.Head (Files));

         if not Is_Open (Get_Kernel (Context), File) then
            Open_File_Editor (Get_Kernel (Context), File);
         end if;

         Create_Line_Information_Column
           (Kernel        => Get_Kernel (Context),
            File          => File,
            Identifier    => Annotation_Id,
            Every_Line    => False);

         Annotate (Get_Current_Ref (Context), File);
         String_List.Next (Files);
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Annotate;

   -----------------------------
   -- On_Menu_Remove_Annotate --
   -----------------------------

   procedure On_Menu_Remove_Annotate
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      Files  : String_List.List;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Kernel, -"VCS: No file selected, cannot remove annotations",
            Mode => Error);
         return;
      end if;

      while not String_List.Is_Empty (Files) loop
         Remove_Line_Information_Column
           (Kernel,
            Create (Full_Filename => String_List.Head (Files)),
            Annotation_Id);
         String_List.Next (Files);
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Remove_Annotate;

   --------------------
   -- On_Menu_Update --
   --------------------

   procedure On_Menu_Update
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Files : String_List.List;
      Ref   : constant VCS_Access := Get_Current_Ref (Context);

   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot update file",
            Mode => Error);
         return;
      end if;

      Update (Ref, Files);
      Get_Status (Ref, Files);
      String_List.Free (Files);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Update;

   ------------------------
   -- On_Menu_Get_Status --
   ------------------------

   procedure On_Menu_Get_Status
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      Files    : String_List.List;

   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Kernel, -"VCS: No file selected, cannot get status",
            Mode => Error);
         return;
      end if;

      Open_Explorer (Kernel, Context);
      Get_Status (Get_Current_Ref (Context), Files);
      String_List.Free (Files);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Get_Status;

   ------------------
   -- Process_Dirs --
   ------------------

   procedure Process_Dirs
     (Context    : Selection_Context_Access;
      Recursive  : Boolean;
      Update     : Boolean;
      Get_Status : Boolean)
   is
      Files        : String_List.List;
      File_Context : File_Selection_Context_Access;
      Status       : File_Status_List.List;
      Ref          : constant VCS_Access := Get_Current_Ref (Context);
      Kernel       : constant Kernel_Handle := Get_Kernel (Context);

      procedure Add_Directory_Files (Dir : String);
      --  Fill the explorer with blank status for all files in Dir.

      procedure Add_Directory_Recursively;
      --  Add Dir and all subdirectories in Dir to Files, and make Node point
      --  to the next node.

      -------------------------
      -- Add_Directory_Files --
      -------------------------

      procedure Add_Directory_Files (Dir : String) is
         use String_List;

         F      : File_Array_Access := Read_Files_From_Dirs (Dir);
      begin
         for J in F'Range loop
            if not Is_Directory (F (J)) then
               File_Status_List.Append
                 (Status,
                  (F (J), VCS.Unknown,
                   Null_List, Null_List, Null_List, Null_List));
            end if;
         end loop;

         Unchecked_Free (F);
      end Add_Directory_Files;

      -------------------------------
      -- Add_Directory_Recursively --
      -------------------------------

      procedure Add_Directory_Recursively is
         use String_List_Utils.String_List;

         Node : String_List.List_Node;
         File : String (1 .. 1024);
         Last : Natural;
         D    : Dir_Type;
      begin
         Node := First (Files);

         while Node /= Null_Node loop
            begin
               Open (D, Data (Node));

               loop
                  begin
                     Read (D, File, Last);

                     if Last = 0 then
                        Close (D);
                        exit;
                     else
                        if File (1 .. Last) /= "."
                          and then File (1 .. Last) /= ".."
                          and then GNAT.OS_Lib.Is_Directory
                            (Data (Node) & File (1 .. Last))
                        then
                           Append
                             (Files,
                              Data (Node) & File (1 .. Last)
                              & GNAT.OS_Lib.Directory_Separator);
                           Add_Directory_Files
                             (Data (Node) & File (1 .. Last));
                        end if;
                     end if;

                  exception
                     when Directory_Error =>
                        Close (D);
                        exit;
                  end;
               end loop;

            exception
               when Directory_Error =>
                  null;
            end;

            Node := Next (Node);
         end loop;
      end Add_Directory_Recursively;

   begin
      Open_Explorer (Get_Kernel (Context), Context);

      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         if Has_Directory_Information (File_Context) then
            String_List.Append (Files, Directory_Information (File_Context));

            Add_Directory_Files (Directory_Information (File_Context));

            if Recursive then
               Add_Directory_Recursively;
            end if;

            Display_File_Status (Kernel, Status, Ref, False, True, False);
            File_Status_List.Free (Status);

            if Update then
               VCS.Update (Ref, Files);
            end if;

            if Get_Status then
               VCS.Get_Status_Dirs (Ref, Files);
            end if;

            String_List.Free (Files);
         end if;
      end if;
   end Process_Dirs;

   ------------------------
   -- On_Menu_Update_Dir --
   ------------------------

   procedure On_Menu_Update_Dir
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

   begin
      Process_Dirs
        (Context, Recursive => False, Update => True, Get_Status => True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Update_Dir;

   ----------------------------------
   -- On_Menu_Update_Dir_Recursive --
   ----------------------------------

   procedure On_Menu_Update_Dir_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      Process_Dirs
        (Context, Recursive => True, Update => True, Get_Status => True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Update_Dir_Recursive;

   ----------------------------
   -- On_Menu_Get_Status_Dir --
   ----------------------------

   procedure On_Menu_Get_Status_Dir
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      Process_Dirs
        (Context, Recursive => False, Update => False, Get_Status => True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Get_Status_Dir;

   --------------------------------------
   -- On_Menu_Get_Status_Dir_Recursive --
   --------------------------------------

   procedure On_Menu_Get_Status_Dir_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      Process_Dirs
        (Context, Recursive => True, Update => False, Get_Status => True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Get_Status_Dir_Recursive;

   --------------------
   -- Update_Project --
   --------------------

   procedure Update_Project
     (Context   : Selection_Context_Access;
      Recursive : Boolean)
   is
      Files        : String_List.List;
      File_Context : File_Selection_Context_Access;
      Ref          : constant VCS_Access := Get_Current_Ref (Context);

   begin
      Open_Explorer (Get_Kernel (Context), Context);

      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         if Has_Project_Information (File_Context) then
            Files := Get_Files_In_Project
              (Project_Information (File_Context),
               Recursive);
         else
            Files := Get_Files_In_Project
              (Get_Project (Get_Kernel (Context)), Recursive);
         end if;

         Update (Ref, Files);
         Get_Status (Ref, Files);

         String_List.Free (Files);
      end if;
   end Update_Project;

   ----------------------------
   -- On_Menu_Update_Project --
   ----------------------------

   procedure On_Menu_Update_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      Update_Project (Context, False);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Update_Project;

   --------------------------------------
   -- On_Menu_Update_Project_Recursive --
   --------------------------------------

   procedure On_Menu_Update_Project_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      Update_Project (Context, True);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Update_Project_Recursive;

   -------------------------
   -- Query_Project_Files --
   -------------------------

   procedure Query_Project_Files
     (Explorer   : VCS_View_Access;
      Kernel     : Kernel_Handle;
      Project    : Project_Type;
      Real_Query : Boolean;
      Recursive  : Boolean)
   is
      pragma Unreferenced (Explorer);

      procedure Query_Status_For_Project (The_Project : Project_Type);
      --  Display the status for The_Project only.

      ------------------------------
      -- Query_Status_For_Project --
      ------------------------------

      procedure Query_Status_For_Project (The_Project : Project_Type) is
         use String_List;
         Ref    : constant VCS_Access := Get_Current_Ref (The_Project);
         Status : File_Status_List.List;
         Files  : String_List.List;

      begin
         if Ref = Unknown_VCS_Reference then
            Insert
              (Kernel,
               -"Warning: no VCS set in project properties for project "
               & Project_Name (The_Project));
         else
            Files := Get_Files_In_Project (The_Project, False);

            if Real_Query then
               Get_Status (Ref, Files);
            else
               Status := Local_Get_Status (Ref, Files);
               Display_File_Status (Kernel, Status, Ref, False, True);
               File_Status_List.Free (Status);
            end if;

            String_List.Free (Files);
         end if;
      end Query_Status_For_Project;

      Iterator : Imported_Project_Iterator := Start (Project, Recursive);
      Current_Project : Project_Type := Current (Iterator);
   begin
      while Current_Project /= No_Project loop
         Query_Status_For_Project (Current_Project);
         Next (Iterator);
         Current_Project := Current (Iterator);
      end loop;
   end Query_Project_Files;

   ------------------------
   -- List_Project_Files --
   ------------------------

   procedure List_Project_Files
     (Context   : Selection_Context_Access;
      Recursive : Boolean)
   is
      Kernel       : constant Kernel_Handle := Get_Kernel (Context);
      File_Context : File_Selection_Context_Access;
   begin
      Open_Explorer (Get_Kernel (Context), Context);
      Clear (Get_Explorer (Get_Kernel (Context)));

      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         if Has_Project_Information (File_Context) then
            Query_Project_Files
              (Get_Explorer (Kernel),
               Kernel,
               Project_Information (File_Context),
               False, Recursive);
         else
            Query_Project_Files
              (Get_Explorer (Kernel),
               Kernel,
               Get_Project (Kernel),
               False, Recursive);
         end if;
      end if;
   end List_Project_Files;

   --------------------------------
   -- On_Menu_List_Project_Files --
   --------------------------------

   procedure On_Menu_List_Project_Files
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      List_Project_Files (Context, False);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_List_Project_Files;

   ------------------------------------------
   -- On_Menu_List_Project_Files_Recursive --
   ------------------------------------------

   procedure On_Menu_List_Project_Files_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      List_Project_Files (Context, True);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_List_Project_Files_Recursive;

   ------------------------
   -- Get_Status_Project --
   ------------------------

   procedure Get_Status_Project
     (Context   : Selection_Context_Access;
      Recursive : Boolean)
   is
      Kernel       : constant Kernel_Handle := Get_Kernel (Context);
      File_Context : File_Selection_Context_Access;
   begin
      Open_Explorer (Kernel, Context);
      Clear (Get_Explorer (Kernel));

      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         if Has_Project_Information (File_Context) then
            Query_Project_Files
              (Get_Explorer (Kernel),
               Kernel,
               Project_Information (File_Context),
               True, Recursive);
         else
            Query_Project_Files
              (Get_Explorer (Kernel),
               Kernel,
               Get_Project (Kernel),
               True, Recursive);
         end if;
      end if;
   end Get_Status_Project;

   --------------------------------
   -- On_Menu_Get_Status_Project --
   --------------------------------

   procedure On_Menu_Get_Status_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      Get_Status_Project (Context, False);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Get_Status_Project;

   ------------------------------------------
   -- On_Menu_Get_Status_Project_Recursive --
   ------------------------------------------

   procedure On_Menu_Get_Status_Project_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      Get_Status_Project (Context, True);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Get_Status_Project_Recursive;

   ------------------
   -- On_Menu_Diff --
   ------------------

   procedure On_Menu_Diff
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Files : String_List.List;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot diff",
            Mode => Error);
         return;
      end if;

      if not Save_Files (Get_Kernel (Context), Files) then
         return;
      end if;

      while not String_List.Is_Empty (Files) loop
         Diff (Get_Current_Ref (Context),
               Create (Full_Filename => String_List.Head (Files)));
         String_List.Next (Files);
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Diff;

   --------------------------
   -- On_Menu_Diff_Working --
   --------------------------

   procedure On_Menu_Diff_Working
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Files    : String_List.List;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot diff",
            Mode => Error);
         return;
      end if;

      if not Save_Files (Get_Kernel (Context), Files) then
         return;
      end if;

      while not String_List.Is_Empty (Files) loop
         Diff_Working (Get_Current_Ref (Context),
                       Create (Full_Filename => String_List.Head (Files)));
         String_List.Next (Files);
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Diff_Working;

   ----------------------------
   -- On_Menu_Diff_Base_Head --
   ----------------------------

   procedure On_Menu_Diff_Base_Head
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Files    : String_List.List;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot diff",
            Mode => Error);
         return;
      end if;

      while not String_List.Is_Empty (Files) loop
         Diff_Base_Head
           (Get_Current_Ref (Context),
            Create (Full_Filename => String_List.Head (Files)));
         String_List.Next (Files);
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Diff_Base_Head;

   ----------------------
   -- On_Menu_View_Log --
   ----------------------

   procedure On_Menu_View_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Files    : String_List.List;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot view log",
            Mode => Error);
         return;
      end if;

      while not String_List.Is_Empty (Files) loop
         Log
           (Get_Current_Ref (Context),
            Create (Full_Filename => String_List.Head (Files)),
            "");
         String_List.Next (Files);
      end loop;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_View_Log;

   --------------------------
   -- On_Menu_View_Log_Rev --
   --------------------------

   procedure On_Menu_View_Log_Rev
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Ref      : constant VCS_Access := Get_Current_Ref (Context);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context);
      Explorer : constant VCS_View_Access := Get_Explorer (Kernel, False);
      Files    : String_List.List;
      Revision : String_Access;
      Status   : File_Status_Record;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot view log",
            Mode => Error);
         return;
      end if;

      Status := Get_Cached_Status
        (Explorer, Create (String_List.Head (Files)), Ref);

      if String_List.Is_Empty (Status.Working_Revision) then
         Revision := new String'("");
      else
         Revision := new String'
           (Protect (String_List.Head (Status.Working_Revision)));
      end if;

      declare
         Str : constant String :=
           Execute_GPS_Shell_Command
            (Kernel,
             "MDI.input_dialog"
             & " ""Query history for revision:"""
             & " ""Revision=" & Revision.all & """");
      begin
         if Str /= "" then
            Log
              (Get_Current_Ref (Context),
               Create (Full_Filename => String_List.Head (Files)),
               Str);
         end if;
      end;

      Free (Revision);
      String_List.Free (Files);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_View_Log_Rev;

   ---------------------------
   -- On_Menu_Diff_Specific --
   ---------------------------

   procedure On_Menu_Diff_Specific
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      Comparison (Context, True);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Diff_Specific;

   -------------------
   -- On_Menu_Diff2 --
   -------------------

   procedure On_Menu_Diff2
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
   begin
      Comparison (Context, False);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Diff2;

   ----------------
   -- Comparison --
   ----------------

   procedure Comparison
     (Context : Selection_Context_Access;
      One_Rev : Boolean)
   is
      use String_List;

      Ref        : constant VCS_Access := Get_Current_Ref (Context);
      Kernel     : constant Kernel_Handle := Get_Kernel (Context);
      Explorer   : constant VCS_View_Access := Get_Explorer (Kernel, False);
      Files      : String_List.List;
      Status     : File_Status_Record;
      Revision_1 : String_Access;
      Revision_2 : String_Access;
      Str        : String_Access;
      Index      : Natural;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot diff",
            Mode => Error);
         return;
      end if;

      if not Save_Files (Kernel, Files) then
         return;
      end if;

      Status := Get_Cached_Status (Explorer, Create (Head (Files)), Ref);

      if not One_Rev then
         if Is_Empty (Status.Repository_Revision) then
            Revision_2 := new String'("");
         else
            Revision_2 := new String'
              (Protect (Head (Status.Repository_Revision)));
         end if;
      end if;

      if Is_Empty (Status.Working_Revision) then
         Revision_1 := new String'("");
      else
         Revision_1 := new String'
           (Protect (Head (Status.Working_Revision)));
      end if;

      if One_Rev then
         Str := new String'
           (Execute_GPS_Shell_Command
              (Kernel,
               "MDI.input_dialog"
               & " ""Compare against revision:"""
               & " ""Revision=" & Revision_1.all & """"));
      else
         Str := new String'
           (Execute_GPS_Shell_Command
              (Kernel,
               "MDI.input_dialog"
               & " ""Compare between two revisions:"""
               & " ""Revision 1=" & Revision_1.all & """"
               & " ""Revision 2=" & Revision_2.all & """"));
      end if;

      if One_Rev then
         if Str'Length /= 0 then
            Diff
              (Ref,
               Create (Full_Filename => Head (Files)),
               Str.all,
               "");
         end if;
      else
         if Str'Length /= 0 then
            Index := Str'First;
            Skip_To_Char (Str.all, Index, ASCII.LF);
            Diff
              (Ref,
               Create (Full_Filename => Head (Files)),
               Str (Str'First .. Index - 1),
               Str (Index + 1 .. Str'Last));
         end if;

         Free (Revision_2);
      end if;

      Free (Revision_1);
      Free (Str);

      String_List.Free (Files);
   end Comparison;

   --------------------------
   -- Get_Files_In_Project --
   --------------------------

   function Get_Files_In_Project
     (Project   : Project_Type;
      Recursive : Boolean := True) return String_List.List
   is
      Result : String_List.List;
      Files  : File_Array_Access;
   begin
      Files := Get_Source_Files (Project, Recursive);

      for J in reverse Files.all'Range loop
         String_List.Prepend (Result, Full_Name (Files (J)).all);
      end loop;

      Unchecked_Free (Files);

      return Result;
   end Get_Files_In_Project;

   -------------------------
   -- Get_Dirs_In_Project --
   -------------------------

   function Get_Dirs_In_Project
     (Project   : Project_Type;
      Recursive : Boolean := False) return String_List.List
   is
      Result : String_List.List;
   begin
      declare
         A : constant String_Array_Access
           := Source_Dirs (Project, Recursive);
      begin
         for J in A'Range loop
            String_List.Append (Result, A (J).all);
         end loop;
      end;

      return Result;
   end Get_Dirs_In_Project;

   ------------------------------
   -- Query_Status_For_Project --
   ------------------------------

   procedure Query_Status_For_Project
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Explorer : VCS_View_Access;
   begin
      Open_Explorer (Kernel, null);
      Explorer := Get_Explorer (Kernel);
      Clear (Explorer);
      Query_Project_Files (Explorer, Kernel, Get_Project (Kernel), True, True);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Query_Status_For_Project;

   ----------------
   -- Update_All --
   ----------------

   procedure Update_All
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Dirs : constant String_List.List :=
        Get_Dirs_In_Project (Get_Project (Kernel), True);
      Ref  : constant VCS_Access := Get_Current_Ref (Kernel);
   begin
      Update (Ref, Dirs);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Update_All;

   ---------------------
   -- Context_Factory --
   ---------------------

   function Context_Factory
     (Kernel : access Kernel_Handle_Record'Class;
      Child  : Gtk.Widget.Gtk_Widget) return Selection_Context_Access
   is
      pragma Unreferenced (Child);
      Explorer : VCS_View_Access;
   begin
      Explorer := Get_Explorer (Kernel_Handle (Kernel), False);

      if Explorer /= null then
         return Get_Current_Context (Explorer);
      else
         return null;
      end if;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return null;
   end Context_Factory;

   ---------------------------
   -- Display_Editor_Status --
   ---------------------------

   procedure Display_Editor_Status
     (Kernel : access Kernel_Handle_Record'Class;
      Ref    : VCS_Access;
      Status : File_Status_Record)
   is
      Status_Label   : String_Access;
      Revision_Label : String_Access;

      function Short_Revision (R : String) return String;
      --  If R is too long, return only the last digits.

      --------------------
      -- Short_Revision --
      --------------------

      function Short_Revision (R : String) return String is
      begin
         if R'Length <= Max_Rev_Length then
            return R;

         else
            return "[...]" & R (R'Last - Max_Rev_Length .. R'Last);
         end if;
      end Short_Revision;

      use String_List_Utils.String_List;
   begin
      if Ref = null then
         return;
      end if;

      if Status.Status = VCS.Unknown
        or else Status.Status.Label = null
      then
         Status_Label := new String'("");
      else
         Status_Label := new String'
           (" (" & Status.Status.Label.all & ")");
      end if;

      if not Is_Empty (Status.Working_Revision) then
         Revision_Label := new String'
           (Name (Ref) & ":"
            & Short_Revision (Head (Status.Working_Revision)));
      else
         Revision_Label := new String'(Name (Ref));
      end if;

      Add_Editor_Label
        (Kernel,
         Status.File,
         VCS_Module_Name,
         Revision_Label.all & Status_Label.all);

      Free (Status_Label);
      Free (Revision_Label);
   end Display_Editor_Status;

   -------------------------
   -- VCS_Command_Handler --
   -------------------------

   procedure VCS_Command_Handler
     (Data    : in out GPS.Kernel.Scripts.Callback_Data'Class;
      Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      File   : constant String := Nth_Arg (Data, 1);
      Full   : Virtual_File;
      Prj    : Project_Type;
      Ref    : VCS_Access;
      Files  : String_List.List;

   begin
      if File = "" then
         Insert (Kernel, -"Command "
                 & Command
                 & " must have at least one parameter.",
                 Mode => Error);
      end if;

      Full := Create (File, Kernel, Use_Object_Path => False);

      if Dir_Name (Full).all = "" then
         Insert (Kernel, -"Could not find file: " & File, Mode => Error);
         return;
      end if;

      --  At this point Full must not be null.

      Prj := Get_Project_From_File (Get_Registry (Kernel).all, Full, True);

      if Prj = No_Project then
         Insert
           (Kernel,
              -"Could not find project for file: " & File,
            Mode => Error);

         return;
      end if;

      Ref := Get_Current_Ref (Prj);

      if Ref = null then
         Insert
           (Kernel,
              -"Could not find VCS for project: " & Project_Name (Prj),
            Mode => Error);
         return;
      end if;

      if Ref = Unknown_VCS_Reference then
         Insert
           (Kernel,
              -"There is no VCS associated to project: " & Project_Name (Prj),
            Mode => Error);
         return;
      end if;

      String_List.Append (Files, Full_Name (Full).all);

      --  Process the command.

      if Command = "get_status" then
         Open_Explorer (Kernel, null);
         Get_Status (Ref, Files);

      elsif Command = "update" then
         Update (Ref, Files);
         Get_Status (Ref, Files);

      elsif Command = "commit" then
         Log_Action_Files (Kernel, Ref, Commit, Files);

      elsif Command = "diff_head" then
         if Save_Files (Kernel, Files) then
            Diff (Ref, Full);
         end if;

      elsif Command = "log" then
         if Number_Of_Arguments (Data) = 2 then
            Log (Ref, Full, Nth_Arg (Data, 2));
         else
            Log (Ref, Full, "");
         end if;

      elsif Command = "diff_working" then
         declare
            Status : File_Status_List.List;
         begin
            if not Save_Files (Kernel, Files) then
               return;
            end if;

            Status := Local_Get_Status (Ref, Files);

            Diff
              (Ref,
               File_Status_List.Head (Status).File,
               "",
               String_List.Head (File_Status_List.Head
                                   (Status).Working_Revision));
            File_Status_List.Free (Status);
         end;

      elsif Command = "annotate" then
         Annotate (Ref, Full);

      elsif Command = "remove_annotations" then
         Remove_Line_Information_Column (Kernel, Full, Annotation_Id);

      end if;

      String_List.Free (Files);
   end VCS_Command_Handler;

end VCS_View_API;
