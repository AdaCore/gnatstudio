-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Gtk.Widget;                use Gtk.Widget;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Enums;

with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Gtkada.MDI;                use Gtkada.MDI;

with VCS;                       use VCS;
with VCS.Unknown_VCS;           use VCS.Unknown_VCS;
with VCS_View_Pkg;              use VCS_View_Pkg;

with Glib.Convert;              use Glib.Convert;
with Glide_Intl;                use Glide_Intl;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Project;      use Glide_Kernel.Project;

with String_List_Utils;         use String_List_Utils;

with VCS_Module;                use VCS_Module;
with Log_Utils;                 use Log_Utils;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;
with OS_Utils;                  use OS_Utils;

with Basic_Types;               use Basic_Types;

with Projects.Registry;         use Projects, Projects.Registry;

with Commands;                  use Commands;
with Commands.VCS;              use Commands.VCS;
with Commands.External;         use Commands.External;

with Traces;                    use Traces;
with Ada.Exceptions;            use Ada.Exceptions;

package body VCS_View_API is

   Me : constant Debug_Handle := Create ("VCS_Api");

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

   procedure Commit_Files
     (Kernel : Kernel_Handle;
      Ref    : VCS_Access;
      Files  : String_List.List);
   --  Commit the list of files, assuming that they are all checked-in through
   --  the VCS system identified by Ref.
   --  This subprogram will do all the necessary file/log checks before
   --  committing.

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

   procedure Save_Files
     (Kernel : Kernel_Handle;
      Files  : String_List.List);
   --  Ask the user whether he wants to save the file editors for Files.

   -------------------------------
   -- Contextual menu callbacks --
   -------------------------------

   procedure On_Menu_Open
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Add
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Remove
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Annotate
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Remove_Annotate
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Diff
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_View_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Diff_Local
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Diff_Working_Head
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Update
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Get_Status
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Remove_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Get_Status_Dir
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Update_Dir
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Get_Status_Dir_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Update_Dir_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_List_Project_Files
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_List_Project_Files_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Get_Status_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Get_Status_Project_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Update_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Update_Project_Recursive
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   ---------------------
   -- Get_Current_Ref --
   ---------------------

   function Get_Current_Ref (Project : Project_Type) return VCS_Access is
   begin
      --  ??? maybe we could cache this information.
      return Get_VCS_From_Id
        (Get_Attribute_Value (Project, Vcs_Kind_Attribute, Ide_Package));
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

   ----------
   -- Open --
   ----------

   procedure Open
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : constant VCS_Access := Get_Current_Ref (Kernel);

   begin
      Open (Ref, Files);

      declare
         use String_List;

         L_Temp : List_Node := First (Files);
      begin
         while L_Temp /= Null_Node loop
            Open_File_Editor (Kernel, Data (L_Temp), From_Path => False);
            L_Temp := Next (L_Temp);
         end loop;
      end;

      String_List.Free (Files);
   end Open;

   ------------
   -- Update --
   ------------

   procedure Update
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle) is
   begin
      On_Menu_Update (Widget, Get_Current_Context (Kernel));
   end Update;

   ----------------
   -- Get_Status --
   ----------------

   procedure Get_Status
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
   begin
      if Context = null then
         Console.Insert
           (Kernel, -"VCS: No file selected, cannot get status",
            Mode => Error);
      else
         On_Menu_Get_Status (Widget, Context);
      end if;
   end Get_Status;

   ---------
   -- Add --
   ---------

   procedure Add
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
   begin
      if Context = null then
         Console.Insert
           (Kernel, -"VCS: No file selected, cannot add file",
            Mode => Error);
      else
         On_Menu_Add (Widget, Context);
      end if;
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
   begin
      if Context = null then
         Console.Insert
           (Kernel, -"VCS: No file selected, cannot remove file",
            Mode => Error);
      else
         On_Menu_Remove (Widget, Context);
      end if;
   end Remove;

   ------------
   -- Revert --
   ------------

   procedure Revert
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget, Kernel);
   begin
      --  ??? Not implemented yet.
      --  On_Menu_Revert (Widget, Get_Current_Context (Kernel));
      null;
   end Revert;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
   begin
      --  ??? Is this the behaviour we want ?

      if Context = null then
         Console.Insert
           (Kernel, -"VCS: No file selected, cannot commit", Mode => Error);
      else
         On_Menu_Commit (Widget, Context);
      end if;
   end Commit;

   --------------------
   -- View_Head_Diff --
   --------------------

   procedure View_Head_Diff
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
   begin
      if Context = null then
         Console.Insert
           (Kernel, -"VCS: No file selected, cannot diff",
            Mode => Error);
      else
         On_Menu_Diff (Widget, Context);
      end if;
   end View_Head_Diff;

   --------------------
   -- View_Work_Diff --
   --------------------

   procedure View_Work_Diff
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
   begin
      if Context = null then
         Console.Insert
           (Kernel, -"VCS: No file selected, cannot diff", Mode => Error);
      else
         On_Menu_Diff_Local (Widget, Context);
      end if;
   end View_Work_Diff;

   -------------------------
   -- View_Work_Head_Diff --
   -------------------------

   procedure View_Work_Head_Diff
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
   begin
      if Context = null then
         Console.Insert
           (Kernel, -"VCS: No file selected, cannot diff", Mode => Error);
      else
         On_Menu_Diff_Working_Head (Widget, Context);
      end if;
   end View_Work_Head_Diff;

   --------------
   -- View_Log --
   --------------

   procedure View_Log
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
   begin
      if Context = null then
         Console.Insert
           (Kernel, -"VCS: No file selected, cannot display log",
            Mode => Error);
      else
         On_Menu_View_Log (Widget, Context);
      end if;
   end View_Log;

   -------------------
   -- View_Annotate --
   -------------------

   procedure View_Annotate
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
   begin
      if Context = null then
         Console.Insert
           (Kernel, -"VCS: No file selected, cannot annotate", Mode => Error);
      else
         On_Menu_Annotate (Widget, Context);
      end if;
   end View_Annotate;

   ------------------------
   -- Remove_Annotations --
   ------------------------

   procedure Remove_Annotations
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
   begin
      if Context = null then
         Console.Insert
           (Kernel, -"VCS: No file selected, cannot remove annotations",
            Mode => Error);
      else
         On_Menu_Remove_Annotate (Widget, Context);
      end if;
   end Remove_Annotations;

   --------------
   -- Edit_Log --
   --------------

   procedure Edit_Log
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
   begin
      if Context = null then
         Console.Insert
           (Kernel, -"VCS: No file selected, cannot edit log", Mode => Error);
      else
         On_Menu_Edit_Log (Widget, Context);
      end if;
   end Edit_Log;

   -------------------------
   -- VCS_Contextual_Menu --
   -------------------------

   procedure VCS_Contextual_Menu
     (Object  : access Glib.Object.GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);
      Item      : Gtk_Menu_Item;
      Menu_Item : Gtk_Menu_Item;
      Submenu   : Gtk_Menu;
      File_Name : File_Selection_Context_Access;
      Kernel    : constant Kernel_Handle := Get_Kernel (Context);
   begin
      if Context.all in File_Selection_Context'Class then
         File_Name := File_Selection_Context_Access (Context);
      end if;

      if File_Name /= null
        and then Has_File_Information (File_Name)
      then
         declare
            File_S : constant String := Directory_Information (File_Name)
              & File_Information (File_Name);
         begin
            if File_S'Length > 4
              and then File_S (File_S'Last - 3 .. File_S'Last) = "$log"
            then
               declare
                  Original : constant String :=
                    Get_File_From_Log (Kernel, File_S);
               begin
                  if Original /= "" then
                     Set_File_Information
                       (File_Name,
                        Dir_Name (Original),
                        Base_Name (Original),
                        Get_Project_From_File
                          (Get_Registry (Kernel), Base_Name (Original)));

                     Gtk_New (Item, Label => -"Commit file "
                              & Locale_To_UTF8 (Base_Name (Original)));

                     Append (Menu, Item);
                     Context_Callback.Connect
                       (Item, "activate",
                        Context_Callback.To_Marshaller
                          (On_Menu_Commit'Access),
                        Selection_Context_Access (File_Name));
                  end if;
               end;
            else
               Gtk_New (Item, Label => -"Query status");
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                    (On_Menu_Get_Status'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item, Label => -"Update");
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                    (On_Menu_Update'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item, Label => -"Start editing");
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                    (On_Menu_Open'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item, Label => -"View revision history");
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                    (On_Menu_View_Log'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Menu_Item, Label => -"Compare");
               Append (Menu, Menu_Item);
               Gtk_New (Submenu);
               Set_Submenu (Menu_Item, Gtk_Widget (Submenu));

               Gtk_New (Item, Label => -"Against head revision");
               Append (Submenu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                    (On_Menu_Diff'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item, Label => -"Against working revision");
               Append (Submenu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                    (On_Menu_Diff_Local'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item, Label =>
                        -"Working revision against head revision");
               Append (Submenu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                    (On_Menu_Diff_Working_Head'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item, Label => -"Annotate");
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                    (On_Menu_Annotate'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item, Label => -"Remove annotations");
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                    (On_Menu_Remove_Annotate'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item, Label => -"Edit revision log");
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                    (On_Menu_Edit_Log'Access),
                  Selection_Context_Access (Context));

               if Get_Log_From_File (Kernel, File_S, False) = "" then
                  Gtk_New (Item, Label => -"Commit via revision log");
               else
                  Gtk_New (Item, Label => -"Commit");
               end if;

               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                    (On_Menu_Commit'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item);
               Append (Menu, Item);

               Gtk_New (Item, Label => -"Remove revision log");
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                    (On_Menu_Remove_Log'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item);
               Append (Menu, Item);

               Gtk_New (Item, Label => -"Add to repository");
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                    (On_Menu_Add'Access),
                  Selection_Context_Access (Context));

               Gtk_New (Item, Label => -"Remove from repository");
               Append (Menu, Item);
               Context_Callback.Connect
                 (Item, "activate",
                  Context_Callback.To_Marshaller
                    (On_Menu_Remove'Access),
                  Selection_Context_Access (Context));
            end if;
         end;
      end if;

      if File_Name /= null
        and then Has_File_Information (File_Name)
        and then (Has_Directory_Information (File_Name)
                  or else Has_Project_Information (File_Name))
      then
         Gtk_New (Item);
         Append (Menu, Item);
      end if;

      if File_Name /= null
        and then Has_Directory_Information (File_Name)
      then
         if Has_Project_Information (File_Name)
           or else Has_File_Information (File_Name)
         then
            Gtk_New (Menu_Item, Label => -"Directory");
            Append (Menu, Menu_Item);
            Gtk_New (Submenu);
            Set_Submenu (Menu_Item, Gtk_Widget (Submenu));
         else
            Submenu := Gtk_Menu (Menu);
         end if;

         Gtk_New (Item, Label => -"Query status for directory");
         Append (Submenu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
              (On_Menu_Get_Status_Dir'Access),
            Selection_Context_Access (File_Name));

         Gtk_New (Item, Label => -"Query status for directory recursively");
         Append (Submenu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_Get_Status_Dir_Recursive'Access),
            Selection_Context_Access (File_Name));

         Gtk_New (Item, Label => -"Update directory");
         Append (Submenu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_Update_Dir'Access),
            Selection_Context_Access (File_Name));

         Gtk_New (Item, Label => -"Update directory recursively");
         Append (Submenu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_Update_Dir_Recursive'Access),
            Selection_Context_Access (File_Name));
      end if;

      if File_Name /= null
        and then Has_Project_Information (File_Name)
        and then Has_Directory_Information (File_Name)
      then
         Gtk_New (Item);
         Append (Menu, Item);
      end if;

      if File_Name /= null
        and then Has_Project_Information (File_Name)
      then
         if Has_Directory_Information (File_Name)
           or else Has_File_Information (File_Name)
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
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_List_Project_Files'Access),
            Selection_Context_Access (File_Name));

         Gtk_New (Item, Label => -"Query status for project");
         Append (Submenu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_Get_Status_Project'Access),
            Selection_Context_Access (File_Name));

         Gtk_New (Item, Label => -"Update project");
         Append (Submenu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_Update_Project'Access),
            Selection_Context_Access (File_Name));

         Gtk_New (Item, Label => -"List all files in project and subprojects");
         Append (Submenu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_List_Project_Files_Recursive'Access),
            Selection_Context_Access (File_Name));

         Gtk_New (Item, Label => -"Query status for project and subprojects");
         Append (Submenu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_Get_Status_Project_Recursive'Access),
            Selection_Context_Access (File_Name));

         Gtk_New (Item, Label => -"Update project and subprojects");
         Append (Submenu, Item);
         Context_Callback.Connect
           (Item, "activate",
            Context_Callback.To_Marshaller
            (On_Menu_Update_Project_Recursive'Access),
            Selection_Context_Access (File_Name));
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
      MDI      : constant MDI_Window := Get_MDI (Kernel);
      Explorer : VCS_View_Access := Get_Explorer (Kernel);
      Child    : MDI_Child;
      Dirs     : String_List.List;

   begin
      if Explorer = null then
         --  Must get the current directory (that depends on what module
         --  currently has the focus) before we insert a new child in the MDI.

         String_List.Append (Dirs, Get_Current_Dir (Kernel));

         Gtk_New (Explorer, Kernel);
         Set_Size_Request (Explorer, 400, 400);
         Child := Put (MDI, Explorer);
         Set_Focus_Child (Child);
         Set_Title (Child, -"VCS Explorer");
         Change_Context (Explorer, Context);
      end if;
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

         if Get_Creator (Context) = VCS_Module_ID then
            Explorer := Get_Explorer (Kernel, False);
            List := Get_Selected_Files (Explorer);
         else
            if Has_File_Information (File) then
               String_List.Append
                 (List,
                  Directory_Information (File) & File_Information (File));
            end if;
         end if;
      end if;

      return List;
   end Get_Selected_Files;

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
         Open_File_Editor
           (Kernel, Get_Log_From_File (Kernel, String_List.Head (List), True),
            From_Path => False);
         String_List.Next (List);
      end loop;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Edit_Log;

   ----------------
   -- Save_Files --
   ----------------

   procedure Save_Files
     (Kernel : Kernel_Handle;
      Files  : String_List.List)
   is
      use String_List;
      Child      : MDI_Child;
      Success    : Boolean;
      pragma Unreferenced (Success);
      Files_Temp : List_Node := First (Files);
   begin
      while Files_Temp /= Null_Node loop
         Child := Get_File_Editor (Kernel, Head (Files));

         if Child /= null then
            Success := Save_Child (Kernel, Child, False) /= Cancel;
         end if;

         Files_Temp := Next (Files_Temp);
      end loop;
   end Save_Files;

   ------------------
   -- Commit_Files --
   ------------------

   procedure Commit_Files
     (Kernel : Kernel_Handle;
      Ref    : VCS_Access;
      Files  : String_List.List)
   is
      use String_List;

      Logs               : String_List.List;
      Files_Temp         : List_Node := First (Files);

      Commit_Command     : Commit_Command_Access;
      Get_Status_Command : Get_Status_Command_Access;

      Project            : Project_Type;

      Child              : MDI_Child;
      Success            : Boolean;
      pragma Unreferenced (Success);

      Cancel_All         : Boolean := False;

      Log_Checks         : External_Command_Access;
      File_Checks        : External_Command_Access;

      First_Check, Last_Check : Command_Access := null;

   begin
      Save_Files (Kernel, Files);

      while Files_Temp /= Null_Node loop
         --  Save any open log editors, and then get the corresponding logs.

         Child := Get_File_Editor
           (Kernel,
            Get_Log_From_File (Kernel, Head (Files), False));

         if Child /= null then
            Success := Save_Child (Kernel, Child, True) /= Cancel;
         end if;

         Append (Logs, Get_Log (Kernel, Data (Files_Temp)));
         Files_Temp := Next (Files_Temp);
      end loop;

      --  Create the Commit command.
      Create (Commit_Command, Ref, Files, Logs);

      --  Create the Get_Status command.
      Create (Get_Status_Command, Ref, Files);

      --  The Get_Status command is a consequence of the Commit command.
      Add_Consequence_Action
        (Command_Access (Commit_Command),
         Command_Access (Get_Status_Command));

      --  Create the file checks and the log checks.
      Files_Temp := First (Files);

      while Files_Temp /= Null_Node loop
         Project := Get_Project_From_File
           (Get_Registry (Kernel), Data (Files_Temp));

         if Project /= No_Project then
            declare
               File_Check_Script : constant String := Get_Attribute_Value
                 (Project, Vcs_File_Check, Ide_Package);
               Log_Check_Script  : constant String := Get_Attribute_Value
                 (Project, Vcs_Log_Check, Ide_Package);
               Log_File  : constant String :=
                 Get_Log_From_File (Kernel, Data (Files_Temp), True);
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
                          Check_Handler'Access);

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
                                (-"File could not be read: ") & Log_File);

                     Free (File_Args);
                     Free (Log_Args);
                     Free (Head_List);
                     exit;

                  elsif S.all = "" then
                     if Message_Dialog
                       ((-"File: ") & Data (Files_Temp)
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

                  Append (Log_Args, Log_File);
                  Append
                    (Head_List, -"File: " & Data (Files_Temp) & ASCII.LF
                     & (-"The revision log does not pass the checks."));

                  Create
                    (Log_Checks,
                     Kernel,
                     Log_Check_Script,
                     "",
                     Log_Args,
                     Head_List,
                     Check_Handler'Access);

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
         Enqueue (Get_Queue (Ref), First_Check);
      end if;

      Free (Logs);
   end Commit_Files;

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

      if Get_Creator (Context) = VCS_Module_ID then
         Explorer := Get_Explorer (Kernel, False);
         return Get_Current_Ref (Explorer);

      elsif Context.all in File_Selection_Context'Class then
         File := File_Selection_Context_Access (Context);

         if Has_Project_Information (File) then
            return Get_Current_Ref (Project_Information (File));
         else
            return Get_Current_Ref (Get_Project (Kernel));
         end if;
      end if;

      return Get_VCS_From_Id ("");
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
            File  : constant String := Data (Files_Temp);
            Log   : constant String
              := Get_Log_From_File (Kernel, File, False);
            Dummy : Boolean;
         begin
            if Log /= ""
              and then GNAT.OS_Lib.Is_Regular_File (Log)
            then
               GNAT.OS_Lib.Delete_File (Log, Dummy);
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Remove_Log;

   --------------------
   -- On_Menu_Commit --
   --------------------

   procedure On_Menu_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      use String_List;

      Kernel         : constant Kernel_Handle := Get_Kernel (Context);
      Files          : String_List.List;
      Real_Files     : String_List.List;
      Files_Temp     : String_List.List_Node;
      All_Logs_Exist : Boolean := True;

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
                  L : constant String := Get_File_From_Log (Kernel, S);
               begin
                  if L /= "" then
                     Append (Files, L);
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
         if Get_Log_From_File
           (Kernel, String_List.Data (Files_Temp), False) = ""
         then
            All_Logs_Exist := False;
            Open_File_Editor
              (Kernel,
               Get_Log_From_File
                 (Kernel, String_List.Data (Files_Temp), True),
               From_Path => False);
         end if;

         Files_Temp := String_List.Next (Files_Temp);
      end loop;

      --  If All files have a log, commit the whole lot.

      if All_Logs_Exist then
         Commit_Files (Kernel, Get_Current_Ref (Context), Files);
      end if;

      String_List.Free (Files);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Commit;

   ------------------
   -- On_Menu_Open --
   ------------------

   procedure On_Menu_Open
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      List : String_List.List;
   begin
      List := Get_Selected_Files (Context);

      if String_List.Is_Empty (List) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot open file",
            Mode => Error);
         return;
      end if;

      Open (Get_Current_Ref (Context), List);

      --  ??? The following should be the responsibility of
      --  the VCS modules themselves, due to the possibility
      --  of having asynchronous commands, permission problems, etc.

      --        declare
      --           use String_List;

      --           L_Temp : List_Node := First (List);
      --        begin
      --           while L_Temp /= Null_Node loop
      --              Open_File_Editor (Kernel, Data (L_Temp));
      --              L_Temp := Next (L_Temp);
      --           end loop;
      --        end;

      String_List.Free (List);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Open;

   -----------------
   -- On_Menu_Add --
   -----------------

   procedure On_Menu_Add
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Files    : String_List.List;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot add file",
            Mode => Error);
         return;
      end if;

      Add (Get_Current_Ref (Context), Files);
      Get_Status (Get_Current_Ref (Context), Files, True);

      String_List.Free (Files);
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Add;

   --------------------
   -- On_Menu_Remove --
   --------------------

   procedure On_Menu_Remove
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List;
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot remove file",
            Mode => Error);
         return;
      end if;

      Remove (Get_Current_Ref (Context), Files);
      Get_Status (Get_Current_Ref (Context), Files);

      String_List.Free (Files);
   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot annotate",
            Mode => Error);
         return;
      end if;

      while not String_List.Is_Empty (Files) loop
         Annotate (Get_Current_Ref (Context), String_List.Head (Files));
         String_List.Next (Files);
      end loop;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
            String_List.Head (Files),
            Annotation_Id);
         String_List.Next (Files);
      end loop;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
      Ref          : constant VCS_Access := Get_Current_Ref (Context);

      procedure Add_Directory_Recursively;
      --  Add Dir and all subdirectories in Dir to Files, and make Node point
      --  to the next node.

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
                           Append (Files,
                                   Data (Node) & File (1 .. Last)
                                   & GNAT.OS_Lib.Directory_Separator);
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

            if Recursive then
               Add_Directory_Recursively;
            end if;

            if Update then
               VCS.Update (Ref, Files);
            end if;

            if Get_Status then
               VCS.Get_Status (Ref, Files);
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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

      procedure Query_Status_For_Project (The_Project : Project_Type) is
         use String_List;
         Status         : File_Status_List.List;
         Dirs           : String_List.List;
         Ref            : constant VCS_Access := Get_Current_Ref (The_Project);

      begin
         if Ref = Unknown_VCS_Reference then
            Insert
              (Kernel,
               -"Warning: no VCS set in project properties for project "
               & Project_Name (The_Project));
         else
            Dirs := Get_Dirs_In_Project (The_Project);

            if Real_Query then
               Get_Status (Ref, Dirs);
            else
               Status := Local_Get_Status (Ref, Dirs);
               Display_File_Status (Kernel, Status, Ref, False, True);
               File_Status_List.Free (Status);
            end if;

            String_List.Free (Dirs);
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
      File_Context : File_Selection_Context_Access;
      Kernel       : constant Kernel_Handle := Get_Kernel (Context);
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Menu_List_Project_Files_Recursive;

   ------------------------
   -- Get_Status_Project --
   ------------------------

   procedure Get_Status_Project
     (Context   : Selection_Context_Access;
      Recursive : Boolean)
   is
      File_Context : File_Selection_Context_Access;
      Kernel       : constant Kernel_Handle := Get_Kernel (Context);
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Get_Status_Project_Recursive;

   ------------------
   -- On_Menu_Diff --
   ------------------

   procedure On_Menu_Diff
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

      Save_Files (Get_Kernel (Context), Files);

      while not String_List.Is_Empty (Files) loop
         Diff (Get_Current_Ref (Context), String_List.Head (Files));
         String_List.Next (Files);
      end loop;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Diff;

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
         Log (Get_Current_Ref (Context), String_List.Head (Files));
         String_List.Next (Files);
      end loop;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Menu_View_Log;

   -------------------------------
   -- On_Menu_Diff_Working_Head --
   -------------------------------

   procedure On_Menu_Diff_Working_Head
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      use File_Status_List;
      pragma Unreferenced (Widget);

      Files  : String_List.List;
      Ref    : constant VCS_Access := Get_Current_Ref (Context);
      Status : File_Status_List.List;
      Status_Temp : List_Node;

   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot diff",
            Mode => Error);
         return;
      end if;

      Save_Files (Get_Kernel (Context), Files);
      Status := Local_Get_Status (Ref, Files);
      Status_Temp := First (Status);

      while Status_Temp /= Null_Node loop
         if not String_List.Is_Empty (Data (Status_Temp).Working_Revision) then
            Diff (Ref,
                  String_List.Head (Data (Status_Temp).File_Name),
                  String_List.Head (Data (Status_Temp).Working_Revision),
                  "");
         end if;

         Status_Temp := Next (Status_Temp);
      end loop;

      String_List.Free (Files);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Diff_Working_Head;

   ------------------------
   -- On_Menu_Diff_Local --
   ------------------------

   procedure On_Menu_Diff_Local
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      use File_Status_List;
      pragma Unreferenced (Widget);

      Files  : String_List.List;
      Ref    : constant VCS_Access := Get_Current_Ref (Context);
      Status : File_Status_List.List;
      Status_Temp : List_Node;

   begin
      Files := Get_Selected_Files (Context);

      if String_List.Is_Empty (Files) then
         Console.Insert
           (Get_Kernel (Context), -"VCS: No file selected, cannot diff",
            Mode => Error);
         return;
      end if;

      Save_Files (Get_Kernel (Context), Files);
      Status := Local_Get_Status (Ref, Files);
      Status_Temp := First (Status);

      while Status_Temp /= Null_Node loop
         if not String_List.Is_Empty (Data (Status_Temp).Working_Revision) then
            Diff (Ref,
                  String_List.Head (Data (Status_Temp).File_Name),
                  "",
                  String_List.Head (Data (Status_Temp).Working_Revision));
         end if;

         Status_Temp := Next (Status_Temp);
      end loop;

      String_List.Free (Files);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Menu_Diff_Local;

   --------------------------
   -- Get_Files_In_Project --
   --------------------------

   function Get_Files_In_Project
     (Project   : Project_Type;
      Recursive : Boolean := True) return String_List.List
   is
      Result  : String_List.List;
      Files   : String_Array_Access;
   begin
      Files := Get_Source_Files (Project, Recursive);

      for J in reverse Files.all'Range loop
         String_List.Prepend (Result, Files (J).all);
      end loop;

      Free (Files);

      return Result;
   end Get_Files_In_Project;

   -------------------------
   -- Get_Dirs_In_Project --
   -------------------------

   function Get_Dirs_In_Project
     (Project   : Project_Type;
      Recursive : Boolean := False) return String_List.List
   is
      Result   : String_List.List;
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

      Ref      : constant VCS_Access := Get_Current_Ref (Kernel);
      Explorer : VCS_View_Access;

   begin
      Open_Explorer (Kernel, null);
      Explorer := Get_Explorer (Kernel);
      Clear (Explorer);

      Get_Status (Ref, Get_Files_In_Project (Get_Project (Kernel)));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Update_All;

   --------------------------------
   -- Query_Status_For_Directory --
   --------------------------------

   procedure Query_Status_For_Directory
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
   begin
      if Context = null then
         Console.Insert (Kernel, -"No directory selected", Mode => Error);
      else
         On_Menu_Get_Status_Dir (Widget, Context);
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Query_Status_For_Directory;

   ---------------------
   -- Context_Factory --
   ---------------------

   procedure Update_Directory
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
   begin
      if Context = null then
         Console.Insert (Kernel, -"No directory selected", Mode => Error);
      else
         On_Menu_Update_Dir (Widget, Get_Current_Context (Kernel));
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Update_Directory;

   ------------------------------------------
   -- Query_Status_For_Directory_Recursive --
   ------------------------------------------

   procedure Query_Status_For_Directory_Recursive
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
   begin
      if Context = null then
         Console.Insert (Kernel, -"No directory selected", Mode => Error);
      else
         On_Menu_Get_Status_Dir_Recursive
           (Widget, Get_Current_Context (Kernel));
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Query_Status_For_Directory_Recursive;

   --------------------------------
   -- Update_Directory_Recursive --
   --------------------------------

   procedure Update_Directory_Recursive
     (Widget : access GObject_Record'Class;
      Kernel : Kernel_Handle)
   is
      Context : constant Selection_Context_Access :=
        Get_Current_Context (Kernel);
   begin
      if Context = null then
         Console.Insert (Kernel, -"No directory selected", Mode => Error);
      else
         On_Menu_Update_Dir_Recursive (Widget, Get_Current_Context (Kernel));
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Update_Directory_Recursive;

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
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
         return null;
   end Context_Factory;

   ---------------------
   -- Get_Status_Name --
   ---------------------

   function Get_Status_Name (Status : File_Status) return String is
   begin
      case Status is
         when Unknown =>
            return -"unknown";

         when Not_Registered =>
            return -"not registered";

         when Up_To_Date =>
            return -"up to date";

         when Modified =>
            return -"locally modified";

         when Removed =>
            return -"removed from repository";

         when Needs_Merge =>
            return -"needs merge";

         when Needs_Update =>
            return -"needs update";
      end case;
   end Get_Status_Name;

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

      use String_List_Utils.String_List;
   begin
      if Ref = null then
         return;
      end if;

      if Status.Status = Unknown then
         Status_Label := new String'("");
      else
         Status_Label := new String'
           (" (" & Get_Status_Name (Status.Status) & ")");
      end if;

      if not Is_Empty (Status.Working_Revision) then
         Revision_Label := new String'
           (Name (Ref) & ":" & Head (Status.Working_Revision));
      else
         Revision_Label := new String'(Name (Ref));
      end if;

      Add_Editor_Label
        (Kernel,
         Head (Status.File_Name),
         VCS_Module_Name,
         Revision_Label.all & Status_Label.all);

      Free (Status_Label);
      Free (Revision_Label);
   end Display_Editor_Status;

end VCS_View_API;
