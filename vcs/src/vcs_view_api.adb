-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001-2002                    --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Gtk.Widget;                use Gtk.Widget;
with Gtk.Menu_Item;             use Gtk.Menu_Item;
with Gtk.Arguments;             use Gtk.Arguments;

with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with VCS;                       use VCS;
with VCS_View_Pkg;              use VCS_View_Pkg;

with Glide_Intl;                use Glide_Intl;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;

with Prj_API;                   use Prj_API;

with String_List;

package body VCS_View_API is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure On_Menu_Open
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Diff
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

   procedure On_Menu_Get_Status_Dir
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Update_Dir
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Get_Status_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Update_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Context_Changed
     (Object  : access Gtk_Widget_Record'Class;
      Args    : Gtk_Args);
   --  ???

   ----------
   -- Open --
   ----------

   procedure Open
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);
   begin
      Open (Ref, Files);

      declare
         L_Temp : String_List.List := Files;
      begin
         while not String_List.Is_Empty (L_Temp) loop
            Open_File_Editor (Kernel, String_List.Head (L_Temp));
            L_Temp := String_List.Next (L_Temp);
         end loop;
      end;
   end Open;

   ------------
   -- Update --
   ------------

   procedure Update
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);
   begin
      Update (Ref, Files);
      Get_Status (Ref, Files);
   end Update;

   ----------------
   -- Get_Status --
   ----------------

   procedure Get_Status
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);

   begin
      Open_Explorer (Kernel);
      Get_Status (Ref, Files);
   end Get_Status;

   ---------
   -- Add --
   ---------

   procedure Add
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);
   begin
      Add (Ref, Files);
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);
   begin
      Remove (Ref, Files);
   end Remove;

   ------------
   -- Revert --
   ------------

   procedure Revert
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);
   begin
      Revert (Ref, Files);
   end Revert;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle) is
   begin
      --  ??? Right now, commit opens a log editor for the file.
      --  We should decide what the correct behavior should be.

      Edit_Log (Widget, Kernel);
   end Commit;

   ---------------
   -- View_Diff --
   ---------------

   procedure View_Diff
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);
   begin
      while not String_List.Is_Empty (Files) loop
         Diff (Ref, String_List.Head (Files));
         String_List.Tail (Files);
      end loop;
   end View_Diff;

   --------------
   -- View_Log --
   --------------

   procedure View_Log
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);
   begin
      while not String_List.Is_Empty (Files) loop
         Log (Ref, String_List.Head (Files));
         String_List.Tail (Files);
      end loop;
   end View_Log;

   -------------------
   -- View_Annotate --
   -------------------

   procedure View_Annotate
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);
   begin
      while not String_List.Is_Empty (Files) loop
         Annotate (Ref, String_List.Head (Files));
         String_List.Tail (Files);
      end loop;
   end View_Annotate;

   --------------
   -- Edit_Log --
   --------------

   procedure Edit_Log
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);
   begin
      Edit_Log (null, Kernel, Files, Ref);
      String_List.Free (Files);
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
      Item : Gtk_Menu_Item;

      File : File_Selection_Context_Access;

   begin
      if Context.all in File_Selection_Context'Class then
         File := File_Selection_Context_Access (Context);

         if Has_File_Information (File) then
            Gtk_New (Item, Label => -"Query Status");
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

            Gtk_New (Item, Label => -"Open");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (On_Menu_Open'Access),
               Selection_Context_Access (Context));

            Gtk_New (Item, Label => -"Diff");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (On_Menu_Diff'Access),
               Selection_Context_Access (Context));

            Gtk_New (Item, Label => -"Edit changelog");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (On_Menu_Edit_Log'Access),
               Selection_Context_Access (Context));

            Gtk_New (Item, Label => -"Commit");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (On_Menu_Commit'Access),
               Selection_Context_Access (Context));
         end if;

         if Has_File_Information (File)
           and then (Has_Directory_Information (File)
                     or else Has_Project_Information (File))
         then
            Gtk_New (Item);
            Append (Menu, Item);
         end if;

         if Has_Directory_Information (File) then
            Gtk_New (Item, Label => -"Query status for directory");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (On_Menu_Get_Status_Dir'Access),
               Selection_Context_Access (Context));

            Gtk_New (Item, Label => -"Update directory");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (On_Menu_Update_Dir'Access),
               Selection_Context_Access (Context));
         end if;

         if Has_Project_Information (File)
           and then Has_Directory_Information (File)
         then
            Gtk_New (Item);
            Append (Menu, Item);
         end if;

         if Has_Project_Information (File) then
            Gtk_New (Item, Label => -"Query status for project");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (On_Menu_Get_Status_Project'Access),
               Selection_Context_Access (Context));

            Gtk_New (Item, Label => -"Update project");
            Append (Menu, Item);
            Context_Callback.Connect
              (Item, "activate",
               Context_Callback.To_Marshaller
               (On_Menu_Update_Project'Access),
               Selection_Context_Access (Context));
         end if;

      end if;
   end VCS_Contextual_Menu;

   ------------------------
   -- On_Context_Changed --
   ------------------------

   procedure On_Context_Changed
     (Object  : access Gtk_Widget_Record'Class;
      Args    : Gtk_Args)
   is
      pragma Unreferenced (Object);
      Context      : Selection_Context_Access :=
        To_Selection_Context_Access (To_Address (Args, 1));
      File         : File_Selection_Context_Access;
      Status       : File_Status_List.List;
      Dirs         : String_List.List;
      Explorer     : VCS_View_Access := Get_Explorer (Get_Kernel (Context));

      Ref          : VCS_Access := Get_Current_Ref (Get_Kernel (Context));
   begin
      if Context = null
        or else Explorer = null
      then
         return;
      end if;

      if Context.all in File_Selection_Context'Class then
         File := File_Selection_Context_Access (Context);
         if Has_Directory_Information (File) then
            String_List.Append (Dirs, Directory_Information (File));
            Status :=  Local_Get_Status (Ref, Dirs);
            String_List.Free (Dirs);

            Clear (Explorer);
            Display_File_Status (Get_Kernel (Context), Status, False);
            File_Status_List.Free (Status);
         end if;
      end if;
   end On_Context_Changed;

   -------------------
   -- Open_Explorer --
   -------------------

   procedure Open_Explorer
     (Kernel : Kernel_Handle)
   is
      MDI      : MDI_Window := Get_MDI (Kernel);
      Explorer : VCS_View_Access := Get_Explorer (Kernel);
      Child    : MDI_Child;
      Ref      : VCS_Access := Get_Current_Ref (Kernel);

      Dirs     : String_List.List;
      Status   : File_Status_List.List;

   begin
      if Explorer = null then
         Gtk_New (Explorer, Kernel);
         Set_Size_Request (Explorer, 400, 400);
         Child := Put (MDI, Explorer);
         Set_Title (Child, -"VCS Explorer");

         String_List.Append (Dirs, Get_Current_Dir (Kernel));
         Status :=  Local_Get_Status (Ref, Dirs);
         String_List.Free (Dirs);

         Clear (Explorer);
         Display_File_Status (Kernel, Status, False);
         File_Status_List.Free (Status);

         Widget_Callback.Object_Connect
           (Kernel,
            Context_Changed_Signal,
            On_Context_Changed'Access,
            Explorer);
      end if;
   end Open_Explorer;

   ----------------------
   -- On_Menu_Edit_Log --
   ----------------------

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access) is
   begin
      Edit_Log (Widget, Get_Kernel (Context));
   end On_Menu_Edit_Log;

   --------------------
   -- On_Menu_Commit --
   --------------------

   procedure On_Menu_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access) is
   begin
      Commit (Widget, Get_Kernel (Context));
   end On_Menu_Commit;

   ------------------
   -- On_Menu_Open --
   ------------------

   procedure On_Menu_Open
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access) is
   begin
      Open (Widget, Get_Kernel (Context));
   end On_Menu_Open;

   --------------------
   -- On_Menu_Update --
   --------------------

   procedure On_Menu_Update
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access) is
   begin
      Update (Widget, Get_Kernel (Context));
   end On_Menu_Update;

   ------------------------
   -- On_Menu_Get_Status --
   ------------------------

   procedure On_Menu_Get_Status
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access) is
   begin
      Get_Status (Widget, Get_Kernel (Context));
   end On_Menu_Get_Status;

   ------------------------
   -- On_Menu_Update_Dir --
   ------------------------

   procedure On_Menu_Update_Dir
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Files        : String_List.List;
      File_Context : File_Selection_Context_Access;
   begin
      Open_Explorer (Get_Kernel (Context));

      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         if Has_Directory_Information (File_Context) then
            String_List.Append (Files, Directory_Information (File_Context));
            Update (Get_Current_Ref (Get_Kernel (Context)), Files);
            Get_Status (Get_Current_Ref (Get_Kernel (Context)), Files);
         end if;
      end if;
   end On_Menu_Update_Dir;

   ----------------------------
   -- On_Menu_Get_Status_Dir --
   ----------------------------

   procedure On_Menu_Get_Status_Dir
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Files        : String_List.List;
      File_Context : File_Selection_Context_Access;
   begin
      Open_Explorer (Get_Kernel (Context));

      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         if Has_Directory_Information (File_Context) then
            String_List.Append (Files, Directory_Information (File_Context));
            Get_Status (Get_Current_Ref (Get_Kernel (Context)), Files);
         end if;
      end if;
   end On_Menu_Get_Status_Dir;

   ----------------------------
   -- On_Menu_Update_Project --
   ----------------------------

   procedure On_Menu_Update_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Files        : String_List.List;
      File_Context : File_Selection_Context_Access;
   begin
      Open_Explorer (Get_Kernel (Context));

      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         if Has_Project_Information (File_Context) then
            Files := Get_Files_In_Project
              (Get_Project_From_View (Project_Information (File_Context)));
            Update (Get_Current_Ref (Get_Kernel (Context)), Files);
            Get_Status (Get_Current_Ref (Get_Kernel (Context)), Files);
         end if;
      end if;
   end On_Menu_Update_Project;

   --------------------------------
   -- On_Menu_Get_Status_Project --
   --------------------------------

   procedure On_Menu_Get_Status_Project
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);
      Files        : String_List.List;
      File_Context : File_Selection_Context_Access;
   begin
      Open_Explorer (Get_Kernel (Context));

      if Context.all in File_Selection_Context'Class then
         File_Context := File_Selection_Context_Access (Context);

         if Has_Project_Information (File_Context) then
            Files := Get_Files_In_Project
              (Get_Project_From_View (Project_Information (File_Context)));
            Get_Status (Get_Current_Ref (Get_Kernel (Context)), Files);
         end if;
      end if;
   end On_Menu_Get_Status_Project;

   ------------------
   -- On_Menu_Diff --
   ------------------

   procedure On_Menu_Diff
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access) is
   begin
      View_Diff (Widget, Get_Kernel (Context));
   end On_Menu_Diff;

end VCS_View_API;
