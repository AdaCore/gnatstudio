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

with Gtk.Menu_Item;             use Gtk.Menu_Item;

with VCS;                       use VCS;
with VCS_View_Pkg;              use VCS_View_Pkg;

with Glide_Intl;                use Glide_Intl;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;

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

   procedure On_Menu_Edit_Log
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

   procedure On_Menu_Commit
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);

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

   ---------------
   -- Update --
   ---------------

   procedure Update
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Files : String_List.List := Get_Selected_Files (Kernel);
      Ref   : VCS_Access := Get_Current_Ref (Kernel);
   begin
      Update (Ref, Files);
   end Update;

   ---------------
   -- Commit --
   ---------------

   procedure Commit
     (Widget  : access GObject_Record'Class;
      Kernel  : Kernel_Handle) is
   begin
      --  ??? Right now, commit opens a log editor for the file.
      --  We should decide what the correct behavior should be.

      Edit_Log (Widget, Kernel);
   end Commit;

   ------------------
   -- View_Diff --
   ------------------

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

   ------------------
   -- View_Log --
   ------------------

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

   ----------------------
   -- View_Annotate --
   ----------------------

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

   -----------------
   -- Edit_Log --
   -----------------

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
   begin
      Gtk_New (Item, Label => -"VCS Update");
      Append (Menu, Item);
      Context_Callback.Connect
        (Item, "activate",
         Context_Callback.To_Marshaller
         (On_Menu_Update'Access),
         Selection_Context_Access (Context));

      Gtk_New (Item, Label => -"VCS Open");
      Append (Menu, Item);
      Context_Callback.Connect
        (Item, "activate",
         Context_Callback.To_Marshaller
         (On_Menu_Open'Access),
         Selection_Context_Access (Context));

      Gtk_New (Item, Label => -"VCS Diff");
      Append (Menu, Item);
      Context_Callback.Connect
        (Item, "activate",
         Context_Callback.To_Marshaller
         (On_Menu_Diff'Access),
         Selection_Context_Access (Context));

      Gtk_New (Item, Label => -"VCS Update");
      Append (Menu, Item);
      Context_Callback.Connect
        (Item, "activate",
         Context_Callback.To_Marshaller
         (On_Menu_Update'Access),
         Selection_Context_Access (Context));

      Gtk_New (Item, Label => -"VCS Edit log");
      Append (Menu, Item);
      Context_Callback.Connect
        (Item, "activate",
         Context_Callback.To_Marshaller
         (On_Menu_Edit_Log'Access),
         Selection_Context_Access (Context));

      Gtk_New (Item, Label => -"VCS Commit");
      Append (Menu, Item);
      Context_Callback.Connect
        (Item, "activate",
         Context_Callback.To_Marshaller
         (On_Menu_Commit'Access),
         Selection_Context_Access (Context));
   end VCS_Contextual_Menu;

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
