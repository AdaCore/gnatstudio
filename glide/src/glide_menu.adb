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

with Glib;                         use Glib;
with Gtk.Stock;                    use Gtk.Stock;
with Gtkada.File_Selector;         use Gtkada.File_Selector;
with Gtkada.MDI;                   use Gtkada.MDI;

with Glide_Intl;                   use Glide_Intl;

with Glide_Kernel;                 use Glide_Kernel;
with Glide_Kernel.Preferences;     use Glide_Kernel.Preferences;
with Glide_Kernel.Project;         use Glide_Kernel.Project;
with Glide_Main_Window;            use Glide_Main_Window;
with Project_Viewers;              use Project_Viewers;

with GNAT.Directory_Operations;    use GNAT.Directory_Operations;
with Factory_Data;                 use Factory_Data;
with Ada.Exceptions;               use Ada.Exceptions;
with Traces;                       use Traces;

package body Glide_Menu is

   Me : constant Debug_Handle := Create ("Menu");

   --------------------
   -- Menu Callbacks --
   --------------------

   procedure On_Close
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->Close menu

   procedure On_Save_Desktop
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->Save Desktop menu

   procedure On_Change_Dir
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->Change Directory... menu

   procedure On_Exit
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  File->Exit menu

   procedure On_Preferences
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Edit->Preferences menu

   procedure On_Open_Project
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Project->Open menu

   ---------------------
   -- On_Open_Project --
   ---------------------

   procedure On_Open_Project
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      Kernel : constant Kernel_Handle := Glide_Window (Object).Kernel;
   begin
      declare
         Filename : constant String :=
           Select_File
             (-"Open Project",
              File_Pattern      => "*.gpr",
              Pattern_Name      => -"Project files",
              Use_Native_Dialog => Get_Pref (Kernel, Use_Native_Dialogs),
              Kind              => Open_File,
              History           => Get_History (Kernel));
      begin
         if Filename /= "" then
            Change_Dir (Dir_Name (Filename));
            Load_Project (Kernel, Filename);
            Add_To_Reopen (Kernel, Filename);
         end if;
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Open_Project;

   -------------------
   -- On_Change_Dir --
   -------------------

   procedure On_Change_Dir
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      Kernel : constant Kernel_Handle := Glide_Window (Object).Kernel;
      Dir    : constant String := Select_Directory
        (-"Select a directory", History => Get_History (Kernel));

   begin
      if Dir /= "" then
         Change_Project_Dir (Kernel, Dir);
      end if;
   end On_Change_Dir;

   --------------
   -- On_Close --
   --------------

   procedure On_Close
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      MDI   : constant MDI_Window := Get_MDI (Glide_Window (Object).Kernel);
      Child : constant MDI_Child := Get_Focus_Child (MDI);

   begin
      if Child /= null then
         Close (MDI, Get_Widget (Child));
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Close;

   -------------
   -- On_Exit --
   -------------

   procedure On_Exit
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);
   begin
      Confirm_And_Quit (Glide_Window (Object));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Exit;

   ---------------------
   -- On_Save_Desktop --
   ---------------------

   procedure On_Save_Desktop
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);

      Top  : constant Glide_Window := Glide_Window (Object);
   begin
      Save_Desktop (Top.Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Save_Desktop;

   --------------------
   -- On_Preferences --
   --------------------

   procedure On_Preferences
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget)
   is
      pragma Unreferenced (Action, Widget);
      Top : constant Glide_Window := Glide_Window (Object);
   begin
      Edit_Preferences (Top.Kernel);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Preferences;

   ----------------------
   -- Glide_Menu_Items --
   ----------------------

   function Glide_Menu_Items return Gtk_Item_Factory_Entry_Access is
      File        : constant String := "/_" & (-"File")     & '/';
      Edit        : constant String := "/_" & (-"Edit")     & '/';
      Project     : constant String := "/_" & (-"Project")  & '/';
      Tools       : constant String := "/_" & (-"Tools")    & '/';
      Debug       : constant String := "/_" & (-"Debug")    & '/';
      Data_Sub    : constant String := (-"_Data")           & '/';
      Window      : constant String := "/_" & (-"Window");

   begin
      return new Gtk_Item_Factory_Entry_Array'
        (Gtk_New (File & (-"S_ave...") & '/' & (-"Desktop"), "",
                  On_Save_Desktop'Access),
         Gtk_New (File & "sep1", Item_Type => Separator),
         Gtk_New (File & (-"Change _Directory..."), "", "",
                  On_Change_Dir'Access),
         Gtk_New (File & (-"_Close"), "", Stock_Close, On_Close'Access),
         Gtk_New (File & (-"Close _All"), "", null),
         Gtk_New (File & "sep2", Item_Type => Separator),
         Gtk_New (File & (-"_Exit"), "<control>Q",
                  Stock_Quit, On_Exit'Access),

         Gtk_New (Edit & (-"_Preferences"), "",
                  Stock_Preferences, On_Preferences'Access),

         Gtk_New (Project & (-"_Open..."), "", Stock_Open,
                  On_Open_Project'Access),
         Gtk_New (Project & "sep1", Item_Type => Separator),

         Gtk_New (Debug & Data_Sub & (-"_Call Stack"), "", null, Check_Item),
         Gtk_New (Debug & Data_Sub & (-"_Protection Domains"), "", null),

         Gtk_New (Tools & (-"_Profile"), "", null),
         Gtk_New (Tools & (-"_Memory Analyzer"), "", null),
         Gtk_New (Tools & (-"_Generate API doc"), "", null),

         Gtk_New (Window));
   end Glide_Menu_Items;

end Glide_Menu;
