-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
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

with Glib;                     use Glib;
with Gtk.Accel_Group;          use Gtk.Accel_Group;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Item_Factory;         use Gtk.Item_Factory;
with Gtk.Main;
with Gtk.Menu_Bar;             use Gtk.Menu_Bar;
with Gtk.Window;               use Gtk.Window;
with Gtkada.File_Selection;    use Gtkada.File_Selection;
with Src_Cb;                   use Src_Cb;
with Src_Editor_Box;           use Src_Editor_Box;

with String_Utils;             use String_Utils;
with Ada.Text_IO;              use Ada.Text_IO;

package body Src_Menu is

   File : constant String := "/File";
   File_New      : constant String := File & '/' & "New";
   File_Open     : constant String := File & '/' & "Open";
   File_Save     : constant String := File & '/' & "Save";
   File_Save_As  : constant String := File & '/' & "Save As...";
   File_New_View : constant String := File & '/' & "Create New View";
   File_Quit     : constant String := File & '/' & "Quit";

   Prefs : constant String := "/Preferences";
   Prefs_Font : constant String := Prefs & '/' & "Font";
   Prefs_Line_Num : constant String := Prefs & '/' & "Toggle Line Numbers";
   Prefs_Debug_Mode : constant String := Prefs & '/' & "Debug Mode";

   Edit : constant String := "/Edit";
   Edit_Undo        : constant String := Edit & '/' & "Undo";
   Edit_Redo        : constant String := Edit & '/' & "Redo";
   Edit_Cut         : constant String := Edit & '/' & "Cut";
   Edit_Copy        : constant String := Edit & '/' & "Copy";
   Edit_Paste       : constant String := Edit & '/' & "Paste";
   Edit_Search      : constant String := Edit & '/' & "Search";
   Edit_Goto_Line   : constant String := Edit & '/' & "Goto line...";
   Edit_HL_Line     : constant String := Edit & '/' & "HL Line";
   Edit_UnHL_Line   : constant String := Edit & '/' & "UnHL Line";
   Edit_Cancel_HL_L : constant String := Edit & '/' & "Cancel HL Line";
   Edit_HL_Region   : constant String := Edit & '/' & "HL Region";
   Edit_UnHL_Region : constant String := Edit & '/' & "UnHL Region";
   Edit_Cancel_HL_R : constant String := Edit & '/' & "Cancel HL Region";

   package Factory is new Data_Item (Source_Editor_Box_Record);
   subtype Source_Editor_Box_Access is Factory.Data_Type_Access;

   --------------------------
   -- Forward declarations --
   --------------------------

   procedure Quit_Cb
     (Callback_Data   : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget);

   procedure File_Open_Cb
     (Editor          : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget);

   procedure File_New_View_Cb
     (Editor          : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget);

   procedure Menu_Cb
     (Callback_Data   : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget);

   procedure Goto_Line_Cb
     (Callback_Data   : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget);

   procedure HL_Line_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget);

   procedure Cancel_HL_Line_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget);

   procedure UnHL_Line_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget);

   procedure HL_Region_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget);

   procedure Cancel_HL_Region_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget);

   procedure UnHL_Region_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget);

   procedure File_Num_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget);

   -------------
   -- Quit_Cb --
   -------------

   procedure Quit_Cb
     (Callback_Data   : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget) is
   begin
      Gtk.Main.Main_Quit;
   end Quit_Cb;

   ------------------
   -- File_Open_Cb --
   ------------------

   procedure File_Open_Cb
     (Editor          : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget)
   is
      Filename : constant String :=
        File_Selection_Dialog (Title => "Open file", Must_Exist => True);
      Success : Boolean;
   begin
      if Filename = "" then
         return;
      end if;

      Load_File (Editor, Filename, Success => Success);
      if not Success then
         Put_Line ("   *** Failed to load file : '" & Filename & "'");
      end if;
   end File_Open_Cb;

   ----------------------
   -- File_New_View_Cb --
   ----------------------

   procedure File_New_View_Cb
     (Editor          : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget)
   is
      New_Editor : Source_Editor_Box;
      New_Window : Gtk_Window;
   begin
      Create_New_View (New_Editor, Source => Editor);
      Create_Main_Window (New_Window, New_Editor);
      Show_All (New_Window);
   end File_New_View_Cb;

   -------------
   -- Menu_Cb --
   -------------

   procedure Menu_Cb
     (Callback_Data   : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget) is
   begin
      null;  --  Do nothing for the moment...
   end Menu_Cb;

   ------------------
   -- Goto_Line_Cb --
   ------------------

   procedure Goto_Line_Cb
     (Callback_Data   : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget)
   is
      Line    : Positive;
      Col     : Positive;
   begin
      Get_Cursor_Location (Callback_Data, Line, Col);
      Put_Line ("  Current Position : (" &
                Image (Line) & ", " & Image (Col) & ")");
      Line := Line + 1;
      Col  := Col + 1;
      if Is_Valid_Location (Callback_Data, Line, Col) then
         Set_Cursor_Location (Callback_Data, Line, Col);
      else
         Put_Line ("    Could not move Cusor position to (" &
                   Image (Line) & ", " & Image (Col));
      end if;
   end Goto_Line_Cb;

   ----------------
   -- HL_Line_Cb --
   ----------------

   procedure HL_Line_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget)
   is
      Line          : Positive;
      Column        : Positive;
   begin
      Get_Cursor_Location (Box, Line, Column);
      Put_Line ("+++ Highlighting line " & Image (Line));
      Highlight_Line (Box, Line);
   end HL_Line_Cb;

   -----------------------
   -- Cancel_HL_Line_Cb --
   -----------------------

   procedure Cancel_HL_Line_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget) is
   begin
      Cancel_Highlight_Line (Box);
   end Cancel_HL_Line_Cb;

   ------------------
   -- UnHL_Line_Cb --
   ------------------

   procedure UnHL_Line_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget)
   is
      Line          : Positive;
      Column        : Positive;
   begin
      Get_Cursor_Location (Box, Line, Column);
      Put_Line ("+++ Unhighlighting line " & Image (Line));
      Unhighlight_Line (Box, Line);
   end UnHL_Line_Cb;

   ------------------
   -- HL_Region_Cb --
   ------------------

   procedure HL_Region_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget)
   is
      Start_Line : Positive;
      Start_Col  : Positive;
      End_Line   : Positive;
      End_Col    : Positive;
      Found      : Boolean;
   begin
      Get_Selection_Bounds
        (Box, Start_Line, Start_Col, End_Line, End_Col, Found);

      if Found then
         Highlight_Region (Box, Start_Line, Start_Col, End_Line, End_Col);
      end if;
   end HL_Region_Cb;

   -------------------------
   -- Cancel_HL_Region_Cb --
   -------------------------

   procedure Cancel_HL_Region_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget) is
   begin
      Unhighlight_All (Box);
   end Cancel_HL_Region_Cb;

   --------------------
   -- UnHL_Region_Cb --
   --------------------

   procedure UnHL_Region_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget)
   is
      Start_Line : Positive;
      Start_Col  : Positive;
      End_Line   : Positive;
      End_Col    : Positive;
      Found      : Boolean;
   begin
      Get_Selection_Bounds
        (Box, Start_Line, Start_Col, End_Line, End_Col, Found);

      if Found then
         Unhighlight_Region (Box, Start_Line, Start_Col, End_Line, End_Col);
      end if;
   end UnHL_Region_Cb;

   -----------------
   -- File_Num_Cb --
   -----------------

   procedure File_Num_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget) is
   begin
      Set_Show_Line_Numbers (Box, not Get_Show_Line_Numbers (Box));
   end File_Num_Cb;

   -----------------
   -- Create_Menu --
   -----------------

   procedure Create_Menu
     (Menu : out Gtk.Item_Factory.Gtk_Item_Factory;
      Win  : Gtk.Window.Gtk_Window;
      Box  : Src_Editor_Box.Source_Editor_Box)
   is
      Menu_Items : constant Gtk_Item_Factory_Entry_Array :=
        ( --  File:
         Factory.Gtk_New (File, Item_Type => Branch),
         Factory.Gtk_New (File_New, "<control>N", Menu_Cb'Access),
         Factory.Gtk_New (File_Open, "<control>O", File_Open_Cb'Access),
         Factory.Gtk_New (File_Save, "<control>S", Menu_Cb'Access),
         Factory.Gtk_New (File_Save_As, "", Menu_Cb'Access),
         Factory.Gtk_New (File_New_View, "", File_New_View_Cb'Access),
         Factory.Gtk_New (File & "/sep1", Item_Type => Separator),
         Factory.Gtk_New (File_Quit, "<control>Q", Quit_Cb'Access),
         --  Preferences:
         Factory.Gtk_New (Prefs, Item_Type => Branch),
         Factory.Gtk_New (Prefs_Font, "", Menu_Cb'Access),
         Factory.Gtk_New (Prefs_Line_Num, "", File_Num_Cb'Access),
         Factory.Gtk_New (Prefs_Debug_Mode, "", Menu_Cb'Access),
         --  Edit:
         Factory.Gtk_New (Edit, Item_Type => Branch),
         Factory.Gtk_New (Edit_Undo, "", Menu_Cb'Access),
         Factory.Gtk_New (Edit_Redo, "", Menu_Cb'Access),
         Factory.Gtk_New (Edit_Cut, "<control>X", Menu_Cb'Access),
         Factory.Gtk_New (Edit_Copy, "<control>C", Menu_Cb'Access),
         Factory.Gtk_New (Edit_Paste, "<control>V", Menu_Cb'Access),
         Factory.Gtk_New (Edit & "/sep1", Item_Type => Separator),
         Factory.Gtk_New (Edit_Search, "<control>S", Menu_Cb'Access),
         Factory.Gtk_New (Edit_Goto_Line, "<control>G", Goto_Line_Cb'Access),
         Factory.Gtk_New (Edit_HL_Line, "<control>L", HL_Line_Cb'Access),
         Factory.Gtk_New (Edit_Cancel_HL_L, "", Cancel_HL_Line_Cb'Access),
         Factory.Gtk_New (Edit_UnHL_Line, "", UnHL_Line_Cb'Access),
         Factory.Gtk_New (Edit_HL_Region, "<control>R", HL_Region_Cb'Access),
         Factory.Gtk_New (Edit_Cancel_HL_R, "", Cancel_HL_Region_Cb'Access),
         Factory.Gtk_New (Edit_UnHL_Region, "", UnHL_Region_Cb'Access)
        );
      Accel_Group : Gtk_Accel_Group;
   begin
      Gtk_New (Accel_Group);
      Gtk_New (Menu, Gtk.Menu_Bar.Get_Type, Root, Accel_Group);
      Attach (Accel_Group, Win);
      Factory.Create_Items
        (Menu, Menu_Items, Source_Editor_Box_Access (Box));
   end Create_Menu;

   ------------------------
   -- Create_Main_Window --
   ------------------------

   procedure Create_Main_Window
     (Main_Window  : out Gtk.Window.Gtk_Window;
      Box          : Src_Editor_Box.Source_Editor_Box)
   is
      V_Box : Gtk.Box.Gtk_Box;
      Menu : Gtk_Item_Factory;
   begin

      Gtk_New (Main_Window, Window_Toplevel);
      Set_Title (Main_Window, "The GLIDE Source Editor");
      Set_Default_Size (Main_Window, 390, 550);
      Window_Cb.Connect
        (Main_Window, "destroy",
         Window_Cb.To_Marshaller (Exit_Main'Access));

      Gtk_New_Vbox (V_Box);
      Add (Main_Window, V_Box);

      Create_Menu (Menu, Main_Window, Box);
      Pack_Start (V_Box, Get_Widget (Menu, Root), False, False, 0);

      Attach (Box, V_Box);
   end Create_Main_Window;

end Src_Menu;
