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
with Src_Editor;               use Src_Editor;
with Src_Editor_Box;           use Src_Editor_Box;

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
   Edit_Highlight   : constant String := Edit & '/' & "Highlight...";
   Edit_Unhighlight : constant String := Edit & '/' & "Un-highlight...";
   Edit_Cancel_High : constant String := Edit & '/' & "Cancel highlighting.";

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

   procedure Highlight_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget);

   procedure Cancel_H_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget);

   procedure Unhighlight_Cb
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
      Success : Boolean;
   begin
      Get_Cursor_Location (Callback_Data, Line, Col);
      Put_Line ("  Current Position : (" &
                Image (Line) & ", " & Image (Col) & ")");
      Line := Line + 1;
      Col  := Col + 1;
      Set_Cursor_Location (Callback_Data, Line, Col, Success);
      if not Success then
         Put_Line ("    Could not move Cusor position to (" &
                   Image (Line) & ", " & Image (Col));
      end if;
   end Goto_Line_Cb;

   ------------------
   -- Highlight_Cb --
   ------------------

   procedure Highlight_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget)
   is
      Line          : Positive;
      Column        : Positive;
      Success       : Boolean;
   begin
      Get_Cursor_Location (Box, Line, Column);
      Put_Line ("+++ Highlighting line " & Image (Line));
      Highlight_Line (Box, Line, Success);
      if not Success then
         Put_Line ("*** Highlight_Line failed!");
      end if;
   end Highlight_Cb;

   -----------------
   -- Cancel_H_Cb --
   -----------------

   procedure Cancel_H_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget) is
   begin
      Cancel_Highlight_Line (Box);
   end Cancel_H_Cb;

   --------------------
   -- Unhighlight_Cb --
   --------------------

   procedure Unhighlight_Cb
     (Box             : Source_Editor_Box_Access;
      Callback_Action : Guint;
      Widget          : Factory.Limited_Widget)
   is
      Line          : Positive;
      Column        : Positive;
      Success       : Boolean;
   begin
      Get_Cursor_Location (Box, Line, Column);
      Put_Line ("+++ Unhighlighting line " & Image (Line));
      Unhighlight_Line (Box, Line, Success);
      if not Success then
         Put_Line ("*** Unhighlight_Line failed!");
      end if;
   end Unhighlight_Cb;

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
         Factory.Gtk_New (Edit_Highlight, "<control>L", Highlight_Cb'Access),
         Factory.Gtk_New (Edit_Cancel_High, "", Cancel_H_Cb'Access),
         Factory.Gtk_New (Edit_Unhighlight, "", Unhighlight_Cb'Access)
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
