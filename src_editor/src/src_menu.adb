with Glib;                     use Glib;
with Gtk.Accel_Group;          use Gtk.Accel_Group;
with Gtk.Item_Factory;         use Gtk.Item_Factory;
with Gtk.Main;
with Gtk.Menu_Bar;             use Gtk.Menu_Bar;
with Gtkada.File_Selection;    use Gtkada.File_Selection;
with Src_Editor_Box;           use Src_Editor_Box;

with Ada.Text_IO;              use Ada.Text_IO;

package body Src_Menu is

   File : constant String := "/File";
   File_New     : constant String := File & '/' & "New";
   File_Open    : constant String := File & '/' & "Open";
   File_Save    : constant String := File & '/' & "Save";
   File_Save_As : constant String := File & '/' & "Save As...";
   File_Quit    : constant String := File & '/' & "Quit";

   Prefs : constant String := "/Preferences";
   Prefs_Font : constant String := Prefs & '/' & "Font";
   Prefs_Line_Num : constant String := Prefs & '/' & "Show Line Numbers";
   Prefs_Debug_Mode : constant String := Prefs & '/' & "Debug Mode";

   Edit : constant String := "/Edit";
   Edit_Undo      : constant String := Edit & '/' & "Undo";
   Edit_Redo      : constant String := Edit & '/' & "Redo";
   Edit_Cut       : constant String := Edit & '/' & "Cut";
   Edit_Copy      : constant String := Edit & '/' & "Copy";
   Edit_Paste     : constant String := Edit & '/' & "Paste";
   Edit_Search    : constant String := Edit & '/' & "Search";
   Edit_Goto_Line : constant String := Edit & '/' & "Goto line...";

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

   procedure Menu_Cb
     (Callback_Data   : Source_Editor_Box_Access;
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
         Factory.Gtk_New (File & "/sep1", Item_Type => Separator),
         Factory.Gtk_New (File_Quit, "<control>Q", Quit_Cb'Access),
         --  Preferences:
         Factory.Gtk_New (Prefs, Item_Type => Branch),
         Factory.Gtk_New (Prefs_Font, "", Menu_Cb'Access),
         Factory.Gtk_New (Prefs_Line_Num, "", Menu_Cb'Access),
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
         Factory.Gtk_New (Edit_Goto_Line, "<control>G", Menu_Cb'Access)
        );
      Accel_Group : Gtk_Accel_Group;
   begin
      Gtk_New (Accel_Group);
      Gtk_New (Menu, Gtk.Menu_Bar.Get_Type, Root, Accel_Group);
      Attach (Accel_Group, Win);
      Factory.Create_Items
        (Menu, Menu_Items, Source_Editor_Box_Access (Box));
   end Create_Menu;

end Src_Menu;
