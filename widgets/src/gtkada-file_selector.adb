-----------------------------------------------------------------------
--               GtkAda - Ada95 binding for Gtk+/Gnome               --
--                                                                   --
--                   Copyright (C) 2001 ACT-Europe                   --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib;            use Glib;
with Gtk;             use Gtk;
with Gtk.Widget;      use Gtk.Widget;
with Gtk.Arguments;   use Gtk.Arguments;
with Gtk.Enums;       use Gtk.Enums;
with Gtk.Stock;       use Gtk.Stock;
with Gtk.Main;
with Gtk.Ctree;       use Gtk.Ctree;
with Gdk.Pixmap;      use Gdk.Pixmap;
with Gdk.Bitmap;      use Gdk.Bitmap;
with GUI_Utils;       use GUI_Utils;

with Gtkada.Types;    use Gtkada.Types;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtkada.Intl;     use Gtkada.Intl;

with GUI_Utils; use GUI_Utils;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body Gtkada.File_Selector is

   -----------------------
   -- Local subprograms --
   -----------------------

   procedure Change_Directory
     (Win : File_Selector_Window_Access;
      Dir : String);
   --  Called every time that the contents of a new directory should be
   --  displayed in the File_Explorer. Dir is the absolute pathname to
   --  that directory, starting with a Directory_Separator.

   procedure Refresh_Files
     (Win : File_Selector_Window_Access);

   ---------------
   -- Callbacks --
   ---------------

   procedure Realize
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Back_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Up_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Home_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Refresh_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_Forward_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure Directory_Selected
     (Object : access Gtk_Widget_Record'Class);

   procedure Filter_Selected
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Location_Combo_Entry_Activate
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Explorer_Tree_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args);

   procedure On_File_List_End_Selection
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Selection_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Ok_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Widget_Record'Class);

   -------------
   -- Realize --
   -------------

   procedure Realize
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Win : File_Selector_Window_Access
        := File_Selector_Window_Access (Get_Toplevel (Object));
   begin
      Refresh_Files (Win);
   end Realize;

   ---------------------
   -- Register_Filter --
   ---------------------

   procedure Register_Filter
     (Win    : File_Selector_Window_Access;
      Filter : access File_Filter_Record'Class)
   is
      Current_Node : Filter_List := Win.Filters;
   begin
      if Current_Node = null then
         Win.Filters := new Filter_List_Node
           '(Element => File_Filter (Filter),
             Next    => null);
      else
         while Current_Node.Next /= null loop
            Current_Node := Current_Node.Next;
         end loop;
         Current_Node.Next := new Filter_List_Node'
           (Element => File_Filter (Filter),
            Next    => null);
      end if;
      Add_Unique_Combo_Entry (Win.Filter_Combo, Filter.Label.all);
   end Register_Filter;

   ---------------------
   -- Use_File_Filter --
   ---------------------

   procedure Use_File_Filter
     (Filter    : access Filter_Show_All;
      Win       : in File_Selector_Window_Access;
      Dir       : in String;
      File      : in String;
      State     : out File_State;
      Pixmap    : out Gdk.Pixmap.Gdk_Pixmap;
      Mask      : out Gdk.Bitmap.Gdk_Bitmap;
      Text      : out String_Access) is
   begin
      State := Normal;
      Mask := Gdk.Bitmap.Null_Bitmap;
      Pixmap := Gdk.Pixmap.Null_Pixmap;
      Text := new String'("");
   end Use_File_Filter;

   -------------------
   -- Refresh_Files --
   -------------------

   procedure Refresh_Files
     (Win : File_Selector_Window_Access)
   is
      Dir : String := Win.Current_Directory.all;
      Filter : File_Filter := null;
   begin
      --  Find out which filter to use.
      declare
         S : String := Get_Text (Win.Filter_Combo_Entry);
         C : Filter_List := Win.Filters;
      begin
         while C /= null loop
            if C.Element.Label.all = S then
               Filter := C.Element;
               exit;
            else
               C := C.Next;
            end if;
         end loop;
      end;

      if Filter = null then
         return;
      end if;

      --  Fill the File_List.
      declare
         Directory    : Dir_Type;
         Buffer       : String (1 .. 256);
         Last         : Natural;

         Text         : String_Access;
         State        :  File_State;
         Pixmap       :  Gdk.Pixmap.Gdk_Pixmap;
         Mask         :  Gdk.Bitmap.Gdk_Bitmap;
      begin
         GNAT.Directory_Operations.Open (Directory, Dir);
         Clear (Win.File_List);

         loop
            Read (Directory, Buffer, Last);

            exit when Last = 0;

            if Is_Directory
              (Dir & Directory_Separator & Buffer (1 .. Last))
            then
               null;
            else
               Use_File_Filter
                 (Filter,
                  Win,
                  Dir,
                  Buffer (1 .. Last),
                  State,
                  Pixmap,
                  Mask,
                  Text);

               case State is
                  when Invisible =>
                     null;
                  when Normal =>
                     Insert (Win.File_List, -1,
                             "" + Buffer (1 .. Last) + Text.all);
                  when Highlighted =>
                     null;
                  when Inactive =>
                     null;
               end case;
               Free (Text);
            end if;
         end loop;
         Close (Directory);

      exception
         when Directory_Error =>
            Clear (Win.File_List);
            Insert (Win.File_List, -1, "" + ("Could not open " & Dir) + "");
      end;
   end Refresh_Files;

   ----------------------
   -- Change_Directory --
   ----------------------

   procedure Change_Directory
     (Win : File_Selector_Window_Access;
      Dir : String)
   is
   begin
      --  If the new directory is not the one currently shown in the File_List,
      --  then update the File_List.

      if Dir /= ""
        and then Dir (Dir'First) = Directory_Separator
        and then Win.Current_Directory.all /= Dir
      then
         Free (Win.Current_Directory);
         Win.Current_Directory := new String' (Dir);

         --  If we are currently moving through the history,
         --  do not append items to the Location_Combo.

         if Win.Moving_Through_History then
            Win.Moving_Through_History := False;
         else
            Push (Win.Past_History,
                  new String'(Get_Text (Win.Location_Combo_Entry)));
            Clear (Win.Future_History);
            Set_Sensitive (Win.Back_Button);
            Set_Sensitive (Win.Forward_Button, False);

            Add_Unique_Combo_Entry
              (Win.Location_Combo, Dir);
         end if;

         if Get_Text (Win.Location_Combo_Entry) /= Dir then
            Set_Text (Win.Location_Combo_Entry, Dir);
         end if;

         --  If the new directory is not the one currently shown
         --  in the Explorer_Tree, then update the Explorer_Tree.

         if Dir /= Get_Selection (Win.Explorer_Tree) then
            Show_Directory (Win.Explorer_Tree, Dir);
         end if;

         Refresh_Files (Win);
      end if;
   end Change_Directory;

   ----------------------------
   -- On_Back_Button_Clicked --
   ----------------------------

   procedure On_Back_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));
      S   : String_Access;

   begin
      Pop (Win.Past_History, S);

      if Is_Empty (Win.Past_History) then
         Set_Sensitive (Win.Back_Button, False);
      end if;

      if Is_Empty (Win.Future_History) then
         Set_Sensitive (Win.Forward_Button);
      end if;

      Push (Win.Future_History,
            new String'(Get_Text (Win.Location_Combo_Entry)));

      Set_Text (Win.Location_Combo_Entry, S.all);
      Win.Moving_Through_History := True;
      Show_Directory (Win.Explorer_Tree, S.all);
   end On_Back_Button_Clicked;

   ----------------------------
   -- On_Home_Button_Clicked --
   ----------------------------

   procedure On_Home_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));

   begin
      Change_Directory (Win, Win.Home_Directory.all);
   end On_Home_Button_Clicked;

   --------------------------
   -- On_Up_Button_Clicked --
   --------------------------

   procedure On_Up_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      use type Node_List.Glist;

      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));

   begin
      Show_Parent (Win.Explorer_Tree);
      Change_Directory (Win, Get_Selection (Win.Explorer_Tree));
   end On_Up_Button_Clicked;

   -------------------------------
   -- On_Refresh_Button_Clicked --
   -------------------------------

   procedure On_Refresh_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      use type Node_List.Glist;

      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));

   begin
      Refresh_Files (Win);
   end On_Refresh_Button_Clicked;

   -------------------------------
   -- On_Forward_Button_Clicked --
   -------------------------------

   procedure On_Forward_Button_Clicked
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));
      S   : String_Access;

   begin
      Pop (Win.Future_History, S);

      if Is_Empty (Win.Future_History) then
         Set_Sensitive (Win.Forward_Button, False);
      end if;

      if Is_Empty (Win.Past_History) then
         Set_Sensitive (Win.Back_Button, True);
      end if;

      Push (Win.Past_History,
            new String'(Get_Text (Win.Location_Combo_Entry)));

      Set_Text (Win.Location_Combo_Entry, S.all);
      Win.Moving_Through_History := True;
      Show_Directory (Win.Explorer_Tree, S.all);

   exception
      when Stack_Empty =>
         null;
   end On_Forward_Button_Clicked;

   ------------------------
   -- Directory_Selected --
   ------------------------

   procedure Directory_Selected
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));
   begin
      Change_Directory (Win, Get_Text (Win.Location_Combo_Entry));
   end Directory_Selected;

   ------------------------
   -- Filter_Selected --
   ------------------------

   procedure Filter_Selected
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));
   begin
      Refresh_Files (Win);
   end Filter_Selected;

   --------------------------------------
   -- On_Location_Combo_Entry_Activate --
   --------------------------------------

   procedure On_Location_Combo_Entry_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));

   begin
      Change_Directory (Win, Get_Text (Win.Location_Combo_Entry));
   end On_Location_Combo_Entry_Activate;

   ---------------------------------
   -- On_Explorer_Tree_Select_Row --
   ---------------------------------

   procedure On_Explorer_Tree_Select_Row
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args)
   is
      Win : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));

   begin
      Change_Directory (Win, Get_Selection (Dir_Tree (Object)));
   end On_Explorer_Tree_Select_Row;

   --------------------------------
   -- On_File_List_End_Selection --
   --------------------------------

   procedure On_File_List_End_Selection
     (Object : access Gtk_Widget_Record'Class)
   is
      Win      : constant File_Selector_Window_Access :=
        File_Selector_Window_Access (Get_Toplevel (Object));
      Row_List : constant Gtk.Enums.Gint_List.Glist :=
        Get_Selection (Win.File_List);

   begin
      Set_Text
        (Win.Selection_Entry,
         Get_Text
           (Win.File_List,
            Gtk.Enums.Gint_List.Get_Data (Row_List),
            1));
   end On_File_List_End_Selection;

   --------------------------------
   -- On_Selection_Entry_Changed --
   --------------------------------

   procedure On_Selection_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      null;
   end On_Selection_Entry_Changed;

   --------------------------
   -- On_Ok_Button_Clicked --
   --------------------------

   procedure On_Ok_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Gtk.Main.Main_Quit;
   end On_Ok_Button_Clicked;

   ------------------------------
   -- On_Cancel_Button_Clicked --
   ------------------------------

   procedure On_Cancel_Button_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
   begin
      Gtk.Main.Main_Quit;
   end On_Cancel_Button_Clicked;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (File_Selector_Window : out File_Selector_Window_Access;
      Directory            : in String)
   is
   begin
      File_Selector_Window := new File_Selector_Window_Record;
      Initialize (File_Selector_Window, Directory);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (File_Selector_Window : access File_Selector_Window_Record'Class;
      Directory            : in String) is
   begin
      File_Selector_Window.Home_Directory := new String' (Directory);

      Gtk_New
        (File_Selector_Window.Explorer_Tree,
         File_Selector_Window.Home_Directory.all);

      Set_Indent (File_Selector_Window.Explorer_Tree, 10);

      Gtk.Window.Initialize (File_Selector_Window, Window_Toplevel);
      Set_Default_Size (File_Selector_Window, 600, 500);

      Set_Policy (File_Selector_Window, False, True, False);
      Set_Position (File_Selector_Window, Win_Pos_Center);
      Set_Modal (File_Selector_Window, False);

      Gtk_New_Vbox (File_Selector_Window.File_Selector_Vbox, False, 0);
      Add (File_Selector_Window, File_Selector_Window.File_Selector_Vbox);

      Gtk_New_Hbox (File_Selector_Window.Hbox1, False, 0);
      Pack_Start
        (File_Selector_Window.File_Selector_Vbox,
         File_Selector_Window.Hbox1, False, False, 3);

      Gtk_New_Hbox (File_Selector_Window.Hbox3, False, 0);
      Pack_Start
        (File_Selector_Window.Hbox1,
         File_Selector_Window.Hbox3, True, True, 0);

      Gtk_New
        (File_Selector_Window.Toolbar1,
         Orientation_Horizontal, Toolbar_Both);

      Set_Icon_Size (File_Selector_Window.Toolbar1, Icon_Size_Button);
      Set_Style (File_Selector_Window.Toolbar1,  Toolbar_Icons);
      File_Selector_Window.Back_Button := Insert_Stock
        (Toolbar => File_Selector_Window.Toolbar1,
         Stock_Id => Stock_Go_Back,
         Position => 0);
      Set_Sensitive (File_Selector_Window.Back_Button, False);

      Widget_Callback.Connect
        (File_Selector_Window.Back_Button, "clicked",
         On_Back_Button_Clicked'Access);

      Gtk_New (File_Selector_Window.Up_Icon, Stock_Go_Up, Icon_Size_Button);
      File_Selector_Window.Up_Button := Append_Element
        (Toolbar => File_Selector_Window.Toolbar1,
         The_Type => Toolbar_Child_Button,
         Icon => Gtk_Widget (File_Selector_Window.Up_Icon));
      Widget_Callback.Connect
        (File_Selector_Window.Up_Button, "clicked",
         On_Up_Button_Clicked'Access);

      File_Selector_Window.Forward_Button := Insert_Stock
        (Toolbar => File_Selector_Window.Toolbar1,
         Stock_Id => Stock_Go_Forward,
         Position => 0);
      Set_Sensitive (File_Selector_Window.Forward_Button, False);

      Widget_Callback.Connect
        (File_Selector_Window.Forward_Button, "clicked",
         On_Forward_Button_Clicked'Access);

      Gtk_New
        (File_Selector_Window.Refresh_Icon, Stock_Refresh, Icon_Size_Button);
      File_Selector_Window.Refresh_Button := Append_Element
        (Toolbar => File_Selector_Window.Toolbar1,
         The_Type => Toolbar_Child_Button,
         Icon => Gtk_Widget (File_Selector_Window.Refresh_Icon));
      Widget_Callback.Connect
        (File_Selector_Window.Refresh_Button, "clicked",
         On_Refresh_Button_Clicked'Access);

      File_Selector_Window.Home_Button := Insert_Stock
        (Toolbar => File_Selector_Window.Toolbar1,
         Stock_Id => Stock_Home,
         Position => 0);
      Set_Sensitive (File_Selector_Window.Home_Button, True);
      Widget_Callback.Connect
        (File_Selector_Window.Home_Button, "clicked",
         On_Home_Button_Clicked'Access);

      Pack_Start
        (File_Selector_Window.Hbox3,
         File_Selector_Window.Toolbar1, True, True, 3);

      Gtk_New_Hbox (File_Selector_Window.Hbox2, False, 0);
      Pack_Start
        (File_Selector_Window.File_Selector_Vbox,
         File_Selector_Window.Hbox2, False, False, 3);

      Gtk_New (File_Selector_Window.Label10, -("Exploring :"));
      Pack_Start
        (File_Selector_Window.Hbox2,
         File_Selector_Window.Label10, False, False, 3);

      Gtk_New (File_Selector_Window.Location_Combo);
      Set_Case_Sensitive (File_Selector_Window.Location_Combo, True);
      Pack_Start
        (File_Selector_Window.Hbox2,
         File_Selector_Window.Location_Combo, True, True, 3);
      Widget_Callback.Object_Connect
        (Get_Popup_Window (File_Selector_Window.Location_Combo),
         "hide",
         Widget_Callback.To_Marshaller (Directory_Selected'Access),
         File_Selector_Window.Location_Combo);

      File_Selector_Window.Location_Combo_Entry :=
        Get_Entry (File_Selector_Window.Location_Combo);
      Set_Editable (File_Selector_Window.Location_Combo_Entry, False);
      Set_Max_Length (File_Selector_Window.Location_Combo_Entry, 0);
      Set_Visibility (File_Selector_Window.Location_Combo_Entry, True);
      Widget_Callback.Connect
        (File_Selector_Window.Location_Combo_Entry, "activate",
         Widget_Callback.To_Marshaller
         (On_Location_Combo_Entry_Activate'Access));

      Gtk_New_Hpaned (File_Selector_Window.Hpaned1);
      Set_Position (File_Selector_Window.Hpaned1, 200);
      Set_Handle_Size (File_Selector_Window.Hpaned1, 10);
      Set_Gutter_Size (File_Selector_Window.Hpaned1, 6);
      Pack_Start
        (File_Selector_Window.File_Selector_Vbox,
         File_Selector_Window.Hpaned1, True, True, 3);

      Gtk_New (File_Selector_Window.Explorer_Tree_Scrolledwindow);
      Set_Policy
        (File_Selector_Window.Explorer_Tree_Scrolledwindow,
         Policy_Automatic, Policy_Always);

      Add (File_Selector_Window.Hpaned1,
           File_Selector_Window.Explorer_Tree_Scrolledwindow);

      Set_Column_Width (File_Selector_Window.Explorer_Tree, 0, 80);
      Set_Column_Width (File_Selector_Window.Explorer_Tree, 1, 80);
      Set_Column_Width (File_Selector_Window.Explorer_Tree, 2, 80);
      Widget_Callback.Connect
        (File_Selector_Window.Explorer_Tree, "tree_select_row",
         On_Explorer_Tree_Select_Row'Access);
      Add (File_Selector_Window.Explorer_Tree_Scrolledwindow,
           File_Selector_Window.Explorer_Tree);

      Gtk_New (File_Selector_Window.Files_Scrolledwindow);
      Set_Policy
        (File_Selector_Window.Files_Scrolledwindow,
         Policy_Automatic, Policy_Always);
      Add (File_Selector_Window.Hpaned1,
           File_Selector_Window.Files_Scrolledwindow);

      Gtk_New (File_Selector_Window.File_List, 3);
      Set_Selection_Mode (File_Selector_Window.File_List, Selection_Single);
      Set_Shadow_Type (File_Selector_Window.File_List, Shadow_In);
      Set_Show_Titles (File_Selector_Window.File_List, False);
      Set_Column_Width (File_Selector_Window.File_List, 0, 20);
      Set_Column_Width (File_Selector_Window.File_List, 1, 180);
      Set_Column_Width (File_Selector_Window.File_List, 2, 80);
      Widget_Callback.Connect
        (File_Selector_Window.File_List, "select_row",
         Widget_Callback.To_Marshaller (On_File_List_End_Selection'Access));
      Add (File_Selector_Window.Files_Scrolledwindow,
           File_Selector_Window.File_List);

      Gtk_New (File_Selector_Window.File_Icon_Label, -(""));
      Set_Column_Widget
        (File_Selector_Window.File_List, 0,
         File_Selector_Window.File_Icon_Label);

      Gtk_New (File_Selector_Window.File_Name_Label, -(""));
      Set_Column_Widget
        (File_Selector_Window.File_List, 1,
         File_Selector_Window.File_Name_Label);

      Gtk_New (File_Selector_Window.File_Text_Label, -(""));
      Set_Column_Widget
        (File_Selector_Window.File_List, 2,
         File_Selector_Window.File_Text_Label);

      Gtk_New_Hbox (File_Selector_Window.Hbox4, False, 0);
      Pack_Start
        (File_Selector_Window.File_Selector_Vbox,
         File_Selector_Window.Hbox4, False, False, 3);

      Gtk_New (File_Selector_Window.Filter_Combo);
      Set_Case_Sensitive (File_Selector_Window.Filter_Combo, False);

      Widget_Callback.Object_Connect
        (Get_Popup_Window (File_Selector_Window.Filter_Combo),
         "hide",
         Widget_Callback.To_Marshaller (Filter_Selected'Access),
         File_Selector_Window.Filter_Combo);

      Pack_Start
        (File_Selector_Window.Hbox4,
         File_Selector_Window.Filter_Combo, True, True, 3);

      File_Selector_Window.Filter_Combo_Entry :=
        Get_Entry (File_Selector_Window.Filter_Combo);
      Set_Editable (File_Selector_Window.Filter_Combo_Entry, True);
      Set_Max_Length (File_Selector_Window.Filter_Combo_Entry, 0);
      Set_Visibility (File_Selector_Window.Filter_Combo_Entry, True);

      Gtk_New_Hbox (File_Selector_Window.Hbox5, False, 0);
      Pack_Start
        (File_Selector_Window.File_Selector_Vbox,
         File_Selector_Window.Hbox5, False, False, 3);

      Gtk_New (File_Selector_Window.Selection_Entry);
      Set_Editable (File_Selector_Window.Selection_Entry, True);
      Set_Max_Length (File_Selector_Window.Selection_Entry, 0);
      Set_Visibility (File_Selector_Window.Selection_Entry, True);
      Pack_Start
        (File_Selector_Window.Hbox5,
         File_Selector_Window.Selection_Entry, True, True, 3);
      Widget_Callback.Connect
        (File_Selector_Window.Selection_Entry, "changed",
         Widget_Callback.To_Marshaller (On_Selection_Entry_Changed'Access));

      Gtk_New_Hbox (File_Selector_Window.Hbox6, False, 0);
      Pack_Start
        (File_Selector_Window.File_Selector_Vbox,
         File_Selector_Window.Hbox6, False, False, 3);

      Gtk_New (File_Selector_Window.Hbuttonbox2);
      Set_Spacing (File_Selector_Window.Hbuttonbox2, 30);
      Set_Layout (File_Selector_Window.Hbuttonbox2, Buttonbox_Spread);
      Set_Child_Size (File_Selector_Window.Hbuttonbox2, 85, 27);
      Set_Child_Ipadding (File_Selector_Window.Hbuttonbox2, 7, 0);
      Add (File_Selector_Window.Hbox6, File_Selector_Window.Hbuttonbox2);

      Gtk_New_From_Stock (File_Selector_Window.Ok_Button, Stock_Ok);
      Set_Relief (File_Selector_Window.Ok_Button, Relief_Normal);
      Set_Flags (File_Selector_Window.Ok_Button, Can_Default);
      Widget_Callback.Connect
        (File_Selector_Window.Ok_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Ok_Button_Clicked'Access));
      Add (File_Selector_Window.Hbuttonbox2, File_Selector_Window.Ok_Button);

      Gtk_New_From_Stock (File_Selector_Window.Cancel_Button, Stock_Cancel);
      Set_Relief (File_Selector_Window.Cancel_Button, Relief_Normal);
      Set_Flags (File_Selector_Window.Cancel_Button, Can_Default);
      Widget_Callback.Connect
        (File_Selector_Window.Cancel_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Cancel_Button_Clicked'Access));
      Add (File_Selector_Window.Hbuttonbox2,
           File_Selector_Window.Cancel_Button);

      Show_Directory
        (File_Selector_Window.Explorer_Tree,
         File_Selector_Window.Home_Directory.all);

      Widget_Callback.Connect
        (File_Selector_Window, "realize", Realize'Access);
      Widget_Callback.Connect
        (File_Selector_Window, "destroy",
         Widget_Callback.To_Marshaller (On_Cancel_Button_Clicked'Access));
   end Initialize;

end Gtkada.File_Selector;
