
with Glib;          use Glib;
with Gdk.Bitmap;    use Gdk.Bitmap;
with Gdk.Color;     use Gdk.Color;
with Gdk.Event;     use Gdk.Event;
with Gdk.Types;     use Gdk.Types;
with Gdk.Pixmap;    use Gdk.Pixmap;
with Gtk.Box;       use Gtk.Box;
with Gtk.Enums;     use Gtk.Enums;
with Gtk.GEntry;    use Gtk.GEntry;
with Gtk.Button;    use Gtk.Button;
with Gtk.Label;     use Gtk.Label;
with Gtkada.Types;  use Gtkada.Types;
with Gtk.Widget;    use Gtk.Widget;
with Gtk.Clist;     use Gtk.Clist;
with Gtk.Menu;      use Gtk.Menu;
with Gtk.Menu_Item; use Gtk.Menu_Item;
with Gtk.Handlers;  use Gtk.Handlers;
with Gtkada.Handlers; use Gtkada.Handlers;
with Gtk.Scrolled_Window; use Gtk.Scrolled_Window;
with Gtk.Hbutton_Box; use Gtk.Hbutton_Box;
with Gtk.Arrow;    use Gtk.Arrow;

with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Wizards;      use Wizards;
with Directory_Tree; use Directory_Tree;

package body Creation_Wizard is

   Logo_Xpm : aliased Chars_Ptr_Array (0 .. 0);
   pragma Import (C, Logo_Xpm, "logo_xpm");

   function First_Page (Wiz : access Prj_Wizard_Record'Class)
      return Gtk_Widget;
   function Second_Page (Wiz : access Prj_Wizard_Record'Class)
      return Gtk_Widget;
   function Third_Page (Wiz : access Prj_Wizard_Record'Class)
      return Gtk_Widget;
  --  Return the widget to use for any of the pages in the wizard

   procedure Add_Src_Directory
     (Wiz : access Prj_Wizard_Record'Class;
      Dir : String;
      Recursive : Boolean);
   --  Add Dir in the tree to the list of source directories associated with
   --  the project, and return the matching row in Wiz.Src_Dir_List.  If
   --  recursive is True, then all the subdirectories are also added.

   procedure Add_Src_Directory_Cb (W : access Gtk_Widget_Record'Class);
   --  Callback for the up button in the source directory selection

   procedure Add_Single_Src_Directory_Cb (W : access Gtk_Widget_Record'Class);
   --  Callback for the up button in the source directory selection.
   --  The addition is not recursive.
   --  ??? This could be merged with the above procedure if Object_Connect
   --  could use a User_Data parameter.

   procedure Remove_Src_Directory
     (Wiz : access Prj_Wizard_Record'Class; Recursive : Boolean);
   --  Remove the currently selected directory.
   --  If recursive is True, then all the subdirectories are also removed

   procedure Remove_Src_Directory_Cb (W : access Gtk_Widget_Record'Class);
   --  Callback for the down button in the source directory selection

   procedure Remove_Single_Src_Directory_Cb
     (W : access Gtk_Widget_Record'Class);
   --  Remove the currently selected directory in W.Src_Dir_List. This doesn't
   --  remove children of the directory.

   function Directory_Button_Press_Cb
     (W : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean;
   --  Callback to display the contextual menu in the directory selection
   --  (second page of the wizard).

   function Src_List_Button_Press_Cb
     (W : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean;
   --  Callback to display the contextual menu in the source directory list
   --  (second page of the wizard).

   function Is_Source_Directory
     (Wiz : access Prj_Wizard_Record'Class; Name : String) return Gint;
   --  -1 if Name is not a source directory for the project defined in Wiz.
   --  Otherwise, the index of Name in Wiz.Src_Dir_List is returned.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (Wiz : out Prj_Wizard) is
   begin
      Wiz := new Prj_Wizard_Record;
      Creation_Wizard.Initialize (Wiz);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Wiz : access Prj_Wizard_Record'Class) is
      Pix  : Gdk_Pixmap;
      Mask : Gdk_Bitmap;
   begin
      Wizards.Initialize (Wiz, "Project setup", "#0476bc");

      Create_From_Xpm_D
        (Pix, null, Get_Default_Colormap, Mask, Null_Color, Logo_Xpm);
      Add_Logo (Wiz, Pix, Mask);

      Add_Page (Wiz, First_Page (Wiz), "Naming the project");
      Add_Page (Wiz, Second_Page (Wiz), "Selecting sources");
      Add_Page (Wiz, Third_Page (Wiz), "Selecting build directory");
   end Initialize;

   ----------------
   -- First_Page --
   ----------------

   function First_Page (Wiz : access Prj_Wizard_Record'Class)
      return Gtk_Widget
   is
      Box    : Gtk_Box;
      Hbox   : Gtk_Box;
      Label  : Gtk_Label;
      Button : Gtk_Button;
   begin
      Gtk_New_Vbox (Box, Homogeneous => False, Spacing => 8);
      Set_Border_Width (Box, 5);

      Gtk_New (Label, "Creating a new project");
      Pack_Start (Box, Label, Fill => True, Expand => True);

      Gtk_New_Hbox (Hbox, Homogeneous => False, Spacing => 8);
      Pack_End (Box, Hbox, Expand => False, Fill => False);
      Gtk_New (Wiz.Project_Name, 255);
      Pack_Start (Hbox, Wiz.Project_Name, Expand => True, Fill => True);
      Gtk_New (Button, "...");
      Pack_Start (Hbox, Button, Expand => False, Fill => False);

      Gtk_New (Label, "Enter the directory where to copy the file to:");
      Pack_End (Box, Label, Expand => False, Fill => False);

      Gtk_New_Hbox (Hbox, Homogeneous => False, Spacing => 8);
      Pack_End (Box, Hbox, Expand => False, Fill => False);
      Gtk_New (Wiz.Project_Location, 255);
      Pack_Start (Hbox, Wiz.Project_Location, Expand => True, Fill => True);
      Gtk_New (Button, "...");
      Pack_Start (Hbox, Button, Expand => False, Fill => False);

      Gtk_New (Label, "Enter the name of the project to create:");
      Pack_End (Box, Label, Expand => False, Fill => False);

      return Gtk_Widget (Box);
   end First_Page;

   -----------------
   -- Second_Page --
   -----------------

   function Second_Page (Wiz : access Prj_Wizard_Record'Class)
      return Gtk_Widget
   is
      Box, Vbox : Gtk_Box;
      Bbox      : Gtk_Hbutton_Box;
      Button    : Gtk_Button;
      Scrolled  : Gtk_Scrolled_Window;
      Label     : Gtk_Label;
      Arrow     : Gtk_Arrow;
   begin
      Add_Events (Wiz, Button_Press_Mask or Button_Release_Mask);

      Gtk_New_Vbox (Vbox, Homogeneous => False, Spacing => 8);

      Gtk_New (Label, "Please select the source directories for this project");
      Pack_Start (Vbox, Label, Expand => False, Fill => False);

      Gtk_New_Vbox (Box, Homogeneous => False);
      Pack_End (Vbox, Box, Expand => True, Fill => True);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start (Box, Scrolled, Expand => True, Fill => True);

      Gtk_New (Wiz.Src_Dir_Selection, "/");
      Show_Directory (Wiz.Src_Dir_Selection, Get_Current_Dir);
      Add (Scrolled, Wiz.Src_Dir_Selection);
      Gtkada.Handlers.Return_Callback.Object_Connect
        (Wiz.Src_Dir_Selection, "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller
            (Directory_Button_Press_Cb'Access),
         Wiz);

      Gtk_New (Bbox);
      Set_Layout (Bbox, Buttonbox_Spread);
      Pack_Start (Box, Bbox, Expand => False, Fill => False);

      Gtk_New (Button);
      Gtk_New (Arrow, Arrow_Down, Shadow_In);
      Add (Button, Arrow);
      Pack_Start (Bbox, Button, Expand => False, Fill => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Add_Src_Directory_Cb'Access),
         Wiz);

      Gtk_New (Button);
      Gtk_New (Arrow, Arrow_Up, Shadow_In);
      Add (Button, Arrow);
      Pack_Start (Bbox, Button, Expand => False, Fill => False);
      Widget_Callback.Object_Connect
        (Button, "clicked",
         Widget_Callback.To_Marshaller (Remove_Src_Directory_Cb'Access),
         Wiz);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start (Box, Scrolled, Expand => True, Fill => True);

      Gtk_New (Wiz.Src_Dir_List, Columns => 1);
      Add (Scrolled, Wiz.Src_Dir_List);
      Set_Selection_Mode (Wiz.Src_Dir_List, Selection_Extended);
      Gtkada.Handlers.Return_Callback.Object_Connect
        (Wiz.Src_Dir_List, "button_press_event",
         Gtkada.Handlers.Return_Callback.To_Marshaller
            (Src_List_Button_Press_Cb'Access),
         Wiz);

      --  ??? This is a workaround for a horizontal scrollbar problem: When the
      --  ctree is put in a scrolled window, and if this is not called, the
      --  scrollbar does not allow us to scroll as far right as possible...
      Set_Column_Auto_Resize (Wiz.Src_Dir_Selection, 0, True);
      Set_Column_Auto_Resize (Wiz.Src_Dir_List, 0, True);

      return Gtk_Widget (Vbox);
   end Second_Page;

   ----------------
   -- Third_Page --
   ----------------

   function Third_Page (Wiz : access Prj_Wizard_Record'Class)
      return Gtk_Widget
   is
      Vbox : Gtk_Box;
      Label : Gtk_Label;
      Scrolled  : Gtk_Scrolled_Window;
   begin
      Gtk_New_Vbox (Vbox, Homogeneous => False);

      Gtk_New (Label, "Please select the build directory for this project");
      Pack_Start (Vbox, Label, Expand => False, Fill => False);

      Gtk_New (Scrolled);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start (Vbox, Scrolled, Expand => True, Fill => True);

      Gtk_New (Wiz.Obj_Dir_Selection, "/");
      Show_Directory (Wiz.Obj_Dir_Selection, Get_Current_Dir);
      Add (Scrolled, Wiz.Obj_Dir_Selection);

      return Gtk_Widget (Vbox);
   end Third_Page;

   -------------------------
   -- Is_Source_Directory --
   -------------------------

   function Is_Source_Directory
     (Wiz : access Prj_Wizard_Record'Class; Name : String) return Gint
   is
      Num_Rows : constant Gint := Get_Rows (Wiz.Src_Dir_List);
   begin
      --  Check if the directory is already there
      for J in 0 .. Num_Rows - 1 loop
         if Get_Text (Wiz.Src_Dir_List, J, 0) = Name then
            return J;
         end if;
      end loop;
      return -1;
   end Is_Source_Directory;

   -----------------------
   -- Add_Src_Directory --
   -----------------------

   procedure Add_Src_Directory
     (Wiz : access Prj_Wizard_Record'Class;
      Dir : String;
      Recursive : Boolean)
   is
      Row : Gint;
      Handle : Dir_Type;
      File : String (1 .. 255);
      Last : Natural;
   begin
      Row := Is_Source_Directory (Wiz, Dir);
      if Row = -1 then
         Row := Append (Wiz.Src_Dir_List, Null_Array + Dir);
      end if;
      Select_Row (Wiz.Src_Dir_List, Row, -1);

      if Recursive then
         Open (Handle, Dir);
         loop
            Read (Handle, File, Last);
            exit when Last = 0;

            --  ??? Should share filter with Directory_Tree
            if File (File'First .. Last) /= "."
              and then File (File'First .. Last) /= ".."
              and then Is_Directory (Dir & File (File'First .. Last))
            then
               Add_Src_Directory
                 (Wiz, Dir & File (1 .. Last) & Directory_Separator, True);
            end if;
         end loop;

         Close (Handle);
      end if;
   end Add_Src_Directory;

   --------------------------
   -- Remove_Src_Directory --
   --------------------------

   procedure Remove_Src_Directory
     (Wiz : access Prj_Wizard_Record'Class; Recursive : Boolean)
   is
      use type Gint_List.Glist;
      List : Gint_List.Glist := Get_Selection (Wiz.Src_Dir_List);
      Next : Gint_List.Glist;
      Num  : Guint := Gint_List.Length (List);
   begin
      --  Add the directories recursively to the selection (we can't remove
      --  them right away, since this would cancel the current selection and
      --  thus we wouldn't be able to remove all the user-selected ones).

      if Recursive then
         for J in 1 .. Num loop
            declare
               Row : constant Gint := Gint_List.Get_Data (List);
               Dir : constant String := Get_Text (Wiz.Src_Dir_List, Row, 0);
            begin
               for J in 0 .. Get_Rows (Wiz.Src_Dir_List) - 1 loop
                  declare
                     N : constant String := Get_Text (Wiz.Src_Dir_List, J, 0);
                  begin
                     if N'Length > Dir'Length
                       and then N (N'First .. N'First + Dir'Length - 1) = Dir
                     then
                        Select_Row (Wiz.Src_Dir_List, J, -1);
                     end if;
                  end;
               end loop;
            end;
            List := Gint_List.Next (List);
         end loop;
      end if;

      --  Now remove the whole selection

      List := Get_Selection (Wiz.Src_Dir_List);
      while List /= Gint_List.Null_List loop
         Next := Gint_List.Next (List);
         Remove (Wiz.Src_Dir_List, Gint_List.Get_Data (List));
         List := Next;
      end loop;
   end Remove_Src_Directory;

   --------------------------
   -- Add_Src_Directory_Cb --
   --------------------------

   procedure Add_Src_Directory_Cb (W : access Gtk_Widget_Record'Class) is
      Wiz : Prj_Wizard := Prj_Wizard (W);
   begin
      Freeze (Wiz.Src_Dir_List);
      Add_Src_Directory (Wiz, Get_Selection (Wiz.Src_Dir_Selection), True);
      Sort (Wiz.Src_Dir_List);
      Thaw (Wiz.Src_Dir_List);
   end Add_Src_Directory_Cb;

   ---------------------------------
   -- Add_Single_Src_Directory_Cb --
   ---------------------------------

   procedure Add_Single_Src_Directory_Cb
     (W : access Gtk_Widget_Record'Class)
   is
      Wiz : Prj_Wizard := Prj_Wizard (W);
   begin
      Add_Src_Directory (Wiz, Get_Selection (Wiz.Src_Dir_Selection), False);
      Sort (Wiz.Src_Dir_List);
   end Add_Single_Src_Directory_Cb;

   -----------------------------
   -- Remove_Src_Directory_Cb --
   -----------------------------

   procedure Remove_Src_Directory_Cb (W : access Gtk_Widget_Record'Class) is
      Wiz : Prj_Wizard := Prj_Wizard (W);
   begin
      Freeze (Wiz.Src_Dir_List);
      Remove_Src_Directory (Wiz, Recursive => True);
      Thaw (Wiz.Src_Dir_List);
   end Remove_Src_Directory_Cb;

   ------------------------------------
   -- Remove_Single_Src_Directory_Cb --
   ------------------------------------

   procedure Remove_Single_Src_Directory_Cb
     (W : access Gtk_Widget_Record'Class) is
   begin
      Remove_Src_Directory (Prj_Wizard (W), Recursive => False);
   end Remove_Single_Src_Directory_Cb;

   -------------------------------
   -- Directory_Button_Press_Cb --
   -------------------------------

   function Directory_Button_Press_Cb
     (W : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean
   is
      Wiz  : Prj_Wizard := Prj_Wizard (W);
      Item : Gtk_Menu_Item;
      Selected_Row, Selected_Col : Gint;
      Is_Valid : Boolean;

   begin
      if Get_Button (Event) = 3
        and then Get_Event_Type (Event) = Button_Press
      then
         if Wiz.Dir_Contextual_Menu /= null then
            Destroy (Wiz.Dir_Contextual_Menu);
         end if;

         Get_Selection_Info
           (Wiz.Src_Dir_Selection,
            Gint (Get_X (Event)), Gint (Get_Y (Event)),
            Selected_Row, Selected_Col, Is_Valid);

         if Is_Valid then
            Unselect_All (Wiz.Src_Dir_Selection);
            Select_Row (Wiz.Src_Dir_Selection, Selected_Row, Selected_Col);

            Gtk_New (Wiz.Dir_Contextual_Menu);
            Gtk_New (Item, "Add directory recursive");
            Widget_Callback.Object_Connect
              (Item, "activate",
               Widget_Callback.To_Marshaller (Add_Src_Directory_Cb'Access),
               Wiz);
            Append (Wiz.Dir_Contextual_Menu, Item);

            Gtk_New (Item, "Add directory");
            Widget_Callback.Object_Connect
              (Item, "activate",
               Widget_Callback.To_Marshaller
                 (Add_Single_Src_Directory_Cb'Access),
               Wiz);
            Append (Wiz.Dir_Contextual_Menu, Item);

            Show_All (Wiz.Dir_Contextual_Menu);

            Popup (Wiz.Dir_Contextual_Menu,
                   Button        => Gdk.Event.Get_Button (Event),
                   Activate_Time => Gdk.Event.Get_Time (Event));
            Emit_Stop_By_Name (Wiz.Src_Dir_Selection, "button_press_event");
            return True;
         end if;
      end if;

      return False;
   end Directory_Button_Press_Cb;

   ------------------------------
   -- Src_List_Button_Press_Cb --
   ------------------------------

   function Src_List_Button_Press_Cb
     (W : access Gtk_Widget_Record'Class;
      Event  : Gdk.Event.Gdk_Event) return Boolean
   is
      Wiz  : Prj_Wizard := Prj_Wizard (W);
      Item : Gtk_Menu_Item;
      Selected_Row, Selected_Col : Gint;
      Is_Valid : Boolean;

   begin
      if Get_Button (Event) = 3
        and then Get_Event_Type (Event) = Button_Press
      then
         if Wiz.Src_Dir_Contextual_Menu /= null then
            Destroy (Wiz.Src_Dir_Contextual_Menu);
         end if;

         Get_Selection_Info
           (Wiz.Src_Dir_List,
            Gint (Get_X (Event)), Gint (Get_Y (Event)),
            Selected_Row, Selected_Col, Is_Valid);

         if Is_Valid then
            Unselect_All (Wiz.Src_Dir_List);
            Select_Row (Wiz.Src_Dir_List, Selected_Row, Selected_Col);

            Gtk_New (Wiz.Src_Dir_Contextual_Menu);
            Gtk_New (Item, "Remove directory recursive");
            Widget_Callback.Object_Connect
              (Item, "activate",
               Widget_Callback.To_Marshaller (Remove_Src_Directory_Cb'Access),
               Wiz);
            Append (Wiz.Src_Dir_Contextual_Menu, Item);

            Gtk_New (Item, "Remove directory");
            Widget_Callback.Object_Connect
              (Item, "activate",
               Widget_Callback.To_Marshaller
                 (Remove_Single_Src_Directory_Cb'Access),
               Wiz);
            Append (Wiz.Src_Dir_Contextual_Menu, Item);

            Show_All (Wiz.Src_Dir_Contextual_Menu);

            Popup (Wiz.Src_Dir_Contextual_Menu,
                   Button        => Gdk.Event.Get_Button (Event),
                   Activate_Time => Gdk.Event.Get_Time (Event));
            Emit_Stop_By_Name (Wiz.Src_Dir_List, "button_press_event");
            return True;
         end if;
      end if;

      return False;
   end Src_List_Button_Press_Cb;

end Creation_Wizard;
