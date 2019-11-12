------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------
with Ada.Exceptions;                   use Ada.Exceptions;
with GNATCOLL.Utils;                   use GNATCOLL.Utils;

with Glib;                             use Glib;
with Glib.Object;                      use Glib.Object;
with Glib_Values_Utils;                use Glib_Values_Utils;

with Gdk.Event;                        use Gdk.Event;
with Gdk.Types;                        use Gdk.Types;
with Gdk.Types.Keysyms;                use Gdk.Types.Keysyms;

with GPS.Kernel.Preferences;           use GPS.Kernel.Preferences;

with Gtk.Assistant;                    use Gtk.Assistant;
with Gtk.Box;                          use Gtk.Box;
with Gtk.Button;                       use Gtk.Button;
with Gtk.Combo_Box_Text;               use Gtk.Combo_Box_Text;
with Gtk.Enums;                        use Gtk.Enums;
with Gtk.GEntry;                       use Gtk.GEntry;
with Gtk.Label;                        use Gtk.Label;
with Gtk.Main;
with Gtk.Paned;                        use Gtk.Paned;
with Gtk.Widget;                       use Gtk.Widget;

with Gtkada.File_Selector;             use Gtkada.File_Selector;

with Gtk.Scrolled_Window;              use Gtk.Scrolled_Window;
with Gtk.Tree_Model;                   use Gtk.Tree_Model;
with Gtk.Tree_View;                    use Gtk.Tree_View;
with Gtk.Tree_Selection;               use Gtk.Tree_Selection;
with Gtk.Tree_Store;                   use Gtk.Tree_Store;
with Gtk.Tree_View_Column;             use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;           use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;         use Gtk.Cell_Renderer_Pixbuf;

with Gtkada.Handlers;                  use Gtkada.Handlers;

with Dialog_Utils;                     use Dialog_Utils;

package body Project_Templates.GUI is

   Name_Col        : constant := 0;
   Icon_Name_Col   : constant := 1;
   Num_Col         : constant := 2;
   Desc_Col        : constant := 3;

   --  This constant is used to carry information about the project template
   --  in the tree view.
   --  Num_Col : Index to the pages array of the GtkAssistant
   Column_Types : constant GType_Array :=
     (Name_Col         => GType_String,
      Icon_Name_Col    => GType_String,
      Num_Col          => GType_Int,
      Desc_Col         => GType_String);

   type Variable_Widget_Record is record
      Name   : Unbounded_String;
      Widget : Gtk_Widget;
   end record;

   function Get_Text (V : Variable_Widget_Record) return String
   is
     (if V.Widget = null then
         ""
      elsif V.Widget.all in Gtk_Combo_Box_Text_Record'Class then
         Gtk_Combo_Box_Text (V.Widget).Get_Active_Text
      else
         Gtk_Entry (V.Widget).Get_Text);

   package Variable_Widgets is new Ada.Containers.Doubly_Linked_Lists
     (Variable_Widget_Record);

   -------------------
   -- Template page --
   -------------------

   type Template_Page_Record is new Dialog_View_Record with record
      Template      : Template_Script_Object;
      Var_Widgets   : Variable_Widgets.List;
      Browse_Button : Gtk_Button;
      Location_Ent  : Gtk_Entry;
   end record;
   type Template_Page is access all Template_Page_Record'Class;

   procedure Gtk_New
     (Widget   : out Template_Page;
      Template : Template_Script_Object);
   procedure Initialize
     (Widget   : access Template_Page_Record'Class;
      Template : Template_Script_Object);
   --  Initialization functions

   function Is_Complete
     (Page : access Template_Page_Record'Class) return Boolean;
   --  Return True if the page is complete

   function Get_Assignments
     (Page : access Template_Page_Record'Class)
      return Variable_Assignments.Map;
   --  Return the assignments entered by the user in Page

   procedure On_Browse_Clicked
     (Self : access Glib.Object.GObject_Record'Class);
   --  Called when the 'Browse' button is clicked

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget   : out Template_Page;
      Template : Template_Script_Object) is
   begin
      Widget := new Template_Page_Record;
      Initialize (Widget, Template);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget   : access Template_Page_Record'Class;
      Template : Template_Script_Object)
   is
      use Variables_List;
      C            : Cursor;
      Group_Widget : Dialog_Group_Widget;

      procedure Create_Var_Widget
        (Var : Variable);
      --  Create one widget to represent Var

      -----------------------
      -- Create_Var_Widget --
      -----------------------

      procedure Create_Var_Widget
        (Var : Variable)
      is
         Var_Widget : Variable_Widget_Record;

         function M (From : Character) return Character;
         --  Mapping function. Where are my lambda functions?

         -------
         -- M --
         -------

         function M (From : Character) return Character is
         begin
            if From = '_' then
               return ' ';
            end if;

            return From;
         end M;

      begin
         Var_Widget.Name := Var.Label;

         if Var.Nb_Choices = 0 then
            declare
               Ent : Gtk_Entry;
            begin
               Gtk_New (Ent);
               Set_Text (Ent, To_String (Var.Default_Value));
               Var_Widget.Widget := Gtk_Widget (Ent);
            end;
         else
            declare
               Combo         : Gtk_Combo_Box_Text;
               Default_Index : Gint := 0;
               J             : Gint := 0;
            begin
               Gtk_New (Combo);

               for Choice of Var.Choices loop
                  Combo.Append_Text (To_String (Choice));

                  if Var.Default_Value = Choice then
                     Default_Index := J;
                  end if;

                  J := J + 1;
               end loop;

               Combo.Set_Active (Default_Index);
               Var_Widget.Widget := Gtk_Widget (Combo);
            end;
         end if;

         Widget.Var_Widgets.Append (Var_Widget);

         Group_Widget.Create_Child
           (Var_Widget.Widget,
            Label     => To_String
              (Translate (Var.Label, M'Unrestricted_Access)),
            Doc       => To_String (Var.Description),
            Expand    => False);
      end Create_Var_Widget;

   begin
      Dialog_Utils.Initialize (Widget);
      Widget.Template := Template;

      --  Create the 'Location' group widget

      Group_Widget := new Dialog_Group_Widget_Record;
      Initialize
        (Group_Widget,
         Parent_View         => Widget,
         Group_Name          => "Location",
         Allow_Multi_Columns => False);

      --  Create the location entry and its associated 'Browse' button

      Gtk_New (Widget.Location_Ent);
      Widget.Location_Ent.Set_Text (+Get_Current_Dir.Full_Name.all);

      Gtk_New (Widget.Browse_Button, "Browse");
      Widget.Browse_Button.On_Clicked
        (On_Browse_Clicked'Access,
         Slot => Widget);

      Group_Widget.Create_Child
        (Widget    => Widget.Location_Ent,
         Label     => "Deploy project in",
         Doc       => "The location of the project to create.",
         Button    => Widget.Browse_Button,
         Expand    => True,
         Fill      => True);

      --  Create the 'Settings' group widget
      Group_Widget := new Dialog_Group_Widget_Record;
      Initialize
        (Group_Widget,
         Parent_View  => Widget,
         Group_Name   => "Settings");

      --  Create the fields for variables.
      C := Template.Project.Variables.First;

      while Has_Element (C) loop
         Create_Var_Widget (Element (C));
         Next (C);
      end loop;
   end Initialize;

   -----------------
   -- Is_Complete --
   -----------------

   function Is_Complete
     (Page : access Template_Page_Record'Class) return Boolean
   is
      use Variable_Widgets;
      C : Cursor;
   begin
      C := Page.Var_Widgets.First;

      while Has_Element (C) loop
         if Get_Text (Element (C)) = "" then
            return False;
         end if;
         Next (C);
      end loop;

      return True;
   end Is_Complete;

   ---------------------
   -- Get_Assignments --
   ---------------------

   function Get_Assignments
     (Page : access Template_Page_Record'Class)
      return Variable_Assignments.Map
   is
      use Variable_Widgets;
      C : Cursor;
      R : Variable_Assignments.Map;
      V : Variable_Widget_Record;
   begin
      C := Page.Var_Widgets.First;

      while Has_Element (C) loop
         V := Element (C);
         R.Insert (V.Name, To_Unbounded_String (Get_Text (V)));

         Next (C);
      end loop;

      return R;
   end Get_Assignments;

   -----------------------
   -- On_Browse_Clicked --
   -----------------------

   procedure On_Browse_Clicked
     (Self : access Glib.Object.GObject_Record'Class)
   is
      Page_Widget : constant Template_Page := Template_Page (Self);
      Dir         : constant Virtual_File := Select_Directory
        (Base_Directory    =>
           Create_From_UTF8 (Page_Widget.Location_Ent.Get_Text),
         Parent            => Gtk_Window (Get_Toplevel (Page_Widget)),
         Use_Native_Dialog => Use_Native_Dialogs.Get_Pref);
   begin
      if Dir /= GNATCOLL.VFS.No_File then
         Page_Widget.Location_Ent.Set_Text (Dir.Display_Full_Name);
      end if;
   end On_Browse_Clicked;

   ----------------------
   -- Install_Template --
   ----------------------

   procedure Install_Template
     (Templates     : Templates_Script_Objects_List.List;
      Parent        : not null access Gtk_Window_Record'Class;
      Chosen        : out Template_Script_Object;
      Installed     : out Boolean;
      Dir           : out Virtual_File;
      Project       : out Virtual_File;
      Errors        : out Unbounded_String;
      Default_Label : String := "")
   is
      Assistant    : Gtk_Assistant;

      Scroll       : Gtk_Scrolled_Window;
      Tree         : Gtk_Tree_View;
      Model        : Gtk_Tree_Store;
      Iter         : Gtk_Tree_Iter;
      Default_Path : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Col          : Gtk_Tree_View_Column;
      Rend         : Gtk_Cell_Renderer_Text;
      Pix          : Gtk_Cell_Renderer_Pixbuf;

      Col_Num      : Gint;
      Page_Num     : Gint;
      pragma Unreferenced (Col_Num, Page_Num);
      --  Those variables are used only to store the return value of C
      --  functions and are not actually used by the Ada code.

      Page_Box     : Gtk_Box;
      Descr_Box    : Gtk_Box;
      Hpane        : Gtk_Hpaned;
      Main_Label   : Gtk_Label;
      Bottom_Label : Gtk_Label;

      Next_Page_Number : Gint;

      use Templates_Script_Objects_List;
      C : Cursor;

      package Forwarder is new Set_Forward_Page_Func_User_Data (Boolean);

      function Next_Page
        (Current_Page : Gint; User_Data : Boolean) return Gint;
      --  Next page function

      procedure Add_Template
        (Template : Template_Script_Object);
      --  Add one page to the wizard, and add the template to the tree model

      function Get_Or_Create_Category
        (Category : Unbounded_String) return Gtk_Tree_Iter;
      --  Return the iter for Category. Create it if necessary

      procedure Selection_Changed (Widget : access GObject_Record'Class);
      --  Called when the selection changes in the main tree

      procedure Cancelled (Widget : access GObject_Record'Class);
      --  Press on the "Cancel" button

      function On_Key_Press
        (Widget : access GObject_Record'Class;
         Event  : Gdk_Event) return Boolean;
      --  Callback on a key press

      procedure On_Apply_Assistant (Widget : access GObject_Record'Class);
      --  Press on the "Apply" button

      procedure Check_Completeness (Object : access GObject_Record'Class);
      --  Callback on a 'changed' on an entry in a template page

      ---------------
      -- Next_Page --
      ---------------

      function Next_Page
        (Current_Page : Gint; User_Data : Boolean) return Gint is
         pragma Unreferenced (User_Data);
      begin
         if Current_Page = 0 and then Next_Page_Number > 0 then
            --  We are on the initial page containing the tree view.
            return Next_Page_Number;
         else
            --  We are in the page flow of the project template.
            return Current_Page - 1;
         end if;

      exception
         when E : others =>
            Errors := Errors & ASCII.LF & Exception_Information (E);
            return 0;
      end Next_Page;

      ----------------------------
      -- Get_Or_Create_Category --
      ----------------------------

      function Get_Or_Create_Category
        (Category : Unbounded_String) return Gtk_Tree_Iter
      is
         Splits : constant Unbounded_String_Array :=
           Split (To_String (Category), '/');

         function Find_Child
           (Iter : Gtk_Tree_Iter;
            Name : String) return Gtk_Tree_Iter;
         --  Find iter with Name in children of Iter. Create it if necessary.

         ----------------
         -- Find_Child --
         ----------------

         function Find_Child
           (Iter : Gtk_Tree_Iter;
            Name : String) return Gtk_Tree_Iter
         is
            Child        : Gtk_Tree_Iter;
            Insert_After : Gtk_Tree_Iter := Null_Iter;

         begin
            if Iter = Null_Iter then
               Child := Model.Get_Iter_First;
            else
               Child := Children (Model, Iter);
            end if;

            while Child /= Null_Iter loop
               if Get_String (Model, Child, Name_Col) < Name then
                  Insert_After := Child;
               end if;

               if Get_String (Model, Child, Name_Col) = Name then
                  return Child;
               end if;

               Next (Model, Child);
            end loop;

            --  If we reach this point, child was not found: add it
            Model.Insert_After (Child, Iter, Insert_After);

            --  Populate the iter that we have just added
            Set_All_And_Clear
              (Model, Child,
               (Name_Col        => As_String (Name),
                Icon_Name_Col   => As_String ("gps-emblem-directory-symbolic"),
                Num_Col         => As_Int    (-1),
                Desc_Col        => As_String ("")));

            return Child;
         end Find_Child;

         Iter : Gtk_Tree_Iter := Null_Iter;

      begin
         for J in Splits'Range loop
            Iter := Find_Child (Iter, To_String (Splits (J)));
         end loop;

         return Iter;
      end Get_Or_Create_Category;

      ------------------------
      -- Check_Completeness --
      ------------------------

      procedure Check_Completeness (Object : access GObject_Record'Class) is
         Page : constant Template_Page := Template_Page (Object);
      begin
         Assistant.Set_Page_Complete (Page, Page.Is_Complete);

      exception
         when E : others =>
            Errors := Errors & ASCII.LF & Exception_Information (E);
      end Check_Completeness;

      ------------------
      -- Add_Template --
      ------------------

      procedure Add_Template
        (Template : Template_Script_Object) is

         Cat            : Gtk_Tree_Iter;
         Iter           : Gtk_Tree_Iter;
         Child          : Gtk_Tree_Iter;
         Page_Num       : Gint;
         Page           : Template_Page;
         Nb_Added_Pages : Gint := 0;

      begin

         --  Create the page
         Gtk_New (Page, Template);

         --  In the case when we have a python script we can let it
         --  create its own pages. However, we add the final page in all cases.

         Page_Num := Append_Page (Assistant, Page);
         Assistant.Set_Page_Type (Page, Gtk_Assistant_Page_Confirm);
         Nb_Added_Pages := Nb_Added_Pages + 1;

         if Template.Project.Python_Script /= No_File then
            declare
               Object : Script_Object := Template.Object;
               Nb_Added_Custom_Pages : constant Gint
                  := Object.Add_Pages (Assistant);
            begin
               Nb_Added_Pages := Nb_Added_Pages + Nb_Added_Custom_Pages;

               --  First page of the page flow is the first page added.
               --  The pages are stored in a stack growing downward, thus, we
               --  add the number of created pages to get the first page of the
               --  page flow.
               Page_Num := Page_Num + Nb_Added_Custom_Pages;
            end;
         end if;

         --  Connect the change of entry text to a function that checks the
         --  completeness of the page
         declare
            use Variable_Widgets;
            C : Variable_Widgets.Cursor;
         begin
            C := Page.Var_Widgets.First;

            while Has_Element (C) loop
               Object_Callback.Object_Connect
                 (Element (C).Widget, "changed",
                  Check_Completeness'Unrestricted_Access,
                  Slot_Object => Page);
               Next (C);
            end loop;
         end;

         --  Compute the initial "complete" state
         Assistant.Set_Page_Complete (Page, Is_Complete (Page));

         --  Add the template to the tree model
         Cat := Get_Or_Create_Category (Template.Project.Category);

         Child := Model.Children (Cat);

         while Child /= Null_Iter
           and then Get_String (Model, Child, Name_Col)
           < To_String (Template.Project.Label)
         loop
            Model.Next (Child);
         end loop;

         Model.Insert_Before (Iter, Cat, Child);

         if Template.Project.Label = Default_Label then
            Default_Path := Model.Get_Path (Iter);
         end if;

         Set_All_And_Clear
           (Model, Iter,
            (Name_Col         => As_String (To_String
                                             (Template.Project.Label)),
             Icon_Name_Col    => As_String ("gps-run-symbolic"),
             Num_Col          => As_Int    (Page_Num),
             Desc_Col         => As_String
                                 (To_String (Template.Project.Description))));
      end Add_Template;

      -----------------------
      -- Selection_Changed --
      -----------------------

      procedure Selection_Changed (Widget : access GObject_Record'Class) is
         pragma Unreferenced (Widget);
         Iter  : Gtk_Tree_Iter;
         Dummy : Gtk_Tree_Model;
      begin
         Get_Selected (Get_Selection (Tree), Dummy, Iter);

         if Iter = Null_Iter then
            Next_Page_Number := 0;
         else
            Main_Label.Set_Text (Model.Get_String (Iter, Desc_Col));
            Next_Page_Number := Model.Get_Int (Iter, Num_Col);
         end if;

         if Next_Page_Number <= 0 then
            Main_Label.Set_Text ("No template selected.");
            Assistant.Set_Page_Complete (Page_Box, False);
         else
            Assistant.Set_Page_Complete (Page_Box, True);
         end if;
      exception
         when E : others =>
            Errors := Errors & ASCII.LF & Exception_Information (E);
      end Selection_Changed;

      ---------------
      -- Cancelled --
      ---------------

      procedure Cancelled (Widget : access GObject_Record'Class) is
         pragma Unreferenced (Widget);
      begin
         Installed := False;
         Dir := No_File;
         Gtk.Main.Main_Quit;
      exception
         when E : others =>
            Errors := Errors & ASCII.LF & Exception_Information (E);
            Gtk.Main.Main_Quit;
      end Cancelled;

      ------------------
      -- On_Key_Press --
      ------------------

      function On_Key_Press
        (Widget : access GObject_Record'Class;
         Event  : Gdk_Event) return Boolean
      is
         Current_Page : Gint;

      begin
         case Get_Key_Val (Event) is
            when GDK_Escape =>
               Cancelled (Widget);
               return True;

            when GDK_Return | GDK_KP_Enter =>
               Current_Page := Assistant.Get_Current_Page;

               if Assistant.Get_Page_Complete
                 (Assistant.Get_Nth_Page (Current_Page))
               then
                  if Current_Page <= 0 then
                     Assistant.Set_Current_Page
                       (Next_Page (Current_Page, False));
                  else
                     On_Apply_Assistant (Widget);
                  end if;
               end if;

               return True;

            when others =>
               null;
         end case;

         return False;

      exception
         when E : others =>
            Errors := Errors & ASCII.LF & Exception_Information (E);
            return False;
      end On_Key_Press;

      ------------------------
      -- On_Apply_Assistant --
      ------------------------

      procedure On_Apply_Assistant (Widget : access GObject_Record'Class) is
         pragma Unreferenced (Widget);
         Page : Template_Page;
      begin
         --  We are pressing "Apply" here: install the template
         Page := Template_Page
            (Assistant.Get_Nth_Page (Assistant.Get_Current_Page));

         Dir  := Create (+Page.Location_Ent.Get_Text);

         Instantiate_Template
           (Template    => Page.Template.Project,
            Target_Dir  => Dir,
            Assignments => Page.Get_Assignments,
            Project     => Project,
            Errors      => Errors);

         Installed := True;

         Chosen := Page.Template;

         Gtk.Main.Main_Quit;
      exception
         when E : others =>
            Errors := Errors & ASCII.LF & Exception_Information (E);
            Gtk.Main.Main_Quit;
      end On_Apply_Assistant;

   begin
      Chosen := (Project => Null_Project_Template,
                 Object  => Null_Script_Object);
      Installed := False;

      Gtk_New (Assistant);
      Assistant.Set_Name ("Project Templates Assistant");
      Assistant.Set_Modal (True);
      Assistant.Set_Transient_For (Parent);

      Gtk_New_Vbox (Page_Box, Homogeneous => False);
      Page_Num := Assistant.Append_Page (Page_Box);
      Assistant.Set_Page_Type (Page_Box, Gtk_Assistant_Page_Intro);

      Gtk_New_Hpaned (Hpane);
      Page_Box.Pack_Start (Hpane, Expand => True, Fill => True);

      Gtk_New
        (Bottom_Label,
         "You can modify all the project properties "
         & "later via the Project/Properties... menu.");
      Bottom_Label.Set_Alignment (0.0, 0.5);
      Page_Box.Pack_Start (Bottom_Label, Expand => False, Padding => 5);

      Gtk_New (Scroll);
      Set_Policy (Scroll, Policy_Automatic, Policy_Automatic);
      Add1 (Hpane, Scroll);

      Gtk_New_Vbox (Descr_Box);
      Add2 (Hpane, Descr_Box);

      Gtk_New (Main_Label);
      Pack_Start (Descr_Box, Main_Label, True, True, 3);

      Set_Position (Hpane, 200);

      --  Create the tree and model
      Gtk_New (Model, Column_Types);
      Gtk_New (Tree, Model);
      Add (Scroll, Tree);
      Tree.Set_Headers_Visible (False);
      Tree.Set_Show_Expanders (True);

      Gtk_New (Col);

      Gtk_New (Pix);
      Pack_Start (Col, Pix, False);
      Add_Attribute (Col, Pix, "icon-name", Icon_Name_Col);

      Gtk_New (Rend);
      Pack_Start (Col, Rend, False);
      Add_Attribute (Col, Rend, "text", Name_Col);

      Col_Num := Tree.Append_Column (Col);

      --  Create the pages to select the variables in each template. We create
      --  one page for each template. This way:
      --    - the notebook will be sized properly, big enough to contain the
      --      template that has the most variables, so as to avoid unwanted
      --      resizing after selecting a template
      --    - as an added value, the widget will remember values entered by
      --      an user in all pages, so if the user goes to template A, fills in
      --      some values, then goes to template B, then back to template A,
      --      then all values filled in A will be remembered.

      C := Templates.First;

      while Has_Element (C) loop
         Add_Template (Element (C));
         Next (C);
      end loop;

      --  Connect some signals

      Object_Callback.Connect
        (Get_Selection (Tree),
         Gtk.Tree_Selection.Signal_Changed,
         Selection_Changed'Unrestricted_Access,
         After => True);

      Object_Callback.Connect
        (Assistant, "cancel", Cancelled'Unrestricted_Access);

      Object_Return_Callback.Connect
        (Assistant, "key_press_event", Object_Return_Callback.To_Marshaller
           (On_Key_Press'Unrestricted_Access));

      Object_Callback.Connect
        (Assistant, "apply", On_Apply_Assistant'Unrestricted_Access);

      Forwarder.Set_Forward_Page_Func
        (Assistant, Next_Page'Access, True);

      --  Launch the assistant

      Set_Default_Size (Assistant, 600, 350);

      --  Expand the tree and select the default template or the first leaf
      --  if there is no default template.
      Expand_All (Tree);

      if Default_Path /= Null_Gtk_Tree_Path then
         Tree.Get_Selection.Select_Path (Default_Path);
         Path_Free (Default_Path);

      else
         Iter := Model.Get_Iter_First;

         while Model.Has_Child (Iter) loop
            Iter := Model.Children (Iter);
         end loop;

         if Iter /= Null_Iter then
               Get_Selection (Tree).Select_Iter (Iter);
         end if;
      end if;

      Show_All (Assistant);
      Assistant.Set_Title ("Create Project from Template");

      --  Launch a main loop: we do not want to leave this procedure while the
      --  assistant is running.
      Gtk.Main.Main;

      Assistant.Destroy;
   end Install_Template;

end Project_Templates.GUI;
