------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2016, AdaCore                     --
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

with Ada.Exceptions; use Ada.Exceptions;

with Glib; use Glib;
with Glib.Object; use Glib.Object;

with Gdk.Event;         use Gdk.Event;
with Gdk.Types;         use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;

with Gtk.Assistant;            use Gtk.Assistant;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.File_Chooser;         use Gtk.File_Chooser;
with Gtk.File_Chooser_Button;  use Gtk.File_Chooser_Button;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Label;                use Gtk.Label;
with Gtk.Main;
with Gtk.Paned;                use Gtk.Paned;
with Gtk.Size_Group;           use Gtk.Size_Group;
with Gtk.Widget;               use Gtk.Widget;

with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;

with Gtkada.Handlers;          use Gtkada.Handlers;

with GNATCOLL.Utils; use GNATCOLL.Utils;

package body Project_Templates.GUI is

   Name_Col : constant := 0;
   Icon_Name_Col : constant := 1;
   Num_Col  : constant := 2;
   Desc_Col : constant := 3;

   function Column_Types return GType_Array;
   --  Return the types to use for the templates tree model.

   type Variable_Widget_Record is record
      Name : Unbounded_String;
      Ent  : Gtk_Entry;
   end record;

   package Variable_Widgets is new Ada.Containers.Doubly_Linked_Lists
     (Variable_Widget_Record);

   -------------------
   -- Template page --
   -------------------

   type Template_Page_Record is new Gtk.Box.Gtk_Vbox_Record with record
      Template    : Project_Template;
      Var_Widgets : Variable_Widgets.List;
      Chooser     : Gtk_File_Chooser_Button;
   end record;
   type Template_Page is access all Template_Page_Record'Class;

   procedure Gtk_New
     (Widget   : out Template_Page;
      Template : Project_Template);
   procedure Initialize
     (Widget   : access Template_Page_Record'Class;
      Template : Project_Template);
   --  Initialization functions

   function Is_Complete
     (Page : access Template_Page_Record'Class) return Boolean;
   --  Return True if the page is complete

   function Get_Assignments
     (Page : access Template_Page_Record'Class)
      return Variable_Assignments.Map;
   --  Return the assignments entered by the user in Page

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Widget   : out Template_Page;
      Template : Project_Template) is
   begin
      Widget := new Template_Page_Record;
      Initialize (Widget, Template);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget   : access Template_Page_Record'Class;
      Template : Project_Template)
   is
      use Variables_List;
      C     : Cursor;
      Group : Gtk_Size_Group;

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
         Label      : Gtk_Label;
         Box        : Gtk_Box;
         Main_Box   : Gtk_Box;
         Label_Box  : Gtk_Box;

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
         Gtk_New (Var_Widget.Ent);
         Set_Text (Var_Widget.Ent, To_String (Var.Default_Value));
         Widget.Var_Widgets.Append (Var_Widget);

         Gtk_New (Label, To_String
                  (Translate (Var.Label, M'Unrestricted_Access)) & ":");
         Gtk_New_Hbox (Box);
         Set_Justify (Label, Justify_Left);
         Gtk_New_Hbox (Label_Box);
         Pack_Start (Label_Box, Label, False, False, 3);
         Group.Add_Widget (Label_Box);
         Pack_Start (Box, Label_Box, False, False, 5);
         Pack_Start (Box, Var_Widget.Ent, False, False, 5);

         Gtk_New_Hbox (Main_Box);
         Pack_Start (Main_Box, Box, False, False, 3);

         Gtk_New (Label, To_String (Var.Description));
         Gtk_New_Hbox (Box);
         Pack_Start (Box, Label, False, False, 5);

         Pack_Start (Main_Box, Box, False, False, 3);

         Pack_Start
           (Widget, Main_Box,
            Expand  => False,
            Fill    => False,
            Padding => 3);
      end Create_Var_Widget;

      use type Ada.Containers.Count_Type;

      Label   : Gtk_Label;
      Box     : Gtk_Box;

      Dummy : Boolean;
      pragma Unreferenced (Dummy);
   begin
      Initialize_Vbox (Widget);
      Widget.Template := Template;

      --  Create the fields for variables.
      Gtk_New (Group);
      C := Template.Variables.First;

      while Has_Element (C) loop
         Create_Var_Widget (Element (C));
         Next (C);
      end loop;

      --  Create the chooser for the target directory
      Gtk_New (Widget.Chooser, "Select a directory", Action_Select_Folder);
      Dummy := Set_Filename (+Widget.Chooser, +Get_Current_Dir.Full_Name.all);

      Gtk_New_Hbox (Box);
      Pack_End (Widget, Box, False, False, 3);

      Gtk_New (Label, "Deploy template in: ");
      Pack_Start (Box, Label, False, False, 10);
      Pack_Start (Box, Widget.Chooser, True, True, 3);
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
         if Get_Text (Element (C).Ent) = "" then
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
         R.Insert (V.Name, To_Unbounded_String (Get_Text (V.Ent)));

         Next (C);
      end loop;

      return R;
   end Get_Assignments;

   ------------------
   -- Column_Types --
   ------------------

   function Column_Types return GType_Array is
   begin
      return (Name_Col => GType_String,
              Icon_Name_Col => GType_String,
              Num_Col  => GType_Int,
              Desc_Col => GType_String);
   end Column_Types;

   ----------------------
   -- Install_Template --
   ----------------------

   procedure Install_Template
     (Templates    : Project_Templates_List.List;
      Chosen       : out Project_Template;
      Installed    : out Boolean;
      Dir          : out Virtual_File;
      Project      : out Virtual_File;
      Errors       : out Unbounded_String)
   is
      Assistant : Gtk_Assistant;

      Scroll   : Gtk_Scrolled_Window;
      Tree     : Gtk_Tree_View;
      Model    : Gtk_Tree_Store;
      Iter     : Gtk_Tree_Iter;
      Col      : Gtk_Tree_View_Column;
      Rend     : Gtk_Cell_Renderer_Text;
      Pix      : Gtk_Cell_Renderer_Pixbuf;
      Col_Num  : Gint;
      Page_Num : Gint;
      pragma Unreferenced (Col_Num, Page_Num);
      Box      : Gtk_Box;
      Hpane    : Gtk_Hpaned;
      Main_Label : Gtk_Label;
      Next_Page_Number : Gint;

      use Project_Templates_List;
      C : Cursor;

      package Forwarder is new Set_Forward_Page_Func_User_Data (Boolean);

      function Next_Page
        (Current_Page : Gint; User_Data : Boolean) return Gint;
      --  Next page function

      procedure Add_Template (Template : Project_Template);
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
         if Current_Page = 0
           and then Next_Page_Number > 0
         then
            return Next_Page_Number;
         end if;

         return Current_Page;
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

            Model.Set (Child, Name_Col, Name);
            Model.Set (Child, Desc_Col, "");
            Model.Set (Child, Icon_Name_Col, "gps-emblem-directory-symbolic");
            Model.Set (Child, Num_Col, -1);
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

      procedure Add_Template (Template : Project_Template) is
         Cat      : Gtk_Tree_Iter;
         Iter     : Gtk_Tree_Iter;
         Child    : Gtk_Tree_Iter;
         Page_Num : Gint;
         Page     : Template_Page;
      begin
         --  Create the page
         Gtk_New (Page, Template);

         --  The header image does not look great for now
         --  Assistant.Set_Page_Header_Image (Page, Header_Image);

         --  Add the page to the assistant
         Page_Num := Append_Page (Assistant, Page);
         Assistant.Set_Page_Type (Page, Gtk_Assistant_Page_Confirm);

         --  Titles don't look great in the Gtk+ assistant
--       Assistant.Set_Page_Title
--         (Page,
--          To_String (Template.Category) & " " & To_String (Template.Label));

         --  Connect the change of entry text to a function that checks the
         --  completeness of the page
         declare
            use Variable_Widgets;
            C : Variable_Widgets.Cursor;
         begin
            C := Page.Var_Widgets.First;

            while Has_Element (C) loop
               Object_Callback.Object_Connect
                 (Element (C).Ent, "changed",
                  Check_Completeness'Unrestricted_Access,
                  Slot_Object => Page);
               Next (C);
            end loop;
         end;

         --  Compute the initial "complete" state
         Assistant.Set_Page_Complete (Page, Is_Complete (Page));

         --  Add the template to the tree model
         Cat := Get_Or_Create_Category (Template.Category);

         Child := Model.Children (Cat);

         while Child /= Null_Iter
           and then Get_String (Model, Child, Name_Col)
           < To_String (Template.Label)
         loop
            Model.Next (Child);
         end loop;

         Model.Insert_Before (Iter, Cat, Child);

         Model.Set (Iter, Name_Col, To_String (Template.Label));
         Model.Set (Iter, Desc_Col, To_String (Template.Description));
         Model.Set (Iter, Icon_Name_Col, "gps-run-symbolic");
         Model.Set (Iter, Num_Col, Page_Num);
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
            Assistant.Set_Page_Complete (Hpane, False);
         else
            Assistant.Set_Page_Complete (Hpane, True);
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
         Page := Template_Page (Assistant.Get_Nth_Page (Next_Page_Number));
         Dir  := Create (+Get_Filename (+Page.Chooser));
         Instantiate_Template
           (Template    => Page.Template,
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
      Chosen := Null_Project_Template;
      Installed := False;

      Gtk_New (Assistant);
      Assistant.Set_Position (Win_Pos_Center);

      Gtk_New (Scroll);
      Gtk_New_Hpaned (Hpane);
      Set_Policy (Scroll, Policy_Automatic, Policy_Automatic);
      Add1 (Hpane, Scroll);

      Gtk_New_Vbox (Box);
      Add2 (Hpane, Box);

      Gtk_New (Main_Label);
      Pack_Start (Box, Main_Label, True, True, 3);

      Set_Position (Hpane, 200);
      Page_Num := Assistant.Append_Page (Hpane);

      --  The header image does not look great for now
      --  Assistant.Set_Page_Header_Image (Hpane, Header_Image);

      --  Assistant.Set_Page_Title (Hpane, "Select a template");

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

      --  Expand the tree and select the first leaf
      Expand_All (Tree);

      Iter := Model.Get_Iter_First;

      while Model.Has_Child (Iter) loop
         Iter := Model.Children (Iter);
      end loop;

      if Iter /= Null_Iter then
         Get_Selection (Tree).Select_Iter (Iter);
      end if;

      Show_All (Assistant);
      Assistant.Set_Title ("Create Project from Template");

      --  Launch a main loop: we do not want to leave this procedure while the
      --  assistand is running.
      Gtk.Main.Main;

      Assistant.Destroy;
   end Install_Template;

end Project_Templates.GUI;
