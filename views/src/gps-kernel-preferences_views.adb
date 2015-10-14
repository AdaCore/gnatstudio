------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2015, AdaCore                     --
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

with Default_Preferences;       use Default_Preferences;
with Generic_Views;             use Generic_Views;

with Glib.Object;               use Glib.Object;
with Gtk.Box;                   use Gtk.Box;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Event_Box;             use Gtk.Event_Box;
with Gtk.Frame;                 use Gtk.Frame;
with Gtk.Label;                 use Gtk.Label;
with Gtk.List_Box;              use Gtk.List_Box;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtk.Notebook;              use Gtk.Notebook;
with Gtk.Paned;                 use Gtk.Paned;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Size_Group;            use Gtk.Size_Group;
with Gtk.Style_Context;         use Gtk.Style_Context;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Handlers;           use Gtkada.Handlers;

with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;

with Language;                  use Language;

package body GPS.Kernel.Preferences_Views is

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);
   --  Called when the preferences have changed

   type Preferences_Editor_Record is new Generic_Views.View_Record with record
      Pages_Tree         : Gtk_Tree_View;
      Model              : Gtk_Tree_Store;
      Pages_Notebook     : Gtk_Notebook;
      Current_Page_Index : Gint;
   end record;
   --  Preferences editor view record. This record only encapsulates needed
   --  for callbacks (e.g: Selection_Changed).
   type Preferences_Editor is access all Preferences_Editor_Record'Class;

   type Preferences_Group_Record is new Gtk_Frame_Record with record
      Label_Size_Group       : Gtk_Size_Group;
      Pref_Widget_Size_Group : Gtk_Size_Group;
      Preferences_List       : Gtk_List_Box;
   end record;
   --  Preferences group view record. The Gtk_Size_Group widgets are used
   --  to align the labels and teh preferences widgets.
   type Preferences_Group is access all Preferences_Group_Record'Class;

   package Preferences_Group_Maps is new Ada.Containers.Indefinite_Hashed_Maps
     (String, Preferences_Group, Ada.Strings.Hash, "=");

   type Preferences_Page_Record is new Gtk_Scrolled_Window_Record with record
      Groups_Map : Preferences_Group_Maps.Map;
      Page_Box   : Gtk_Box;
   end record;
   --  Preferences page view record. The Groups_Map field is used to keep track
   --  of the preferences groups contained in a page.
   type Preferences_Page is access all Preferences_Page_Record'Class;

   function Initialize
     (Self : access Preferences_Editor_Record'Class) return Gtk_Widget;
   --  Initialize and add all the widgets needed for the
   --  Preferences_Editor_Record view given in parameter.

   package Preferences_Editor_Views is new Generic_Views.Simple_Views
     (Module_Name               => "Preferences",
      View_Name                 => "Preferences",
      Formal_View_Record        => Preferences_Editor_Record,
      Formal_MDI_Child          => GPS_MDI_Child_Record,
      Reuse_If_Exist            => True,
      Local_Toolbar             => False,
      Local_Config              => False,
      Group                     => Group_Default,
      Areas                     => Gtkada.MDI.Both,
      Default_Width             => 800,
      Default_Height            => 700,
      Commands_Category         => -"/Edit/Preferences",
      Add_Close_Button_On_Float => True,
      MDI_Flags                 =>
         All_Buttons or Float_To_Main or Always_Destroy_Float,
      Position                  => Position_Float,
      Initialize                => Initialize);
   use Preferences_Editor_Views;
   --  Instantiation of the Generic_Views.Simple_Views package with
   --  the parameters we want for our Preferences editor views.

   procedure Selection_Changed (Widget : access Gtk_Widget_Record'Class);
   --  Called when the selected page has changed.

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed (Widget : access Gtk_Widget_Record'Class) is
      Pref_View : constant Preferences_Editor := Preferences_Editor (Widget);
      Iter      : Gtk_Tree_Iter;
      M         : Gtk_Tree_Model;
   begin
      Get_Selected (Get_Selection (Pref_View.Pages_Tree), M, Iter);

      if Iter /= Null_Iter then
         --  Get the newly selected page index from the model and
         --  set it as the current page to display for the notebook.
         Pref_View.Current_Page_Index := Get_Int (Pref_View.Model, Iter, 1);
         Pref_View.Pages_Notebook.Set_Current_Page
           (Pref_View.Current_Page_Index);
      end if;
   end Selection_Changed;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference) is
      pragma Unreferenced (Self, Kernel);
   begin
      if Has_Gobject_To_Update (Pref) then
         Pref.Update_On_Pref_Changed (Get_GObject_To_Update (Pref));
      end if;
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Preferences_Editor_Record'Class) return Gtk_Widget
   is
      Manager        : constant GPS_Preferences :=
                     GPS_Preferences (Self.Kernel.Preferences);
      Filename       : constant Virtual_File := Self.Kernel.Preferences_File;
      Main_Pane      : Gtk_Paned;

      procedure Select_First_Page;
      --  Select the first page of preferences to be active in the Tree_View.
      --  This procedure is called right after the initialization.

      function Find_Or_Create_Page
        (Page_Name : String; Widget : Gtk_Widget) return Gtk_Tree_Iter;
      --  Return the page index stored in the Gtk_Tree_Model.
      --  If no such page already exists, then either Widget (if non null) is
      --  inserted for it, or a new page is created and appended to the
      --  notebook

      function Find_Or_Create_Group
        (Page_Name  : String;
         Group_Name : String;
         Widget     : Gtk_Widget) return Preferences_Group;
      --  Return the widget associated with a specific group. If a widget
      --  associated with the group name is already in the model,
      --  this function returns it. If not, it creates a widget for the group
      --  given in paremeter and appends it to the model.

      function Get_Page_Name (Pref_Name : String) return String;
      --  Return the page name in the preference name conatined in the
      --  preference name.

      function Get_Group_Name (Pref_Name : String) return String;
      --  Return the group name in the preference name, if any. If not, return
      --  an empty string.

      -------------------
      -- Get_Page_Name --
      -------------------

      function Get_Page_Name (Pref_Name : String) return String is
         Current_Index : Integer := Pref_Name'First;
      begin
         --  Find the delimitor for group names
         while Current_Index <= Pref_Name'Last
           and then Pref_Name (Current_Index) /= ':' loop
            Current_Index := Current_Index + 1;
         end loop;

         --  No group specified in the preference name
         if Current_Index >= Pref_Name'Last then
            return Pref_Name;
         end if;

         return Pref_Name (Pref_Name'First .. Current_Index - 1);
      end Get_Page_Name;

      --------------------
      -- Get_Group_Name --
      --------------------

      function Get_Group_Name (Pref_Name : String) return String is
         Current_Index : Integer := Pref_Name'First;
      begin
         --  Find the delimitor for group names
         while Current_Index <= Pref_Name'Last
           and then Pref_Name (Current_Index) /= ':' loop
            Current_Index := Current_Index + 1;
         end loop;

         --  No group specified in the preference name
         if Current_Index >= Pref_Name'Last then
            return "";
         end if;

         return Pref_Name (Current_Index + 1 .. Pref_Name'Last);
      end Get_Group_Name;

      -----------------------
      -- Select_First_Page --
      -----------------------

      procedure Select_First_Page is
         First_Iter      : Gtk_Tree_Iter;
         First_Page_Path : Gtk_Tree_Path;
      begin
         First_Iter := Get_Iter_First (Self.Model);

         if First_Iter /= Null_Iter then
            First_Page_Path := Self.Model.Get_Path (First_Iter);

            Set_Cursor (Self.Pages_Tree, First_Page_Path, null, False);
         end if;
      end Select_First_Page;

      --------------------------
      -- Find_Or_Create_Group --
      --------------------------

      function Find_Or_Create_Group
        (Page_Name  : String;
         Group_Name : String;
         Widget     : Gtk_Widget) return Preferences_Group
      is
         Page_Iter    : Gtk_Tree_Iter;
         Page_Index   : Gint;
         Page         : Preferences_Page;
         Group        : Preferences_Group := null;
         use Preferences_Group_Maps;
      begin
         --  Get the page where we want to insert the preferences group
         Page_Iter := Find_Or_Create_Page (Page_Name, Widget);
         Page_Index := Get_Int (Self.Model, Page_Iter, 1);
         Page := Preferences_Page
           (Self.Pages_Notebook.Get_Nth_Page (Page_Index));

         --  If the group has already been created for this page, just retrieve
         --  if from the map
         if Page.Groups_Map.Contains (Group_Name) then
            Group := Page.Groups_Map (Group_Name);
         end if;

         --  If the group doesn't exist for this page yet, create a new one
         --  and insert in the map
         if Group = null then
            Group := new Preferences_Group_Record;
            Gtk.Frame.Initialize (Group);
            Group.Set_Border_Width (3);

            Gtk_New (Group.Label_Size_Group);
            Gtk_New (Group.Pref_Widget_Size_Group);
            Gtk_New (Group.Preferences_List);
            Group.Preferences_List.Set_Selection_Mode (Selection_None);
            Get_Style_Context (Group.Preferences_List).Add_Class
              ("gps-preferences-groups");

            Group.Add (Group.Preferences_List);

            Page.Page_Box.Pack_Start (Group);

            if Group_Name /= "" then
               Group.Set_Label (Group_Name);
            end if;

            Page.Groups_Map.Insert (Group_Name, Group);
         end if;

         return Group;
      end Find_Or_Create_Group;

      -------------------------
      -- Find_Or_Create_Page --
      -------------------------

      function Find_Or_Create_Page
        (Page_Name : String; Widget : Gtk_Widget) return Gtk_Tree_Iter
      is
         Current       : Gtk_Tree_Iter := Null_Iter;
         Child         : Gtk_Tree_Iter;
         First, Last   : Integer := Page_Name'First;
         Page          : Preferences_Page;
         W             : Gtk_Widget;
      begin
         while First <= Page_Name'Last loop
            Last := First;

            while Last <= Page_Name'Last
              and then Page_Name (Last) /= '/'
            loop
               Last := Last + 1;
            end loop;

            if Current = Null_Iter then
               Child := Get_Iter_First (Self.Model);
            else
               Child := Children (Self.Model, Current);
            end if;

            while Child /= Null_Iter
              and then
                Get_String (Self.Model, Child, 0) /= Page_Name
              (First .. Last - 1)
            loop
               Next (Self.Model, Child);
            end loop;

            if Child = Null_Iter then
               if Widget = null then
                  --  Create a new page
                  Page := new Preferences_Page_Record;
                  Gtk.Scrolled_Window.Initialize (Page);
                  Page.Set_Policy (Policy_Automatic, Policy_Automatic);

                  --  Create the new Vbox which will hold the preferences
                  --  groups
                  Gtk_New_Vbox (Page.Page_Box);
                  Page.Page_Box.Set_Margin_Start (10);
                  Page.Page_Box.Set_Margin_End (10);
                  Page.Add (Page.Page_Box);

                  W := Gtk_Widget (Page);
               else
                  W := Widget;
               end if;

               Self.Pages_Notebook.Append_Page (Child     => W,
                                                Tab_Label => null);

               Append (Self.Model, Child, Current);
               Set (Self.Model, Child, 0, Page_Name (First .. Last - 1));
               Set (Self.Model, Child, 1, Self.Pages_Notebook.Get_N_Pages - 1);
            end if;

            Current := Child;

            First := Last + 1;
         end loop;

         return  Current;
      end Find_Or_Create_Page;
      Group                   : Preferences_Group;
      Group_Row               : Gtk_Box;
      Col                     : Gtk_Tree_View_Column;
      Render                  : Gtk_Cell_Renderer_Text;
      Num                     : Gint;
      Scrolled_Pages_Tree     : Gtk_Scrolled_Window;
      Pref                    : Preference;
      Backup_Created          : Boolean;
      Widget                  : Gtk_Widget;
      Event                   : Gtk_Event_Box;
      Label                   : Gtk_Label;
      C                       : Default_Preferences.Cursor;
      Backup_File             : constant Virtual_File :=
                        Create (Full_Filename => Filename.Full_Name & ".bkp");

      pragma Unreferenced (Num);
   begin
      Filename.Copy (Backup_File.Full_Name, Success => Backup_Created);

      Initialize_Vbox (Self);
      Self.Set_Name ("Preferences");  --  for testsuite

      Manager.Set_Editor (Self);

      --  Create an horizontal paned view and add it to the Preferences
      --  view.
      Gtk_New_Hpaned (Main_Pane);
      Self.Pack_Start (Child   => Main_Pane,
                       Expand  => True,
                       Fill    => True);

      --  Create the scrolled window which will contain the pages tree view
      --  on the left side of the paned view.
      Gtk_New (Scrolled_Pages_Tree);
      Scrolled_Pages_Tree.Set_Policy (Policy_Never, Policy_Automatic);
      Main_Pane.Pack1 (Child  => Scrolled_Pages_Tree,
                       Resize => False,
                       Shrink => False);

      --  Create the notebook window which will contain the selected page
      --  on the right side of the paned view.
      Gtk_New (Self.Pages_Notebook);
      Self.Pages_Notebook.Set_Show_Border (False);
      Self.Pages_Notebook.Set_Show_Tabs (False);
      Main_Pane.Pack2 (Child  => Self.Pages_Notebook,
                       Resize => True,
                       Shrink => False);

      --  Create the pages tree view and add it to its parent scrolled window
      Gtk_New (Self.Model, (0 => GType_String, 1 => GType_Int));
      Gtk_New (Self.Pages_Tree, Self.Model);
      Scrolled_Pages_Tree.Add (Self.Pages_Tree);
      Self.Pages_Tree.Set_Headers_Visible (False);

      Gtk_New (Col);
      Num := Self.Pages_Tree.Append_Column (Col);
      Gtk_New (Render);
      Col.Pack_Start (Render, Expand => True);
      Col.Add_Attribute (Render, "text", 0);

      Widget_Callback.Object_Connect
        (Get_Selection (Self.Pages_Tree),
         Gtk.Tree_Selection.Signal_Changed,
         Selection_Changed'Unrestricted_Access,
         Self);

      --  For each registered preference, create or find the corresponding
      --  preference page and add the preferences widget to it.
      C := Manager.Get_First_Reference;
      loop
         Pref := Get_Pref (C);
         exit when Pref = null;

         if Pref.Get_Page /= "" then
            Group := Find_Or_Create_Group
              (Page_Name  => Get_Page_Name (Pref.Get_Page),
               Group_Name => Get_Group_Name (Pref.Get_Page),
               Widget     => null);

            Gtk_New_Hbox (Group_Row);
            Group_Row.Set_Border_Width (3);

            if Pref.Editor_Needs_Label then
               Gtk_New (Event);
               Gtk_New (Label, Pref.Get_Label);
               Event.Add (Label);
               Event.Set_Tooltip_Text (Pref.Get_Doc);
               Label.Set_Alignment (0.0, 0.5);

               Group.Label_Size_Group.Add_Widget (Event);
               Group_Row.Pack_Start (Event);

               Widget := Edit
                 (Pref      => Pref,
                  Manager   => Manager);

               if Widget /= null then
                  Widget.Set_Hexpand (False);
                  Group.Pref_Widget_Size_Group.Add_Widget (Widget);
                  Group_Row.Pack_Start (Widget);
               end if;
            else
               Widget := Edit
                 (Pref      => Pref,
                  Manager   => Manager);
               Widget.Set_Tooltip_Text (Pref.Get_Doc);

               if Widget /= null then
                  Widget.Set_Hexpand (False);
                  Group.Pref_Widget_Size_Group.Add_Widget (Widget);
                  Group_Row.Pack_Start (Widget);
               end if;
            end if;

            Group.Preferences_List.Add (Group_Row);
         end if;

         Manager.Next (C);
      end loop;

      Self.Set_Can_Focus (True);

      --  Show all pages for more convenient access
      Self.Pages_Tree.Expand_All;

      --  Register a hook function which will update all the prefernces
      --  widgets when preferences changes.
      Preferences_Changed_Hook.Add (new On_Pref_Changed);

      --  Select and display the first page when opening the Preferences view
      Select_First_Page;

      return Gtk_Widget (Self);
   end Initialize;

   -------------------------------
   -- Register_Preferences_Menu --
   -------------------------------

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Preferences_Editor_Views.Register_Module (Kernel);
   end Register_Module;

end GPS.Kernel.Preferences_Views;
