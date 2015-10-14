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
with Gtk.Grid;                  use Gtk.Grid;
with Gtk.Label;                 use Gtk.Label;
with Gtkada.MDI;                use Gtkada.MDI;
with Gtk.Notebook;              use Gtk.Notebook;
with Gtk.Paned;                 use Gtk.Paned;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Selection;        use Gtk.Tree_Selection;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Viewport;              use Gtk.Viewport;
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
        (Name : String; Widget : Gtk_Widget) return Gint;
      --  Return the page index stored in the Gtk_Tree_Model.
      --  If no such page already exists, then either Widget (if non null) is
      --  inserted for it, or a new page is created and appended to the
      --  notebook.

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

      -------------------------
      -- Find_Or_Create_Page --
      -------------------------

      function Find_Or_Create_Page
        (Name : String; Widget : Gtk_Widget) return Gint
      is
         Current       : Gtk_Tree_Iter := Null_Iter;
         Child         : Gtk_Tree_Iter;
         First, Last   : Integer := Name'First;
         Page          : Gtk_Grid;
         Scrolled_Page : Gtk_Scrolled_Window;
         W             : Gtk_Widget;

      begin
         while First <= Name'Last loop
            Last := First;

            while Last <= Name'Last
              and then Name (Last) /= '/'
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
                Get_String (Self.Model, Child, 0) /= Name (First .. Last - 1)
            loop
               Next (Self.Model, Child);
            end loop;

            if Child = Null_Iter then
               if Widget = null then
                  Gtk_New (Page);
                  Page.Set_Margin_Start (10);
                  Page.Set_Margin_End (10);
                  Page.Set_Column_Spacing (5);
                  Page.Set_Row_Spacing (5);
                  Page.Set_Column_Homogeneous (False);

                  Gtk_New (Scrolled_Page);
                  Scrolled_Page.Set_Policy
                    (Policy_Automatic, Policy_Automatic);
                  Scrolled_Page.Add (Page);

                  W := Gtk_Widget (Scrolled_Page);
               else
                  W := Widget;
               end if;

               Self.Pages_Notebook.Append_Page (Child     => W,
                                                Tab_Label => null);

               Append (Self.Model, Child, Current);
               Set (Self.Model, Child, 0, Name (First .. Last - 1));
               Set (Self.Model, Child, 1, Self.Pages_Notebook.Get_N_Pages - 1);
            end if;

            Current := Child;

            First := Last + 1;
         end loop;

         return  Get_Int (Self.Model, Current, 1);
      end Find_Or_Create_Page;

      Page                    : Gtk_Grid;
      Scrolled_Page           : Gtk_Scrolled_Window;
      Scrolled_Page_Viewport  : Gtk_Viewport;
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
      Self.Pack_Start (Main_Pane);

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
            --  Get/Create the page associated to this preference
            Scrolled_Page := Gtk_Scrolled_Window
              (Self.Pages_Notebook.Get_Nth_Page
                 (Find_Or_Create_Page (Pref.Get_Page, null)));
            Scrolled_Page_Viewport := Gtk_Viewport (Scrolled_Page.Get_Child);
            Page := Gtk_Grid (Scrolled_Page_Viewport.Get_Child);

            if Pref.Editor_Needs_Label then
               Gtk_New (Event);
               Gtk_New (Label, Pref.Get_Label);
               Event.Add (Label);
               Event.Set_Tooltip_Text (Pref.Get_Doc);
               Label.Set_Alignment (0.0, 0.5);

               Page.Attach (Child  => Event,
                            Left   => 0,
                            Top    => Page.Get_Baseline_Row,
                            Width  => 1,
                            Height => 1);

               Widget := Edit
                 (Pref      => Pref,
                  Manager   => Manager);

               if Widget /= null then
                  Widget.Set_Hexpand (False);
                  Page.Attach (Child  => Widget,
                               Left   => 1,
                               Top    => Page.Get_Baseline_Row,
                               Width  => 1,
                               Height => 1);
               end if;
            else
               Widget := Edit
                 (Pref      => Pref,
                  Manager   => Manager);
               Widget.Set_Tooltip_Text (Pref.Get_Doc);

               if Widget /= null then
                  Widget.Set_Hexpand (False);
                  Page.Attach (Child  => Widget,
                               Left   => 0,
                               Top    => Page.Get_Baseline_Row,
                               Width  => 1,
                               Height => 1);
               end if;
            end if;
            Page.Set_Baseline_Row (Page.Get_Baseline_Row + 1);
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
