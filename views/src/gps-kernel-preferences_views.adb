------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2016, AdaCore                     --
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

with Default_Preferences;           use Default_Preferences;
with Default_Preferences.GUI;       use Default_Preferences.GUI;
with Generic_Views;                 use Generic_Views;
with GUI_Utils;                     use GUI_Utils;

with GNATCOLL.Traces;               use GNATCOLL.Traces;

with Glib.Object;                   use Glib.Object;
with Gtk.Box;                       use Gtk.Box;
with Gtk.Cell_Renderer_Text;        use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                     use Gtk.Enums;
with Gtk.Event_Box;                 use Gtk.Event_Box;
with Gtk.List_Box_Row;              use Gtk.List_Box_Row;
with Gtkada.MDI;                    use Gtkada.MDI;
with Gtk.Notebook;                  use Gtk.Notebook;
with Gtk.Paned;                     use Gtk.Paned;
with Gtk.Scrolled_Window;           use Gtk.Scrolled_Window;
with Gtk.Style_Context;             use Gtk.Style_Context;
with Gtk.Toolbar;                   use Gtk.Toolbar;
with Gtk.Tree_Model;                use Gtk.Tree_Model;
with Gtk.Tree_Model_Filter;         use Gtk.Tree_Model_Filter;
with Gtk.Tree_Model_Sort;           use Gtk.Tree_Model_Sort;
with Gtk.Tree_Selection;            use Gtk.Tree_Selection;
with Gtk.Tree_Store;                use Gtk.Tree_Store;
with Gtk.Tree_View;                 use Gtk.Tree_View;
with Gtk.Tree_View_Column;          use Gtk.Tree_View_Column;
with Gtk.Widget;                    use Gtk.Widget;
with Gtkada.Handlers;               use Gtkada.Handlers;

with GPS.Intl;                      use GPS.Intl;
with GPS.Kernel.Hooks;              use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;                use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;        use GPS.Kernel.Preferences;
with GPS.Kernel.Search;             use GPS.Kernel.Search;
with GPS.Kernel.Search.Plugins;     use GPS.Kernel.Search.Plugins;
with GPS.Kernel.Search.Preferences; use GPS.Kernel.Search.Preferences;
with GPS.Search;                    use GPS.Search;
with GPS.Search.GUI;                use GPS.Search.GUI;
with Startup_Module;                use Startup_Module;

with Language;                      use Language;

package body GPS.Kernel.Preferences_Views is

   Me : constant Trace_Handle := Create (-"PREFERENCES_VIEWS");

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);
   --  Called when the preferences have changed

   type GPS_Preferences_Editor_Record is new Generic_Views.View_Record
     and Preferences_Editor_Interface with record
      Pages_Tree         : Gtk_Tree_View;
      Model              : Gtk_Tree_Store;
      Filter             : Gtk_Tree_Model_Filter;
      Sort               : Gtk_Tree_Model_Sort;
      Filter_Pattern     : Search_Pattern_Access;
      Pages_Notebook     : Gtk_Notebook;
      Current_Page_Index : Gint;
      Highlighted_Pref   : Preference;
   end record;
   type GPS_Preferences_Editor is
     access all GPS_Preferences_Editor_Record'Class;
   --  Preferences editor view record. This record only encapsulates needed
   --  for callbacks (e.g: Selection_Changed).

   overriding procedure Create_Toolbar
     (View    : not null access GPS_Preferences_Editor_Record;
      Toolbar : not null access Gtk_Toolbar_Record'Class);
   overriding procedure Filter_Changed
     (Self    : not null access GPS_Preferences_Editor_Record;
      Pattern : in out Search_Pattern_Access);
   overriding function Get_Widget
     (Self : not null access GPS_Preferences_Editor_Record)
      return Gtk.Widget.Gtk_Widget;
   overriding procedure Display_Page
     (Self      : not null access GPS_Preferences_Editor_Record;
      Page_Name : String);
   overriding procedure Display_Pref
     (Self      : not null access GPS_Preferences_Editor_Record;
      Pref      : not null Preference;
      Highlight : Boolean := False);
   overriding function Get_Page_View
     (Self      : not null access GPS_Preferences_Editor_Record;
      Page_Name : String) return Gtk.Widget.Gtk_Widget;
   --  See inherited subprograms documentation

   type Custom_Preferences_Search_Provider is new Preferences_Search_Provider
   with null record;
   --  Extend the GPS.Kernel.Search.Preferences_Search_Provider used by the
   --  omnisearch to have a custom local search bar.

   overriding function Create_Preferences_Search_Result
     (Self  : not null access Custom_Preferences_Search_Provider;
      Pref  : not null Default_Preferences.Preference;
      Short : GNAT.Strings.String_Access;
      Long  : GNAT.Strings.String_Access;
      Score : Natural) return GPS.Search.Search_Result_Access;
   --  Override this function to return a Custom_Preferences_Search_Result
   --  and have dispatching on the returned Custom_Preferences_Search_Result.

   type Custom_Preferences_Search_Result is new Preferences_Search_Result
   with null record;
   --  Extend the GPS.Kernel.Search.Preferences_Search_Result type to override
   --  some primitives.

   overriding procedure Execute
     (Self       : not null access Custom_Preferences_Search_Result;
      Give_Focus : Boolean);
   --  Override this function to unhighlight the currently highlighted
   --  preference (if any) after selecting a match in the search bar
   --  completion list popup.

   overriding function Full
     (Self       : not null access Custom_Preferences_Search_Result)
      return Gtk.Widget.Gtk_Widget;
   --  Override this function to highlight the preference and display the page
   --  containing it as a preview.
   --  Return null so that no additional preview widget is displayed.

   type Custom_Plugins_Search_Provider is new Plugins_Search_Provider
   with null record;
   --  Extend the GPS.Kernel.Search.Plugins_Provider type to override some
   --  primitives.

   overriding function Create_Plugins_Search_Result
     (Self        : not null access Custom_Plugins_Search_Provider;
      Plugin_Page : not null Startup_Module.Plugin_Preferences_Page;
      Short       : GNAT.Strings.String_Access;
      Long        : GNAT.Strings.String_Access;
      Score       : Natural) return GPS.Search.Search_Result_Access;
   --  Override this function to return a Custom_Plugins_Search_Result
   --  and have dispatching on the returned Custom_Plugins_Search_Result.

   type Custom_Plugins_Search_Result is new Plugins_Search_Result
   with null record;
   --  Extend the GPS.Kernel.Search.Plugins_Search_Result type to override
   --  some primitives.

   overriding function Full
     (Self       : not null access Custom_Plugins_Search_Result)
      return Gtk.Widget.Gtk_Widget;
   --  Override this function to display the plugin page associated to
   --  the given result.
   --  Return null so that no additional preview widget is displayed.

   type Overall_Preferences_Search_Provider is new Overall_Search_Provider with
     null record;
   --  Extend the GPS.Search.GUI.Overall_Search_Provider type used in the
   --  omnisearch to override some primitives (e.g: Edit_Settings).

   overriding procedure Edit_Settings
     (Self      : not null access Overall_Preferences_Search_Provider;
      Box       : not null access Gtk.Box.Gtk_Box_Record'Class;
      Data      : not null access Glib.Object.GObject_Record'Class;
      On_Change : On_Settings_Changed_Callback) is null;
   --  We don't want any non-GUI related settings (e.g: no possibilty to change
   --  the providers order).

   package Preferences_Editor_Visible_Funcs is new
     Gtk.Tree_Model_Filter.Set_Visible_Func_User_Data (GPS_Preferences_Editor);
   function Page_Is_Visible
     (Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Data  : GPS_Preferences_Editor) return Boolean;
   --  Selects whether a given row should be visible in the preferences
   --  editor tree view.

   function Initialize
     (Self : access GPS_Preferences_Editor_Record'Class) return Gtk_Widget;
   --  Initialize and add all the widgets needed for the
   --  Preferences_Editor_Record view given in parameter.

   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class);
   --  Used to reset the Preferences_GObjects_Map mapping the preferences names
   --  with the widget we want to update when preferences change.

   package Preferences_Editor_Views is new Generic_Views.Simple_Views
     (Module_Name               => "Preferences",
      View_Name                 => "Preferences",
      Formal_View_Record        => GPS_Preferences_Editor_Record,
      Formal_MDI_Child          => GPS_MDI_Child_Record,
      Reuse_If_Exist            => True,
      Local_Toolbar             => True,
      Local_Config              => False,
      Group                     => Group_Default,
      Areas                     => Gtkada.MDI.Both,
      Default_Width             => 800,
      Default_Height            => 700,
      Add_Close_Button_On_Float => True,
      MDI_Flags                 =>
         All_Buttons or Float_To_Main or Always_Destroy_Float,
      Position                  => Position_Float,
      Initialize                => Initialize);
   use Preferences_Editor_Views;
   --  Instantiation of the Generic_Views.Simple_Views package with
   --  the parameters we want for our Preferences editor views.

   function Get_Selected_Page_View
     (Self : not null GPS_Preferences_Editor) return Preferences_Page_View;
   --  Return the currently selected page view.

   procedure Selection_Changed (Widget : access Gtk_Widget_Record'Class);
   --  Called when the selected page has changed.

   function Find_Page_Iter
     (Editor           : not null GPS_Preferences_Editor;
      Page_Iter        : out Gtk_Tree_Iter;
      Page_Name        : String;
      Create_If_Needed : Boolean := False) return Boolean;
   --  If a Gtk_Tree_Iter has been found for this page in the model, return
   --  True and return this existing node in Page_Iter.
   --  If not and If_Create_If_Needed is True, create a new Gtk_Tree_Iter node
   --  at the right location and return it in Page_Iter.

   --------------------------------------
   -- Create_Preferences_Search_Result --
   --------------------------------------

   overriding function Create_Preferences_Search_Result
     (Self  : not null access Custom_Preferences_Search_Provider;
      Pref  : not null Default_Preferences.Preference;
      Short : GNAT.Strings.String_Access;
      Long  : GNAT.Strings.String_Access;
      Score : Natural) return GPS.Search.Search_Result_Access
   is
      Name  : constant String := Get_Name (Pref);
   begin
      return new Custom_Preferences_Search_Result'
        (Kernel   => Self.Kernel,
         Provider => Self,
         Score    => Score,
         Short    => Short,
         Long     => Long,
         Id       => new String'(Name),
         Pref     => Pref);
   end Create_Preferences_Search_Result;

   ----------------------------------
   -- Create_Plugins_Search_Result --
   ----------------------------------

   overriding function Create_Plugins_Search_Result
     (Self        : not null access Custom_Plugins_Search_Provider;
      Plugin_Page : not null Startup_Module.Plugin_Preferences_Page;
      Short       : GNAT.Strings.String_Access;
      Long        : GNAT.Strings.String_Access;
      Score       : Natural) return GPS.Search.Search_Result_Access is
   begin
      return new Custom_Plugins_Search_Result'
        (Kernel       => Self.Kernel,
         Provider     => Self,
         Score        => Score,
         Short        => Short,
         Long         => Long,
         Id           => new String'(Long.all),
         Plugin_Page  => Plugin_Page);
   end Create_Plugins_Search_Result;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self       : not null access Custom_Preferences_Search_Result;
      Give_Focus : Boolean)
   is
      Editor : constant Preferences_Editor :=
                 Self.Kernel.Get_Preferences.Get_Editor;
      pragma Unreferenced (Give_Focus);
   begin
      --  Display the preference's page, without highlighting it
      Editor.Display_Pref (Pref      => Self.Pref,
                           Highlight => False);
   end Execute;

   ----------
   -- Full --
   ----------

   overriding function Full
     (Self       : not null access Custom_Preferences_Search_Result)
      return Gtk.Widget.Gtk_Widget
   is
      Editor : constant Preferences_Editor :=
                 Self.Kernel.Get_Preferences.Get_Editor;
   begin
      --  Go to the corresponding page and highlight the selected preference
      Editor.Display_Pref (Pref      => Self.Pref,
                           Highlight => True);

      --  Return null so that no preview widget is displayed
      return null;
   end Full;

   ----------
   -- Full --
   ----------

   overriding function Full
     (Self       : not null access Custom_Plugins_Search_Result)
      return Gtk.Widget.Gtk_Widget is
   begin
      --  Display the plugins root page
      Self.Kernel.Get_Preferences.Get_Editor.Display_Page
        (Self.Plugin_Page.Get_Name);

      --  Return null so that no preview widget is displayed
      return null;
   end Full;

   ---------------------
   -- Page_Is_Visible --
   ---------------------

   function Page_Is_Visible
     (Model : Gtk_Tree_Model;
      Iter  : Gtk_Tree_Iter;
      Data  : GPS_Preferences_Editor) return Boolean
   is
      Row_Visible : Boolean := True;
      Child       : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      --  Compute the row itself should be visible (not withstanding its
      --  children.
      if Data.Filter_Pattern /= null then
         Row_Visible :=
           Data.Filter_Pattern.Start (Get_String (Model, Iter, 0)) /= No_Match;
      end if;

      --  If the row should be invisible, but any of its children is visible,
      --  we display it anyway.
      if not Row_Visible then
         Child := Children (Model, Iter);
         while Child /= Null_Iter loop
            if Page_Is_Visible (Model, Child, Data) then
               return True;
            end if;
            Next (Model, Child);
         end loop;
      end if;

      return Row_Visible;

   exception
      when E : others =>
         Trace (Me, E);
         return True;
   end Page_Is_Visible;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access GPS_Preferences_Editor_Record;
      Toolbar : not null access Gtk_Toolbar_Record'Class) is

      Overall_Provider     : access Overall_Preferences_Search_Provider;
      Preferences_Provider : access Custom_Preferences_Search_Provider;
      Plugins_Provider     : access Custom_Plugins_Search_Provider;
      Registry : Search_Provider_Registry_Access;
   begin
      --  Create the provider for preferences
      Preferences_Provider := new Custom_Preferences_Search_Provider;
      Preferences_Provider.Kernel := View.Kernel;
      Preferences_Provider.Rank := 1;

      --  Create the provider for plugins
      Plugins_Provider := new Custom_Plugins_Search_Provider;
      Plugins_Provider.Kernel := View.Kernel;
      Plugins_Provider.Rank := 2;

      --  Create the overall provider, and register the preferences and
      --  plugins provider.
      Overall_Provider := new Overall_Preferences_Search_Provider;
      Overall_Provider.Kernel := View.Kernel;

      Registry := new Search_Provider_Registry;
      Registry.Register (Preferences_Provider);
      Registry.Register (Plugins_Provider);

      Overall_Provider.Initialize (Registry);

      View.Build_Search
        (Toolbar        => Toolbar,
         P              => Overall_Provider,
         Name           => "preferences_editor_search",
         Case_Sensitive => False);
   end Create_Toolbar;

   --------------------
   -- Filter_Changed --
   --------------------

   overriding procedure Filter_Changed
     (Self    : not null access GPS_Preferences_Editor_Record;
      Pattern : in out Search_Pattern_Access) is
   begin
      Free (Self.Filter_Pattern);
      Self.Filter_Pattern := Pattern;
      Self.Filter.Refilter;

      if Pattern /= null then
         Self.Pages_Tree.Expand_All;  --  show all results more conveniently
      end if;
   end Filter_Changed;

   ----------------
   -- Get_Widget --
   ----------------

   overriding function Get_Widget
     (Self : not null access GPS_Preferences_Editor_Record)
      return Gtk.Widget.Gtk_Widget is
   begin
      return Gtk_Widget (Self);
   end Get_Widget;

   ------------------
   -- Display_Page --
   ------------------

   overriding procedure Display_Page
     (Self      : not null access GPS_Preferences_Editor_Record;
      Page_Name : String)
   is
      Page_Found : Boolean;
      Page_Iter  : Gtk_Tree_Iter;
      Page_Path  : Gtk_Tree_Path;
   begin
      --  Try to get the page containing it
      Page_Found := Find_Page_Iter
        (Self, Page_Iter, Page_Name);

      --  If found, select it in the editor's tree view
      if Page_Found then
         Page_Path := Self.Model.Get_Path (Page_Iter);
         Self.Pages_Tree.Set_Cursor (Page_Path, null, False);
      else
         declare
            Root_Page_Name : constant String := Get_Root_Page (Page_Name);
            Root_Page_View : Preferences_Page_View;
         begin
            Page_Found := Find_Page_Iter (Self, Page_Iter, Root_Page_Name);
            Page_Path := Self.Model.Get_Path (Page_Iter);
            Root_Page_View := Preferences_Page_View
              (Self.Pages_Notebook.Get_Nth_Page
                 (Get_Int (Self.Model, Page_Iter, 1)));

            Self.Pages_Tree.Set_Cursor (Page_Path, null, False);
            Root_Page_View.Display_Subpage (Page_Name);
         end;
      end if;
   end Display_Page;

   ------------------
   -- Display_Pref --
   ------------------

   overriding procedure Display_Pref
     (Self      : not null access GPS_Preferences_Editor_Record;
      Pref      : not null Preference;
      Highlight : Boolean := False)
   is
      Page_View : Preferences_Page_View;
   begin
      if Self.Highlighted_Pref /= null then
         Page_View := Get_Selected_Page_View (Self);
         Page_View.Set_Pref_Highlighted (Pref      => Self.Highlighted_Pref,
                                         Highlight => False);
      end if;

      Self.Display_Page (Pref.Get_Page_Name);

      if Highlight then
         Page_View := Get_Selected_Page_View (Self);
         Page_View.Set_Pref_Highlighted (Pref      => Pref,
                                         Highlight => Highlight);
         Self.Highlighted_Pref := Pref;
      end if;
   end Display_Pref;

   -------------------
   -- Get_Page_View --
   -------------------

   overriding function Get_Page_View
     (Self      : not null access GPS_Preferences_Editor_Record;
      Page_Name : String) return Gtk.Widget.Gtk_Widget
   is
      Page_Found : Boolean;
      Page_Iter  : Gtk_Tree_Iter;
   begin
      Page_Found := Find_Page_Iter
        (Self, Page_Iter, Page_Name);

      if Page_Found then
         return Self.Pages_Notebook.Get_Nth_Page
           (Get_Int (Self.Model, Page_Iter, 1));
      end if;

      return null;
   end Get_Page_View;

   ----------------------------
   -- Get_Selected_Page_View --
   ----------------------------

   function Get_Selected_Page_View
     (Self : not null GPS_Preferences_Editor) return Preferences_Page_View
   is
      Iter      : Gtk_Tree_Iter;
      M         : Gtk_Tree_Model;
   begin
      Get_Selected (Get_Selection (Self.Pages_Tree), M, Iter);

      return Preferences_Page_View
        (Self.Pages_Notebook.Get_Nth_Page (Get_Int (M, Iter, 1)));
   end Get_Selected_Page_View;

   -----------------------
   -- Selection_Changed --
   -----------------------

   procedure Selection_Changed (Widget : access Gtk_Widget_Record'Class) is
      Pref_View : constant GPS_Preferences_Editor :=
                    GPS_Preferences_Editor (Widget);
      Iter      : Gtk_Tree_Iter;
      M         : Gtk_Tree_Model;
   begin
      Get_Selected (Get_Selection (Pref_View.Pages_Tree), M, Iter);

      if Iter /= Null_Iter then
         --  Get the newly selected page index from the model and set it as the
         --  current page to display for the notebook.
         Pref_View.Current_Page_Index := Get_Int (M, Iter, 1);
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
      if Pref /= null and then Has_GObject_To_Update (Pref) then
         Pref.Update_On_Pref_Changed (Get_GObject_To_Update (Pref));
      end if;
   end Execute;

   --------------------
   -- Find_Page_Iter --
   --------------------

   function Find_Page_Iter
     (Editor           : not null GPS_Preferences_Editor;
      Page_Iter        : out Gtk_Tree_Iter;
      Page_Name        : String;
      Create_If_Needed : Boolean := False) return Boolean
   is
      Current       : Gtk_Tree_Iter := Null_Iter;
      Child         : Gtk_Tree_Iter;
      First, Last   : Integer := Page_Name'First;
      Page_Found    : Boolean := True;
   begin
      while First <= Page_Name'Last loop
         Last := First;

         while Last <= Page_Name'Last
           and then Page_Name (Last) /= '/'
         loop
            Last := Last + 1;
         end loop;

         if Current = Null_Iter then
            Child := Get_Iter_First (Editor.Model);
         else
            Child := Children (Editor.Model, Current);
         end if;

         while Child /= Null_Iter
           and then
             Get_String (Editor.Model, Child, 0) /= Page_Name
           (First .. Last - 1)
         loop
            Next (Editor.Model, Child);
         end loop;

         if Child = Null_Iter then
            if Create_If_Needed then
               Page_Found := False;
               Append (Editor.Model, Child, Current);
               Set (Editor.Model, Child, 0, Page_Name (First .. Last - 1));
               Set
                 (Editor.Model, Child, 1, Editor.Pages_Notebook.Get_N_Pages);
            else
               return False;
            end if;
         end if;

         Current := Child;
         First := Last + 1;
      end loop;

      Page_Iter := Current;

      return Page_Found;
   end Find_Page_Iter;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access GPS_Preferences_Editor_Record'Class) return Gtk_Widget
   is
      Manager        : constant Preferences_Manager := Self.Kernel.Preferences;
      Filename       : constant Virtual_File := Self.Kernel.Preferences_File;
      Main_Pane               : Gtk_Paned;
      Col                     : Gtk_Tree_View_Column;
      Render                  : Gtk_Cell_Renderer_Text;
      Num                     : Gint;
      Scrolled_Pages_Tree     : Gtk_Scrolled_Window;
      Page                    : Preferences_Page;
      Page_Iter               : Gtk_Tree_Iter;
      Page_Curs               : Page_Cursor := Manager.Get_First_Reference;
      Page_Found              : Boolean;
      Backup_Created          : Boolean;
      Backup_File             : constant Virtual_File :=
                        Create (Full_Filename => Filename.Full_Name & ".bkp");

      pragma Unreferenced (Page_Found, Num);
   begin
      Filename.Copy (Backup_File.Full_Name, Success => Backup_Created);

      Initialize_Vbox (Self);
      Self.Set_Name ("Preferences");  --  for testsuite
      Self.On_Destroy (On_Destroy'Access);

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
      Gtk_New (Self.Filter, +Self.Model);
      Preferences_Editor_Visible_Funcs.Set_Visible_Func
        (Self.Filter, Page_Is_Visible'Access, Self);
      Gtk_New (Self.Pages_Tree, Self.Filter);
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

      --  Iterate over all the registered preferences pages and append their
      --  widgets to the preferences editor notebook.
      loop
         Page := Get_Page (Page_Curs);
         exit when Page = null;

         --  Don't display pages without names
         if Page.Get_Page_Type = Visible_Page then
            Page_Found := Find_Page_Iter (Editor           => Self,
                                          Page_Iter        => Page_Iter,
                                          Page_Name        => Page.Get_Name,
                                          Create_If_Needed => True);

            Self.Pages_Notebook.Append_Page (Page.Get_Widget (Manager), null);
         end if;

         Next (Page_Curs);
      end loop;

      Self.Set_Can_Focus (True);

      --  Show all pages for more convenient access
      Self.Pages_Tree.Expand_All;

      --  Register a hook function which will update all the prefernces
      --  widgets when preferences changes.
      Preferences_Changed_Hook.Add (new On_Pref_Changed);

      --  Select and display the first page when opening the Preferences view
      Select_First_Row (Self.Pages_Tree);

      return Gtk_Widget (Self);
   end Initialize;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class) is
      Editor  : constant GPS_Preferences_Editor :=
                  GPS_Preferences_Editor (Widget);
      Manager : constant GPS_Preferences_Manager :=
                  GPS_Preferences_Manager (Editor.Kernel.Get_Preferences);
   begin
      --  Clear the maps mapping preferences and alert the manager that its
      --  editor has been destroyed.
      Remove_All_GObjects_To_Update;
      Manager.Set_Editor (null);
   end On_Destroy;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Preferences_Editor_Views.Register_Module (Kernel);
   end Register_Module;

end GPS.Kernel.Preferences_Views;
