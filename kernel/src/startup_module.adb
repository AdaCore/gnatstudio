------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2016, AdaCore                     --
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

with GNAT.OS_Lib;                           use GNAT.OS_Lib;
with GNAT.Directory_Operations;             use GNAT.Directory_Operations;
with GNATCOLL.Projects;
with GNATCOLL.Utils;                        use GNATCOLL.Utils;
with GNATCOLL.VFS;                          use GNATCOLL.VFS;
with GUI_Utils;                             use GUI_Utils;
with String_Utils;                          use String_Utils;

with Glib.Object;                           use Glib, Glib.Object;
with Glib.Values;                           use Glib.Values;
with Gtk.Box;                               use Gtk.Box;
with Gtk.Cell_Renderer;                     use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Toggle;              use Gtk.Cell_Renderer_Toggle;
with Gtk.Dialog;                            use Gtk.Dialog;
with Gtk.Enums;                             use Gtk.Enums;
with Gtk.Label;                             use Gtk.Label;
with Gtk.Link_Button;                       use Gtk.Link_Button;
with Gtk.List_Box_Row;                      use Gtk.List_Box_Row;
with Gtk.Notebook;                          use Gtk.Notebook;
with Gtk.Paned;                             use Gtk.Paned;
with Gtk.Scrolled_Window;                   use Gtk.Scrolled_Window;
with Gtk.Text_Buffer;                       use Gtk.Text_Buffer;
with Gtk.Text_Iter;                         use Gtk.Text_Iter;
with Gtk.Text_Tag;                          use Gtk.Text_Tag;
with Gtk.Text_View;                         use Gtk.Text_View;
with Gtk.Tree_Model;                        use Gtk.Tree_Model;
with Gtk.Tree_Selection;                    use Gtk.Tree_Selection;
with Gtk.Tree_Store;                        use Gtk.Tree_Store;
with Gtk.Tree_View;                         use Gtk.Tree_View;
with Gtk.Tree_View_Column;                  use Gtk.Tree_View_Column;
with Gtk.Widget;                            use Gtk.Widget;
with Gtkada.Handlers;                       use Gtkada.Handlers;

with GPS.Kernel;                            use GPS.Kernel;
with GPS.Kernel.Custom;                     use GPS.Kernel.Custom;
with GPS.Kernel.Hooks;                      use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;                use GPS.Kernel.Preferences;
with GPS.Intl;                              use GPS.Intl;
with GPS.Main_Window;                       use GPS.Main_Window;

--------------------
-- Startup_Module --
--------------------

package body Startup_Module is

   Startup_Module : Startup_Module_ID;

   Column_Load_Name   : aliased String := "Load";
   Column_Name_Name   : aliased String := "Name";
   Column_Loaded_Name : aliased String := "Loaded at startup";

   Column_Load          : constant := 0;
   Column_Name          : constant := 1;
   Column_Loaded        : constant := 2;
   Column_Explicit      : constant := 3;
   Column_Modified      : constant := 4;
   Column_Background    : constant := 5;
   Column_Name_With_Ext : constant := 6;
   Column_Subpage_Name  : constant := 7;
   Column_Page          : constant := 8;

   type Kernel_Link_Button_Record is new Gtk_Link_Button_Record with record
      Kernel : Kernel_Handle;
   end record;
   type Kernel_Link_Button is access all Kernel_Link_Button_Record'Class;

   procedure Register_All_Plugins_Preferences_Pages
     (Kernel : not null access Kernel_Handle_Record'Class);
   --  Register a page in the preferences for each plugin that was found.

   function Get_Plugin_Doc
     (File : GNATCOLL.VFS.Virtual_File) return GNAT.Strings.String_Access;
   --  Return the plugin documentation by reading the python docstrings
   --  of the given plugin file.

   procedure On_Load_Toggled
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Toggled when the loading status of a script is changed.

   procedure Set_Modified
     (Editor : not null access Startup_Editor_Page_View_Record'Class;
      Iter   : Gtk_Tree_Iter);
   --  Set the given iter as modified in the model.

   procedure Save (Editor : access Startup_Editor_Page_View_Record'Class;
                   Iter   : Gtk_Tree_Iter);
   --  Save the changes done for this plugin page.

   procedure On_Selection_Changed (Widget : access Gtk_Widget_Record'Class);
   --  Called when the selection in the tree has changed.

   function On_File_Clicked
     (Self : access Gtk_Link_Button_Record'Class) return Boolean;
   --  Called when the user requests to view the source file.

   procedure On_Destroy_Preferences_Dialog
     (Widget : access Gtk_Widget_Record'Class);
   --  Called when the preferences dialog is being closed.
   --  If the used has set/unset some startup scripts, display a dialog
   --  asking the user if he wants to restart now or not.

   function Is_Loaded
     (Editor : not null access Startup_Editor_Page_View_Record'Class;
      Iter   : Gtk_Tree_Iter) return String;
   --  Return a String indicating if the script refered by Iter is currently
   --  loaded and how it has been loaded.

   function Get_Subpage_Iter
     (Editor       : not null access Startup_Editor_Page_View_Record'Class;
      Subpage_Name : String) return Gtk_Tree_Iter;
   --  Return the page index in the notebook for Subpage_Name.

   ----------------------
   -- Get_Subpage_Iter --
   ----------------------

   function Get_Subpage_Iter
     (Editor       : not null access Startup_Editor_Page_View_Record'Class;
      Subpage_Name : String) return Gtk_Tree_Iter
   is
      Iter        : Gtk_Tree_Iter;
   begin
      Iter := Get_Iter_First (Editor.Model);

      while Iter /= Null_Iter loop
         exit when Get_String
           (Editor.Model, Iter, Column_Subpage_Name) = Subpage_Name;

         Next (Editor.Model, Iter);
      end loop;

      return Iter;
   end Get_Subpage_Iter;

   ---------------------
   -- Display_Subpage --
   ---------------------

   overriding procedure Display_Subpage
     (Self         : not null access Startup_Editor_Page_View_Record;
      Subpage_Name : String)
   is
      Subpage_Iter : constant Gtk_Tree_Iter :=
                      Get_Subpage_Iter (Editor       => Self,
                                        Subpage_Name => Subpage_Name);
      Subpage_Path : constant Gtk_Tree_Path :=
                       Self.Model.Get_Path (Subpage_Iter);
   begin
      Self.Tree.Set_Cursor (Subpage_Path, null, False);
   end Display_Subpage;

   --------------------------
   -- Set_Pref_Highlighted --
   --------------------------

   overriding procedure Set_Pref_Highlighted
     (Self      : not null access Startup_Editor_Page_View_Record;
      Pref      : not null access Preference_Record'Class;
      Highlight : Boolean)
   is
      Subpage_Iter  : constant Gtk_Tree_Iter :=
                       Get_Subpage_Iter (Editor       => Self,
                                         Subpage_Name => Pref.Get_Page_Name);
      Subpage_Index : constant Gint :=
                          Get_Int (Self.Model, Subpage_Iter, Column_Page);
      Subpage_View  : constant Preferences_Page_View :=
                        Preferences_Page_View
                          (Self.Plugins_Notebook.Get_Nth_Page
                             (Subpage_Index));

   begin
      Subpage_View.Set_Pref_Highlighted (Pref, Highlight);
   end Set_Pref_Highlighted;

   ---------------
   -- Is_Loaded --
   ---------------

   function Is_Loaded
     (Editor : not null access Startup_Editor_Page_View_Record'Class;
      Iter   : Gtk_Tree_Iter) return String
   is
      Loaded : constant String :=
                 (if Get_Boolean (Editor.Model, Iter, Column_Load) then
                     -"yes"
                  else
                     -"no ");
   begin
      if Get_Boolean (Editor.Model, Iter, Column_Modified) then
         return Loaded
           & (-" (Modified in this dialog)");
      elsif Get_Boolean (Editor.Model, Iter, Column_Explicit) then
         return Loaded & (-" (explicitly set by user)");
      elsif Get_Boolean (Editor.Model, Iter, Column_Load) then
         return Loaded & (-" (found in auto-loading directory)");
      else
         return Loaded & (-" (found in no auto-loading directory)");
      end if;
   end Is_Loaded;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed (Widget : access Gtk_Widget_Record'Class)
   is
      Editor     : constant Startup_Editor := Startup_Editor (Widget);
      Iter       : Gtk_Tree_Iter;
      M          : Gtk_Tree_Model;
      Page_Index : Gint;
   begin
      Get_Selected (Get_Selection (Editor.Tree), M, Iter);

      if Iter /= Null_Iter then
         --  Get the newly selected page index from the model and set it as the
         --  current page to display for the notebook.
         Page_Index := Get_Int (M, Iter, Column_Page);
         Editor.Plugins_Notebook.Set_Current_Page (Page_Index);
      end if;
   end On_Selection_Changed;

   ---------------------
   -- On_Load_Toggled --
   ---------------------

   procedure On_Load_Toggled
     (Widget : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      Editor      : constant Startup_Editor := Startup_Editor (Widget);
      Path_String : constant String := Get_String (Nth (Params, 1));
      Iter        : Gtk_Tree_Iter;
   begin
      Iter := Get_Iter_From_String (Editor.Model, Path_String);
      Set_Modified (Editor, Iter);
   end On_Load_Toggled;

   ------------------
   -- Set_Modified --
   ------------------

   procedure Set_Modified
     (Editor : not null access Startup_Editor_Page_View_Record'Class;
      Iter   : Gtk_Tree_Iter)
   is
      Modified : constant Boolean :=
                   Get_Boolean (Editor.Model, Iter, Column_Modified);
   begin
      Set (Editor.Model, Iter, Column_Modified, not Modified);
      Set (Editor.Model, Iter, Column_Loaded,
           Is_Loaded (Editor, Iter));

      Save (Editor, Iter);
   end Set_Modified;

   --------------------
   -- Get_Plugin_Doc --
   --------------------

   function Get_Plugin_Doc
     (File : GNATCOLL.VFS.Virtual_File) return GNAT.Strings.String_Access
   is
      Contents    : GNAT.Strings.String_Access;
      Doc         : GNAT.Strings.String_Access := null;
      First, Last : Integer;
   begin
      Contents := Read_File (File);

      if Contents /= null then
         First := Contents'First;
         Skip_Blanks (Contents.all, First);

         if Looking_At (Contents.all, First, """""""") then
            First := First + 3;
            Last := First;
            Skip_To_String (Contents.all, Last, """""""");
            Last := Last - 1;
         elsif Looking_At (Contents.all, First, "'''") then
            First := First + 3;
            Last := First;
            Skip_To_String (Contents.all, Last, "'''");
            Last := Last - 1;
         else
            Last := Contents'Last;
         end if;

         Skip_Blanks (Contents.all, First);
         Skip_Blanks_Backward (Contents.all, Last);
         Doc := new String'(Contents (First .. Last));
         Free (Contents);
      end if;

      return Doc;
   end Get_Plugin_Doc;

   ----------------
   -- Get_Widget --
   ----------------

   overriding function Get_Widget
     (Self    : not null access Root_Plugins_Preferences_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget
   is
      Editor       : Startup_Editor;
      Scrolled     : Gtk_Scrolled_Window;
      Pane         : Gtk_Paned;
      Subpage_Curs : Subpage_Cursor;
      Subpage      : Preferences_Page;
      List         : Cell_Renderer_List.Glist;

      procedure Append_Plugin_Subpage;
      --  Append a plugin subpage to the model and the notebook.

      procedure Append_Plugin_Subpage is
         Iter : Gtk_Tree_Iter;
      begin
         if Subpage.all in Plugin_Preferences_Page_Record'Class then
            declare
               Plugin_Subpage : constant Plugin_Preferences_Page :=
                                  Plugin_Preferences_Page (Subpage);
               Name           : constant String :=
                                  (if Plugin_Subpage.Plugin_Name /= null then
                                      Plugin_Subpage.Plugin_Name.all
                                   else
                                      "");
            begin
               --  Append a node to the model
               Append (Editor.Model, Iter, Null_Iter);
               Set (Editor.Model, Iter, Column_Load,
                    Plugin_Subpage.Loaded_At_Startup);
               Set (Editor.Model, Iter, Column_Name,
                    Base_Name (Name, File_Extension (Name)));
               Set (Editor.Model, Iter, Column_Explicit,
                    Plugin_Subpage.Explicit);
               Set (Editor.Model, Iter, Column_Loaded,
                    Is_Loaded (Editor, Iter));
               Set (Editor.Model, Iter, Column_Modified, False);
               Set (Editor.Model, Iter, Column_Name_With_Ext, Name);
               Set (Editor.Model, Iter, Column_Subpage_Name, Subpage.Get_Name);
               Set (Editor.Model, Iter, Column_Page,
                    Editor.Plugins_Notebook.Get_N_Pages);

               --  Append the plugin page widget to the notebook
               Editor.Plugins_Notebook.Append_Page
                 (Plugin_Subpage.Get_Widget (Manager), null);
            end;
         end if;
      end Append_Plugin_Subpage;

   begin
      --  Create the page for the plugins editor
      Editor := new Startup_Editor_Page_View_Record;
      Default_Preferences.GUI.Initialize (Preferences_Page_View (Editor));

      --  Set On_Destroy so that we know when the preferences dialog
      --  is being closed.
      Editor.On_Destroy (On_Destroy_Preferences_Dialog'Access);

      --  Create the list tree view which list all the registered plugins
      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Editor.Tree := Create_Tree_View
        (Column_Types => (Column_Load          => GType_Boolean,
                          Column_Name          => GType_String,
                          Column_Loaded        => GType_String,
                          Column_Explicit      => GType_Boolean,
                          Column_Modified      => GType_Boolean,
                          Column_Background    => GType_String,
                          Column_Name_With_Ext => GType_String,
                          Column_Subpage_Name  => GType_String,
                          Column_Page          => GType_Int),
         Column_Names       => (Column_Name + 1   =>
                                    Column_Name_Name'Unchecked_Access,
                                Column_Load + 1   =>
                                  Column_Load_Name'Unchecked_Access,
                                Column_Loaded + 1 =>
                                  Column_Loaded_Name'Unchecked_Access),
         Show_Column_Titles => True,
         Initial_Sort_On    => Column_Name + 1);
      Editor.Model := -Get_Model (Editor.Tree);
      Scrolled.Add (Editor.Tree);

      --  Set the different callbacks for the events triggered by the tree
      Widget_Callback.Object_Connect
        (Get_Selection (Editor.Tree), Gtk.Tree_Selection.Signal_Changed,
         On_Selection_Changed'Access, Editor);
      List := Get_Cells (Get_Column (Editor.Tree, Column_Load));
      Widget_Callback.Object_Connect
        (Cell_Renderer_List.Get_Data (List),
         Gtk.Cell_Renderer_Toggle.Signal_Toggled,
         On_Load_Toggled'Access, Editor, After => True);
      Cell_Renderer_List.Free (List);

      --  Create the notebook which will contain all plugin pages
      Gtk_New (Editor.Plugins_Notebook);
      Editor.Plugins_Notebook.Set_Show_Tabs (False);

      --  Create a vertical paned view, containing the list tree view and
      --  the notebook.
      Gtk_New_Vpaned (Pane);
      Pane.Pack1 (Scrolled, True, True);
      Pane.Pack2 (Editor.Plugins_Notebook, True, True);
      Editor.Add (Pane);

      --  Iterate over all the registered plugin subpages
      Subpage_Curs := Get_First_Reference (Self);

      loop
         Subpage := Get_Subpage (Subpage_Curs);
         exit when Subpage = null;

         Append_Plugin_Subpage;

         Next (Subpage_Curs);
      end loop;

      --  Select the first row of the plugins list
      Select_First_Row (Editor.Tree);

      return Gtk_Widget (Editor);
   end Get_Widget;

   --------------
   -- Add_Pref --
   --------------

   overriding procedure Add_Pref
     (Self : not null access Plugin_Preferences_Page_Record;
      Pref : not null Preference) is
   begin
      --  Add automatically to a group called 'Preferences' if no group has
      --  been specified when registering this preference
      if Get_Group_Name (Pref) = "" then
         Preferences_Page_Record (Self.all).Add_Pref
           (Pref       => Pref,
            Group_Name => "Preferences");
      else
         Preferences_Page_Record (Self.all).Add_Pref (Pref);
      end if;
   end Add_Pref;

   ----------------
   -- Get_Widget --
   ----------------

   overriding function Get_Widget
     (Self    : not null access Plugin_Preferences_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget
   is
      Page_View        : Preferences_Page_View;
      Page_Box         : Gtk_Box;
      Prefs_Box        : Preferences_Box;
      Doc_Group_Widget : Preferences_Group_Widget;
      Doc_Text_View    : Gtk_Text_View;
      Doc_Text_Buffer  : Gtk_Text_Buffer;
      Doc_Text_Iter    : Gtk_Text_Iter;

      procedure Add_Startup_Widgets;
      --  Add all the widgets not linked with preferences (i.e: not
      --  interacting with preferences.xml file).

      -------------------------
      -- Add_Startup_Widgets --
      -------------------------

      procedure Add_Startup_Widgets
      is
         Group_Widget     : Preferences_Group_Widget;
         File_Row         : Gtk_Box;
         File_Label       : Gtk_Label;
         File_Link        : Kernel_Link_Button;
      begin
         --  Create the group
         Group_Widget := new Preferences_Group_Widget_Record;
         Group_Widget.Initialize (Group_Name => "General",
                                  Align  => False);
         Page_Box.Pack_Start (Group_Widget, Expand => False);

         --  Create the file row
         Gtk_New_Hbox (File_Row, Homogeneous => False);
         Group_Widget.Append (Widget => Gtk_Widget (File_Row));
         Gtk_New (File_Label, "File: ");
         File_Label.Set_Alignment (0.0, 0.5);
         File_Row.Pack_Start (File_Label, Expand => False);

         --  Create the file link and add it to the file row
         File_Link := new Kernel_Link_Button_Record;
         File_Link.Kernel :=
           Kernel_Handle (GPS_Preferences_Manager (Manager).Get_Kernel);
         Initialize (File_Link, "file://" & Self.File.Display_Full_Name);
         File_Link.Set_Alignment (0.0, 0.5);
         File_Link.Set_Label (Self.File.Display_Full_Name);
         File_Link.On_Activate_Link (On_File_Clicked'Access);
         File_Row.Pack_Start (File_Link, Expand => True, Fill => True);
      end Add_Startup_Widgets;

   begin
      --  Create a new page
      Page_View := new Preferences_Page_View_Record;
      Default_Preferences.GUI.Initialize (Page_View);

      --  Create the new Vbox which will hold all the page's widgets
      Gtk_New_Vbox (Page_Box);
      Page_View.Add (Page_Box);

      --  Create and add all the widgets non-related with preferences
      Add_Startup_Widgets;

      --  Create the preferences box for all the preferences registered in this
      --  page and add it to the page view.
      Prefs_Box := new Preferences_Box_Record;
      Prefs_Box.Build (Page    => Self,
                       Manager => Manager);
      Page_View.Set_Prefs_Box (Prefs_Box);
      Page_Box.Pack_Start (Prefs_Box, Expand => False);

      --  Create the text view group which will contain the plugin
      --  documentation
      Doc_Group_Widget := new Preferences_Group_Widget_Record;
      Doc_Group_Widget.Initialize (Group_Name => "Documentation",
                                   Align  => False);
      Page_Box.Pack_Start (Doc_Group_Widget);
      Gtk_New (Doc_Text_View);
      Doc_Text_View.Set_Wrap_Mode (Wrap_None);
      Doc_Text_View.Set_Editable (False);
      Doc_Text_View.Modify_Font (Default_Style.Get_Pref_Font);
      Doc_Text_Buffer := Doc_Text_View.Get_Buffer;
      Doc_Text_Buffer.Get_End_Iter (Doc_Text_Iter);
      Doc_Text_Buffer.Insert (Iter => Doc_Text_Iter,
                              Text => Self.Doc.all);
      Doc_Group_Widget.Append (Gtk_Widget (Doc_Text_View));

      return Gtk_Widget (Page_View);
   end Get_Widget;

   ----------
   -- Free --
   ----------

   overriding procedure Free (Self : in out Plugin_Preferences_Page_Record) is
   begin
      Free (Preferences_Page_Record (Self));

      Free (Self.Plugin_Name);
      Free (Self.Doc);
   end Free;

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Self : not null access Plugin_Preferences_Page_Record) return String
   is
     (if Self.Doc = null then "" else Self.Doc.all);

   ---------------------
   -- Get_Plugin_Name --
   ---------------------

   function Get_Plugin_Name
     (Self : not null access Plugin_Preferences_Page_Record) return String
   is
      (Self.Plugin_Name.all);

   --------------------------------------------
   -- Register_All_Plugins_Preferences_Pages --
   --------------------------------------------

   procedure Register_All_Plugins_Preferences_Pages
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      Manager   : constant Preferences_Manager := Kernel.Get_Preferences;
      Root_Page : constant Root_Plugins_Preferences_Page :=
                    new Root_Plugins_Preferences_Page_Record;
      procedure Register_Plugin_Preferences_Page
        (Name     : String;
         File     : GNATCOLL.VFS.Virtual_File;
         Loaded   : Boolean;
         Explicit : Boolean);

      procedure Register_Plugin_Preferences_Page
        (Name     : String;
         File     : GNATCOLL.VFS.Virtual_File;
         Loaded   : Boolean;
         Explicit : Boolean)
      is
         Plugin_Page : constant Plugin_Preferences_Page :=
                         new Plugin_Preferences_Page_Record;
         Page_Name   : constant String := "Plugins/" &
                         Base_Name (Name, File_Extension (Name)) & '/';
      begin
         --  Set the plugin page attributes
         Plugin_Page.Plugin_Name := new String'(Name);
         Plugin_Page.File := File;
         Plugin_Page.Loaded_At_Startup := Loaded;
         Plugin_Page.Explicit := Explicit;
         Plugin_Page.Doc := Get_Plugin_Doc (File);

         --  Register the plugin subpage.
         Root_Page.Register_Subpage
           (Subpage          => Preferences_Page (Plugin_Page),
            Subpage_Name     => Page_Name,
            Replace_If_Exist => True,
            Subpage_Type     => Integrated_Page);
      end Register_Plugin_Preferences_Page;

   begin
      --  Register the root 'Plugins' page
      Register_Page
        (Self             => Manager,
         Name             => "Plugins/",
         Page             => Preferences_Page (Root_Page),
         Priority         => -2,
         Replace_If_Exist => True);

      --  Register all the plugins subpages
      For_All_Startup_Scripts
        (Kernel, Register_Plugin_Preferences_Page'Access);
   end Register_All_Plugins_Preferences_Pages;

   ---------------------
   -- On_File_Clicked --
   ---------------------

   function On_File_Clicked
     (Self : access Gtk_Link_Button_Record'Class) return Boolean is
   begin
      Open_File_Action_Hook.Run
        (Kernel_Link_Button (Self).Kernel,
         Project  => GNATCOLL.Projects.No_Project,
         File     => Create (+Self.Get_Label));

      return True;
   end On_File_Clicked;

   ----------
   -- Save --
   ----------

   procedure Save (Editor : access Startup_Editor_Page_View_Record'Class;
                   Iter   : Gtk_Tree_Iter)
   is
      Kernel : constant Kernel_Handle := Startup_Module.Get_Kernel;
   begin
      Override_Startup_Script
        (Kernel    => Kernel,
         Base_Name =>
           Get_String (Editor.Model, Iter, Column_Name_With_Ext),
         Load      => Get_Boolean (Editor.Model, Iter, Column_Load));

      Startup_Module.Has_Changed := True;
   end Save;

   -----------------------------------
   -- On_Destroy_Preferences_Dialog --
   -----------------------------------

   procedure On_Destroy_Preferences_Dialog
     (Widget : access Gtk_Widget_Record'Class)
   is
      Kernel         : constant Kernel_Handle := Startup_Module.Get_Kernel;
      Editor         : constant Startup_Editor := Startup_Editor (Widget);

      function Is_Modified return Boolean;
      --  Return True if the set of startup scripts to load has been modified,
      --  False otherwise.

      function Is_Modified return Boolean
      is
         Iter         : Gtk_Tree_Iter;
      begin
         Iter := Get_Iter_First (Editor.Model);

         while Iter /= Null_Iter loop
            if Get_Boolean (Editor.Model, Iter, Column_Modified) then
               return True;
            end if;

            Next (Editor.Model, Iter);
         end loop;

         return False;
      end Is_Modified;

   begin
      --  If the set of startup scripts to load has changed, display a dialog
      --  asking if the user wants to restart or not.
      if Is_Modified then
         declare
            Dialog       : Gtk_Dialog;
            Button       : Gtk_Widget;
            Label        : Gtk_Label;
            Must_Restart : Boolean := False;
         begin
            Gtk_New (Dialog,
                     Title  => -"Restart GPS ?",
                     Parent => Get_Main_Window (Kernel),
                     Flags  => Modal);
            Gtk_New
              (Label,
               -("You have changed the status of some scripts. You will"
                 & ASCII.LF
                 & "need to restart GPS to take this change into account."
                 & ASCII.LF & ASCII.LF
                 & "Do you want to exit GPS now ?"));
            Pack_Start
              (Get_Content_Area (Dialog),
               Label, Expand => True, Fill => True);
            Button := Add_Button (Dialog, -"Exit GPS", Gtk_Response_OK);
            Button := Add_Button
              (Dialog, -"Will restart later", Gtk_Response_Cancel);
            Grab_Default (Button);

            Show_All (Dialog);
            Must_Restart := Run (Dialog) = Gtk_Response_OK;
            Destroy (Dialog);

            if Must_Restart then
               Quit (GPS_Window (Get_Main_Window (Kernel)),
                     Status => 100);
            end if;
         end;
      end if;

      --  Save the startup scripts
      Save_Startup_Scripts_List (Kernel);

      --  Destroy the page view
      On_Destroy_Page_View (Editor);
   end On_Destroy_Preferences_Dialog;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Startup_Module := new Startup_Module_ID_Record;
      Register_Module (Startup_Module, Kernel, "Plug-ins manager");

      Register_All_Plugins_Preferences_Pages (Kernel);
   end Register_Module;

end Startup_Module;
