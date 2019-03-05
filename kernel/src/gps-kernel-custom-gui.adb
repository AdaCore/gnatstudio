------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2019, AdaCore                     --
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

with GNAT.Directory_Operations;             use GNAT.Directory_Operations;
with GNATCOLL.Projects;
with GNATCOLL.Utils;                        use GNATCOLL.Utils;
with GNATCOLL.VFS;                          use GNATCOLL.VFS;
with GUI_Utils;                             use GUI_Utils;
with String_Utils;                          use String_Utils;

with Glib.Object;                           use Glib, Glib.Object;
with Glib.Values;                           use Glib.Values;
with Glib_Values_Utils;                     use Glib_Values_Utils;

with Gtk.Box;                               use Gtk.Box;
with Gtk.Cell_Renderer;                     use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Toggle;              use Gtk.Cell_Renderer_Toggle;
with Gtk.Dialog;                            use Gtk.Dialog;
with Gtk.Enums;                             use Gtk.Enums;
with Gtk.Label;                             use Gtk.Label;
with Gtk.List_Box_Row;                      use Gtk.List_Box_Row;
with Gtk.Notebook;                          use Gtk.Notebook;
with Gtk.Paned;                             use Gtk.Paned;
with Gtk.Scrolled_Window;                   use Gtk.Scrolled_Window;
with Gtk.Text_Tag;                          use Gtk.Text_Tag;
with Gtk.Tree_Model;                        use Gtk.Tree_Model;
with Gtk.Tree_Selection;                    use Gtk.Tree_Selection;
with Gtk.Tree_Store;                        use Gtk.Tree_Store;
with Gtk.Tree_View;                         use Gtk.Tree_View;
with Gtk.Tree_View_Column;                  use Gtk.Tree_View_Column;
with Gtk.Widget;                            use Gtk.Widget;
with Gtkada.Handlers;                       use Gtkada.Handlers;

with Dialog_Utils;                          use Dialog_Utils;
with GPS.Kernel;                            use GPS.Kernel;
with GPS.Kernel.Custom;                     use GPS.Kernel.Custom;
with GPS.Intl;                              use GPS.Intl;
with GPS.Main_Window;                       use GPS.Main_Window;

package body GPS.Kernel.Custom.GUI is

   Startup_Module : Startup_Module_ID;

   Column_Load_Name   : aliased String := "Load";
   Column_Name_Name   : aliased String := "Name";

   Column_Load          : constant := 0;
   Column_Name          : constant := 1;
   Column_Explicit      : constant := 2;
   Column_Modified      : constant := 3;
   Column_Background    : constant := 4;
   Column_Plugin_Name   : constant := 5;
   Column_Subpage_Name  : constant := 6;
   Column_Page          : constant := 7;

   Column_Types : constant GType_Array :=
     (Column_Load          => GType_Boolean,
      Column_Name          => GType_String,
      Column_Explicit      => GType_Boolean,
      Column_Modified      => GType_Boolean,
      Column_Background    => GType_String,
      Column_Plugin_Name   => GType_String,
      Column_Subpage_Name  => GType_String,
      Column_Page          => GType_Int);

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

   procedure On_Destroy_Preferences_Dialog
     (Widget : access Gtk_Widget_Record'Class);
   --  Called when the preferences dialog is being closed.
   --  If the used has set/unset some startup scripts, display a dialog
   --  asking the user if he wants to restart now or not.

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
      Path : constant Gtk_Tree_Path :=
        Self.Model.Get_Path (Get_Subpage_Iter (Self, Subpage_Name));

   begin
      Self.Tree.Set_Cursor (Path, null, False);
      Path_Free (Path);
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
      Editor.Tree.Get_Selection.Get_Selected (M, Iter);

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
         W    : Gtk_Widget;

      begin
         if Subpage.all in Plugin_Preferences_Page_Record'Class then
            declare
               Plugin_Subpage : constant Plugin_Preferences_Page :=
                                  Plugin_Preferences_Page (Subpage);
               Plugin_Name    : constant String :=
                                  Plugin_Subpage.Get_Plugin_Name;
               Script         : constant Script_Description_Access :=
                                  Get_Script_From_Base_Name
                                    (Kernel    => Startup_Module.Get_Kernel,
                                     Base_Name => Plugin_Name);
            begin
               --  Append a node to the model
               Append (Editor.Model, Iter, Null_Iter);

               Set_And_Clear
                 (Editor.Model, Iter,
                  (Column_Load, Column_Name, Column_Explicit, Column_Modified,
                   Column_Plugin_Name, Column_Subpage_Name, Column_Page),
                  (1 => As_Boolean
                       (Script.Loaded or else Script.Mode = Explicit_On),
                   2 => As_String  (Plugin_Subpage.Get_Plugin_Label),
                   3 => As_Boolean (Plugin_Subpage.Explicit),
                   4 => As_Boolean (False),
                   5 => As_String  (Plugin_Name),
                   6 => As_String  (Plugin_Subpage.Get_Name),
                   7 => As_Int     (Editor.Plugins_Notebook.Get_N_Pages)));

               --  Append the plugin page widget to the notebook
               W := Plugin_Subpage.Get_Widget (Manager);
               --  GtkNotebook refuses to switch to a page unless the child
               --  widget is visible. Therefore, it is recommended to
               --  show child widgets before adding them to a notebook.
               W.Show_All;
               Editor.Plugins_Notebook.Append_Page (W, null);
            end;
         end if;
      end Append_Plugin_Subpage;

   begin
      --  Create the page for the plugins editor
      Editor := new Startup_Editor_Page_View_Record;
      Dialog_Utils.Initialize (Preferences_Page_View (Editor));

      --  Set On_Destroy so that we know when the preferences dialog
      --  is being closed.
      Editor.On_Destroy (On_Destroy_Preferences_Dialog'Access);

      --  Create the list tree view which list all the registered plugins
      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Never, Policy_Automatic);
      Editor.Tree := Create_Tree_View
        (Column_Types       => Column_Types,
         Column_Names       => (Column_Name + 1   =>
                                    Column_Name_Name'Unchecked_Access,
                                Column_Load + 1   =>
                                  Column_Load_Name'Unchecked_Access),
         Show_Column_Titles => True,
         Initial_Sort_On    => Column_Name + 1);
      Editor.Model := -Get_Model (Editor.Tree);
      Set_Search_Column (Editor.Tree, Column_Name);
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
      Gtk_New_Hpaned (Pane);
      Pane.Pack1 (Scrolled, Resize => False, Shrink => False);
      Pane.Pack2 (Editor.Plugins_Notebook, Resize => True, Shrink => True);
      Editor.Append (Pane, Expand => True, Fill => True);

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

   ----------------
   -- Get_Widget --
   ----------------

   overriding function Get_Widget
     (Self    : not null access Plugins_Preferences_Assistant_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget
   is
      Editor : constant Startup_Editor := Startup_Editor
        (Root_Plugins_Preferences_Page_Record
           (Self.all).Get_Widget (Manager));
   begin
      Editor.Show_Restart_Dialog := False;

      return Gtk_Widget (Editor);
   end Get_Widget;

   --------------------
   -- Register_Group --
   --------------------

   overriding procedure Register_Group
     (Self             : not null access Plugin_Preferences_Page_Record;
      Name             : String;
      Group            : not null access Preferences_Group_Record'Class;
      Priority         : Integer := -1;
      Replace_If_Exist : Boolean := False) is
   begin
      if Name /= "" then
         Preferences_Page_Record (Self.all).Register_Group
           (Name             => Name,
            Group            => Group,
            Priority         => Priority,
            Replace_If_Exist => Replace_If_Exist);
      else
         Preferences_Page_Record (Self.all).Register_Group
           (Name             => "Preferences",
            Group            => Group,
            Priority         => Priority,
            Replace_If_Exist => Replace_If_Exist);
      end if;
   end Register_Group;

   ----------------
   -- Get_Widget --
   ----------------

   overriding function Get_Widget
     (Self    : not null access Plugin_Preferences_Page_Record;
      Manager : not null Preferences_Manager)
      return Gtk.Widget.Gtk_Widget
   is
      Page_View        : Preferences_Page_View;
      Doc_Group_Widget : Preferences_Group_Widget;
      Doc_Label        : Gtk_Label;
   begin
      --  Create a new page
      Page_View := new Preferences_Page_View_Record;
      Dialog_Utils.Initialize (Page_View);

      --  Create and build the preferences page view from Self
      Page_View.Build (Page => Self, Manager => Manager);

      --  Create the text view group which will contain the plugin
      --  documentation
      Doc_Group_Widget := new Preferences_Group_Widget_Record;
      Doc_Group_Widget.Initialize
        (Group_Name  => "Documentation",
         Parent_View => Page_View);
      Gtk_New (Doc_Label, Self.Get_Documentation);
      Doc_Label.Set_Alignment (0.0, 0.0);
      Doc_Group_Widget.Append_Child (Doc_Label);

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
     (if Self.Plugin_Name /= null then Self.Plugin_Name.all else "");

   ----------------------
   -- Get_Plugin_Label --
   ----------------------

   function Get_Plugin_Label
     (Self : not null access Plugin_Preferences_Page_Record)
      return String
   is
     (if Self.Plugin_Label /= null then Self.Plugin_Label.all else "");

   --------------------------------------------
   -- Register_All_Plugins_Preferences_Pages --
   --------------------------------------------

   procedure Register_All_Plugins_Preferences_Pages
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      Manager   : constant Preferences_Manager := Kernel.Get_Preferences;
      Root_Page : Root_Plugins_Preferences_Page :=
                    new Root_Plugins_Preferences_Page_Record;

      procedure Register_Plugin_Page
        (Name      : String;
         File      : GNATCOLL.VFS.Virtual_File;
         Loaded    : Boolean;
         Explicit  : Boolean;
         Page_Type : Preferences_Page_Type := Integrated_Page);
      --  Used to register a plugin page in the Preferences Editor or the
      --  Preferences Assistant.

      procedure Register_Plugin_Preferences_Page
        (Name      : String;
         File      : GNATCOLL.VFS.Virtual_File;
         Loaded    : Boolean;
         Explicit  : Boolean);
      --  Register a plugin page in the Preferences editor

      procedure Register_Plugin_Preferences_Assistant_Page
        (Base_Name : String);
      --  Register a plugin page in the Preferences Assistant

      --------------------------
      -- Register_Plugin_Page --
      --------------------------

      procedure Register_Plugin_Page
        (Name      : String;
         File      : GNATCOLL.VFS.Virtual_File;
         Loaded    : Boolean;
         Explicit  : Boolean;
         Page_Type : Preferences_Page_Type := Integrated_Page)
      is
         pragma Unreferenced (Loaded);
         Plugin_Page      : constant Plugin_Preferences_Page :=
                              new Plugin_Preferences_Page_Record;
         Name_Without_Ext : constant String :=
                              Base_Name (Name, File_Extension (Name));
         Page_Name        : constant String :=
                              Root_Page.Get_Name & Name_Without_Ext & '/';
         Label            : constant String := Format_Title (Name_Without_Ext);
      begin
         --  Set the plugin page attributes
         Plugin_Page.Plugin_Name := new String'(Name);
         Plugin_Page.Plugin_Label := new String'(Label);
         Plugin_Page.File := File;
         Plugin_Page.Explicit := Explicit;
         Plugin_Page.Doc := Get_Plugin_Doc (File);

         --  Register the plugin subpage.
         Kernel.Get_Preferences.Register_Page
           (Name             => Page_Name,
            Page             => Preferences_Page (Plugin_Page),
            Priority         => -2,
            Page_Type        => Page_Type,
            Replace_If_Exist => True);
      end Register_Plugin_Page;

      --------------------------------------
      -- Register_Plugin_Preferences_Page --
      --------------------------------------

      procedure Register_Plugin_Preferences_Page
        (Name      : String;
         File      : GNATCOLL.VFS.Virtual_File;
         Loaded    : Boolean;
         Explicit  : Boolean)
      is
      begin
         Register_Plugin_Page
           (Name      => Name,
            File      => File,
            Loaded    => Loaded,
            Explicit  => Explicit,
            Page_Type => Integrated_Page);
      end Register_Plugin_Preferences_Page;

      ------------------------------------------------
      -- Register_Plugin_Preferences_Assistant_Page --
      ------------------------------------------------

      procedure Register_Plugin_Preferences_Assistant_Page
        (Base_Name : String)
      is
         Script : constant Script_Description_Access :=
                    Get_Script_From_Base_Name
                      (Kernel    => Kernel,
                       Base_Name => Base_Name);
      begin
         if Script = null then
            return;
         end if;

         Register_Plugin_Page
           (Name      => Base_Name,
            File      => Script.File,
            Loaded    => Script.Loaded,
            Explicit  => Script.Mode /= Automatic,
            Page_Type => Assistant_Page);
      end Register_Plugin_Preferences_Assistant_Page;

   begin
      --  Register the root 'Plugins' page
      Manager.Register_Page
        (Name             => "Plugins/",
         Page             => Preferences_Page (Root_Page),
         Priority         => -2,
         Replace_If_Exist => True);

      --  Register all the plugins subpages
      For_All_Startup_Scripts
        (Kernel, Register_Plugin_Preferences_Page'Access);

      --  Create a plugins preferences page for the Preferences Assistant and
      --  register only the relevant plugins for newcomers.

      Root_Page := new Plugins_Preferences_Assistant_Page_Record;
      Manager.Register_Page
        (Name             => "Preferences Assistant Plugins/",
         Page             => Preferences_Page (Root_Page),
         Page_Type        => Assistant_Page);

      Register_Plugin_Preferences_Assistant_Page ("copy_paste.py");
      Register_Plugin_Preferences_Assistant_Page ("copy_paste_toolbar.py");
      Register_Plugin_Preferences_Assistant_Page ("closeold.py");
      Register_Plugin_Preferences_Assistant_Page ("ispell.py");
   end Register_All_Plugins_Preferences_Pages;

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
           Get_String (Editor.Model, Iter, Column_Plugin_Name),
         Load      => Get_Boolean (Editor.Model, Iter, Column_Load));
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
      --  If the editor should display a dialog when closed and if the set of
      --  startup scripts to load has changed, display a dialog asking if the
      --  user wants to restart or not.

      if Editor.Show_Restart_Dialog and then Is_Modified then
         declare
            Dialog       : Gtk_Dialog;
            Button       : Gtk_Widget;
            Label        : Gtk_Label;
            Response     : Gtk_Response_Type;
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

            Dialog.Show_All;
            Response := Dialog.Run;
            Destroy (Dialog);

            Save_Startup_Scripts_List (Kernel);

            --  Quit GPS if the user wants to restart imediately
            if Response = Gtk_Response_OK then
               Quit (GPS_Window (Get_Main_Window (Kernel)),
                     Status => 100);
            end if;
         end;
      else
         Save_Startup_Scripts_List (Kernel);
      end if;
   end On_Destroy_Preferences_Dialog;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Startup_Module := new Startup_Module_ID_Record;
      Register_Module (Startup_Module, Kernel, "Plugins manager");

      Register_All_Plugins_Preferences_Pages (Kernel);
   end Register_Module;

end GPS.Kernel.Custom.GUI;
