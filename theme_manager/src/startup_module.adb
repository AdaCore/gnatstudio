------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2015, AdaCore                     --
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

with Commands.Interactive;     use Commands, Commands.Interactive;
with Glib.Object;              use Glib, Glib.Object;
with Glib.Values;              use Glib.Values;
with Gtk.Cell_Layout;          use Gtk.Cell_Layout;
with GNAT.OS_Lib;              use GNAT.OS_Lib;
with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.Custom;        use GPS.Kernel.Custom;
with GPS.Kernel.Modules;       use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with GPS.Intl;                 use GPS.Intl;
with GPS.Main_Window;          use GPS.Main_Window;
with Gtkada.Handlers;          use Gtkada.Handlers;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Cell_Renderer;        use Gtk.Cell_Renderer;
with Gtk.Cell_Renderer_Toggle; use Gtk.Cell_Renderer_Toggle;
with Gtk.Dialog;               use Gtk.Dialog;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Label;                use Gtk.Label;
with Gtk.Link_Button;          use Gtk.Link_Button;
with Gtk.Paned;                use Gtk.Paned;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Stock;                use Gtk.Stock;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_Iter;            use Gtk.Text_Iter;
with Gtk.Text_Tag;             use Gtk.Text_Tag;
with Gtk.Text_View;            use Gtk.Text_View;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Selection;       use Gtk.Tree_Selection;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View;            use Gtk.Tree_View;
with Gtk.Widget;               use Gtk.Widget;
with GUI_Utils;                use GUI_Utils;
with Pango.Enums;              use Pango.Enums;
with Pango.Font;               use Pango.Font;
with String_Utils;             use String_Utils;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNATCOLL.Projects;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with GNATCOLL.VFS;             use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;      use GNATCOLL.VFS.GtkAda;

package body Startup_Module is

      Column_Load       : constant := 0;
   Column_Name       : constant := 1;
   Column_Explicit   : constant := 2;
   Column_File       : constant := 3;
   Column_Modified   : constant := 4;
   Column_Background : constant := 5;
   Column_Name_With_Ext : constant := 6;

   type Kernel_Link_Button_Record is new Gtk_Link_Button_Record with record
      Kernel : Kernel_Handle;
   end record;
   type Kernel_Link_Button is access all Kernel_Link_Button_Record'Class;

   type Startup_Editor_Record is new Gtk_Dialog_Record with record
      Kernel      : Kernel_Handle;
      Tree        : Gtk_Tree_View;
      Model       : Gtk_Tree_Store;
      Description : Gtk_Text_Buffer;
      File_Link   : Kernel_Link_Button;
      Loaded_At_Startup : Gtk_Label;

      Edited_Iter         : Gtk_Tree_Iter;
      --  Currently selected script
   end record;
   type Startup_Editor is access all Startup_Editor_Record'Class;

   procedure Gtk_New
     (Editor : out Startup_Editor;
      Kernel : access Kernel_Handle_Record'Class);
   --  Initialize a new editor

   procedure On_Selection_Changed (Editor : access Gtk_Widget_Record'Class);
   --  Called when the selection in the tree has changed.

   procedure On_Load_Toggled
     (Editor : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues);
   --  Toggled when the loading status of a script is changed.

   procedure Save (Editor : access Startup_Editor_Record'Class);
   --  Save the changes done in Editor into the kernel

   procedure Set_Modified
     (Editor : access Startup_Editor_Record'Class;
      Iter   : Gtk_Tree_Iter);
   --  Mark the corresponding script as modified

   function On_File_Clicked
     (Self : access Gtk_Link_Button_Record'Class) return Boolean;
   --  Called when the user requests to view the source file.

   type Open_Plug_Ins_Dialog is new Interactive_Command with null record;
   overriding function Execute
      (Self : access Open_Plug_Ins_Dialog;
       Context : Interactive_Command_Context) return Command_Return_Type;
   --  Called when the user selects the menu to edit startup plug-ins

   ---------------------
   -- On_File_Clicked --
   ---------------------

   function On_File_Clicked
     (Self : access Gtk_Link_Button_Record'Class) return Boolean is
   begin
      Open_File_Editor
        (Kernel_Link_Button (Self).Kernel,
         Project  => GNATCOLL.Projects.No_Project,
         Filename => Create (+Self.Get_Label));
      return True;
   end On_File_Clicked;

   ------------------
   -- Set_Modified --
   ------------------

   procedure Set_Modified
     (Editor : access Startup_Editor_Record'Class;
      Iter   : Gtk_Tree_Iter)
   is
   begin
      Set (Editor.Model, Iter, Column_Modified, True);
      Set (Editor.Model, Iter, Column_Background, "grey");
   end Set_Modified;

   --------------------------
   -- On_Selection_Changed --
   --------------------------

   procedure On_Selection_Changed (Editor : access Gtk_Widget_Record'Class) is
      Ed         : constant Startup_Editor := Startup_Editor (Editor);
      Selection  : constant Gtk_Tree_Selection := Get_Selection (Ed.Tree);
      Model      : Gtk_Tree_Model;
      Iter       : Gtk_Tree_Iter;
      Text_Iter  : Gtk_Text_Iter;
      Contents   : String_Access;
      File       : GNATCOLL.VFS.Virtual_File := GNATCOLL.VFS.No_File;
      F2         : Virtual_File;
      First, Last : Integer;

      function Is_Loaded return String;
      function Is_Loaded return String is
         Loaded : constant String :=
           (if Get_Boolean (Model, Iter, Column_Load) then -"yes" else -"no");

      begin
         if Get_Boolean (Model, Iter, Column_Modified) then
            return Loaded
              & (-" (Modified in this dialog -- Press Cancel to revert)");
         elsif Get_Boolean (Model, Iter, Column_Explicit) then
            return Loaded & (-" (explicitly set by user)");
         elsif Get_Boolean (Model, Iter, Column_Load) then
            return Loaded & (-" (found in auto-loading directory)");
         else
            return Loaded & (-" (found in no auto-loading directory)");
         end if;
      end Is_Loaded;

   begin
      Ed.Description.Set_Text ("");

      Get_Selected (Selection, Model, Iter);
      if Iter /= Null_Iter then
         Ed.Edited_Iter := Iter;

         Get_End_Iter (Ed.Description, Text_Iter);

         File := Get_File (Ed.Model, Iter, Column_File);

         if File.Is_Directory then
            F2 := Create_From_Dir (File, "__init__.py");
            if F2.Is_Regular_File then
               File := F2;
            end if;
         end if;

         Ed.File_Link.Set_Label (File.Display_Full_Name);
         Ed.File_Link.Set_Uri ("file://" & File.Display_Full_Name);

         Ed.Loaded_At_Startup.Set_Text (Is_Loaded);

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

            Insert (Ed.Description, Text_Iter, Contents (First .. Last));
            Free (Contents);
         end if;
      end if;
   end On_Selection_Changed;

   ----------
   -- Save --
   ----------

   procedure Save (Editor : access Startup_Editor_Record'Class) is
      Iter : Gtk_Tree_Iter;
      Is_Modified : Boolean := False;
      Dialog : Gtk_Dialog;
      Button : Gtk_Widget;
      Label  : Gtk_Label;
      Must_Restart : Boolean := False;
   begin
      Iter := Get_Iter_First (Editor.Model);
      while Iter /= Null_Iter loop
         if Get_Boolean (Editor.Model, Iter, Column_Modified) then
            if not Is_Modified then
               Gtk_New (Dialog,
                        Title  => -"Restart GPS ?",
                        Parent => Get_Main_Window (Editor.Kernel),
                        Flags  => Destroy_With_Parent);
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

               --  Do not ask again
               Is_Modified := True;
            end if;

            Override_Startup_Script
              (Kernel    => Editor.Kernel,
               Base_Name =>
                 Get_String (Editor.Model, Iter, Column_Name_With_Ext),
               Load      => Get_Boolean (Editor.Model, Iter, Column_Load));
         end if;

         Next (Editor.Model, Iter);
      end loop;

      if Must_Restart then
         Quit (GPS_Window (Get_Main_Window (Editor.Kernel)),
               Status => 100);
      end if;
   end Save;

   ---------------------
   -- On_Load_Toggled --
   ---------------------

   procedure On_Load_Toggled
     (Editor : access Gtk_Widget_Record'Class;
      Params : Glib.Values.GValues)
   is
      Ed          : constant Startup_Editor := Startup_Editor (Editor);
      Path_String : constant String := Get_String (Nth (Params, 1));
      Iter        : Gtk_Tree_Iter;
   begin
      Iter := Get_Iter_From_String (Ed.Model, Path_String);
      Set_Modified (Ed, Iter);

      --  Refresh the description box
      On_Selection_Changed (Editor);
   end On_Load_Toggled;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor : out Startup_Editor;
      Kernel : access Kernel_Handle_Record'Class)
   is

      procedure Add_Script
        (Name     : String;
         File     : GNATCOLL.VFS.Virtual_File;
         Loaded   : Boolean;
         Explicit : Boolean);
      --  Add a startup script to the list

      procedure Add_Script
        (Name     : String;
         File     : GNATCOLL.VFS.Virtual_File;
         Loaded   : Boolean;
         Explicit : Boolean)
      is
         Iter : Gtk_Tree_Iter;
      begin
         Append (Editor.Model, Iter, Null_Iter);
         Set (Editor.Model, Iter, Column_Load, Loaded);
         Set (Editor.Model, Iter, Column_Name,
              Base_Name (Name, File_Extension (Name)));
         Set (Editor.Model, Iter, Column_Explicit, Explicit);
         Set_File (Editor.Model, Iter, Column_File, File);
         Set (Editor.Model, Iter, Column_Modified, False);
         Set (Editor.Model, Iter, Column_Name_With_Ext, Name);
      end Add_Script;

      Button      : Gtk_Widget;
      Scrolled    : Gtk_Scrolled_Window;
      Box, B      : Gtk_Box;
      Pane        : Gtk_Paned;
      Text        : Gtk_Text_View;
      List        : Glib.Object.Object_Simple_List.Glist;
      Label       : Gtk_Label;
      Font        : Pango_Font_Description;
      pragma Unreferenced (Button);

   begin
      Editor := new Startup_Editor_Record;
      Editor.Kernel := Kernel_Handle (Kernel);
      Initialize
        (Editor,
         Title  => -"Startup Plug-ins",
         Parent => Get_Main_Window (Kernel),
         Flags  => Destroy_With_Parent
           or Use_Header_Bar_From_Settings (Get_Main_Window (Kernel)));
      Set_Default_Size (Editor, 800, 600);
      Set_Name (Editor, "Startup Plug-ins Editor");

      Gtk_New_Hpaned (Pane);
      Pack_Start
        (Get_Content_Area (Editor), Pane, Expand => True, Fill => True);

      Gtk_New (Scrolled);
      Pack1 (Pane, Scrolled, True, True);
      Set_Size_Request (Scrolled, 200, -1);
      Set_Policy (Scrolled, Policy_Automatic, Policy_Automatic);

      Editor.Tree := Create_Tree_View
        (Column_Types => (Column_Load       => GType_Boolean,
                          Column_Name       => GType_String,
                          Column_Explicit   => GType_Boolean,
                          Column_File       => Get_Virtual_File_Type,
                          Column_Modified   => GType_Boolean,
                          Column_Background => GType_String,
                          Column_Name_With_Ext => GType_String),
         Column_Names => (Column_Load + 1 => null,
                          Column_Name + 1 => null),
         Show_Column_Titles => False,
         Initial_Sort_On    => Column_Name + 1);
      Add (Scrolled, Editor.Tree);

      Gtk_New_Vbox (Box, Homogeneous => False);
      Pack2 (Paned => Pane, Child => Box, Resize => True, Shrink => True);
      Set_Size_Request (Box, 600, -1);

      Font := Copy (Default_Font.Get_Pref_Font);
      Set_Weight (Font, Pango_Weight_Bold);

      Gtk_New_Hbox (B, Homogeneous => False);
      Box.Pack_Start (B, Expand => False);
      Gtk_New (Label, "File: ");
      Label.Override_Font (Font);
      B.Pack_Start (Label, Expand => False);

      Editor.File_Link := new Kernel_Link_Button_Record;
      Editor.File_Link.Kernel := Kernel_Handle (Kernel);
      Initialize (Editor.File_Link, "");
      Editor.File_Link.On_Activate_Link (On_File_Clicked'Access);
      Editor.File_Link.Set_Alignment (0.0, 0.5);
      B.Pack_Start (Editor.File_Link, Expand => True, Fill => True);

      Gtk_New_Hbox (B, Homogeneous => False);
      Box.Pack_Start (B, Expand => False);
      Gtk_New (Label, "Loaded at startup: ");
      Label.Override_Font (Font);
      B.Pack_Start (Label, Expand => False);
      Gtk_New (Editor.Loaded_At_Startup, "no");
      Editor.Loaded_At_Startup.Set_Alignment (0.0, 0.5);
      B.Pack_Start (Editor.Loaded_At_Startup, Expand => True, Fill => True);

      Gtk_New (Scrolled);
      Scrolled.Set_Margin_Top (20);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Box.Pack_Start (Scrolled, Expand => True, Fill => True);

      Gtk_New (Editor.Description);
      Gtk_New (Text, Editor.Description);
      Unref (Editor.Description);

      Scrolled.Add (Text);

      Text.Set_Wrap_Mode (Wrap_None);
      Text.Set_Editable (False);
      Text.Modify_Font (Default_Style.Get_Pref_Font);

      List := Get_Cells (+Get_Column (Editor.Tree, Column_Load));
      Add_Attribute
        (Get_Column (Editor.Tree, Column_Load),
         Gtk_Cell_Renderer (Object_Simple_List.Get_Data (List)),
         "cell_background", Column_Background);
      Widget_Callback.Object_Connect
        (Gtk_Cell_Renderer (Object_Simple_List.Get_Data (List)),
         Gtk.Cell_Renderer_Toggle.Signal_Toggled,
         On_Load_Toggled'Access, Editor, After => True);
      Object_Simple_List.Free (List);

      List := Get_Cells (+Get_Column (Editor.Tree, Column_Name));
      Add_Attribute
        (Get_Column (Editor.Tree, Column_Name),
         Gtk_Cell_Renderer (Object_Simple_List.Get_Data (List)),
         "cell_background", Column_Background);
      Object_Simple_List.Free (List);

      Widget_Callback.Object_Connect
        (Get_Selection (Editor.Tree), Gtk.Tree_Selection.Signal_Changed,
         On_Selection_Changed'Access, Editor);

      Editor.Model := -Get_Model (Editor.Tree);

      For_All_Startup_Scripts (Kernel, Add_Script'Access);

      Select_Iter (Get_Selection (Editor.Tree), Get_Iter_First (Editor.Model));

      Button := Add_Button (Editor, Stock_Ok, Gtk_Response_OK);
      Button := Add_Button (Editor, Stock_Cancel, Gtk_Response_Cancel);
   end Gtk_New;

   -------------
   -- Execute --
   -------------

   overriding function Execute
      (Self : access Open_Plug_Ins_Dialog;
       Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Self);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      Editor : Startup_Editor;
   begin
      Gtk_New (Editor, Kernel);

      Show_All (Editor);
      if Run (Editor) = Gtk_Response_OK then
         Save (Editor);
         Save_Startup_Scripts_List (Kernel);
      end if;
      Destroy (Editor);
      return Commands.Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Module : Module_ID;
   begin
      Module := new Module_ID_Record;
      Register_Module (Module, Kernel, "Plug-ins manager");
      Register_Action
        (Kernel, "open plug-ins dialog", new Open_Plug_Ins_Dialog,
         -"Opens a dialog to select which plug-ins should be activated");
   end Register_Module;

end Startup_Module;
