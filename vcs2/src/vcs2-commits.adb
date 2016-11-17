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

with Ada.Strings.Unbounded;       use Ada.Strings.Unbounded;
with Commands.Interactive;        use Commands, Commands.Interactive;
with Default_Preferences;         use Default_Preferences;
with Gdk.Event;                   use Gdk.Event;
with Gdk.Rectangle;               use Gdk.Rectangle;
with Gdk.RGBA;                    use Gdk.RGBA;
with Generic_Views;
with Glib.Object;                 use Glib.Object;
with Glib.Values;                 use Glib.Values;
with Glib;                        use Glib;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GNATCOLL.VFS;                use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;         use GNATCOLL.VFS.GtkAda;
with GPS.Intl;                    use GPS.Intl;
with GPS.Kernel.Actions;          use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;         use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;            use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;              use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;       use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;
with GPS.VCS;                     use GPS.VCS;
with GPS.VCS_Engines;             use GPS.VCS_Engines;
with Gtkada.Multi_Paned;          use Gtkada.Multi_Paned;
with Gtk.Box;                     use Gtk.Box;
with Gtk.Cell_Renderer_Pixbuf;    use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;      use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;    use Gtk.Cell_Renderer_Toggle;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Label;                   use Gtk.Label;
with Gtk.Menu;                    use Gtk.Menu;
with Gtk.Scrolled_Window;         use Gtk.Scrolled_Window;
with Gtk.Text_Tag;                use Gtk.Text_Tag;
with Gtk.Text_View;               use Gtk.Text_View;
with Gtk.Toolbar;                 use Gtk.Toolbar;
with Gtk.Tree_Model;              use Gtk.Tree_Model;
with Gtk.Tree_View_Column;        use Gtk.Tree_View_Column;
with Gtk.Widget;                  use Gtk.Widget;
with Gtkada.Handlers;             use Gtkada.Handlers;
with Gtkada.Combo_Tool_Button;    use Gtkada.Combo_Tool_Button;
with Gtkada.MDI;                  use Gtkada.MDI;
with Gtkada.Tree_View;            use Gtkada.Tree_View;
with GUI_Utils;                   use GUI_Utils;
with Tooltips;                    use Tooltips;
with Pango.Layout;                use Pango.Layout;

package body VCS2.Commits is
   Me : constant Trace_Handle := Create ("COMMITS");

   Column_File          : constant := 0;
   Column_Staged        : constant := 1;
   Column_Name          : constant := 2;
   Column_Icon          : constant := 3;
   Column_Inconsistent  : constant := 4;
   Column_Check_Visible : constant := 5;
   Column_Foreground    : constant := 6;
   subtype All_Columns is Gint range Column_File .. Column_Foreground;

   Color_Untracked    : constant Gdk_RGBA := (0.6, 0.6, 0.6, 1.0);
   Color_Normal       : constant Gdk_RGBA := (0.0, 0.0, 0.0, 1.0);
   Color_Category     : constant Gdk_RGBA := (0.0, 0.0, 0.0, 1.0);
   Color_Inactive_VCS : constant Gdk_RGBA := (0.8, 0.8, 0.8, 1.0);
   --  Colors for text

   Show_Untracked_Files   : Boolean_Preference;
   Relative_Names         : Boolean_Preference;
   Group_By_Category      : Boolean_Preference;
   Hide_Other_VCS         : Boolean_Preference;

   type Commit_View_Config is record
      Hidden_Files_Pattern : Unbounded_String;
      Initialized          : Boolean := False;
      Show_Hidden_Files    : Boolean := False;
      Show_Untracked_Files : Boolean := False;
      Relative_Names       : Boolean := False;
      Group_By_Category    : Boolean := False;
      Hide_Other_VCS       : Boolean := False;
   end record;

   subtype Commit_Tree_Record is Tree_View_Record;
   subtype Commit_Tree_View is Tree_View;

   function Get_File_From_Node
     (Self : not null access Commit_Tree_Record'Class;
      Iter : Gtk_Tree_Iter) return Virtual_File
     is (Get_File (Self.Model, Iter, Column_File));

   package Expansion is new Expansion_Support
     (Tree_Record    => Commit_Tree_Record,
      Id             => Virtual_File,
      Get_Id         => Get_File_From_Node,
      Hash           => GNATCOLL.VFS.Full_Name_Hash);

   type Commit_View_Record is new Generic_Views.View_Record with record
      Tree        : Commit_Tree_View;
      Text_Render : Gtk_Cell_Renderer_Text;

      VCS_Count   : Natural := 0;
      --  Number of VCS systems in the project

      Config      : Commit_View_Config;
      Commit      : Gtk_Text_View;
   end record;
   overriding procedure Create_Toolbar
     (View    : not null access Commit_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure Create_Menu
     (View    : not null access Commit_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding procedure On_Create
     (Self    : not null access Commit_View_Record;
      Child   : not null access GPS.Kernel.MDI.GPS_MDI_Child_Record'Class);

   function Initialize
     (Self : access Commit_View_Record'Class) return Gtk_Widget;
   --  Create a new view

   procedure Create_Node
     (Self         : not null access Commit_View_Record'Class;
      VCS          : not null access VCS_Engine'Class;
      File         : Virtual_File;
      Props        : VCS_File_Properties;
      From_Active_VCS : Boolean;
      Parent       : Gtk_Tree_Iter := Null_Iter);
   function Create_Category_Node
     (Self         : not null access Commit_View_Record'Class;
      Name         : String) return Gtk_Tree_Iter;
   --  Add a new node in the tree.

   type Commit_Child_Record is new GPS_MDI_Child_Record with null record;
   overriding function Build_Context
     (Self  : not null access Commit_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context;

   package Commit_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Commits",
      View_Name          => "Commits",
      Formal_View_Record => Commit_View_Record,
      Formal_MDI_Child   => Commit_Child_Record,
      Reuse_If_Exist     => True,
      Local_Toolbar      => True,
      Local_Config       => True,
      Areas              => Gtkada.MDI.Both,
      Position           => Position_Right,
      Initialize         => Initialize);
   use Commit_Views;
   subtype Commit_View is Commit_Views.View_Access;

   type On_All_Files_Available_In_Cache is new Task_Completed_Callback with
      record
         Kernel : not null access Kernel_Handle_Record'Class;
      end record;
   overriding procedure Execute
     (Self : not null access On_All_Files_Available_In_Cache;
      VCS  : access VCS_Engine'Class);

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      View : Commit_View;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed

   type Commit_Tooltips is new Tooltips.Tooltips with record
      View   : access Commit_View_Record'Class;
   end record;
   overriding function Create_Contents
     (Self     : not null access Commit_Tooltips;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget;

   type Stage_File is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Stage_File;
      Context : Interactive_Command_Context) return Command_Return_Type
     is (Commands.Success);

   type Unstage_File is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Unstage_File;
      Context : Interactive_Command_Context) return Command_Return_Type
     is (Commands.Success);

   type Commit is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Commit;
      Context : Interactive_Command_Context) return Command_Return_Type
     is (Commands.Success);

   type Kernel_Combo_Tool_Record is new Gtkada_Combo_Tool_Button_Record with
      record
         Kernel : Kernel_Handle;
      end record;
   type Kernel_Combo_Tool is access all Kernel_Combo_Tool_Record'Class;
   procedure On_Active_VCS_Selected (Widget : access Gtk_Widget_Record'Class);
   --  Called when a new active VCS is selected by the user in the toolbar.
   --  This is *not* the same as the VCS_Active_Changed_Hook, which should be
   --  monitored to react to actual changes

   type On_Active_VCS_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Active_VCS_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access Commit_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return Selection_Context
   is
      T : constant Commit_View := Commit_Views.View_From_Child (Self);
      Filter_Iter : constant Gtk_Tree_Iter :=
        Find_Iter_For_Event (T.Tree, Event);
      Iter        : Gtk_Tree_Iter;
      Context     : Selection_Context :=
        GPS_MDI_Child_Record (Self.all).Build_Context (Event);  --  inherited
   begin
      if Filter_Iter /= Null_Iter then
         Iter := T.Tree.Convert_To_Store_Iter (Filter_Iter);
         Set_File_Information
           (Context,
            Files   => (1 => Get_File_From_Node (T.Tree, Iter)));
      end if;

      return Context;
   end Build_Context;

   ---------------------
   -- Create_Contents --
   ---------------------

   overriding function Create_Contents
     (Self     : not null access Commit_Tooltips;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Widget);
      Filter_Iter : Gtk_Tree_Iter;
      Iter        : Gtk_Tree_Iter;
      Area        : Gdk_Rectangle;
      Label       : Gtk_Label;
      File        : Virtual_File;
   begin
      Initialize_Tooltips (Self.View.Tree, X, Y, Area, Filter_Iter);
      Iter := Self.View.Tree.Convert_To_Store_Iter (Filter_Iter);
      if Iter /= Null_Iter then
         File := Get_File_From_Node (Self.View.Tree, Iter);
         if File /= No_File then
            Self.Set_Tip_Area (Area);
            Gtk_New
              (Label,
               Get_Tooltip_For_File
                 (Kernel       => Self.View.Kernel,
                  File         => File));
            Label.Set_Use_Markup (True);
         end if;
      end if;
      return Gtk_Widget (Label);
   end Create_Contents;

   ----------------------------
   -- On_Active_VCS_Selected --
   ----------------------------

   procedure On_Active_VCS_Selected
     (Widget : access Gtk_Widget_Record'Class)
   is
      Combo    : constant Kernel_Combo_Tool := Kernel_Combo_Tool (Widget);
      Selected : constant String := Combo.Get_Selected_Item;

      procedure On_VCS (VCS : not null access VCS_Engine'Class);
      procedure On_VCS (VCS : not null access VCS_Engine'Class) is
         N : constant String :=
           VCS.Name & " (" & VCS.Working_Directory.Display_Full_Name & ")";
      begin
         if N = Selected then
            Set_Active_VCS (Combo.Kernel, VCS);
         end if;
      end On_VCS;

   begin
      For_Each_VCS (Combo.Kernel, On_VCS'Access);
   end On_Active_VCS_Selected;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (View    : not null access Commit_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class)
   is
      Combo : Kernel_Combo_Tool;

      procedure On_VCS (VCS : not null access VCS_Engine'Class);
      procedure On_VCS (VCS : not null access VCS_Engine'Class) is
         N : constant String :=
           VCS.Name & " (" & VCS.Working_Directory.Display_Full_Name & ")";
      begin
         Combo.Add_Item (N, Short_Name => VCS.Name);
         if VCS = Active_VCS (View.Kernel) then
            Combo.Select_Item (N);
         end if;
      end On_VCS;

   begin
      if View.VCS_Count > 1 then
         Combo := new Kernel_Combo_Tool_Record;
         Combo.Kernel := View.Kernel;
         Initialize (Combo, Icon_Name => "");

         Combo.Set_Tooltip_Text (-"Right-click to select the repository");
         Toolbar.Insert (Combo, 0);

         For_Each_VCS (View.Kernel, On_VCS'Access);

         Widget_Callback.Connect
           (Combo, Gtkada.Combo_Tool_Button.Signal_Selection_Changed,
            On_Active_VCS_Selected'Access);
      end if;
   end Create_Toolbar;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access Commit_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      K : constant Kernel_Handle := View.Kernel;
   begin
      Append_Menu (Menu, K, Show_Ellipsis);
      Append_Menu (Menu, K, Show_Hidden_Files);
      Append_Menu (Menu, K, Show_Untracked_Files);
      Append_Menu (Menu, K, Relative_Names);
      Append_Menu (Menu, K, Group_By_Category);
      Append_Menu (Menu, K, Hide_Other_VCS);
   end Create_Menu;

   -----------------
   -- Create_Node --
   -----------------

   procedure Create_Node
     (Self         : not null access Commit_View_Record'Class;
      VCS          : not null access VCS_Engine'Class;
      File         : Virtual_File;
      Props        : VCS_File_Properties;
      From_Active_VCS : Boolean;
      Parent          : Gtk_Tree_Iter := Null_Iter)
   is
      Iter     : Gtk_Tree_Iter;
      V        : Glib.Values.GValue_Array (All_Columns);
      Staged   : constant Boolean := (Props.Status and Mask_Staged) /= 0;
      Unstaged : constant Boolean :=
        (Props.Status and Mask_Modified_Unstaged) /= 0;
      Untracked : constant Boolean := (Props.Status and Mask_Untracked) /= 0;
      Display   : constant Status_Display := VCS.Get_Display (Props.Status);
   begin
      Self.Tree.Model.Append (Iter, Parent => Parent);

      Glib.Values.Init (V (Column_File), Get_Virtual_File_Type);
      Set_File (V (Column_File), File);

      Init_Set_Boolean (V (Column_Staged), Staged and not Unstaged);
      Init_Set_Boolean (V (Column_Inconsistent), Staged and Unstaged);

      if Self.Config.Relative_Names then
         Init_Set_String
           (V (Column_Name), +File.Relative_Path (VCS.Working_Directory));
      else
         Init_Set_String (V (Column_Name), File.Display_Full_Name);
      end if;

      Init_Set_String (V (Column_Icon), To_String (Display.Icon_Name));
      Init_Set_Boolean
        (V (Column_Check_Visible),
         From_Active_VCS or Untracked);

      Init (V (Column_Foreground), Gdk.RGBA.Get_Type);
      if not From_Active_VCS then
         Gdk.RGBA.Set_Value (V (Column_Foreground), Color_Inactive_VCS);
      elsif Untracked and then not Self.Config.Group_By_Category then
         Gdk.RGBA.Set_Value (V (Column_Foreground), Color_Untracked);
      else
         Gdk.RGBA.Set_Value (V (Column_Foreground), Color_Normal);
      end if;

      Self.Tree.Model.Set (Iter, V);
   end Create_Node;

   --------------------------
   -- Create_Category_Node --
   --------------------------

   function Create_Category_Node
     (Self         : not null access Commit_View_Record'Class;
      Name         : String) return Gtk_Tree_Iter
   is
      Iter : Gtk_Tree_Iter;
      V        : Glib.Values.GValue_Array (All_Columns);

   begin
      Self.Tree.Model.Append (Iter, Parent => Null_Iter);

      Glib.Values.Init (V (Column_File), Get_Virtual_File_Type);
      Set_File (V (Column_File), No_File);

      Init_Set_Boolean (V (Column_Staged), False);
      Init_Set_Boolean (V (Column_Inconsistent), False);
      Init_Set_Boolean (V (Column_Check_Visible), False);
      Init_Set_String (V (Column_Name), Name);
      Init_Set_String (V (Column_Icon), "gps-emblem-directory-open");
      Init (V (Column_Foreground), Gdk.RGBA.Get_Type);
      Gdk.RGBA.Set_Value (V (Column_Foreground), Color_Category);

      Self.Tree.Model.Set (Iter, V);

      return Iter;
   end Create_Category_Node;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      Config : Commit_View_Config;
   begin
      Set_Font_And_Colors (Self.View.Tree, Fixed_Font => True, Pref => Pref);

      if Pref = null
        or else Pref = Preference (Show_Ellipsis)
      then
         Set_Property
           (Self.View.Text_Render,
            Gtk.Cell_Renderer_Text.Ellipsize_Property,
            (if Show_Ellipsis.Get_Pref
             then Ellipsize_Middle else Ellipsize_None));
         Self.View.Queue_Resize;
         Self.View.Tree.Queue_Draw;
      end if;

      Config :=
        (Initialized          => True,
         Hidden_Files_Pattern =>
           To_Unbounded_String (Hidden_Files_Pattern.Get_Pref),
         Show_Untracked_Files => Show_Untracked_Files.Get_Pref,
         Relative_Names       => Relative_Names.Get_Pref,
         Group_By_Category    => Group_By_Category.Get_Pref,
         Show_Hidden_Files    => Show_Hidden_Files.Get_Pref,
         Hide_Other_VCS       => Hide_Other_VCS.Get_Pref);

      if Config /= Self.View.Config then
         Self.View.Config := Config;
         Ensure_Status_For_All_Files_In_All_Engines
           (Kernel, On_Complete => new On_All_Files_Available_In_Cache'
              (Task_Completed_Callback with Kernel => Kernel));
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Active_VCS_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
   begin
      Ensure_Status_For_All_Files_In_All_Engines
        (Kernel, On_Complete => new On_All_Files_Available_In_Cache'
           (Task_Completed_Callback with Kernel => Kernel));
   end Execute;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Commit_View_Record'Class) return Gtk_Widget
   is
      Scrolled   : Gtk_Scrolled_Window;
      Scrolled2  : Gtk_Scrolled_Window;
      Paned      : Gtkada_Multi_Paned;
      Col        : Gtk_Tree_View_Column;
      Check      : Gtk_Cell_Renderer_Toggle;
      Pixbuf     : Gtk_Cell_Renderer_Pixbuf;
      Dummy      : Gint;
      Tooltip    : access Commit_Tooltips'Class;
   begin
      Initialize_Vbox (Self, Homogeneous => False);

      Gtk_New (Paned);
      Paned.Set_Opaque_Resizing (True);
      Self.Pack_Start (Paned);

      --  Top widget

      Gtk_New (Scrolled2);
      Scrolled2.Set_Policy (Policy_Automatic, Policy_Always);
      Paned.Add_Child
        (Scrolled2, Orientation => Orientation_Vertical);
      Paned.Set_Size (Scrolled2, Height => 10);

      Gtk_New (Self.Commit);
      Scrolled2.Add (Self.Commit);
      Set_Placeholder
        (Self.Commit,
         -"Short review message" & ASCII.LF & ASCII.LF & "Details");

      --  Bottom widget

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Always);
      Paned.Split
        (Ref_Widget  => Scrolled2,
         New_Child   => Scrolled,
         Orientation => Orientation_Vertical);

      Gtk_New (Self.Tree,
               (Column_File          => Get_Virtual_File_Type,
                Column_Name          => GType_String,
                Column_Icon          => GType_String,
                Column_Inconsistent  => GType_Boolean,
                Column_Foreground    => Gdk.RGBA.Get_Type,
                Column_Check_Visible => GType_Boolean,
                Column_Staged        => GType_Boolean));
      Self.Tree.Set_Headers_Visible (False);
      Scrolled.Add (Self.Tree);

      --  Tree

      Gtk_New (Self.Text_Render);
      Gtk_New (Check);
      Gtk_New (Pixbuf);

      Gtk_New (Col);
      Col.Set_Expand (True);
      Dummy := Self.Tree.Append_Column (Col);

      Col.Pack_Start (Check, False);
      Col.Add_Attribute (Check, "active",       Column_Staged);
      Col.Add_Attribute (Check, "inconsistent", Column_Inconsistent);
      Col.Add_Attribute (Check, "visible",      Column_Check_Visible);

      Col.Pack_Start (Pixbuf, False);
      Col.Add_Attribute (Pixbuf, "icon-name", Column_Icon);

      Col.Pack_Start (Self.Text_Render, True);
      Col.Add_Attribute (Self.Text_Render, "text", Column_Name);
      Col.Add_Attribute
        (Self.Text_Render, "foreground-rgba", Column_Foreground);

      Self.Tree.Model.Set_Sort_Column_Id (Column_Name, Sort_Ascending);

      Tooltip := new Commit_Tooltips;
      Tooltip.View := Self;
      Tooltip.Set_Tooltip (Self.Tree);

      Setup_Contextual_Menu (Self.Kernel, Self.Tree);

      return Gtk_Widget (Self.Tree);
   end Initialize;

   ---------------
   -- On_Create --
   ---------------

   overriding procedure On_Create
     (Self    : not null access Commit_View_Record;
      Child   : not null access GPS.Kernel.MDI.GPS_MDI_Child_Record'Class)
   is
      pragma Unreferenced (Child);
      P : access On_Pref_Changed;
   begin
      P := new On_Pref_Changed;
      P.View := Commit_View (Self);
      Preferences_Changed_Hook.Add (P, Watch => Self);
      P.Execute (Self.Kernel, null);   --  also calls Refresh

      Vcs_Active_Changed_Hook.Add (new On_Active_VCS_Changed, Watch => Self);
   end On_Create;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self : not null access On_All_Files_Available_In_Cache;
      VCS  : access VCS_Engine'Class)
   is
      View : constant Commit_View := Commit_Views.Retrieve_View (Self.Kernel);
      --  Have to fetch the view, in case it was closed while the status was
      --  fetching in the background

      Local_VCS : access VCS_Engine'Class;
      Is_Active : Boolean;

      Category_Staged    : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Category_Modified  : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Category_Untracked : Gtk_Tree_Path := Null_Gtk_Tree_Path;

      procedure On_File (File : Virtual_File; Props : VCS_File_Properties);
      procedure On_File (File : Virtual_File; Props : VCS_File_Properties) is
         Show         : constant Boolean :=
           Is_Active or else not View.Config.Hide_Other_VCS;
         Is_Staged    : Boolean;
         Is_Modified  : Boolean;
         Is_Untracked : Boolean;
      begin
         Is_Staged := (Props.Status and Mask_Staged) /= 0
           and then Show;
         Is_Modified := (Props.Status and Mask_Modified_Unstaged) /= 0
           and then (not Is_Staged or else View.Config.Group_By_Category)
           and then Show;
         Is_Untracked := View.Config.Show_Untracked_Files
           and then (Props.Status and Mask_Untracked) /= 0
           and then (not (Is_Staged or Is_Modified)
                    or else View.Config.Group_By_Category)
           and then not Self.Kernel.Is_Hidden (File);

         --  A file could be in multiple categories

         if Is_Staged then
            View.Create_Node
              (Local_VCS, File, Props,
               From_Active_VCS => Is_Active,
               Parent => View.Tree.Model.Get_Iter (Category_Staged));
         end if;

         if Is_Modified then
            View.Create_Node
              (Local_VCS, File, Props,
               From_Active_VCS => Is_Active,
               Parent => View.Tree.Model.Get_Iter (Category_Modified));
         end if;

         if Is_Untracked then
            View.Create_Node
              (Local_VCS, File, Props,
               From_Active_VCS => Is_Active,
               Parent => View.Tree.Model.Get_Iter (Category_Untracked));
         end if;
      end On_File;

      procedure On_VCS (VCS : not null access VCS_Engine'Class);
      procedure On_VCS (VCS : not null access VCS_Engine'Class) is
      begin
         Trace (Me, "Got all files in cache for " & VCS.Name);
         View.VCS_Count := View.VCS_Count + 1;
         Local_VCS := VCS;
         Is_Active := VCS = Active_VCS (Self.Kernel);
         VCS.For_Each_File_In_Cache (On_File'Access);
      end On_VCS;

      Iter : Gtk_Tree_Iter;

   begin
      --  When all engines have refreshed, do a global refresh
      if VCS = null and then View /= null then

         View.VCS_Count := 0;

         declare
            Dummy : constant Expansion.Detached_Model :=
              Expansion.Detach_Model_From_View (View.Tree);
         begin
            View.Tree.Model.Clear;

            if View.Config.Group_By_Category then
               Iter := Create_Category_Node (View, -"Staged Files");
               Category_Staged := View.Tree.Model.Get_Path (Iter);

               Iter := Create_Category_Node (View, -"Modified Files");
               Category_Modified := View.Tree.Model.Get_Path (Iter);

               if View.Config.Show_Untracked_Files then
                  Iter := Create_Category_Node (View, -"Untracked Files");
                  Category_Untracked := View.Tree.Model.Get_Path (Iter);
               end if;
            end if;

            For_Each_VCS (Self.Kernel, On_VCS'Access);

            if View.Config.Group_By_Category then
               Path_Free (Category_Staged);
               Path_Free (Category_Modified);
               Path_Free (Category_Untracked);
            end if;
         end;

         if View.Config.Group_By_Category then
            View.Tree.Expand_All;
         end if;

         Commit_Views.Reset_Toolbar (View);
      end if;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access Kernel_Handle_Record'Class)
    is
   begin
      Commit_Views.Register_Module (Kernel);

      Show_Untracked_Files := Kernel.Get_Preferences.Create_Invisible_Pref
        ("commit-view-show-untracked-files",
         Default => True,
         Label   => -"Show untracked files");

      Relative_Names := Kernel.Get_Preferences.Create_Invisible_Pref
        ("commit-view-relative-names",
         Default => True,
         Label   => -"Names relative to working directory");

      Group_By_Category := Kernel.Get_Preferences.Create_Invisible_Pref
        ("commit-view-group-by-category",
         Default  => False,
         Label    => -"Group by category (staged, modified, untracked)");

      Hide_Other_VCS := Kernel.Get_Preferences.Create_Invisible_Pref
        ("commit-view-hide-other-vcs",
         Default  => False,
         Label    => -"Hide files from VCS other than the active one");

      Register_Action
        (Kernel, "vcs commit staged files",
         Description => -"Commit all staged files",
         Command     => new Commit,
         Category    => "VCS2",
         Icon_Name   => "github-commit-symbolic");

      Register_Action
        (Kernel, "vcs stage file",
         Description =>
           -"Stage the selected file, so that it is part of the next commit",
         Command     => new Stage_File,
         Category    => "VCS2",
         Icon_Name   => "gps-add-symbolic");

      Register_Action
        (Kernel, "vcs unstage file",
         Description =>
           -("Unstage the selected file, so that it is no longer part"
             & "  of the next commit"),
         Command     => new Unstage_File,
         Category    => "VCS2",
         Icon_Name   => "gps-remove-symbolic");

   end Register_Module;

end VCS2.Commits;
