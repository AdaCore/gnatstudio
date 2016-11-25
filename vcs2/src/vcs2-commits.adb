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
with Generic_Views;               use Generic_Views;
with Glib.Object;                 use Glib.Object;
with Glib.Values;                 use Glib.Values;
with Glib;                        use Glib;
with GNATCOLL.Traces;             use GNATCOLL.Traces;
with GNATCOLL.VFS;                use GNATCOLL.VFS;
with GNATCOLL.VFS.GtkAda;         use GNATCOLL.VFS.GtkAda;
with GNAT.Strings;                use GNAT.Strings;
with GPS.Intl;                    use GPS.Intl;
with GPS.Kernel.Actions;          use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;         use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;            use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;              use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;       use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;
with GPS.Kernel.Properties;       use GPS.Kernel.Properties;
with GPS.Properties;              use GPS.Properties;
with GPS.Search;                  use GPS.Search;
with GPS.VCS;                     use GPS.VCS;
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
with Informational_Popups;        use Informational_Popups;
with Tooltips;                    use Tooltips;
with Pango.Layout;                use Pango.Layout;
with VCS2.Engines;                use VCS2.Engines;

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

   type Commit_Tree_Record is new Tree_View_Record with record
      User_Filter : GPS.Search.Search_Pattern_Access;
   end record;
   type Commit_Tree_View is access all Commit_Tree_Record'Class;
   overriding function Is_Visible
     (Self : not null access Commit_Tree_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean;

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

      Active_VCS  : VCS_Engine_Access;
      --  A cache for the active VCS. This is used to save the commit message
      --  for the proper VCS.

      VCS_Count   : Natural := 0;
      --  Number of VCS systems in the project

      Computing_File_Status : Boolean := False;
      --  Whether there is background work to recompute the status of all
      --  files. During that time, we do not react to VCS_File_Status_Changed
      --  hook to refresh the view, since it will be wiped out when the
      --  background job finishes anyway.

      Category_Staged    : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Category_Modified  : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Category_Untracked : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      --  The nodes for the categories (if they are displayed)

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
   overriding procedure Filter_Changed
     (Self    : not null access Commit_View_Record;
      Pattern : in out GPS.Search.Search_Pattern_Access);

   function Initialize
     (Self : access Commit_View_Record'Class) return Gtk_Widget;
   --  Create a new view

   procedure Create_Nodes
     (Self         : not null access Commit_View_Record'Class;
      File         : Virtual_File;
      Props        : VCS_File_Properties;
      VCS          : not null access VCS_Engine'Class;
      Select_Nodes : Boolean := False);
   function Create_Category_Node
     (Self         : not null access Commit_View_Record'Class;
      Name         : String) return Gtk_Tree_Iter;
   --  Add one or more new nodes in the tree.

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
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Reload_Status is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Reload_Status;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Commit is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Commit;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type On_Committed is new Vcs_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Committed;
      Kernel : not null access Kernel_Handle_Record'Class;
      VCS    : not null access Abstract_VCS_Engine'Class);
   --  Called when a commit has been completed

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

   function On_Commit_Focus_Out
     (View  : access GObject_Record'Class;
      Event : Gdk_Event_Focus) return Boolean;
   --  Called when the focus moves out of the Commit message.
   --  Used to save it in the properties.

   procedure Save_Commit_Message
     (Self : not null access Commit_View_Record'Class);
   --  Save the commit message in the properties

   procedure Refresh (Self : not null access Commit_View_Record'Class);
   --  Refresh the contents of the view.

   procedure On_Staged_Toggled
     (Self : access GObject_Record'Class;
      Path : String);
   --  Called when the user clicks on one of the checkboxes

   procedure On_Destroyed (View : access Gtk_Widget_Record'Class);
   --  Called when the view is destroyed

   function Get_Commit_Message_From_Properties
     (VCS    : not null access VCS_Engine'Class) return String;
   procedure Set_Commit_Message
     (VCS    : not null access VCS_Engine'Class;
      Msg    : String);
   --  The commit message saved for the given VCS

   type On_VCS_File_Status_Changed is new Vcs_File_Status_Hooks_Function
     with null record;
   overriding procedure Execute
     (Self          : On_VCS_File_Status_Changed;
      Kernel        : not null access Kernel_Handle_Record'Class;
      Vcs           : not null access Abstract_VCS_Engine'Class;
      File          : GNATCOLL.VFS.Virtual_File;
      Props         : VCS_File_Properties);

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

      View.Build_Filter
        (Toolbar     => Toolbar,
         Hist_Prefix => "commits",
         Tooltip     => -"Filter the contents of the commits view",
         Placeholder => -"filter",
         Options     =>
           Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy);
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

   ------------------
   -- Create_Nodes --
   ------------------

   procedure Create_Nodes
     (Self         : not null access Commit_View_Record'Class;
      File         : Virtual_File;
      Props        : VCS_File_Properties;
      VCS          : not null access VCS_Engine'Class;
      Select_Nodes : Boolean := False)
   is
      Is_Active : constant Boolean := VCS = Self.Active_VCS;
      Staged    : constant Boolean := (Props.Status and Mask_Staged) /= 0;
      Unstaged  : constant Boolean :=
        (Props.Status and Mask_Modified_Unstaged) /= 0;
      Untracked : constant Boolean := (Props.Status and Mask_Untracked) /= 0;
      Display   : constant Status_Display := VCS.Get_Display (Props.Status);

      procedure Internal (Parent : Gtk_Tree_Iter := Null_Iter);
      --  Actual node creation

      procedure Internal (Parent : Gtk_Tree_Iter := Null_Iter) is
         Iter      : Gtk_Tree_Iter;
         V         : Glib.Values.GValue_Array (All_Columns);
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
         Init_Set_Boolean (V (Column_Check_Visible), Is_Active or Untracked);

         Init (V (Column_Foreground), Gdk.RGBA.Get_Type);
         if not Is_Active then
            Gdk.RGBA.Set_Value (V (Column_Foreground), Color_Inactive_VCS);
         elsif Untracked and then not Self.Config.Group_By_Category then
            Gdk.RGBA.Set_Value (V (Column_Foreground), Color_Untracked);
         else
            Gdk.RGBA.Set_Value (V (Column_Foreground), Color_Normal);
         end if;

         Self.Tree.Model.Set (Iter, V);

         if Select_Nodes then
            Self.Tree.Get_Selection.Select_Iter
              (Self.Tree.Convert_To_Filter_Iter (Iter));
         end if;
      end Internal;

      Show         : constant Boolean :=
        Is_Active or else not Self.Config.Hide_Other_VCS;
      In_Staged    : Boolean;
      In_Modified  : Boolean;
      In_Untracked : Boolean;

   begin
      In_Staged := (Props.Status and Mask_Staged) /= 0
        and then Show;
      In_Modified := (Props.Status and Mask_Modified_Unstaged) /= 0
        and then (not In_Staged or else Self.Config.Group_By_Category)
        and then Show;
      In_Untracked := Self.Config.Show_Untracked_Files
        and then (Props.Status and Mask_Untracked) /= 0
        and then (not (In_Staged or In_Modified)
                 or else Self.Config.Group_By_Category)
        and then not Self.Kernel.Is_Hidden (File);

      --  A file could be in multiple categories

      if In_Untracked then
         Internal
           ((if Self.Category_Untracked = Null_Gtk_Tree_Path
            then Null_Iter
            else Self.Tree.Model.Get_Iter (Self.Category_Untracked)));
      end if;

      if In_Staged then
         Internal
           ((if Self.Category_Staged = Null_Gtk_Tree_Path
            then Null_Iter
            else Self.Tree.Model.Get_Iter (Self.Category_Staged)));
      end if;

      if In_Modified then
         Internal
           ((if Self.Category_Modified = Null_Gtk_Tree_Path
            then Null_Iter
            else Self.Tree.Model.Get_Iter (Self.Category_Modified)));
      end if;
   end Create_Nodes;

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
      pragma Unreferenced (Kernel);
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
         Refresh (Self.View);
      end if;
   end Execute;

   ----------------------------------------
   -- Get_Commit_Message_From_Properties --
   ----------------------------------------

   function Get_Commit_Message_From_Properties
     (VCS    : not null access VCS_Engine'Class)
      return String
   is
      P     : String_Property;
      Found : Boolean;
   begin
      Get_Property
        (P,
         Key      => VCS.Name & "--" & VCS.Working_Directory.Display_Full_Name,
         Name     => "commit_msg",
         Found    => Found);
      if Found and then P.Value /= null then
         return P.Value.all;
      else
         return "";
      end if;
   end Get_Commit_Message_From_Properties;

   ------------------------
   -- Set_Commit_Message --
   ------------------------

   procedure Set_Commit_Message
     (VCS    : not null access VCS_Engine'Class;
      Msg    : String)
   is
      P    : access String_Property;
   begin
      P := new String_Property'
        (Property_Record with Value => new String'(Msg));
      Set_Property
        (VCS.Kernel,
         Key      => VCS.Name & "--" & VCS.Working_Directory.Display_Full_Name,
         Name       => "commit_msg",
         Property   => P,
         Persistent => True);
   end Set_Commit_Message;

   -------------
   -- Refresh --
   -------------

   procedure Refresh (Self : not null access Commit_View_Record'Class) is
   begin
      --  Save and restore commit message
      Save_Commit_Message (Self);
      Self.Active_VCS := Active_VCS (Self.Kernel);

      if Self.Active_VCS /= null then
         Self.Commit.Get_Buffer.Set_Text
           (Get_Commit_Message_From_Properties (Self.Active_VCS));
      else
         Self.Commit.Get_Buffer.Set_Text ("");
      end if;

      Show_Placeholder_If_Needed (Self.Commit);

      --  Update all statuses in background

      Self.Computing_File_Status := True;
      Ensure_Status_For_All_Files_In_All_Engines
        (Self.Kernel, On_Complete => new On_All_Files_Available_In_Cache'
           (Task_Completed_Callback with Kernel => Self.Kernel));
   end Refresh;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Active_VCS_Changed;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      V    : constant Commit_View := Commit_Views.Retrieve_View (Kernel);
   begin
      if V /= null then
         Refresh (V);
      end if;
   end Execute;

   -------------------------
   -- Save_Commit_Message --
   -------------------------

   procedure Save_Commit_Message
     (Self : not null access Commit_View_Record'Class)
   is
      Text : constant String := Get_Text_Without_Placeholder (Self.Commit);
      VCS  : constant VCS_Engine_Access := Self.Active_VCS;
   begin
      if VCS /= null then
         Set_Commit_Message (VCS, Text);
      end if;
   end Save_Commit_Message;

   -------------------------
   -- On_Commit_Focus_Out --
   -------------------------

   function On_Commit_Focus_Out
     (View  : access GObject_Record'Class;
      Event : Gdk_Event_Focus) return Boolean
   is
      pragma Unreferenced (Event);
   begin
      Save_Commit_Message (Commit_View (View));
      return False;
   end On_Commit_Focus_Out;

   -----------------------
   -- On_Staged_Toggled --
   -----------------------

   procedure On_Staged_Toggled
     (Self : access GObject_Record'Class;
      Path : String)
   is
      View        : constant Commit_View := Commit_View (Self);
      Filter_Path : constant Gtk_Tree_Path :=
        Gtk_Tree_Path_New_From_String (Path);
      Iter        : constant Gtk_Tree_Iter :=
        View.Tree.Get_Store_Iter_For_Filter_Path (Filter_Path);
      File : constant Virtual_File := Get_File_From_Node (View.Tree, Iter);
   begin
      if View.Active_VCS /= null then
         View.Tree.Get_Selection.Unselect_All;
         View.Active_VCS.Stage_Or_Unstage_Files
           ((1 => File),
            Stage => not View.Tree.Model.Get_Boolean (Iter, Column_Staged));
      end if;
      Path_Free (Filter_Path);
   end On_Staged_Toggled;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Stage_File;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View   : constant Commit_View :=
        Commit_Views.Retrieve_View (Get_Kernel (Context.Context));
      Files  : File_Array_Access;
      Staged : Boolean;

      procedure On_Selection
        (Model : Gtk_Tree_Model;
         Path  : Gtk_Tree_Path;
         Iter  : Gtk_Tree_Iter);
      procedure On_Selection
        (Model : Gtk_Tree_Model;
         Path  : Gtk_Tree_Path;
         Iter  : Gtk_Tree_Iter)
      is
         pragma Unreferenced (Model, Path);
         F : constant Virtual_File := Get_File_From_Node
           (View.Tree, View.Tree.Convert_To_Store_Iter (Iter));
      begin
         if F /= No_File then
            Append (Files, F);
            Staged := View.Tree.Filter.Get_Boolean (Iter, Column_Staged);
         end if;
      end On_Selection;

   begin
      if View /= null then
         View.Tree.Get_Selection.Selected_Foreach
           (On_Selection'Unrestricted_Access);

         if Files /= null then
            View.Tree.Get_Selection.Unselect_All;
            View.Active_VCS.Stage_Or_Unstage_Files
              (Files.all, Stage => not Staged);
            Unchecked_Free (Files);
         end if;
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Committed;
      Kernel : not null access Kernel_Handle_Record'Class;
      VCS    : not null access Abstract_VCS_Engine'Class)
   is
      pragma Unreferenced (Self);
      View : constant Commit_View := Commit_Views.Retrieve_View (Kernel);
   begin
      Trace (Me, "Commit completed successfully");
      Set_Commit_Message (VCS_Engine_Access (VCS), "");
      if View /= null then
         View.Commit.Get_Buffer.Set_Text ("");
         Show_Placeholder_If_Needed (View.Commit);
      end if;

      Display_Informational_Popup
        (Parent    => Get_Main_Window (Kernel),
         Icon_Name => "github-commit-symbolic",
         Text      => -"Committed");
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Commit;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      VCS    : constant VCS_Engine_Access := Active_VCS (Kernel);
      View   : constant Commit_View := Commit_Views.Retrieve_View (Kernel);
   begin
      if VCS /= null then
         declare
            --  Get the commit message from the widget, not from the
            --  properties, so that any change done by the user (but not saved
            --  yet because the editor still has focus) is taken into account.

            Msg : constant String :=
              (if View /= null and then View.Active_VCS = VCS
               then Get_Text_Without_Placeholder (View.Commit)
               else Get_Commit_Message_From_Properties (VCS));
         begin
            if Msg /= "" then
               VCS.Commit_Staged_Files (Msg);
            else
               Insert (Kernel, "No commit message specified", Mode => Error);
            end if;
         end;
      else
         Trace (Me, "No VCS selected");
      end if;

      return Commands.Success;
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
      Self.On_Destroy (On_Destroyed'Access);

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
      Self.Commit.On_Focus_Out_Event (On_Commit_Focus_Out'Access, Self);

      --  Bottom widget

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Always);
      Paned.Split
        (Ref_Widget  => Scrolled2,
         New_Child   => Scrolled,
         Orientation => Orientation_Vertical);

      Self.Tree := new Commit_Tree_Record;
      Initialize (Self.Tree,
                  (Column_File          => Get_Virtual_File_Type,
                   Column_Name          => GType_String,
                   Column_Icon          => GType_String,
                   Column_Inconsistent  => GType_Boolean,
                   Column_Foreground    => Gdk.RGBA.Get_Type,
                   Column_Check_Visible => GType_Boolean,
                   Column_Staged        => GType_Boolean),
                  Filtered         => True,
                  Set_Visible_Func => True);
      Self.Tree.Set_Headers_Visible (False);
      Self.Tree.Get_Selection.Set_Mode (Selection_Multiple);
      Scrolled.Add (Self.Tree);

      --  Tree

      Gtk_New (Self.Text_Render);
      Gtk_New (Check);
      Gtk_New (Pixbuf);

      Check.On_Toggled (On_Staged_Toggled'Access, Self);

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

      Vcs_File_Status_Changed_Hook.Add
        (new On_VCS_File_Status_Changed, Watch => Self);
   end On_Create;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self          : On_VCS_File_Status_Changed;
      Kernel        : not null access Kernel_Handle_Record'Class;
      Vcs           : not null access Abstract_VCS_Engine'Class;
      File          : GNATCOLL.VFS.Virtual_File;
      Props         : VCS_File_Properties)
   is
      pragma Unreferenced (Self);
      View : constant Commit_View := Commit_Views.Retrieve_View (Kernel);

      procedure Remove_Node_In_List (List : Gtk_Tree_Iter);
      --  Remove the node for File in List and its siblings

      procedure Remove_From_Tree;
      --  Remove all nodes for File in the tree

      procedure Remove_Node_In_List (List : Gtk_Tree_Iter) is
         Iter : Gtk_Tree_Iter := List;
      begin
         while Iter /= Null_Iter loop
            if Get_File_From_Node (View.Tree, Iter) = File then
               View.Tree.Model.Remove (Iter);
               return;
            end if;
            View.Tree.Model.Next (Iter);
         end loop;
      end Remove_Node_In_List;

      procedure Remove_From_Tree is
         Iter : Gtk_Tree_Iter;
      begin
         if View.Config.Group_By_Category then
            Iter := View.Tree.Model.Get_Iter_First;
            while Iter /= Null_Iter loop
               Remove_Node_In_List (View.Tree.Model.Children (Iter));
               View.Tree.Model.Next (Iter);
            end loop;

         else
            Remove_Node_In_List (View.Tree.Model.Get_Iter_First);
         end if;
      end Remove_From_Tree;

   begin
      if not View.Computing_File_Status then
         Remove_From_Tree;
         View.Create_Nodes
           (File, Props, VCS_Engine_Access (Vcs), Select_Nodes => True);
         View.Tree.Expand_All;
      end if;
   end Execute;

   --------------------
   -- Filter_Changed --
   --------------------

   overriding procedure Filter_Changed
     (Self    : not null access Commit_View_Record;
      Pattern : in out GPS.Search.Search_Pattern_Access)
   is
   begin
      GPS.Search.Free (Self.Tree.User_Filter);
      Self.Tree.User_Filter := Pattern;
      Self.Tree.Refilter;
   end Filter_Changed;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self : not null access Commit_Tree_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      F : constant Virtual_File := Get_File_From_Node (Self, Iter);
   begin
      if F = No_File then
         return True;   --  categories
      elsif Self.User_Filter = null then
         return True;   --  no filter
      else
         return Self.User_Filter.Start
           (F.Display_Base_Name) /= GPS.Search.No_Match;
      end if;
   end Is_Visible;

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

      procedure On_File (File : Virtual_File; Props : VCS_File_Properties);
      procedure On_File (File : Virtual_File; Props : VCS_File_Properties) is
      begin
         View.Create_Nodes (File, Props, Local_VCS);
      end On_File;

      procedure On_VCS (VCS : not null access VCS_Engine'Class);
      procedure On_VCS (VCS : not null access VCS_Engine'Class) is
      begin
         Trace (Me, "Got all files in cache for " & VCS.Name);
         View.VCS_Count := View.VCS_Count + 1;
         Local_VCS := VCS;
         VCS.For_Each_File_In_Cache (On_File'Access);
      end On_VCS;

      Iter : Gtk_Tree_Iter;

   begin
      --  When all engines have refreshed, do a global refresh
      if VCS = null and then View /= null then

         View.VCS_Count := 0;
         View.Computing_File_Status := False;

         declare
            Dummy : constant Expansion.Detached_Model :=
              Expansion.Detach_Model_From_View (View.Tree);
         begin
            View.Tree.Model.Clear;
            Path_Free (View.Category_Staged);
            View.Category_Staged := Null_Gtk_Tree_Path;
            Path_Free (View.Category_Modified);
            View.Category_Modified := Null_Gtk_Tree_Path;
            Path_Free (View.Category_Untracked);
            View.Category_Untracked := Null_Gtk_Tree_Path;

            if View.Config.Group_By_Category then
               Iter := Create_Category_Node (View, -"Staged Files");
               View.Category_Staged := View.Tree.Model.Get_Path (Iter);

               Iter := Create_Category_Node (View, -"Modified Files");
               View.Category_Modified := View.Tree.Model.Get_Path (Iter);

               if View.Config.Show_Untracked_Files then
                  Iter := Create_Category_Node (View, -"Untracked Files");
                  View.Category_Untracked := View.Tree.Model.Get_Path (Iter);
               end if;
            end if;

            For_Each_VCS (Self.Kernel, On_VCS'Access);
         end;

         if View.Config.Group_By_Category then
            View.Tree.Expand_All;

            --  Find the location of the category nodes, after sorting has
            --  been done

            Path_Free (View.Category_Staged);
            Path_Free (View.Category_Modified);
            Path_Free (View.Category_Untracked);
            View.Category_Modified  := Gtk_Tree_Path_New_From_String ("0");
            View.Category_Staged    := Gtk_Tree_Path_New_From_String ("1");
            View.Category_Untracked := Gtk_Tree_Path_New_From_String ("2");
         end if;

         Commit_Views.Reset_Toolbar (View);
      end if;
   end Execute;

   ------------------
   -- On_Destroyed --
   ------------------

   procedure On_Destroyed (View : access Gtk_Widget_Record'Class) is
      Self : constant Commit_View := Commit_View (View);
   begin
      Path_Free (Self.Category_Staged);
      Path_Free (Self.Category_Modified);
      Path_Free (Self.Category_Untracked);
   end On_Destroyed;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Reload_Status;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View : constant Commit_View :=
        Commit_Views.Retrieve_View (Get_Kernel (Context.Context));
   begin
      Invalidate_All_Caches (Get_Kernel (Context.Context));

      if View /= null then
         Refresh (View);
      end if;
      return Commands.Success;
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
        (Kernel, "vcs toggle stage file",
         Description =>
           -("Stage or unstage the selected file, so that it is part of the"
             & " next commit"),
         Command     => new Stage_File,
         Category    => "VCS2",
         Icon_Name   => "github-check-symbolic");

      Register_Action
        (Kernel, "vcs reload status",
         Description =>
           -("Reload the status of all files from the disk." & ASCII.LF
           & "Use if you have performed operations outside of GPS."),
         Command     => new Reload_Status,
         Category    => "VCS2",
         Icon_Name   => "gps-refresh-symbolic");

      Vcs_Commit_Done_Hook.Add (new On_Committed);

   end Register_Module;

end VCS2.Commits;
