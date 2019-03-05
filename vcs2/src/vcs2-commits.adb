------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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
with Glib_Values_Utils;           use Glib_Values_Utils;
with Glib;                        use Glib;
with Glib.Convert;                use Glib.Convert;
with GNATCOLL.Projects;           use GNATCOLL.Projects;
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
with Gtkada.Dialogs;              use Gtkada.Dialogs;
with Gtkada.Multi_Paned;          use Gtkada.Multi_Paned;
with Gtkada.Style;                use Gtkada.Style;
with Gtk.Box;                     use Gtk.Box;
with Gtk.Cell_Renderer_Pixbuf;    use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;      use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Toggle;    use Gtk.Cell_Renderer_Toggle;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Gesture_Long_Press;      use Gtk.Gesture_Long_Press;
with Gtk.Gesture_Multi_Press;     use Gtk.Gesture_Multi_Press;
with Gtk.Label;                   use Gtk.Label;
with Gtk.Menu;                    use Gtk.Menu;
with Gtk.Scrolled_Window;         use Gtk.Scrolled_Window;
with Gtk.Text_Tag;                use Gtk.Text_Tag;
with Gtk.Text_View;               use Gtk.Text_View;
with Gtk.Toolbar;                 use Gtk.Toolbar;
with Gtk.Tree_Model;              use Gtk.Tree_Model;
with Gtk.Tree_View_Column;        use Gtk.Tree_View_Column;
with Gtk.Widget;                  use Gtk.Widget;
with Gtkada.MDI;                  use Gtkada.MDI;
with Gtkada.Tree_View;            use Gtkada.Tree_View;
with GUI_Utils;                   use GUI_Utils;
with Tooltips;                    use Tooltips;
with VCS2.Engines;                use VCS2.Engines;
with VCS2.Views;                  use VCS2.Views;

package body VCS2.Commits is
   Me : constant Trace_Handle := Create ("GPS.VCS.COMMITS");

   Column_File          : constant := 0;
   Column_Staged        : constant := 1;
   Column_Name          : constant := 2;
   Column_Icon          : constant := 3;
   Column_Inconsistent  : constant := 4;
   Column_Check_Visible : constant := 5;
   Column_Foreground    : constant := 6;
   subtype All_Columns is Gint range Column_File .. Column_Foreground;

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
     (Self : not null access Tree_View_Record'Class;
      Iter : Gtk_Tree_Iter) return Virtual_File
     is (Get_File (Self.Model, Iter, Column_File));

   package Expansion is new Expansion_Support
     (Tree_Record    => Tree_View_Record,
      Id             => Virtual_File,
      Get_Id         => Get_File_From_Node,
      Hash           => GNATCOLL.VFS.Full_Name_Hash);

   type Commit_View_Record is new Base_VCS_View_Record with record
      Active_VCS  : VCS_Engine_Access;
      --  A cache for the active VCS. This is used to save the commit message
      --  for the proper VCS.

      Computing_File_Status : Boolean := False;
      --  Whether there is background work to recompute the status of all
      --  files. During that time, we do not react to VCS_File_Status_Changed
      --  hook to refresh the view, since it will be wiped out when the
      --  background job finishes anyway.

      Category_Staged    : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Category_Modified  : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      Category_Untracked : Gtk_Tree_Path := Null_Gtk_Tree_Path;
      --  The nodes for the categories (if they are displayed)

      Multipress  : Gtk_Gesture_Multi_Press;
      Longpress   : Gtk_Gesture_Long_Press;

      Config      : Commit_View_Config;
      Commit      : Gtk_Text_View;
   end record;
   overriding procedure Create_Menu
     (View    : not null access Commit_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding procedure On_Create
     (Self    : not null access Commit_View_Record;
      Child   : not null access GPS.Kernel.MDI.GPS_MDI_Child_Record'Class);
   overriding procedure Filter_Changed
     (Self    : not null access Commit_View_Record;
      Pattern : in out GPS.Search.Search_Pattern_Access);
   overriding procedure On_Preferences_Changed
     (Self    : not null access Commit_View_Record;
      Pref    : Preference);
   overriding procedure Refresh
     (Self : not null access Commit_View_Record);

   procedure Refresh
     (Self      : not null access Commit_View_Record'Class;
      From_User : Boolean);
   --  Internal implementation for Refresh, which indicates whether the
   --  action comes from the user pressing the 'Reload' button.

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

   procedure On_Multipress
     (Self    : access Glib.Object.GObject_Record'Class;
      N_Press : Gint;
      X, Y    : Gdouble);
   procedure On_Longpress
     (Self    : access Glib.Object.GObject_Record'Class;
      X, Y    : Gdouble);
   --  Called every time a row is clicked with specific gesture

   type On_All_Files_Available_In_Cache is new Task_Visitor with
      record
         Kernel : not null access Kernel_Handle_Record'Class;
      end record;
   overriding procedure On_Terminate
     (Self : not null access On_All_Files_Available_In_Cache;
      VCS  : access VCS_Engine'Class);

   type Commit_Tooltips is new Tooltips.Tooltips with record
      View   : access Commit_View_Record'Class;
   end record;
   overriding function Create_Contents
     (Self     : not null access Commit_Tooltips;
      Widget   : not null access Gtk.Widget.Gtk_Widget_Record'Class;
      X, Y     : Glib.Gint) return Gtk.Widget.Gtk_Widget;

   type Is_Staged_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Self    : access Is_Staged_Filter;
      Context : Selection_Context) return Boolean;
   --  True if the file is at least partially staged (i.e. there could still
   --  be some unstaged changes)

   type Is_Unstaged_Filter is new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Self    : access Is_Unstaged_Filter;
      Context : Selection_Context) return Boolean;
   --  True if the file is at least partially unstaged and modified (i.e.
   --  there could still be some staged changes).

   type Toggle_Stage_Selected_Files is
     new Interactive_Command with null record;
   overriding function Execute
     (Command : access Toggle_Stage_Selected_Files;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Stage_File is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Stage_File;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Stage for commit the file described in the context.

   type Unstage_File is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Unstage_File;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Unstage the file described in the context.

   type Reload_Status is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Reload_Status;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Commit is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Commit;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Discard_Changes is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Discard_Changes;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Commit_Visitor is new Task_Visitor with null record;
   overriding procedure On_Success
      (Self   : not null access Commit_Visitor;
       Kernel : not null access Kernel_Handle_Record'Class);
   --  Called when a commit has been completed

   type On_Active_VCS_Changed is new Simple_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Active_VCS_Changed;
      Kernel : not null access Kernel_Handle_Record'Class);

   type On_VCS_Refresh is new Vcs_Refresh_Hooks_Function with null record;
   overriding procedure Execute
     (Self          : On_VCS_Refresh;
      Kernel        : not null access Kernel_Handle_Record'Class;
      Is_File_Saved : Boolean);

   function On_Commit_Focus_Out
     (View  : access GObject_Record'Class;
      Event : Gdk_Event_Focus) return Boolean;
   --  Called when the focus moves out of the Commit message.
   --  Used to save it in the properties.

   procedure Save_Commit_Message
     (Self : not null access Commit_View_Record'Class);
   --  Save the commit message in the properties

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
      Files         : File_Sets.Set;
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
               -("Click on the checkbox to stage the file, so that it is part"
                 & " of the next commit")
               & Get_Tooltip_For_File
                 (Kernel       => Self.View.Kernel,
                  File         => File));
            Label.Set_Use_Markup (True);
         end if;
      end if;
      return Gtk_Widget (Label);
   end Create_Contents;

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
              (V (Column_Name),
               Escape_Text (+File.Relative_Path (VCS.Working_Directory)));
         else
            Init_Set_String
              (V (Column_Name),
               Escape_Text (File.Display_Full_Name));
         end if;

         Init_Set_String (V (Column_Icon), To_String (Display.Icon_Name));
         Init_Set_Boolean (V (Column_Check_Visible), Is_Active or Untracked);

         Init (V (Column_Foreground), Gdk.RGBA.Get_Type);
         if not Is_Active then
            Gdk.RGBA.Set_Value
              (V (Column_Foreground),
               Shade_Or_Lighten (Default_Style.Get_Pref_Fg, 0.6));
         elsif Untracked and then not Self.Config.Group_By_Category then
            Gdk.RGBA.Set_Value
              (V (Column_Foreground),
               Shade_Or_Lighten (Default_Style.Get_Pref_Fg));
         else
            Gdk.RGBA.Set_Value
              (V (Column_Foreground), Default_Style.Get_Pref_Fg);
         end if;

         Set_All_And_Clear (Self.Tree.Model, Iter, V);

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
      Init_Set_String (V (Column_Name), "<b>" & Escape_Text (Name) & "</b>");
      Init_Set_String (V (Column_Icon), "gps-emblem-directory-open");
      Init (V (Column_Foreground), Gdk.RGBA.Get_Type);
      Gdk.RGBA.Set_Value
        (V (Column_Foreground),
         Default_Style.Get_Pref_Fg);

      Self.Tree.Model.Set (Iter, V);

      return Iter;
   end Create_Category_Node;

   ----------------------------
   -- On_Preferences_Changed --
   ----------------------------

   overriding procedure On_Preferences_Changed
     (Self    : not null access Commit_View_Record;
      Pref    : Preference)
   is
      Config : Commit_View_Config;
   begin
      Base_VCS_View_Record (Self.all).On_Preferences_Changed (Pref);

      Config :=
        (Initialized          => True,
         Hidden_Files_Pattern =>
           To_Unbounded_String (Hidden_Files_Pattern.Get_Pref),
         Show_Untracked_Files => Show_Untracked_Files.Get_Pref,
         Relative_Names       => Relative_Names.Get_Pref,
         Group_By_Category    => Group_By_Category.Get_Pref,
         Show_Hidden_Files    => Show_Hidden_Files.Get_Pref,
         Hide_Other_VCS       => Hide_Other_VCS.Get_Pref);

      if Config /= Self.Config or else Pref = Preference (Default_Style) then
         Self.Config := Config;
         Refresh (Self);
      end if;
   end On_Preferences_Changed;

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
     (VCS : not null access VCS_Engine'Class;
      Msg : String) is
   begin
      Set_Property
        (VCS.Kernel,
         Key        =>
           VCS.Name & "--" & VCS.Working_Directory.Display_Full_Name,
         Name       => "commit_msg",
         Property   => new String_Property'(Value => new String'(Msg)),
         Persistent => True);
   end Set_Commit_Message;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self          : On_VCS_Refresh;
      Kernel        : not null access Kernel_Handle_Record'Class;
      Is_File_Saved : Boolean)
   is
      pragma Unreferenced (Self, Is_File_Saved);
      View : constant Commit_View := Commit_Views.Retrieve_View (Kernel);
   begin
      if View /= null then
         Refresh (View, From_User => True);
      end if;
   end Execute;

   -------------
   -- Refresh --
   -------------

   overriding procedure Refresh
     (Self : not null access Commit_View_Record) is
   begin
      Refresh (Self, From_User => False);
   end Refresh;

   -------------
   -- Refresh --
   -------------

   procedure Refresh
     (Self : not null access Commit_View_Record'Class;
      From_User : Boolean) is
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
        (Self.Kernel,
         Visitor => new On_All_Files_Available_In_Cache'
           (Task_Visitor with Kernel => Self.Kernel),
         From_User => From_User);
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
     (Command : access Toggle_Stage_Selected_Files;
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

   overriding function Execute
     (Command : access Discard_Changes;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View   : constant Commit_View :=
        Commit_Views.Retrieve_View (Get_Kernel (Context.Context));
      Files  : File_Array_Access;

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
         end if;
      end On_Selection;

   begin
      if View /= null then
         View.Tree.Get_Selection.Selected_Foreach
           (On_Selection'Unrestricted_Access);

         if Files /= null
           and then Message_Dialog
             (-("Discard local changes ?" & ASCII.LF
                & "This operation cannot be undone."),
              Dialog_Type    => Confirmation,
              Buttons        => Button_Yes or Button_No,
              Default_Button => Button_No,
              Title          => -"Confirm discard",
              Parent         => Get_Main_Window (View.Kernel)) = Button_Yes
         then
            View.Active_VCS.Queue_Discard_Local_Changes
              (Files   => Files,  --  freed by Queue_Discard_Local_Changes
               Visitor => Refresh_On_Terminate (View.Kernel));
         end if;
      end if;
      return Commands.Success;
   end Execute;

   ----------------
   -- On_Success --
   ----------------

   overriding procedure On_Success
      (Self   : not null access Commit_Visitor;
       Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Self);
      View : constant Commit_View := Commit_Views.Retrieve_View (Kernel);
   begin
      Trace (Me, "Commit completed successfully");
      Set_Commit_Message (Active_VCS (Kernel), "");
      if View /= null then
         View.Commit.Get_Buffer.Set_Text ("");
         Show_Placeholder_If_Needed (View.Commit);
      end if;

      --  Force a refresh of all VCS status
      Kernel.VCS.Invalidate_All_Caches;
      Vcs_Refresh_Hook.Run (Kernel, Is_File_Saved => False);
   end On_Success;

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
               VCS.Queue_Commit_Staged_Files
                 (Message => Msg,
                  Visitor => new Commit_Visitor);
            else
               Insert (Kernel, "No commit message specified", Mode => Error);
            end if;
         end;
      else
         Trace (Me, "No VCS selected");
      end if;

      return Commands.Success;
   end Execute;

   ------------------
   -- On_Longpress --
   ------------------

   procedure On_Longpress
     (Self    : access Glib.Object.GObject_Record'Class;
      X, Y    : Gdouble)
   is
      View           : constant Commit_View := Commit_View (Self);
      Filter_Path    : Gtk_Tree_Path;
      Col            : Gtk_Tree_View_Column;
      Success        : Boolean;
      Cell_X, Cell_Y : Gint;
   begin
      View.Tree.Get_Path_At_Pos
        (Gint (X), Gint (Y), Filter_Path,
         Col, Cell_X, Cell_Y, Success);
      if Success then
         --  Select the row
         View.Tree.Set_Cursor (Filter_Path, null, Start_Editing => False);

         Success := Execute_Action
            (View.Kernel,
             Action => "diff against head for file");

         Path_Free (Filter_Path);
         View.Longpress.Set_State (Event_Sequence_Claimed);
      end if;
   end On_Longpress;

   -------------------
   -- On_Multipress --
   -------------------

   procedure On_Multipress
     (Self    : access Glib.Object.GObject_Record'Class;
      N_Press : Gint;
      X, Y    : Gdouble)
   is
      View           : constant Commit_View := Commit_View (Self);
      File           : Virtual_File;
      Filter_Path    : Gtk_Tree_Path;
      Column         : Gtk_Tree_View_Column;
      Success        : Boolean;
      Cell_X, Cell_Y : Gint;
   begin
      if N_Press = 2 then
         View.Tree.Get_Path_At_Pos
           (Gint (X), Gint (Y), Filter_Path,
            Column, Cell_X, Cell_Y, Success);
         if Success then
            --  Select the row that was clicked
            View.Tree.Set_Cursor (Filter_Path, null, Start_Editing => False);
            File := Get_File_From_Node
              (View.Tree,
               View.Tree.Get_Store_Iter_For_Filter_Path (Filter_Path));
            if File /= No_File then
               Open_File_Action_Hook.Run
                 (View.Kernel,
                  File,
                  Project => GNATCOLL.Projects.No_Project,
                  Line    => 0,
                  Column  => 0);
            end if;

            Path_Free (Filter_Path);
            View.Multipress.Set_State (Event_Sequence_Claimed);
         end if;
      end if;
   end On_Multipress;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Commit_View_Record'Class) return Gtk_Widget
   is
      Scrolled  : Gtk_Scrolled_Window;
      Scrolled2 : Gtk_Scrolled_Window;
      Paned     : Gtkada_Multi_Paned;
      Col       : Gtk_Tree_View_Column;
      Check     : Gtk_Cell_Renderer_Toggle;
      Pixbuf    : Gtk_Cell_Renderer_Pixbuf;
      Dummy     : Gint;
      Tooltip   : Tooltips.Tooltips_Access;

   begin
      Initialize_Vbox (Self, Homogeneous => False);
      Self.On_Destroy (On_Destroyed'Access);

      Base_VCS_View_Record (Self.all).Filter_Hist_Prefix :=
        To_Unbounded_String ("commits");

      Gtk_New (Paned);
      Paned.Set_Opaque_Resizing (True);
      Self.Pack_Start (Paned);

      --  Top widget

      Gtk_New (Scrolled2);
      Scrolled2.Set_Policy (Policy_Automatic, Policy_Automatic);
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
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
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
                  Capability_Type  => Filtered,
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
      Col.Add_Attribute (Self.Text_Render, "markup", Column_Name);
      Col.Add_Attribute
        (Self.Text_Render, "foreground-rgba", Column_Foreground);

      Self.Tree.Model.Set_Sort_Column_Id (Column_Name, Sort_Ascending);

      Tooltip := new Commit_Tooltips'(Tooltips.Tooltips with View => Self);
      Tooltip.Set_Tooltip (Self.Tree);

      Setup_Contextual_Menu (Self.Kernel, Self.Tree);

      Gtk_New (Self.Multipress, Widget => Self.Tree);
      Self.Multipress.On_Pressed (On_Multipress'Access, Slot => Self);
      Self.Multipress.Watch (Self);

      Gtk_New (Self.Longpress, Widget => Self.Tree);
      Self.Longpress.On_Pressed (On_Longpress'Access, Slot => Self);
      Self.Longpress.Watch (Self);

      return Gtk_Widget (Self.Tree);
   end Initialize;

   ---------------
   -- On_Create --
   ---------------

   overriding procedure On_Create
     (Self    : not null access Commit_View_Record;
      Child   : not null access GPS.Kernel.MDI.GPS_MDI_Child_Record'Class) is
   begin
      Base_VCS_View_Record (Self.all).On_Create (Child);   --  inherited

      Vcs_Refresh_Hook.Add (new On_VCS_Refresh, Watch => Self);
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
      Files         : File_Sets.Set;
      Props         : VCS_File_Properties)
   is
      pragma Unreferenced (Self);
      View : constant Commit_View := Commit_Views.Retrieve_View (Kernel);

      procedure Remove_Node_In_List (List : Gtk_Tree_Iter);
      --  Remove the node for Files in List and its siblings

      procedure Remove_From_Tree;
      --  Remove all nodes for File in the tree

      procedure Remove_Node_In_List (List : Gtk_Tree_Iter) is
         Iter : Gtk_Tree_Iter := List;
      begin
         while Iter /= Null_Iter loop
            if Files.Contains (Get_File_From_Node (View.Tree, Iter)) then
               View.Tree.Model.Remove (Iter);
            else
               View.Tree.Model.Next (Iter);
            end if;
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
         --  Detached the view: it doesn't bring anything for performance, but
         --  ensures that the selection is preserved while we move the file's
         --  nodes around.
         declare
            Dummy : constant Expansion.Detached_Model :=
              Expansion.Detach_Model_From_View (View.Tree);
         begin
            Remove_From_Tree;
            for F of Files loop
               View.Create_Nodes
                 (F, Props, VCS_Engine_Access (Vcs), Select_Nodes => False);
            end loop;
         end;

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
      GPS.Search.Free (Commit_Tree_View (Self.Tree).User_Filter);
      Commit_Tree_View (Self.Tree).User_Filter := Pattern;
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

   ------------------
   -- On_Terminate --
   ------------------

   overriding procedure On_Terminate
     (Self : not null access On_All_Files_Available_In_Cache;
      VCS  : access VCS_Engine'Class)
   is
      procedure On_File (File : Virtual_File; Props : VCS_File_Properties);

      procedure On_VCS (VCS : not null access VCS_Engine'Class);

      View : constant Commit_View := Commit_Views.Retrieve_View (Self.Kernel);
      --  Have to fetch the view, in case it was closed while the status was
      --  fetching in the background

      Local_VCS : access VCS_Engine'Class;

      -------------
      -- On_File --
      -------------

      procedure On_File (File : Virtual_File; Props : VCS_File_Properties) is
      begin
         View.Create_Nodes (File, Props, Local_VCS);
      end On_File;

      ------------
      -- On_VCS --
      ------------

      procedure On_VCS (VCS : not null access VCS_Engine'Class) is
      begin
         Trace (Me, "Got all files in cache for " & VCS.Name);
         Local_VCS := VCS;
         VCS.For_Each_File_In_Cache
           (On_File'Access, Only_If_Up_To_Date => True);
      end On_VCS;

      Iter : Gtk_Tree_Iter;

   begin
      --  When all engines have refreshed, do a global refresh
      if VCS = null and then View /= null then

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
   end On_Terminate;

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
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
   begin
      Invalidate_All_Caches (Kernel.VCS);
      Vcs_Refresh_Hook.Run (Kernel, Is_File_Saved => False);
      return Commands.Success;
   end Execute;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Self    : access Is_Staged_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Self);
      File   : constant Virtual_File := File_Information (Context);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      VCS    : VCS_Engine_Access;
      Props  : VCS_File_Properties;
   begin
      if File /= No_File then
         VCS := VCS_Engine_Access
           (Kernel.VCS.Guess_VCS_For_Directory (File.Dir));
         Props := VCS.File_Properties_From_Cache (File);
         return (Props.Status and Mask_Staged) /= 0;
      end if;
      return False;
   end Filter_Matches_Primitive;

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Self    : access Is_Unstaged_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Self);
      File   : constant Virtual_File := File_Information (Context);
      Kernel : constant Kernel_Handle := Get_Kernel (Context);
      VCS    : VCS_Engine_Access;
      Props  : VCS_File_Properties;
   begin
      if File /= No_File then
         VCS := VCS_Engine_Access
           (Kernel.VCS.Guess_VCS_For_Directory (File.Dir));
         Props := VCS.File_Properties_From_Cache (File);
         return (Props.Status and Mask_Modified_Unstaged) /= 0
           or else (Props.Status  and Mask_Untracked) /= 0;
      end if;
      return False;
   end Filter_Matches_Primitive;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Stage_File;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      File   : constant Virtual_File := File_Information (Context.Context);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      VCS    : VCS_Engine_Access;
   begin
      if File /= No_File then
         VCS := VCS_Engine_Access
           (Kernel.VCS.Guess_VCS_For_Directory (File.Dir));
         VCS.Stage_Or_Unstage_Files ((1 => File), Stage => True);
      end if;
      return Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Unstage_File;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      File   : constant Virtual_File := File_Information (Context.Context);
      Kernel : constant Kernel_Handle := Get_Kernel (Context.Context);
      VCS    : VCS_Engine_Access;
   begin
      if File /= No_File then
         VCS := VCS_Engine_Access
           (Kernel.VCS.Guess_VCS_For_Directory (File.Dir));
         VCS.Stage_Or_Unstage_Files ((1 => File), Stage => False);
      end if;
      return Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : not null access Kernel_Handle_Record'Class)
   is
      Is_Staged   : constant Action_Filter := new Is_Staged_Filter;
      Is_Unstaged : constant Action_Filter := new Is_Unstaged_Filter;
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
         Default  => True,
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
        (Kernel, "vcs toggle stage selected files",
         Description =>
           -("Stage or unstage the selected file, so that it is part of the"
             & " next commit"),
         Command     => new Toggle_Stage_Selected_Files,
         Filter      => Lookup_Filter (Kernel, "File"),
         Category    => "VCS2",
         Icon_Name   => "github-check-symbolic");

      Register_Action
        (Kernel, "vcs stage file",
         Description =>
           -"Stage the current file so that it is part of the next commit",
         Command     => new Stage_File,
         Filter      => Lookup_Filter (Kernel, "File") and Is_Unstaged,
         Category    => "VCS2");

      Register_Action
        (Kernel, "vcs unstage file",
         Description =>
           -("Unstage the current file so that it is not part of the"
             & " next commit"),
         Command     => new Unstage_File,
         Filter      => Lookup_Filter (Kernel, "File") and Is_Staged,
         Category    => "VCS2");

      Register_Action
        (Kernel, "vcs discard local changes",
         Command     => new Discard_Changes,
         Description =>
            -("Undo all local changes, and revert to the latest commit"
              & " on the current branch"),
         Icon_Name   => "vcs-discard-changes-symbolic",
         Filter      => Lookup_Filter (Kernel, "File"),
         Category    => "VCS2");

      Register_Contextual_Menu
        (Kernel,
         Action      => "vcs stage file",
         Label       => "Version Control/Stage for commit");
      Register_Contextual_Menu
        (Kernel,
         Action      => "vcs unstage file",
         Label       => "Version Control/Unstage from commit");

      Register_Action
        (Kernel, "vcs reload status",
         Description =>
           -("Reload the status of all files from the disk." & ASCII.LF
           & "Use if you have performed operations outside of GPS."),
         Command     => new Reload_Status,
         Category    => "VCS2",
         Icon_Name   => "gps-refresh-symbolic");

   end Register_Module;

end VCS2.Commits;
