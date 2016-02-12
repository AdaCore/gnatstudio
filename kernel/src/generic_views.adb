------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2016, AdaCore                     --
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

with Ada.Calendar;            use Ada.Calendar;

with Glib.Main;               use Glib.Main;
with Glib.Object;             use Glib, Glib.Object;
with XML_Utils;               use XML_Utils;
with Gtk.Editable;
with Gdk.Event;               use Gdk.Event;
with Gdk.Rectangle;           use Gdk.Rectangle;
with Gdk.Screen;              use Gdk.Screen;
with Gdk.Window;              use Gdk.Window;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Button;              use Gtk.Button;
with Gtk.Check_Menu_Item;     use Gtk.Check_Menu_Item;
with Gtk.Dialog;              use Gtk.Dialog;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Event_Box;           use Gtk.Event_Box;
with Gtk.Image;               use Gtk.Image;
with Gtk.GEntry;              use Gtk.GEntry;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Style_Context;       use Gtk.Style_Context;
with Gtk.Radio_Menu_Item;     use Gtk.Radio_Menu_Item;
with Gtk.Separator_Menu_Item; use Gtk.Separator_Menu_Item;
with Gtk.Separator_Tool_Item; use Gtk.Separator_Tool_Item;
with Gtk.Tool_Item;           use Gtk.Tool_Item;
with Gtk.Toolbar;             use Gtk.Toolbar;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;
with Gtkada.Handlers;         use Gtkada.Handlers;
with Gtkada.Search_Entry;     use Gtkada.Search_Entry;
with Gtkada.MDI;              use Gtkada.MDI;

with Ada.Tags;                  use Ada.Tags;
with Commands.Interactive;      use Commands, Commands.Interactive;
with GNAT.Strings;              use GNAT.Strings;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with GNATCOLL.Utils;            use GNATCOLL.Utils;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;        use GPS.Kernel.Actions;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Modules.UI;     use GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Intl;                  use GPS.Intl;
with GPS.Search;                use GPS.Search;
with GPS.Stock_Icons;           use GPS.Stock_Icons;
with GUI_Utils;                 use GUI_Utils;
with Histories;                 use Histories;
with System;

with Config;    use Config;

package body Generic_Views is
   Me : constant Trace_Handle := Create ("Views");

   function Has_Toolbar_Separator
     (Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class)
      return Gint;
   --  Return the index of the separator that right aligns items, or -1 if
   --  there is none.

   function Report_Filter_Changed_Idle
     (View : Abstract_View_Access) return Boolean;
   procedure Report_Filter_Changed (View : access GObject_Record'Class);
   --  Report a change in the filter panel. This is done in an idle so that
   --  if the user types fast we do not refresh too much.

   procedure Get_Filter_Preferred_Width
     (Widget       : System.Address;
      Minimum_Size : out Glib.Gint;
      Natural_Size : out Glib.Gint);
   pragma Convention (C, Get_Filter_Preferred_Width);

   package View_Sources is new Glib.Main.Generic_Sources
     (Abstract_View_Access);

   Filter_Class_Record : Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;

   procedure On_Pattern_Config_Menu
     (Self  : access GObject_Record'Class;
      Pos   : Gtk_Entry_Icon_Position;
      Event : Gdk_Event_Button);
   --  Creates the popup menu to configure the filter settings.

   procedure On_Destroy_Filter (Self : access Gtk_Widget_Record'Class);
   --  Called when a filter panel is destroyed

   function Build_Filter_Pattern
     (Self : not null access Filter_Panel_Record'Class)
      return Search_Pattern_Access;
   --  Build the search pattern corresponding to the filter

   function On_Filter_Focus_Out
     (Filter : access GObject_Record'Class;
      Event  : Gdk_Event_Focus) return Boolean;
   --  Called when the focus leaves the filter field, to update the history.

   procedure Update_Recent_Entries
     (Panel : not null access Filter_Panel_Record'Class;
      Add   : access Search_Pattern'Class := null);
   --  If Add is specified, add it to the search pattern to the history.
   --  In all cases, update the menu to show all recent entries.

   procedure Filter_Panel_Class_Init (Self : GObject_Class);
   pragma Convention (C, Filter_Panel_Class_Init);
   --  Initialize the gtk+ class

   type Recent_Entry_Item_Record is new Gtk_Menu_Item_Record with record
      Pattern    : String_Access;
      Kind       : Search_Kind;
      Invert     : Boolean;
      Whole_Word : Boolean;
      Panel      : access Filter_Panel_Record;
   end record;
   type Recent_Entry_Item is access all Recent_Entry_Item_Record'Class;

   procedure On_Destroy_Recent_Item (Self : access Gtk_Widget_Record'Class);
   --  Called hwne an Recent_Entry_Item is destroyed

   procedure On_Recent_Item_Activate
     (Item : access Gtk_Menu_Item_Record'Class);
   --  Called when selecting a past search string

   ----------------------------
   -- On_Destroy_Recent_Item --
   ----------------------------

   procedure On_Destroy_Recent_Item (Self : access Gtk_Widget_Record'Class) is
      Item : constant Recent_Entry_Item := Recent_Entry_Item (Self);
   begin
      Free (Item.Pattern);
   end On_Destroy_Recent_Item;

   -----------------------
   -- On_Destroy_Filter --
   -----------------------

   procedure On_Destroy_Filter (Self : access Gtk_Widget_Record'Class) is
      Filter : constant Filter_Panel := Filter_Panel (Self);
   begin
      if Filter.Pattern_Config_Menu /= null then
         Unref (Filter.Pattern_Config_Menu);
      end if;

      Free (Filter.History_Prefix);

      if Filter.Timeout /= Glib.Main.No_Source_Id then
         Glib.Main.Remove (Filter.Timeout);
         Filter.Timeout := Glib.Main.No_Source_Id;
      end if;
   end On_Destroy_Filter;

   ----------------------------
   -- On_Pattern_Config_Menu --
   ----------------------------

   procedure On_Pattern_Config_Menu
      (Self  : access GObject_Record'Class;
       Pos   : Gtk_Entry_Icon_Position;
       Event : Gdk_Event_Button)
   is
      pragma Unreferenced (Pos);  --  unreliable with gtk+ 3.8
      View : constant Abstract_View_Access := Abstract_View_Access (Self);

      procedure Func
        (Menu    : not null access Gtk_Menu_Record'Class;
         X, Y    : out Gint;
         Push_In : out Boolean);
      procedure Func
        (Menu    : not null access Gtk_Menu_Record'Class;
         X, Y    : out Gint;
         Push_In : out Boolean)
      is
         pragma Unreferenced (Menu);
      begin
         X := Gint (Event.X_Root);
         Y := Gint (Event.Y_Root);
         Push_In := True;
      end Func;

   begin
      if View.Filter.Pattern.Get_Icon_Position (Event) =
        Gtk_Entry_Icon_Primary
      then
         View.Filter.Pattern_Config_Menu.Show_All;
         View.Filter.Pattern_Config_Menu.Popup
           (Func => Func'Unrestricted_Access);
      end if;
   end On_Pattern_Config_Menu;

   --------------------------------
   -- Get_Filter_Preferred_Width --
   --------------------------------

   procedure Get_Filter_Preferred_Width
     (Widget       : System.Address;
      Minimum_Size : out Glib.Gint;
      Natural_Size : out Glib.Gint)
   is
      pragma Unreferenced (Widget);
   begin
      Minimum_Size := 30;
      Natural_Size := 150; --  should ask widget;
   end Get_Filter_Preferred_Width;

   ----------------
   -- Set_Kernel --
   ----------------

   procedure Set_Kernel
     (View   : not null access View_Record'Class;
      Kernel : not null access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      View.Kernel := Kernel_Handle (Kernel);
   end Set_Kernel;

   --------------------------
   -- Build_Filter_Pattern --
   --------------------------

   function Build_Filter_Pattern
     (Self : not null access Filter_Panel_Record'Class)
      return Search_Pattern_Access
   is
      Regexp      : constant Boolean :=
        Self.Regexp /= null and then Self.Regexp.Get_Active;
      Approximate : constant Boolean :=
        Self.Approximate /= null and then Self.Approximate.Get_Active;
      Fuzzy       : constant Boolean :=
        Self.Fuzzy /= null and then Self.Fuzzy.Get_Active;
      Negate      : constant Boolean :=
        Self.Negate /= null and then Self.Negate.Get_Active;
      Whole       : constant Boolean :=
        Self.Whole_Word /= null and then Self.Whole_Word.Get_Active;
      Text        : constant String := Self.Pattern.Get_Text;
      Kind        : constant Search_Kind :=
        (if Regexp then
            GPS.Search.Regexp
         elsif Approximate then
            GPS.Search.Approximate
         elsif Fuzzy then
            GPS.Search.Fuzzy
         else
            GPS.Search.Full_Text);

   begin
      if Text /= "" then
         if Starts_With (Text, "not:") then
            return Build
              (Pattern         => Text (Text'First + 4 .. Text'Last),
               Case_Sensitive  => False,
               Whole_Word      => Whole,
               Negate          => True,  --  force
               Kind            => Kind,
               Allow_Highlight => False);
         else
            return Build
              (Pattern         => Text,
               Case_Sensitive  => False,
               Whole_Word      => Whole,
               Negate          => Negate,
               Kind            => Kind,
               Allow_Highlight => False);
         end if;
      end if;
      return null;
   end Build_Filter_Pattern;

   --------------------------------
   -- Report_Filter_Changed_Idle --
   --------------------------------

   function Report_Filter_Changed_Idle
     (View : Abstract_View_Access) return Boolean
   is
      Pattern : Search_Pattern_Access := Build_Filter_Pattern (View.Filter);
   begin
      View.Filter_Changed (Pattern);  --  Pattern freed by Filter_Changed
      View.Filter.Timeout := Glib.Main.No_Source_Id;
      return False;
   end Report_Filter_Changed_Idle;

   ---------------------------
   -- Report_Filter_Changed --
   ---------------------------

   procedure Report_Filter_Changed (View : access GObject_Record'Class) is
      V : constant Abstract_View_Access := Abstract_View_Access (View);
   begin
      if V.Filter /= null
        and then V.Filter.Timeout = Glib.Main.No_Source_Id
      then
         V.Filter.Timeout := View_Sources.Idle_Add
           (Report_Filter_Changed_Idle'Access, Data => V);
      end if;
   end Report_Filter_Changed;

   -----------------------------
   -- Filter_Panel_Class_Init --
   -----------------------------

   procedure Filter_Panel_Class_Init (Self : GObject_Class) is
   begin
      Set_Default_Get_Preferred_Width_Handler
        (Self, Get_Filter_Preferred_Width'Access);
   end Filter_Panel_Class_Init;

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
     (Self : not null access View_Record;
      Text : String)
   is
   begin
      if Self.Filter /= null then
         Self.Filter.Pattern.Set_Text (Text);
      end if;
   end Set_Filter;

   ---------------------------
   -- Update_Recent_Entries --
   ---------------------------

   procedure Update_Recent_Entries
     (Panel : not null access Filter_Panel_Record'Class;
      Add   : access Search_Pattern'Class := null)
   is
      Key : constant History_Key :=
        History_Key (Panel.History_Prefix.all & "-filter-recent");
      Item : Recent_Entry_Item;
      List : GNAT.Strings.String_List_Access;
      Prefix : Character;

      function Is_Recent_Entry
        (W : access Gtk_Widget_Record'Class) return Boolean
        is (W.all in Recent_Entry_Item_Record'Class);
      --  Whether W is a menu item for a recent search

   begin
      --  Create the history if necessary

      Create_New_Key_If_Necessary
        (Hist     => Panel.Kernel.Get_History.all,
         Key      => Key,
         Key_Type => Strings);
      Set_Max_Length
        (Hist     => Panel.Kernel.Get_History.all,
         Key      => Key,
         Num      => 5);

      --  Remove all existing menu entries

      Remove_All_Children
        (Panel.Pattern_Config_Menu,
         Filter => Is_Recent_Entry'Unrestricted_Access);

      --  Add to history if necessary

      if Add /= null then
         case Add.Get_Kind is
            when Full_Text   => Prefix := 'f';
            when Regexp      => Prefix := 'r';
            when Fuzzy       => Prefix := 'y';
            when Approximate => Prefix := 'a';
         end case;

         Add_To_History
           (Hist  => Panel.Kernel.Get_History.all,
            Key   => Key,
            New_Entry =>
              Prefix
              & (if Add.Get_Negate then '-' else '+')
              & (if Add.Get_Whole_Word then 'w' else ' ')
              & Add.Get_Text);
      end if;

      --  Add menu entries for each previous search

      List := Get_History (Panel.Kernel.Get_History.all, Key);
      if List /= null then
         --  Add a separator
         Item := new Recent_Entry_Item_Record;
         Gtk.Menu_Item.Initialize (Item);
         Panel.Pattern_Config_Menu.Append (Item);

         for L in List'Range loop
            declare
               V : constant String := List (L).all;
            begin
               Item := new Recent_Entry_Item_Record;

               case V (V'First) is
                  when 'r' => Item.Kind := Regexp;
                  when 'y' => Item.Kind := Fuzzy;
                  when 'a' => Item.Kind := Approximate;
                  when others => Item.Kind := Full_Text;
               end case;

               Item.Invert     := V (V'First + 1) = '-';
               Item.Whole_Word := V (V'First + 2) = 'w';
               Item.Pattern    := new String'(V (V'First + 3 .. V'Last));
               Item.Panel      := Panel;

               Gtk.Menu_Item.Initialize (Item, Item.Pattern.all);
               Item.On_Destroy (On_Destroy_Recent_Item'Access);
               Item.On_Activate (On_Recent_Item_Activate'Access);
               Panel.Pattern_Config_Menu.Append (Item);
            end;
         end loop;
      end if;
   end Update_Recent_Entries;

   -----------------------------
   -- On_Recent_Item_Activate --
   -----------------------------

   procedure On_Recent_Item_Activate
     (Item : access Gtk_Menu_Item_Record'Class)
   is
      Self : constant Recent_Entry_Item := Recent_Entry_Item (Item);
   begin
      Self.Panel.Pattern.Set_Text (Self.Pattern.all);
      if Self.Panel.Whole_Word /= null then
         Self.Panel.Whole_Word.Set_Active (Self.Whole_Word);
      end if;
      if Self.Panel.Negate /= null then
         Self.Panel.Negate.Set_Active (Self.Invert);
      end if;
      if Self.Panel.Full_Text /= null then
         Self.Panel.Full_Text.Set_Active (Self.Kind = Full_Text);
      end if;
      if Self.Panel.Regexp /= null then
         Self.Panel.Regexp.Set_Active (Self.Kind = Regexp);
      end if;
      if Self.Panel.Fuzzy /= null then
         Self.Panel.Fuzzy.Set_Active (Self.Kind = Fuzzy);
      end if;
      if Self.Panel.Approximate /= null then
         Self.Panel.Approximate.Set_Active (Self.Kind = Approximate);
      end if;
   end On_Recent_Item_Activate;

   -------------------------
   -- On_Filter_Focus_Out --
   -------------------------

   function On_Filter_Focus_Out
     (Filter : access GObject_Record'Class;
      Event  : Gdk_Event_Focus) return Boolean
   is
      pragma Unreferenced (Event);
      F : constant Filter_Panel := Filter_Panel (Filter);
      Pattern : Search_Pattern_Access := Build_Filter_Pattern (F);
   begin
      if Pattern /= null then
         Update_Recent_Entries (F, Pattern);
         Free (Pattern);
      end if;
      return False;  --  propagate event
   end On_Filter_Focus_Out;

   ------------------
   -- Build_Filter --
   ------------------

   procedure Build_Filter
     (Self        : not null access View_Record;
      Toolbar     : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class;
      Hist_Prefix : History_Key;
      Tooltip     : String := "";
      Placeholder : String := "";
      Options     : Filter_Options_Mask := 0)
   is
      F   : Filter_Panel;
      Sep : Gtk_Separator_Menu_Item;
   begin
      if Self.Filter /= null then
         return;
      end if;

      --  The filter will be automatically deallocated by gtk+, because it is
      --  put in the toolbar of the view, which is destroyed when the view is
      --  destroyed.

      Self.Filter := new Filter_Panel_Record;
      F := Self.Filter;
      F.History_Prefix := new String'(String (Hist_Prefix));
      F.Kernel := Self.Kernel;

      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Tool_Item.Get_Type,
         Class_Record => Filter_Class_Record,
         Type_Name    => "FilterPanel",
         Class_Init   => Filter_Panel_Class_Init'Access);

      G_New (F, Filter_Class_Record.The_Type);
      Self.Append_Toolbar (Toolbar, F, Is_Filter => True);
      F.Set_Expand (True);
      F.Set_Homogeneous (False);

      Self.Filter.On_Destroy (On_Destroy_Filter'Access);

      Gtk_New (F.Pattern, Placeholder => Placeholder);
      Set_Font_And_Colors (F.Pattern, Fixed_Font => True);
      Object_Callback.Object_Connect
        (F.Pattern, Gtk.Editable.Signal_Changed,
         Report_Filter_Changed'Access, Self);
      F.Pattern.On_Focus_Out_Event (On_Filter_Focus_Out'Access, F);
      F.Add (F.Pattern);

      F.Pattern.Set_Tooltip_Markup
        (Tooltip & ASCII.LF
         & "Start with <b>not:</b> to invert the filter");

      if Options /= 0 then
         F.Pattern.Set_Icon_From_Icon_Name
           (Gtk_Entry_Icon_Primary, "gps-search-and-menu-symbolic");
         F.Pattern.Set_Icon_Activatable (Gtk_Entry_Icon_Primary, True);
         F.Pattern.On_Icon_Release (On_Pattern_Config_Menu'Access, Self);

         Gtk_New (F.Pattern_Config_Menu);
         Ref (F.Pattern_Config_Menu);  --  unref'ed in On_Destroy

         Gtk_New (F.Full_Text, Widget_SList.Null_List, -"Full text match");
         F.Full_Text.On_Toggled (Report_Filter_Changed'Access, Self);
         F.Pattern_Config_Menu.Add (F.Full_Text);

         if (Options and Has_Regexp) /= 0 then
            Gtk_New (F.Regexp, Label => -"Regular Expression",
                     Group => F.Full_Text.Get_Group);
            Associate (Get_History (Self.Kernel).all,
                       Hist_Prefix & "-filter-is-regexp",
                       F.Regexp, Default => False);
            F.Regexp.Set_Tooltip_Text
              (-"Whether filter is a regular expression");
            F.Regexp.On_Toggled (Report_Filter_Changed'Access, Self);
            F.Pattern_Config_Menu.Add (F.Regexp);
         end if;

         if (Options and Has_Approximate) /= 0 then
            Gtk_New (F.Approximate, Label => -"Approximate matching",
                     Group => F.Full_Text.Get_Group);
            Associate (Get_History (Self.Kernel).all,
                       Hist_Prefix & "-filter-approximate",
                       F.Approximate, Default => False);
            F.Approximate.Set_Tooltip_Text
              (-"Matching allows some errors (e.g. extra or missing text)");
            F.Approximate.On_Toggled (Report_Filter_Changed'Access, Self);
            F.Pattern_Config_Menu.Add (F.Approximate);
         end if;

         if (Options and Has_Fuzzy) /= 0 then
            Gtk_New (F.Fuzzy, Label => -"Fuzzy matching",
                     Group => F.Full_Text.Get_Group);
            Associate (Get_History (Self.Kernel).all,
                       Hist_Prefix & "-filter-fuzzy",
                       F.Fuzzy, Default => False);
            F.Fuzzy.Set_Tooltip_Text (-"Matching allows missing characters");
            F.Fuzzy.On_Toggled (Report_Filter_Changed'Access, Self);
            F.Pattern_Config_Menu.Add (F.Fuzzy);
         end if;

         Gtk_New (Sep);
         F.Pattern_Config_Menu.Add (Sep);

         if (Options and Has_Negate) /= 0 then
            Gtk_New (F.Negate, -"Invert filter");
            Associate (Get_History (Self.Kernel).all,
                       Hist_Prefix & "-filter-negate",
                       F.Negate, Default => False);
            F.Negate.Set_Tooltip_Text
              (-"invert filter : hide matching items");
            F.Negate.On_Toggled (Report_Filter_Changed'Access, Self);
            F.Pattern_Config_Menu.Add (F.Negate);
         end if;

         if (Options and Has_Whole_Word) /= 0 then
            Gtk_New (F.Whole_Word, -"Whole word");
            Associate (Get_History (Self.Kernel).all,
                       Hist_Prefix & "-filter-whole-word",
                       F.Whole_Word, Default => False);
            F.Whole_Word.Set_Tooltip_Text (-"Match whole words only");
            F.Whole_Word.On_Toggled (Report_Filter_Changed'Access, Self);
            F.Pattern_Config_Menu.Add (F.Whole_Word);
         end if;

         Update_Recent_Entries (F);
      end if;
   end Build_Filter;

   ------------
   -- Kernel --
   ------------

   function Kernel
     (Self : not null access View_Record'Class)
      return GPS.Kernel.Kernel_Handle is
   begin
      return Self.Kernel;
   end Kernel;

   ---------------------------
   -- Has_Toolbar_Separator --
   ---------------------------

   function Has_Toolbar_Separator
     (Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class)
      return Gint
   is
      Count : constant Gint := Toolbar.Get_N_Items;
      Item  : Gtk_Tool_Item;
   begin
      for J in reverse 0 .. Count - 1 loop
         Item := Toolbar.Get_Nth_Item (J);
         if Item.all in Gtk_Separator_Tool_Item_Record'Class
           and then Gtk_Separator_Tool_Item (Item).Get_Expand
         then
            return J;
         end if;
      end loop;
      return -1;
   end Has_Toolbar_Separator;

   --------------------
   -- Append_Toolbar --
   --------------------

   procedure Append_Toolbar
     (Self        : not null access View_Record;
      Toolbar     : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class;
      Item        : not null access Gtk.Tool_Item.Gtk_Tool_Item_Record'Class;
      Is_Filter   : Boolean := False;
      Homogeneous : Boolean := True)
   is
      pragma Unreferenced (Self);
      Sep : Gtk_Separator_Tool_Item;
      Loc : Gint;
   begin
      if Is_Filter then
         if Has_Toolbar_Separator (Toolbar) = -1 then
            Gtk_New (Sep);
            Sep.Set_Draw (False);
            Sep.Set_Expand (True);
            Toolbar.Insert (Sep);
         end if;

         Toolbar.Insert (Item);

      else
         Loc := Has_Toolbar_Separator (Toolbar);
         if Loc /= -1 then
            --  Insert before the item, to left align
            Toolbar.Insert (Item, Pos => Loc - 1);
         else
            Toolbar.Insert (Item, Pos => -1);
         end if;
      end if;

      Item.Set_Homogeneous (Homogeneous);
   end Append_Toolbar;

   ------------------
   -- Simple_Views --
   ------------------

   package body Simple_Views is
      Module : Module_ID;

      Window_X_Hist_Key  : constant History_Key :=
        "window_x_" & History_Key (View_Name);
      Window_Y_Hist_Key  : constant History_Key :=
        "window_y_" & History_Key (View_Name);

      type Local_Formal_MDI_Child_Access
         is access all Local_Formal_MDI_Child'Class;

      type Open_Command is new Interactive_Command with null record;
      overriding function Execute
        (Self    : access Open_Command;
         Context : Interactive_Command_Context) return Command_Return_Type;
      --  Open the view

      type Toplevel_Box is new Gtk_Box_Record with record
         Initial : View_Access;
      end record;
      --  When using a local toolbar, the contents of the widget as set by the
      --  application is nested inside a box. We use a dedicated tagged type so
      --  that we can more easily find the child in the desktop by tag.

      procedure Create_If_Needed
        (Kernel       : access Kernel_Handle_Record'Class;
         Child        : out GPS_MDI_Child;
         View         : out View_Access);
      --  Create or reuse a view.

      procedure Find
        (Kernel       : access Kernel_Handle_Record'Class;
         Child        : out GPS_MDI_Child;
         View         : out View_Access);
      --  Find any existing view

      procedure Store_Position (View : View_Access);
      procedure Restore_Position (View : View_Access);
      --  Store and restore the position of the view's dialog.

      ----------------------
      -- Restore_Position --
      ----------------------

      procedure Restore_Position (View : View_Access) is
         X, Y    : Gint;
         Win     : constant Gtk_Widget := View.Get_Toplevel;
         Hist_X  : constant String_List_Access := Get_History
           (Get_History (View.Kernel).all, Window_X_Hist_Key);
         Hist_Y  : constant String_List_Access := Get_History
           (Get_History (View.Kernel).all, Window_Y_Hist_Key);

         Screen : Gdk_Screen;
         Monitor : Gint;
         Rect    : Gdk_Rectangle;
      begin
         if Hist_X = null or else Hist_Y = null then
            return;
         end if;

         --  Ensure the window is at least partially visible on the current
         --  screen.
         --  Screen.Get_{Width,Height} returns the total size for all monitors
         --    for instance 5760x1200
         --  So we need to look at the specific monitor that the window is on.

         Screen := Gtk_Window (Win).Get_Screen;
         Monitor := Screen.Get_Monitor_At_Window (Win.Get_Window);
         Screen.Get_Monitor_Geometry (Monitor, Rect);

         X := Gint'Value (Hist_X (Hist_X'First).all);
         X := Gint'Min (Gint'Max (X, Rect.X), Rect.X + Rect.Width - 10);

         Y := Gint'Value (Hist_Y (Hist_Y'First).all);
         Y := Gint'Min (Gint'Max (Y, Rect.Y), Rect.Y + Rect.Height - 10);

         Move (Gtk_Window (Win), X, Y);
      end Restore_Position;

      --------------------
      -- Store_Position --
      --------------------

      procedure Store_Position (View : View_Access) is
         Child   : constant MDI_Child := Child_From_View (View);
         Win  : Gtk_Widget;
         X, Y : Gint;
      begin
         if Child.Is_Floating then
            --  Store the position of the floating window

            Win := View.Get_Toplevel;
            Get_Root_Origin (Get_Window (Win), X, Y);

            Add_To_History
              (Get_History (View.Kernel).all, Window_X_Hist_Key,
               Gint'Image (X));
            Add_To_History
              (Get_History (View.Kernel).all, Window_Y_Hist_Key,
               Gint'Image (Y));
         end if;
      end Store_Position;

      ---------------------
      -- On_Delete_Event --
      ---------------------

      function On_Delete_Event
        (Box : access Gtk.Widget.Gtk_Widget_Record'Class) return Boolean
      is
         Event : Gdk_Event;
         Prevent_Delete : Boolean;
      begin
         Gdk_New (Event, Delete);
         Event.Any.Window := Toplevel_Box (Box.all).Initial.Get_Window;
         Prevent_Delete := Return_Callback.Emit_By_Name
           (Toplevel_Box (Box.all).Initial,
            Gtk.Widget.Signal_Delete_Event,
            Event);

         Event.Any.Window := null;
         Free (Event);
         return Prevent_Delete;
      end On_Delete_Event;

      -----------------------------
      -- On_Display_Local_Config --
      -----------------------------

      function On_Display_Local_Config
        (View  : access Gtk_Widget_Record'Class;
         Event : Gdk.Event.Gdk_Event) return Boolean
      is
         V : constant View_Access := View_Access (View);
         Menu : Gtk_Menu;
         Time_Before_Factory : Time;
      begin
         if Get_Button (Event) /= 1
           or else Get_Event_Type (Event) /= Button_Press
         then
            return False;
         end if;

         if Host = Windows then
            Time_Before_Factory := Clock;
         end if;

         Gtk_New (Menu);
         V.Create_Menu (Menu);
         View.Grab_Focus;
         Menu.Show_All;

         --  See comments in GUI_Utils.Button_Press_For_Contextual_Menu

         if Host = Windows then
            Popup (Menu,
                   Button        => Gdk.Event.Get_Button (Event),
                   Activate_Time => Gdk.Event.Get_Time (Event)
                   + Guint32 ((Clock - Time_Before_Factory) * 1000));
         else
            Popup (Menu,
                   Button        => Gdk.Event.Get_Button (Event),
                   Activate_Time => Gdk.Event.Get_Time (Event));
         end if;

         return True;
      end On_Display_Local_Config;

      -----------
      -- Close --
      -----------

      procedure Close
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
      is
         View : constant View_Access := Retrieve_View (Kernel);

      begin
         if View /= null then
            Close_Child (Child_From_View (View));
         end if;
      end Close;

      ---------------------
      -- Child_From_View --
      ---------------------

      function Child_From_View
        (View : not null access Formal_View_Record'Class)
         return access Local_Formal_MDI_Child'Class
      is
      begin
         if Local_Config or else Local_Toolbar then
            return Access_Local_Formal_MDI_Child (Find_MDI_Child
              (Get_MDI (View.Kernel),
               View.Get_Parent));  --  the box
         else
            return Access_Local_Formal_MDI_Child
              (Find_MDI_Child (Get_MDI (View.Kernel), View));
         end if;
      end Child_From_View;

      -----------------------
      -- Get_Actual_Widget --
      -----------------------

      overriding function Get_Actual_Widget
        (Self : not null access Local_Formal_MDI_Child) return Gtk_Widget
      is
      begin
         return Gtk_Widget (View_From_Widget (Get_Widget (Self)));
      end Get_Actual_Widget;

      ----------------------
      -- View_From_Widget --
      ----------------------

      function View_From_Widget
        (Widget : not null access Glib.Object.GObject_Record'Class)
         return View_Access
      is
      begin
         if Local_Toolbar or else Local_Config then
            return Toplevel_Box (Widget.all).Initial;
         else
            return View_Access (Widget);
         end if;
      end View_From_Widget;

      ----------
      -- Find --
      ----------

      procedure Find
        (Kernel       : access Kernel_Handle_Record'Class;
         Child        : out GPS_MDI_Child;
         View         : out View_Access)
      is
         T   : Ada.Tags.Tag;
         MDI : MDI_Window;
      begin
         if Local_Toolbar or else Local_Config then
            T := Toplevel_Box'Tag;
         else
            T := Formal_View_Record'Tag;
         end if;

         MDI := Get_MDI (Kernel);
         View := null;

         if MDI /= null then
            Child := GPS_MDI_Child (MDI.Find_MDI_Child_By_Tag (T));
            if Child /= null then
               View := View_From_Widget (Child.Get_Widget);
            end if;
         end if;
      end Find;

      -----------------------------
      -- On_Close_Floating_Child --
      -----------------------------

      procedure On_Close_Floating_Child
        (Self : access Gtk_Widget_Record'Class)
      is
         View : constant View_Access := View_Access (Self);
         C    : constant MDI_Child := Child_From_View (View);
      begin
         if Local_Toolbar or else Local_Config then
            --  If we have Toplevel_Box query child for delete

            if On_Delete_Event (View.Get_Parent) then
               return;  --  Сhild rejected delete query
            end if;
         end if;

         Store_Position (View);
         C.Close_Child (Force => True);

         --  Give the focus back to the main Window, since this is not always
         --  done by the window manager (e.g. under Windows)

         Gdk.Window.Gdk_Raise (View.Kernel.Get_Main_Window.Get_Window);
      end On_Close_Floating_Child;

      --------------------
      -- On_Float_Child --
      --------------------

      procedure On_Float_Child (Child : access Gtk_Widget_Record'Class) is
         Self   : constant Local_Formal_MDI_Child_Access :=
           Local_Formal_MDI_Child_Access (Child);
         View   : constant View_Access := View_From_Widget (Self.Get_Widget);
         Close_Button : Gtk_Button;
         Req : Gtk_Requisition;
      begin
         if Self.Is_Floating then
            Gtk_Dialog (View.Get_Toplevel).Set_Resizable (True);

            --  Add the "Close" button.
            Close_Button := Gtk_Button
              (Gtk_Dialog (View.Get_Toplevel).Add_Button
                   (-"Close", Gtk_Response_Cancel));

            Widget_Callback.Object_Connect
              (Close_Button, Gtk.Button.Signal_Clicked,
               On_Close_Floating_Child_Access, View);

            --  Set the position of the floating window
            Restore_Position (View);
            View.Set_Size_Request (-1, -1);
            Size_Request (View, Req);
            View.Set_Size_Request (Req.Width, Req.Height);
         else
            --  Store the position of the floating window
            Store_Position (View);
            View.Set_Size_Request (-1, -1);
         end if;
      end On_Float_Child;

      ----------------------
      -- Create_If_Needed --
      ----------------------

      procedure Create_If_Needed
        (Kernel       : access Kernel_Handle_Record'Class;
         Child        : out GPS_MDI_Child;
         View         : out View_Access)
      is
         Focus_Widget : Gtk_Widget;
         Toolbar      : Gtk_Toolbar;
         Box          : Gtk_Box;
         W            : Gtk_Widget;
         Item         : Gtk_Tool_Item;
         Image        : Gtk_Image;
         Event_Box    : Gtk_Event_Box;

      begin
         if Reuse_If_Exist then
            Find (Kernel, Child, View);
            if View /= null then
               return;
            end if;
         end if;

         View := new Formal_View_Record;
         Set_Kernel (View, Kernel_Handle (Kernel));
         Focus_Widget := Initialize (View);
         if Focus_Widget = null then
            Focus_Widget := Gtk_Widget (View);
         end if;

         if Local_Toolbar or else Local_Config then
            Box := new Toplevel_Box;
            Initialize_Vbox (Box);
            Toplevel_Box (Box.all).Initial := View;

            Toolbar := Create_Toolbar (Kernel, Id => View_Name);
            Toolbar.Set_Icon_Size (Icon_Size_Local_Toolbar);
            Toolbar.Set_Style (Toolbar_Icons);
            Get_Style_Context (Toolbar).Add_Class ("gps-local-toolbar");
            Box.Pack_Start (Toolbar, Expand => False, Fill => False);

            Box.Pack_Start (View, Expand => True, Fill => True);
            W := Gtk_Widget (Box);
            View.Create_Toolbar (Toolbar);
            Toolbar.Show_All;

            --  We need to propagate the delete event to the view
            Return_Callback.Connect
              (Box, Gtk.Widget.Signal_Delete_Event, On_Delete_Event_Access);

         else
            W := Gtk_Widget (View);
         end if;

         if Local_Config then
            Gtk_New (Item);
            Gtk_New (Event_Box);
            Item.Add (Event_Box);
            Gtk_New_From_Icon_Name
              (Image, "gps-config-menu-symbolic", Icon_Size_Menu);
            Event_Box.Add (Image);
            Event_Box.Set_Name ("local-config");
            Item.Set_Homogeneous (False);
            Item.Set_Tooltip_Text (-"Configure this panel");
            View.Append_Toolbar (Toolbar, Item, Is_Filter => True);

            Add_Events
              (Event_Box,
               Button_Press_Mask or Button_Release_Mask or Key_Press_Mask);

            Gtkada.Handlers.Return_Callback.Object_Connect
              (Event_Box, Signal_Button_Press_Event,
               Gtkada.Handlers.Return_Callback.Event_Marshaller.To_Marshaller
                 (On_Display_Local_Config_Access), View);
            Item.Show_All;
         end if;

         --  A simple check that the widget can indeed get the keyboard focus.
         --  If it can't, this might result in surprising behavior: for
         --  instance, clicking on a MDI tab will send a "child_selected"
         --  event with no module information set, and the current focus
         --  widget will not have changed, thus the menus are not correctly
         --  refreshed.

         Assert
           (Me,
            Focus_Widget = null or else Focus_Widget.Get_Can_Focus,
            "Focus_Widget cannot in fact receive keyboard focus",
            Raise_Exception => False);

         --  Child does not exist yet, create it
         Child := new Local_Formal_MDI_Child;
         Initialize (Child, W,
                     Kernel         => Kernel,
                     Default_Width  => Default_Width,
                     Default_Height => Default_Height,
                     Focus_Widget   => Focus_Widget,
                     Flags          => MDI_Flags,
                     Module         => Module,
                     Group          => Group,
                     Areas          => Areas);
         Set_Title (Child, View_Name, View_Name);

         if Add_Close_Button_On_Float then
            Widget_Callback.Connect
              (Child, Signal_Float_Child, On_Float_Child_Access);
            Widget_Callback.Connect
              (Child, Signal_Unfloat_Child, On_Float_Child_Access);
         end if;

         Put (Get_MDI (Kernel), Child, Initial_Position => Position);
      end Create_If_Needed;

      ----------------
      -- Get_Module --
      ----------------

      function Get_Module return GPS.Kernel.Modules.Module_ID is
      begin
         return Module;
      end Get_Module;

      ------------------
      -- Load_Desktop --
      ------------------

      function Load_Desktop
        (MDI  : MDI_Window;
         Node : Node_Ptr;
         User : Kernel_Handle) return MDI_Child
      is
         pragma Unreferenced (MDI);
         View         : View_Access;
         Child        : GPS_MDI_Child;
      begin
         if Node.Tag.all = Module_Name then
            Create_If_Needed (User, Child, View);
            Load_From_XML (View, Node);
            return MDI_Child (Child);
         end if;
         return null;
      end Load_Desktop;

      ------------------
      -- Save_Desktop --
      ------------------

      overriding function Save_Desktop
        (Self : not null access Local_Formal_MDI_Child) return Node_Ptr
      is
         Widget : constant Gtk_Widget := Get_Widget (Self);
         N : Node_Ptr;
         Tb : constant Boolean := Local_Toolbar or else Local_Config;
      begin
         --  Test inherited first, in case the user has provided his own. This
         --  is also needed when a module is written in python, since
         --  python_module.adb in that case uses its own mdi_child to call the
         --  python function

         N := Formal_MDI_Child (Self.all).Save_Desktop;
         if N /= null then
            return N;
         end if;

         if Tb and then Widget.all in Toplevel_Box'Class then
            N := new Node;
            N.Tag := new String'(Module_Name);
            Save_To_XML (Toplevel_Box (Widget.all).Initial, N);
            return N;

         elsif not Tb
           and then Widget.all in Formal_View_Record'Class
         then
            N := new Node;
            N.Tag := new String'(Module_Name);
            Save_To_XML (View_Access (Widget), N);
            return N;
         end if;
         return null;
      end Save_Desktop;

      -------------
      -- Execute --
      -------------

      overriding function Execute
        (Self    : access Open_Command;
         Context : Interactive_Command_Context) return Command_Return_Type
      is
         Ignore : View_Access;
         pragma Unreferenced (Self, Ignore);
      begin
         Ignore := Get_Or_Create_View (Get_Kernel (Context.Context));
         return Commands.Success;
      end Execute;

      -------------------
      -- Retrieve_View --
      -------------------

      function Retrieve_View
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
         return View_Access
      is
         Child        : GPS_MDI_Child;
         View         : View_Access;
      begin
         Find (Kernel, Child, View);
         return View;
      end Retrieve_View;

      ------------------------
      -- Get_Or_Create_View --
      ------------------------

      function Get_Or_Create_View
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
         Focus  : Boolean := True)
         return View_Access
      is
         Child        : GPS_MDI_Child;
         View         : View_Access;
      begin
         Create_If_Needed (Kernel, Child, View);

         if Focus then
            Raise_Child (Child);
            Set_Focus_Child (Child);

            --  ??? browsers used to do the following:

            --  Add_Navigation_Location (Kernel, -"Call graph Browser");
         end if;

         if Child = null then
            return null;
         else
            return View;
         end if;
      end Get_Or_Create_View;

      ---------------------
      -- Register_Module --
      ---------------------

      procedure Register_Module
        (Kernel      : access GPS.Kernel.Kernel_Handle_Record'Class;
         ID          : GPS.Kernel.Modules.Module_ID := null) is
      begin
         if ID = null then
            Module := new Module_ID_Record;
         else
            Module := ID;
         end if;

         if Commands_Category /= "" then
            Register_Action
              (Kernel, "open " & View_Name,
               new Open_Command, "Open (or reuse if it already exists) the '"
               & View_Name & "' view", null, Commands_Category);
         end if;

         Register_Module
           (Module      => Module,
            Kernel      => Kernel,
            Module_Name => Module_Name,
            Priority    => GPS.Kernel.Modules.Default_Priority);
         Register_Desktop_Functions (null, Load_Desktop_Access);
      end Register_Module;
   end Simple_Views;

end Generic_Views;
