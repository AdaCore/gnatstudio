------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

with GNAT.Strings;             use GNAT.Strings;
with Interfaces.C.Strings;
with System;

with Glib.Object;              use Glib.Object;

with Gtk.Editable;
with Gdk.Event;                use Gdk.Event;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Menu_Item;            use Gtk.Menu_Item;
with Gtk.Separator_Menu_Item;  use Gtk.Separator_Menu_Item;

with Gtkada.Handlers;          use Gtkada.Handlers;

with GNATCOLL.Utils;           use GNATCOLL.Utils;

with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GUI_Utils;                use GUI_Utils;

package body Filter_Panels is

   Filter_Class_Record : Glib.Object.Ada_GObject_Class :=
     Glib.Object.Uninitialized_Class;

   Signals : constant Interfaces.C.Strings.chars_ptr_array :=
     (1 => Interfaces.C.Strings.New_String (String (Signal_Filter_Changed)));

   Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
     (1 => (1 => Glib.GType_None));

   procedure Filter_Panel_Class_Init (Self : GObject_Class);
   pragma Convention (C, Filter_Panel_Class_Init);
   --  Initialize the gtk+ class

   procedure Get_Filter_Preferred_Width
     (Widget       : System.Address;
      Minimum_Size : out Glib.Gint;
      Natural_Size : out Glib.Gint);
   pragma Convention (C, Get_Filter_Preferred_Width);

   procedure Report_Filter_Changed
     (Object : access GObject_Record'Class);

   package Filter_Sources is new Glib.Main.Generic_Sources (Filter_Panel);

   procedure On_Destroy_Filter (Self : access GObject_Record'Class);
   --  Called when a filter panel is destroyed

   procedure On_Pattern_Config_Menu
     (Self  : access GObject_Record'Class;
      Pos   : Gtk_Entry_Icon_Position;
      Event : Gdk_Event_Button);
   --  Creates the popup menu to configure the filter settings.

   function On_Filter_Focus_Out
     (Filter : access GObject_Record'Class;
      Event  : Gdk_Event_Focus) return Boolean;
   --  Called when the focus leaves the filter field, to update the history.

   type Recent_Entry_Item_Record is new Gtk_Menu_Item_Record with record
      Pattern    : String_Access;
      Kind       : Search_Kind;
      Invert     : Boolean;
      Whole_Word : Boolean;
      Panel      : access Filter_Panels.Filter_Panel_Record;
   end record;
   type Recent_Entry_Item is access all Recent_Entry_Item_Record'Class;

   procedure On_Destroy_Recent_Item (Self : access Gtk_Widget_Record'Class);
   --  Called hwne an Recent_Entry_Item is destroyed

   procedure On_Recent_Item_Activate
     (Item : access Gtk_Menu_Item_Record'Class);
   --  Called when selecting a past search string

   -----------------------------
   -- Filter_Panel_Class_Init --
   -----------------------------

   procedure Filter_Panel_Class_Init (Self : GObject_Class) is
   begin
      Set_Default_Get_Preferred_Width_Handler
        (Self, Get_Filter_Preferred_Width'Access);
   end Filter_Panel_Class_Init;

   ------------------------
   -- Get_Filter_Pattern --
   ------------------------

   function Get_Filter_Pattern
     (Self : not null access Filter_Panel_Record'Class)
      return Search_Pattern_Access
   is
      use Ada.Strings.Unbounded;
   begin
      if Self.Data_Pattern /= Null_Unbounded_String then
         return Build
           (Pattern         => To_String (Self.Data_Pattern),
            Case_Sensitive  => False,
            Whole_Word      => Self.Data_Whole_Word,
            Negate          => Self.Data_Negate,
            Kind            => Self.Data_Kind,
            Allow_Highlight => False);
      end if;
      return null;
   end Get_Filter_Pattern;

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

   ----------------------
   -- Get_Focus_Widget --
   ----------------------

   function Get_Focus_Widget
     (Self : not null access Filter_Panel_Record'Class)
      return Gtk_Widget is
   begin
      return Gtk_Widget (Self.Pattern);
   end Get_Focus_Widget;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Panel       : out Filter_Panel;
      Kernel      : GPS.Kernel.Kernel_Handle;
      Hist_Prefix : Histories.History_Key;
      Tooltip     : String := "";
      Placeholder : String := "";
      Options     : Filter_Options_Mask := 0;
      Name        : String := "") is
   begin
      Panel := new Filter_Panel_Record;
      Initialize
        (Panel, Kernel, Hist_Prefix, Tooltip, Placeholder, Options, Name);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self        : not null access Filter_Panel_Record'Class;
      Kernel      : GPS.Kernel.Kernel_Handle;
      Hist_Prefix : Histories.History_Key;
      Tooltip     : String := "";
      Placeholder : String := "";
      Options     : Filter_Options_Mask := 0;
      Name        : String := "")
   is
      Sep : Gtk_Separator_Menu_Item;
   begin
      Self.History_Prefix := new String'(String (Hist_Prefix));
      Self.Kernel         := Kernel;
      Self.Options        := Options;

      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Tool_Item.Get_Type,
         Class_Record => Filter_Class_Record,
         Type_Name    => "FilterPanel",
         Class_Init   => Filter_Panel_Class_Init'Access,
         Signals      => Signals,
         Parameters   => Signal_Parameters);

      G_New (Self, Filter_Class_Record.The_Type);
      Self.Set_Expand (True);
      Self.Set_Homogeneous (False);

      Self.On_Destroy (On_Destroy_Filter'Access, Self);

      Gtk_New (Self.Pattern, Placeholder => Placeholder);
      Set_Font_And_Colors (Self.Pattern, Fixed_Font => True);
      if Name /= "" then
         Self.Pattern.Set_Name (Name);
      end if;

      if (Options and Debounce) /= 0 then
         Object_Callback.Object_Connect
           (Self.Pattern, Gtk.GEntry.Signal_Activate,
            Report_Filter_Changed'Access, Self);
      else
         Object_Callback.Object_Connect
           (Self.Pattern, Gtk.Editable.Signal_Changed,
            Report_Filter_Changed'Access, Self);
      end if;

      Self.Pattern.On_Focus_Out_Event (On_Filter_Focus_Out'Access, Self);
      Self.Add (Self.Pattern);

      --  Ensure that Get_Can_Focus returns True, even if the
      --  filter entry has not been realized and/or mapped yet.
      --  This is safe because we know that Gtk_Entry widgets can
      --  receive the focus.
      Self.Pattern.Set_Can_Focus (True);

      Self.Pattern.Set_Tooltip_Markup
        (Tooltip
         & (if (Options and Has_Negate) /= 0
           then ASCII.LF & "Start with <b>not:</b> to invert the filter"
           else "")
         & (if (Options and Debounce) /= 0
           then ASCII.LF & "Press enter to apply the filter"
           else ""));

      if Options /= 0 then
         Self.Pattern.Set_Icon_From_Icon_Name
           (Gtk_Entry_Icon_Primary, "gps-search-and-menu-symbolic");
         Self.Pattern.Set_Icon_Activatable (Gtk_Entry_Icon_Primary, True);
         Self.Pattern.On_Icon_Release (On_Pattern_Config_Menu'Access, Self);

         Gtk_New (Self.Pattern_Config_Menu);
         Ref (Self.Pattern_Config_Menu);  --  unref'ed in On_Destroy

         Gtk_New
           (Self.Full_Text, Widget_SList.Null_List, Get_Label (Full_Text));
         Self.Full_Text.On_Toggled (Report_Filter_Changed'Access, Self);
         Self.Pattern_Config_Menu.Add (Self.Full_Text);

         if (Options and Has_Regexp) /= 0 then
            Gtk_New (Self.Regexp, Label => Get_Label (Regexp),
                     Group => Self.Full_Text.Get_Group);
            Associate (Get_History (Self.Kernel).all,
                       Hist_Prefix & "-filter-is-regexp",
                       Self.Regexp, Default => False);
            Self.Regexp.Set_Tooltip_Text
              ("Whether filter is a regular expression");
            Self.Regexp.On_Toggled (Report_Filter_Changed'Access, Self);
            Self.Pattern_Config_Menu.Add (Self.Regexp);
         end if;

         if (Options and Has_Approximate) /= 0 then
            Gtk_New (Self.Approximate, Label => Get_Label (Approximate),
                     Group => Self.Full_Text.Get_Group);
            Associate (Get_History (Self.Kernel).all,
                       Hist_Prefix & "-filter-approximate",
                       Self.Approximate, Default => False);
            Self.Approximate.Set_Tooltip_Text
              ("Matching allows some errors (e.g. extra or missing text)");
            Self.Approximate.On_Toggled (Report_Filter_Changed'Access, Self);
            Self.Pattern_Config_Menu.Add (Self.Approximate);
         end if;

         if (Options and Has_Fuzzy) /= 0 then
            Gtk_New (Self.Fuzzy, Label => Get_Label (Fuzzy),
                     Group => Self.Full_Text.Get_Group);
            Associate (Get_History (Self.Kernel).all,
                       Hist_Prefix & "-filter-fuzzy",
                       Self.Fuzzy, Default => False);
            Self.Fuzzy.Set_Tooltip_Text ("Matching allows missing characters");
            Self.Fuzzy.On_Toggled (Report_Filter_Changed'Access, Self);
            Self.Pattern_Config_Menu.Add (Self.Fuzzy);
         end if;

         Gtk_New (Sep);
         Self.Pattern_Config_Menu.Add (Sep);

         if (Options and Has_Negate) /= 0 then
            Gtk_New (Self.Negate, "Invert filter");
            Associate (Get_History (Self.Kernel).all,
                       Hist_Prefix & "-filter-negate",
                       Self.Negate, Default => False);
            Self.Negate.Set_Tooltip_Text
              ("invert filter : hide matching items");
            Self.Negate.On_Toggled (Report_Filter_Changed'Access, Self);
            Self.Pattern_Config_Menu.Add (Self.Negate);
         end if;

         if (Options and Has_Whole_Word) /= 0 then
            Gtk_New (Self.Whole_Word, "Whole word");
            Associate (Get_History (Self.Kernel).all,
                       Hist_Prefix & "-filter-whole-word",
                       Self.Whole_Word, Default => False);
            Self.Whole_Word.Set_Tooltip_Text ("Match whole words only");
            Self.Whole_Word.On_Toggled (Report_Filter_Changed'Access, Self);
            Self.Pattern_Config_Menu.Add (Self.Whole_Word);
         end if;

         Self.Update_Recent_Entries;
      end if;
   end Initialize;

   -----------------------
   -- On_Destroy_Filter --
   -----------------------

   procedure On_Destroy_Filter (Self : access GObject_Record'Class) is
      use Glib.Main;

      Panel : constant Filter_Panel := Filter_Panel (Self);
   begin
      if Panel.Pattern_Config_Menu /= null then
         Unref (Panel.Pattern_Config_Menu);
      end if;

      Free (Panel.History_Prefix);

      if Panel.Timeout /= Glib.Main.No_Source_Id then
         Glib.Main.Remove (Panel.Timeout);
         Panel.Timeout := Glib.Main.No_Source_Id;
      end if;
   end On_Destroy_Filter;

   ----------------------------
   -- On_Destroy_Recent_Item --
   ----------------------------

   procedure On_Destroy_Recent_Item (Self : access Gtk_Widget_Record'Class) is
      Item : constant Recent_Entry_Item := Recent_Entry_Item (Self);
   begin
      Free (Item.Pattern);
   end On_Destroy_Recent_Item;

   -------------------------
   -- On_Filter_Focus_Out --
   -------------------------

   function On_Filter_Focus_Out
     (Filter : access GObject_Record'Class;
      Event  : Gdk_Event_Focus) return Boolean
   is
      pragma Unreferenced (Event);
      F       : constant Filter_Panel := Filter_Panel (Filter);
      Pattern : Search_Pattern_Access;
   begin
      F.Store_Filter_Data;
      Pattern := F.Get_Filter_Pattern;

      if Pattern /= null then
         Update_Recent_Entries (F, Pattern);
         Free (Pattern);
      end if;
      return False;  --  propagate event
   end On_Filter_Focus_Out;

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

   ----------------------------
   -- On_Pattern_Config_Menu --
   ----------------------------

   procedure On_Pattern_Config_Menu
      (Self  : access GObject_Record'Class;
       Pos   : Gtk_Entry_Icon_Position;
       Event : Gdk_Event_Button)
   is
      pragma Unreferenced (Pos);  --  unreliable with gtk+ 3.8
      use Glib;

      Panel : constant Filter_Panel := Filter_Panel (Self);

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
      if Panel.Pattern.Get_Icon_Position (Event) =
        Gtk_Entry_Icon_Primary
      then
         Panel.Pattern_Config_Menu.Show_All;
         Panel.Pattern_Config_Menu.Popup (Func => Func'Unrestricted_Access);
      end if;
   end On_Pattern_Config_Menu;

   ---------------------------
   -- Report_Filter_Changed --
   ---------------------------

   procedure Report_Filter_Changed
     (Object : access GObject_Record'Class)
   is
      use Glib.Main;

      Self : constant Filter_Panel := Filter_Panel (Object);

   begin
      if Self.Timeout = Glib.Main.No_Source_Id then
         Self.Timeout := Filter_Sources.Idle_Add
           (Report_Filter_Changed_Idle'Access, Data => Self);
      end if;
   end Report_Filter_Changed;

   --------------------------------
   -- Report_Filter_Changed_Idle --
   --------------------------------

   function Report_Filter_Changed_Idle
     (Self : Filter_Panel) return Boolean is
   begin
      Self.Store_Filter_Data;

      Widget_Callback.Emit_By_Name (Self, Signal_Filter_Changed);

      Self.Timeout := Glib.Main.No_Source_Id;
      return False;
   end Report_Filter_Changed_Idle;

   ----------------
   -- Set_Filter --
   ----------------

   procedure Set_Filter
     (Self : not null access Filter_Panel_Record;
      Text : String) is
   begin
      Self.Pattern.Set_Text (Text);
      if (Self.Options and Debounce) /= 0 then
         Report_Filter_Changed (Self);
      end if;
   end Set_Filter;

   -----------------------
   -- Store_Filter_Data --
   -----------------------

   procedure Store_Filter_Data
     (Self : not null access Filter_Panel_Record'Class)
   is
      use Ada.Strings.Unbounded;

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
         Self.Data_Whole_Word := Whole;
         Self.Data_Kind       := Kind;

         if Starts_With (Text, "not:") then
            Self.Data_Pattern := To_Unbounded_String
              (Text (Text'First + 4 .. Text'Last));
            Self.Data_Negate := True;

         else
            Self.Data_Pattern := To_Unbounded_String (Text);
            Self.Data_Negate  := Negate;
         end if;
      else
         Self.Data_Pattern := Null_Unbounded_String;
      end if;
   end Store_Filter_Data;

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

end Filter_Panels;
