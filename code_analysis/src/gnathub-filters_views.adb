------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016-2019, AdaCore                   --
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

with Ada.Unchecked_Deallocation;
with GNATCOLL.Traces;                   use GNATCOLL.Traces;

with Glib;                              use Glib;
with Glib.Values;

with Gtk.Box;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Flow_Box;
with Gtk.Menu;
with Gtk.Tree_Model;
with Gtk.Widget;

with Gtkada.Handlers;
with Gtkada.MDI;

with Default_Preferences;              use Default_Preferences;
with Generic_Views;
with GNAThub.Generic_Criteria_Editors;
with GNAThub.Messages;                 use GNAThub.Messages;
with GNAThub.Module;                   use GNAThub.Module;

with GPS.Kernel;                       use GPS.Kernel;
with GPS.Kernel.Hooks;                 use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;
with GPS.Kernel.Preferences;
with GPS.Kernel.Search;
with GPS.Search.GUI;

package body GNAThub.Filters_Views is

   -- Constants --

   Me : constant Trace_Handle := Create ("GNATHUB.FILTER_VIEWS")
     with Unreferenced;

   GNAThub_Module : GNAThub_Module_Id;

   -- Tools --

   procedure Get_Tool_Value
     (Self   : GNAThub.Tool_Access;
      View   : Gtk.Widget.Gtk_Widget;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);

   function Get_History_Name
     (Item : GNAThub.Tool_Record; Dummy_View : Gtk.Widget.Gtk_Widget)
      return String is (Ada.Strings.Unbounded.To_String (Item.Name));

   function Is_Tool_Visible
     (Item : GNAThub.Tool_Access;
      View : Gtk.Widget.Gtk_Widget)
      return Boolean;

   package Tools_Editors is
     new GNAThub.Generic_Criteria_Editors
       (GNAThub.Tool_Record,
        GNAThub.Tool_Access,
        (0 => Glib.GType_String, 1 => Glib.GType_String),
        False,
        Get_Tool_Value,
        Get_History_Name,
        Less,
        Tools_Ordered_Sets,
        Is_Tool_Visible'Access);

   -- Severities --

   procedure Get_Severity_Value
     (Self   : GNAThub.Severity_Access;
      View   : Gtk.Widget.Gtk_Widget;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);

   function Get_History_Name
     (Item : GNAThub.Severity_Record; Dummy_View : Gtk.Widget.Gtk_Widget)
      return String is (Ada.Strings.Unbounded.To_String (Get_Name (Item)));

   function Is_Severity_Visible
     (Item : GNAThub.Severity_Access;
      View : Gtk.Widget.Gtk_Widget)
      return Boolean;

   package Severities_Editors is
     new GNAThub.Generic_Criteria_Editors
       (GNAThub.Severity_Record,
        GNAThub.Severity_Access,
        (0 => Glib.GType_String, 1 => Glib.GType_String),
        False,
        Get_Severity_Value,
        Get_History_Name,
        Less,
        Severities_Ordered_Sets,
        Is_Severity_Visible'Access);

   -- Rules --

   procedure Get_Rule_Value
     (Self   : GNAThub.Rule_Access;
      View   : Gtk.Widget.Gtk_Widget;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);

   function Get_History_Name
     (Item : GNAThub.Rule_Record; Dummy_View : Gtk.Widget.Gtk_Widget)
      return String is (Ada.Strings.Unbounded.To_String (Item.Name));

   function Is_Rule_Visible
     (Item : GNAThub.Rule_Access;
      View : Gtk.Widget.Gtk_Widget)
      return Boolean;

   package Rules_Editors is
     new GNAThub.Generic_Criteria_Editors
       (GNAThub.Rule_Record,
        GNAThub.Rule_Access,
        (0 => Glib.GType_String, 1 => Glib.GType_String),
        False,
        Get_Rule_Value,
        Get_History_Name,
        Less,
        Rule_Sets,
        Is_Rule_Visible'Access);

   ----------------------
   -- Message_Listener --
   ----------------------

   type Message_Listener (View : Generic_Views.Abstract_View_Access) is
     new GPS.Kernel.Messages.Abstract_Listener with null record;
   type Message_Listener_Access is access all Message_Listener'Class;

   overriding procedure Message_Added
     (Self    : not null access Message_Listener;
      Message : not null access GPS.Kernel.Messages.Abstract_Message'Class);

   overriding procedure Message_Removed
     (Self    : not null access Message_Listener;
      Message : not null access GPS.Kernel.Messages.Abstract_Message'Class);

   procedure Free is new Ada.Unchecked_Deallocation
     (Message_Listener'Class, Message_Listener_Access);

   -----------
   --  View --
   -----------

   type Filters_View_Record is new Generic_Views.View_Record with record
      Tools_Editor      : Tools_Editors.Criteria_Editor;
      Severities_Editor : Severities_Editors.Criteria_Editor;
      Rules_Editor      : Rules_Editors.Criteria_Editor;
      Flow_Box          : Gtk.Flow_Box.Gtk_Flow_Box;
      Messages_Listener : Message_Listener_Access;
   end record;

   type Filters_View_Access is access all Filters_View_Record;

   function Initialize
     (Self : access Filters_View_Record'Class)
      return Gtk.Widget.Gtk_Widget;
   --  Create a new explorer, and return the focus widget.

   overriding procedure Create_Menu
     (View : not null access Filters_View_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class);

   type Filters_Child_Record is
     new GPS.Kernel.MDI.GPS_MDI_Child_Record with null record;

   package Views is new Generic_Views.Simple_Views
     (Module_Name        => "gnathub_filters",
      View_Name          => "Filters",
      Formal_View_Record => Filters_View_Record,
      Formal_MDI_Child   => Filters_Child_Record,
      Reuse_If_Exist     => True,
      Local_Config       => True,
      Areas              => Gtkada.MDI.Sides_Only,
      Position           => Gtkada.MDI.Position_Left,
      Initialize         => Initialize);
   use Views;

   procedure On_Destroy (View : access Gtk.Widget.Gtk_Widget_Record'Class);
   --  Called when the view is destroyed

   -- Internal Routines --

   procedure On_Flow_Box_Size_Allocated
     (Self       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Allocation : Gtk.Widget.Gtk_Allocation);

   procedure Apply_Filters
     (View : not null access Filters_View_Record'Class);

   package Filters_Callbacks is
     new Gtk.Handlers.Callback (Filters_View_Record);

   procedure On_Filters_Changed
     (View : access Filters_View_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path);

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      View : Filters_View_Access;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);

   type On_Analysis_Finished is new Simple_Hooks_Function with record
      View : Filters_View_Access;
   end record;
   overriding procedure Execute
      (Self   : On_Analysis_Finished;
       Kernel : not null access Kernel_Handle_Record'Class);

   ---------------------
   -- Search_Provider --
   ---------------------

   package Search_Provider is

      type Provider is abstract
        new GPS.Kernel.Search.Kernel_Search_Provider with private;

      overriding procedure Free (Self : in out Provider);
      procedure Copy_Pattern
        (Self    : not null access Provider;
         Pattern : not null access GPS.Search.Search_Pattern'Class);

      type Severities_Provider is new Provider with private;
      overriding procedure Set_Pattern
        (Self    : not null access Severities_Provider;
         Pattern : not null access GPS.Search.Search_Pattern'Class;
         Limit   : Natural := Natural'Last);
      overriding procedure Next
        (Self     : not null access Severities_Provider;
         Result   : out GPS.Search.Search_Result_Access;
         Has_Next : out Boolean);
      overriding function Display_Name
        (Self : not null access Severities_Provider) return String
      is ("GNATHub severities");
      overriding function Documentation
        (Self : not null access Severities_Provider) return String
      is ("Search for severeties in the GNATHub's filters");

      type Rules_Provider is new Provider with private;
      overriding procedure Set_Pattern
        (Self    : not null access Rules_Provider;
         Pattern : not null access GPS.Search.Search_Pattern'Class;
         Limit   : Natural := Natural'Last);
      overriding procedure Next
        (Self     : not null access Rules_Provider;
         Result   : out GPS.Search.Search_Result_Access;
         Has_Next : out Boolean);
      overriding function Display_Name
        (Self : not null access Rules_Provider) return String
      is ("GNATHub rules");
      overriding function Documentation
        (Self : not null access Rules_Provider) return String
      is ("Search for rules in the GNATHub's filters");

   private

      -- Provider --
      type Provider is abstract new GPS.Kernel.Search.Kernel_Search_Provider
      with record
         Pattern : GPS.Search.Search_Pattern_Access;
         --  Current pattern, do not free.

         Pattern_Needs_Free : Boolean := False;
         --  True if Pattern has been allocated by the provider,
         --  False otherwise.
      end record;

      -- Severities_Provider --
      type Severities_Provider is new Provider with record
         Cursor : GNAThub.Severities_Ordered_Sets.Cursor;
      end record;

      -- Rules_Provider --
      type Rules_Provider is new Provider with record
         Cursor : GNAThub.Rule_Sets.Cursor;
      end record;

   end Search_Provider;

   package body Search_Provider is separate;

   ----------------
   -- Close_View --
   ----------------

   procedure Close_View
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Views.Close (Kernel);
   end Close_View;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View : not null access Filters_View_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (View);
   begin
      GPS.Kernel.Preferences.Append_Menu
        (Menu, GNAThub_Module.Kernel,
         GNAThub.Module.Always_Display_The_Rules);
   end Create_Menu;

   --------------------
   -- Get_Rule_Value --
   --------------------

   procedure Get_Rule_Value
     (Self   : GNAThub.Rule_Access;
      View   : Gtk.Widget.Gtk_Widget;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      pragma Unreferenced (View);
   begin
      if Column = 0 then
         Glib.Values.Init_Set_String
           (Value, Ada.Strings.Unbounded.To_String (Self.Name));

      elsif Column = 1 then
         Glib.Values.Init_Set_String
           (Value, Self.Image);
      else
         Glib.Values.Init (Value, Glib.GType_Invalid);
      end if;
   end Get_Rule_Value;

   ------------------------
   -- Get_Severity_Value --
   ------------------------

   procedure Get_Severity_Value
     (Self   : GNAThub.Severity_Access;
      View   : Gtk.Widget.Gtk_Widget;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      pragma Unreferenced (View);
   begin
      if Column = 0 then
         Glib.Values.Init_Set_String
           (Value, Ada.Strings.Unbounded.To_String (Get_Name (Self.all)));

      elsif Column = 1 then
         Glib.Values.Init_Set_String (Value, Self.Image);

      else
         Glib.Values.Init (Value, Glib.GType_Invalid);
      end if;
   end Get_Severity_Value;

   --------------------
   -- Get_Tool_Value --
   --------------------

   procedure Get_Tool_Value
     (Self   : GNAThub.Tool_Access;
      View   : Gtk.Widget.Gtk_Widget;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      pragma Unreferenced (View);
   begin
      if Column = 0 then
         Glib.Values.Init_Set_String
           (Value, Ada.Strings.Unbounded.To_String (Self.Name));

      elsif Column = 1 then
         Glib.Values.Init_Set_String (Value, Self.Image);

      else
         Glib.Values.Init (Value, Glib.GType_Invalid);
      end if;
   end Get_Tool_Value;

   ---------------------
   -- Is_Tool_Visible --
   ---------------------

   function Is_Tool_Visible
     (Item : GNAThub.Tool_Access;
      View : Gtk.Widget.Gtk_Widget)
      return Boolean
   is
      (True);

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Filters_View_Record'Class)
      return Gtk.Widget.Gtk_Widget is
   begin
      Gtk.Box.Initialize_Vbox (Self, Homogeneous => False);

      Gtk.Flow_Box.Gtk_New (Self.Flow_Box);
      Self.Flow_Box.Set_Selection_Mode (Gtk.Enums.Selection_None);
      Self.Flow_Box.On_Size_Allocate (On_Flow_Box_Size_Allocated'Access);
      Self.Pack_Start (Self.Flow_Box, Expand => True, Fill => True);

      Tools_Editors.Gtk_New
        (Editor         => Self.Tools_Editor,
         Kernel         => Self.Kernel,
         View           => Gtk.Widget.Gtk_Widget (Self),
         Titles         => (0 => To_Unbounded_String ("Tools"),
                            1 => To_Unbounded_String ("Total")),
         History_Prefix => "gnathub-tools",
         Items          => GNAThub_Module.Tools,
         Default        => True);
      Self.Tools_Editor.Set_Name ("gnathub tools editor");
      Self.Flow_Box.Add (Self.Tools_Editor);

      Severities_Editors.Gtk_New
        (Editor         => Self.Severities_Editor,
         Kernel         => Self.Kernel,
         View           => Gtk.Widget.Gtk_Widget (Self),
         Titles         => (0 => To_Unbounded_String ("Importance"),
                            1 => To_Unbounded_String ("Total")),
         History_Prefix => Severity_History_Prefix,
         Items          => GNAThub_Module.Severities,
         Default        => True);
      Self.Severities_Editor.Set_Name ("gnathub severities editor");
      Self.Flow_Box.Add (Self.Severities_Editor);

      Rules_Editors.Gtk_New
        (Editor         => Self.Rules_Editor,
         Kernel         => Self.Kernel,
         View           => Gtk.Widget.Gtk_Widget (Self),
         Titles         => (0 => To_Unbounded_String ("Rules"),
                            1 => To_Unbounded_String ("Total")),
         History_Prefix => "gnathub-rules",
         Items          => GNAThub_Module.Rules,
         Default        => True);
      Self.Rules_Editor.Set_Name ("gnathub rules editor");
      Self.Flow_Box.Add (Self.Rules_Editor);

      -- signals--

      Filters_Callbacks.Object_Connect
        (Self.Tools_Editor,
         Tools_Editors.Signal_Criteria_Changed,
         Filters_Callbacks.To_Marshaller (On_Filters_Changed'Access),
         Self);

      Filters_Callbacks.Object_Connect
        (Self.Severities_Editor,
         Severities_Editors.Signal_Criteria_Changed,
         Filters_Callbacks.To_Marshaller (On_Filters_Changed'Access),
         Self);

      Filters_Callbacks.Object_Connect
        (Self.Rules_Editor,
         Rules_Editors.Signal_Criteria_Changed,
         Filters_Callbacks.To_Marshaller (On_Filters_Changed'Access),
         Self);

      Self.Apply_Filters;

      Self.Messages_Listener := new Message_Listener
        (Generic_Views.Abstract_View_Access (Self));

      GPS.Kernel.Messages.Register_Listener
        (GNAThub_Module.Kernel.Get_Messages_Container,
         GPS.Kernel.Messages.Listener_Access (Self.Messages_Listener),
         GPS.Kernel.Messages.Locations_Only);

      Gtkada.Handlers.Widget_Callback.Connect
        (Self, Gtk.Widget.Signal_Destroy, On_Destroy'Access);

      Preferences_Changed_Hook.Add
        (new On_Pref_Changed'(Preferences_Hooks_Function
         with View => Filters_View_Access (Self)),
         Watch => Self);

      Analysis_Loading_Finsished_Hook.Add
        (new On_Analysis_Finished'(Simple_Hooks_Function
         with View => Filters_View_Access (Self)),
         Watch => Self);

      return Gtk.Widget.Gtk_Widget (Self.Flow_Box);
   end Initialize;

   ---------------------
   -- Is_Rule_Visible --
   ---------------------

   function Is_Rule_Visible
     (Item : GNAThub.Rule_Access;
      View : Gtk.Widget.Gtk_Widget)
      return Boolean
   is
     (Item.Total > 0 or else Always_Display_The_Rules.Get_Pref);

   -------------------------
   -- Is_Severity_Visible --
   -------------------------

   function Is_Severity_Visible
     (Item : GNAThub.Severity_Access;
      View : Gtk.Widget.Gtk_Widget)
      return Boolean
   is
     (Item.Total > 0);

   -------------------
   -- Message_Added --
   -------------------

   overriding procedure Message_Added
     (Self    : not null access Message_Listener;
      Message : not null access GPS.Kernel.Messages.Abstract_Message'Class)
   is
      pragma Unreferenced (Self);
   begin
      if Message.all in GNAThub_Message'Class then
         declare
            View : constant Views.View_Access := Views.Retrieve_View
              (GNAThub_Module.Kernel);
         begin
            GNAThub_Message_Access (Message).Increment_Current_Counters;

            View.Tools_Editor.Update;
            View.Severities_Editor.Update;
            View.Rules_Editor.Update;
         end;
      end if;
   end Message_Added;

   ---------------------
   -- Message_Removed --
   ---------------------

   overriding procedure Message_Removed
     (Self    : not null access Message_Listener;
      Message : not null access GPS.Kernel.Messages.Abstract_Message'Class) is
      pragma Unreferenced (Self);
   begin
      if Message.all in GNAThub_Message'Class then
         declare
            View : constant Views.View_Access := Views.Retrieve_View
              (GNAThub_Module.Kernel);
         begin
            GNAThub_Message_Access (Message).Decrement_Current_Counters;

            View.Tools_Editor.Update;
            View.Severities_Editor.Update;
            View.Rules_Editor.Update;
         end;
      end if;
   end Message_Removed;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (View : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Self : constant Filters_View_Access := Filters_View_Access (View);
   begin
      GPS.Kernel.Messages.Unregister_Listener
        (GNAThub_Module.Kernel.Get_Messages_Container,
         GPS.Kernel.Messages.Listener_Access (Self.Messages_Listener));

      Free (Self.Messages_Listener);
   end On_Destroy;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference)
   is
      pragma Unreferenced (Kernel);
   begin
      if Pref /= null
        and then Pref = Preference (GNAThub.Module.Always_Display_The_Rules)
      then
         Self.View.Rules_Editor.Update;
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Analysis_Finished;
      Kernel : not null access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Self.View.Tools_Editor.Update;
      Self.View.Severities_Editor.Update;
      Self.View.Rules_Editor.Update;
   end Execute;

   -------------------
   -- Apply_Filters --
   -------------------

   procedure Apply_Filters
     (View : not null access Filters_View_Record'Class) is
   begin
      GNAThub_Module.Filter.Fill
        (View.Tools_Editor.Get_Visible_Items,
         View.Severities_Editor.Get_Visible_Items,
         View.Rules_Editor.Get_Visible_Items);
   end Apply_Filters;

   --------------------------------
   -- On_Flow_Box_Size_Allocated --
   --------------------------------

   procedure On_Flow_Box_Size_Allocated
     (Self       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Allocation : Gtk.Widget.Gtk_Allocation)
   is
      Box : constant Gtk.Flow_Box.Gtk_Flow_Box :=
        Gtk.Flow_Box.Gtk_Flow_Box (Self);
   begin
      if Allocation.Height > Allocation.Width then
         if Box.Get_Max_Children_Per_Line > 1 then
            Box.Set_Max_Children_Per_Line (1);
            Box.Check_Resize;
         end if;

      else
         if Box.Get_Max_Children_Per_Line < 3 then
            Box.Set_Max_Children_Per_Line (3);
            Box.Check_Resize;
         end if;
      end if;
   end On_Flow_Box_Size_Allocated;

   ------------------------
   -- On_Filters_Changed --
   ------------------------

   procedure On_Filters_Changed
     (View : access Filters_View_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      pragma Unreferenced (Path);
   begin
      Apply_Filters (View);
   end On_Filters_Changed;

   ---------------
   -- Open_View --
   ---------------

   procedure Open_View
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      Module : not null access GNAThub.Module.GNAThub_Module_Id_Record'Class)
   is
      View : Views.View_Access;
      pragma Unreferenced (View);

   begin
      GNAThub_Module := GNAThub_Module_Id (Module);
      View := Views.Get_Or_Create_View (Kernel);
   end Open_View;

   ------------------------
   -- Set_Tool_Selection --
   ------------------------

   procedure Set_Tool_Selection
     (Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Tool     : not null Tool_Access;
      Selected :  Boolean)
   is
      View : constant Views.View_Access := Views.Retrieve_View (Kernel);
   begin
      if View /= null then
         if Selected then
            View.Tools_Editor.Choose (Tool);
         else
            View.Tools_Editor.Unselect (Tool);
         end if;

         View.Apply_Filters;
      end if;
   end Set_Tool_Selection;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      P : GPS.Kernel.Search.Kernel_Search_Provider_Access;

   begin
      P := new Search_Provider.Severities_Provider;
      GPS.Search.GUI.Register_Provider_And_Action (Kernel, P);
      P := new Search_Provider.Rules_Provider;
      GPS.Search.GUI.Register_Provider_And_Action (Kernel, P);
   end Register_Module;

end GNAThub.Filters_Views;
