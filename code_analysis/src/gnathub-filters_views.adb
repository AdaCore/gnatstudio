------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016-2018, AdaCore                   --
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

with Commands;                         use Commands;
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
with GPS.Kernel.Task_Manager;          use GPS.Kernel.Task_Manager;
with GPS.Search.GUI;

package body GNAThub.Filters_Views is

   -- Constants --

   Update_View_Command_ID : constant String := "update_report_filters_task";

   -- Tools --

   procedure Get_Tool_Value
     (Self   : GNAThub.Tool_Access;
      View   : Gtk.Widget.Gtk_Widget;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue);

   function Get_History_Name
     (Item : GNAThub.Tool_Record; Dummy_View : Gtk.Widget.Gtk_Widget)
      return String is (Ada.Strings.Unbounded.To_String (Item.Name));

   package Tools_Editors is
     new GNAThub.Generic_Criteria_Editors
       (GNAThub.Tool_Record,
        GNAThub.Tool_Access,
        (0 => Glib.GType_String, 1 => Glib.GType_String),
        False,
        Get_Tool_Value,
        Get_History_Name,
        Less,
        Tools_Ordered_Sets);

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

   type Update_View_Command_Type is new Root_Command with record
      Apply_Filters : Boolean := False;
   end record;
   type Update_View_Command_Access is access all Update_View_Command_Type;
   --  Commands used to update the view asynchronously.

   overriding function Execute
     (Command : access Update_View_Command_Type) return Command_Return_Type;

   type Filters_View_Record is new Generic_Views.View_Record with record
      On_Update         : Boolean := True;

      Tools_Editor      : Tools_Editors.Criteria_Editor;
      Severities_Editor : Severities_Editors.Criteria_Editor;
      Rules_Editor      : Rules_Editors.Criteria_Editor;

      Flow_Box          : Gtk.Flow_Box.Gtk_Flow_Box;
      Listener          : Message_Listener_Access;

      Update_Report_Cmd : Update_View_Command_Access;
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

   procedure Apply_Filters (View : access Filters_View_Record'Class);
   --  Apply selected rules/severities/tools

   procedure Update_Models (View : access Filters_View_Record'Class);
   --  Update the rules/severities/tools editor models

   -- Internal Routines --

   procedure Update_View_Async
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Apply_Filters : Boolean);
   --  Update the Filters view asynchronously.
   --  If Apply_Filters is True, the message filters will be applied too.

   procedure On_Flow_Box_Size_Allocated
     (Self       : access Gtk.Widget.Gtk_Widget_Record'Class;
      Allocation : Gtk.Widget.Gtk_Allocation);

   package Tools_Callbacks is new Gtk.Handlers.Callback (Filters_View_Record);

   procedure On_Tools_Changed
     (View : access Filters_View_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path);

   package Severities_Callbacks is
     new Gtk.Handlers.Callback (Filters_View_Record);

   procedure On_Severities_Changed
     (View : access Filters_View_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path);

   package Rules_Callbacks is
     new Gtk.Handlers.Callback (Filters_View_Record);

   procedure On_Rules_Changed
     (View : access Filters_View_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path);

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      View : Filters_View_Access;
   end record;

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Default_Preferences.Preference);

   function Img (Item : Natural) return String;
   function Img (Total, Selected : Natural) return String;

   function Is_Selected
     (Item : Tool_Access;
      View : Views.View_Access)
      return Boolean;

   function Is_Selected
     (Item : Severity_Access;
      View : Views.View_Access)
      return Boolean;

   function Count (Item : Rule_Access) return Natural;

   function Count
     (Item     : Rule_Access;
      Severity : Severity_Access)
      return Natural;

   function Is_Severity_Visible
     (Severity : Severity_Access;
      Tool     : Tool_Access)
      return Boolean;

   GNAThub_Module : GNAThub_Module_Id;

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

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Update_View_Command_Type) return Command_Return_Type
   is
      View : constant Views.View_Access := Views.Retrieve_View
        (GNAThub_Module.Kernel);
   begin
      if View /= null then
         View.Update_Models;

         if Command.Apply_Filters then
            View.Apply_Filters;
         end if;
      end if;

      return Success;
   end Execute;

   -------------------
   -- Apply_Filters --
   -------------------

   procedure Apply_Filters (View : access Filters_View_Record'Class)
   is

      procedure Add (From : Counts_Array; To : in out Counts_Array);

      procedure Calculate
        (Messages : Messages_Vectors.Vector;
         Counts   : in out Counts_Array);

      ---------
      -- Add --
      ---------

      procedure Add (From : Counts_Array; To : in out Counts_Array) is
      begin
         for Index in From'Range loop
            To (Index) := To (Index) + From (Index);
         end loop;
      end Add;

      ---------------
      -- Calculate --
      ---------------

      procedure Calculate
        (Messages : Messages_Vectors.Vector;
         Counts   : in out Counts_Array)
      is
         Severity_Id   : Positive;
         Message       : GNAThub_Message_Access;
         Filter_Result : GPS.Kernel.Messages.Filter_Result;
      begin
         for Item of Messages loop
            Message := GNAThub_Message_Access (Item.Message);
            if Message /= null then
               Filter_Result := GNAThub_Module.Filter.Apply (Message.all);
               if not Filter_Result.Non_Applicable
                 and then Filter_Result.Flags (GPS.Kernel.Messages.Locations)
               then
                  Severity_Id := GNAThub_Module.Severities_Id
                    (Message.Get_Severity);
                  Counts (Severity_Id) := Counts (Severity_Id) + 1;
                  Counts (Counts'Last) := Counts (Counts'Last) + 1;
               end if;
            end if;
         end loop;
      end Calculate;

   begin
      GNAThub_Module.Filter.Fill
        (View.Tools_Editor.Get_Visible_Items,
         View.Severities_Editor.Get_Visible_Items,
         View.Rules_Editor.Get_Visible_Items);

      --  Update tree counts

      for Project of GNAThub_Module.Tree.all loop
         GNAThub_Project_Access (Project).Counts := (others => 0);

         for File of GNAThub_Project_Access (Project).Files loop
            GNAThub_File_Access (File).Counts := (others => 0);

            for Subprogram of GNAThub_File_Access (File).Subprograms loop
               GNAThub_Subprogram_Access (Subprogram).Counts := (others => 0);
               Calculate (GNAThub_Subprogram_Access (Subprogram).Messages,
                          GNAThub_Subprogram_Access (Subprogram).Counts);
               Add (GNAThub_Subprogram_Access (Subprogram).Counts,
                    GNAThub_File_Access (File).Counts);
            end loop;
            Calculate (GNAThub_File_Access (File).Messages,
                       GNAThub_File_Access (File).Counts);
            Add (GNAThub_File_Access (File).Counts,
                 GNAThub_Project_Access (Project).Counts);
         end loop;
      end loop;

      GNAThub_Module.Update_Report;

      View.On_Update := False;
   end Apply_Filters;

   -------------------
   -- Update_Models --
   -------------------

   procedure Update_Models (View : access Filters_View_Record'Class) is
   begin
      View.Tools_Editor.Update;
      View.Rules_Editor.Update;
      View.Severities_Editor.Update;
   end Update_Models;

   ----------------
   -- Close_View --
   ----------------

   procedure Close_View
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Views.Close (Kernel);
   end Close_View;

   -----------
   -- Count --
   -----------

   function Count (Item : Rule_Access) return Natural is
      Result : Natural := 0;
   begin
      for Count of Item.Count loop
         Result := Result + Count;
      end loop;

      return Result;
   end Count;

   -----------
   -- Count --
   -----------

   function Count
     (Item     : Rule_Access;
      Severity : Severity_Access)
      return Natural
   is
      C : constant Severity_Natural_Maps.Cursor := Item.Count.Find (Severity);
   begin
      if Severity_Natural_Maps.Has_Element (C) then
         return Severity_Natural_Maps.Element (C);
      else
         return 0;
      end if;
   end Count;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View : not null access Filters_View_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (View);
      Pref : Color_Preference;
   begin
      for Severety of GNAThub_Module.Severities loop
         Pref := Get_Color_Preference (Severety.Style);

         if Pref /= null then
            GPS.Kernel.Preferences.Append_Menu
              (Menu, GNAThub_Module.Kernel, Pref);
         end if;
      end loop;
      GPS.Kernel.Preferences.Append_Menu
        (Menu, GNAThub_Module.Kernel,
         GNAThub.Module.Hide_Node_Without_Messages);
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
      Filters : constant Views.View_Access := Views.View_Access (View);
   begin
      if Column = 0 then
         Glib.Values.Init_Set_String
           (Value, Ada.Strings.Unbounded.To_String (Self.Name));

      elsif Column = 1 then
         declare
            Cursor : Severity_Natural_Maps.Cursor := Self.Count.First;
            Total, Selected, C : Natural := 0;
         begin
            while Severity_Natural_Maps.Has_Element (Cursor) loop
               C     := Severity_Natural_Maps.Element (Cursor);
               Total := Total + C;
               if Is_Selected
                 (Severity_Natural_Maps.Key (Cursor), Filters)
               then
                  Selected := Selected + C;
               end if;

               Severity_Natural_Maps.Next (Cursor);
            end loop;

            Glib.Values.Init_Set_String (Value, Img (Total, Selected));
         end;

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
      Filters : constant Views.View_Access := Views.View_Access (View);
   begin
      if Column = 0 then
         Glib.Values.Init_Set_String
           (Value, Ada.Strings.Unbounded.To_String (Get_Name (Self.all)));

      elsif Column = 1 then
         declare
            Total, Selected, C : Natural := 0;
         begin
            for Rule of GNAThub_Module.Rules loop
               C     := Count (Rule, Self);
               Total := Total + C;
               if Is_Selected (Rule.Tool, Filters) then
                  Selected := Selected + C;
               end if;
            end loop;

            Glib.Values.Init_Set_String (Value, Img (Total, Selected));
         end;

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
         declare
            C : Natural := 0;
         begin
            for Rule of GNAThub_Module.Rules loop
               if Rule.Tool = Self then
                  C := C + Count (Rule);
               end if;
            end loop;

            Glib.Values.Init_Set_String (Value, Img (C));
         end;

      else
         Glib.Values.Init (Value, Glib.GType_Invalid);
      end if;
   end Get_Tool_Value;

   ---------
   -- Img --
   ---------

   function Img (Item : Natural) return String is
      Result : constant String := Item'Img;
   begin
      return Result (Result'First + 1 .. Result'Last);
   end Img;

   ---------
   -- Img --
   ---------

   function Img (Total, Selected : Natural) return String is
   begin
      if Selected = Total then
         return Img (Total);
      else
         return Img (Selected) & " (" & Img (Total) & ")";
      end if;
   end Img;

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
         Title          => "Tools",
         History_Prefix => "gnathub-tools",
         Items          => GNAThub_Module.Tools,
         Default        => True);
      Self.Tools_Editor.Set_Name ("gnathub tools editor");
      Self.Flow_Box.Add (Self.Tools_Editor);

      Severities_Editors.Gtk_New
        (Editor         => Self.Severities_Editor,
         Kernel         => Self.Kernel,
         View           => Gtk.Widget.Gtk_Widget (Self),
         Title          => "Severities",
         History_Prefix => Severity_History_Prefix,
         Items          => GNAThub_Module.Severities,
         Default        => True);
      Self.Severities_Editor.Set_Name ("gnathub severities editor");
      Self.Flow_Box.Add (Self.Severities_Editor);

      Rules_Editors.Gtk_New
        (Editor         => Self.Rules_Editor,
         Kernel         => Self.Kernel,
         View           => Gtk.Widget.Gtk_Widget (Self),
         Title          => "Rules",
         History_Prefix => "gnathub-rules",
         Items          => GNAThub_Module.Rules,
         Default        => True);
      Self.Rules_Editor.Set_Name ("gnathub rules editor");
      Self.Flow_Box.Add (Self.Rules_Editor);

      -- signals--

      Tools_Callbacks.Object_Connect
        (Self.Tools_Editor,
         Tools_Editors.Signal_Criteria_Changed,
         Tools_Callbacks.To_Marshaller (On_Tools_Changed'Access),
         Self);

      Severities_Callbacks.Object_Connect
        (Self.Severities_Editor,
         Severities_Editors.Signal_Criteria_Changed,
         Severities_Callbacks.To_Marshaller (On_Severities_Changed'Access),
         Self);

      Rules_Callbacks.Object_Connect
        (Self.Rules_Editor,
         Rules_Editors.Signal_Criteria_Changed,
         Rules_Callbacks.To_Marshaller (On_Rules_Changed'Access),
         Self);

      Apply_Filters (Views.View_Access (Self));

      Self.Listener := new Message_Listener
        (Generic_Views.Abstract_View_Access (Self));

      GPS.Kernel.Messages.Register_Listener
        (GNAThub_Module.Kernel.Get_Messages_Container,
         GPS.Kernel.Messages.Listener_Access (Self.Listener),
         GPS.Kernel.Messages.Locations_Only);

      Gtkada.Handlers.Widget_Callback.Connect
        (Self, Gtk.Widget.Signal_Destroy, On_Destroy'Access);

      Preferences_Changed_Hook.Add
        (new On_Pref_Changed'(Preferences_Hooks_Function
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
      C       : Severity_Natural_Maps.Cursor := Item.Count.First;
      Filters : constant Views.View_Access   := Views.View_Access (View);
   begin
      if GNAThub.Module.Always_Display_The_Rules.Get_Pref then
         return True;
      end if;

      if Is_Selected (Item.Tool, Filters) then
         while Severity_Natural_Maps.Has_Element (C) loop
            if Severity_Natural_Maps.Element (C) > 0
              and then Is_Selected
                (Severity_Natural_Maps.Key (C), Filters)
            then
               return True;
            end if;
            Severity_Natural_Maps.Next (C);
         end loop;
      end if;

      return False;
   end Is_Rule_Visible;

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected
     (Item : Tool_Access;
      View : Views.View_Access)
      return Boolean is
   begin
      return View.Tools_Editor.Get_Visible_Items.Contains (Item);
   end Is_Selected;

   -----------------
   -- Is_Selected --
   -----------------

   function Is_Selected
     (Item : Severity_Access;
      View : Views.View_Access)
      return Boolean is
   begin
      return View.Severities_Editor.Get_Visible_Items.Contains (Item);
   end Is_Selected;

   -------------------------
   -- Is_Severity_Visible --
   -------------------------

   function Is_Severity_Visible
     (Item : GNAThub.Severity_Access;
      View : Gtk.Widget.Gtk_Widget)
      return Boolean
   is
      Filters : constant Views.View_Access := Views.View_Access (View);
   begin
      for Rule of GNAThub_Module.Rules loop
         if Count (Rule, Item) > 0
           and then Is_Selected (Rule.Tool, Filters)
         then
            return True;
         end if;
      end loop;

      return False;
   end Is_Severity_Visible;

   -------------------------
   -- Is_Severity_Visible --
   -------------------------

   function Is_Severity_Visible
     (Severity : Severity_Access;
      Tool     : Tool_Access)
      return Boolean is
   begin
      for Rule of GNAThub_Module.Rules loop
         if Rule.Tool = Tool
           and then Rule.Count.Contains (Severity)
         then
            return True;
         end if;
      end loop;

      return False;
   end Is_Severity_Visible;

   -----------------------
   -- Update_View_Async --
   -----------------------

   procedure Update_View_Async
     (Kernel        : not null access Kernel_Handle_Record'Class;
      Apply_Filters : Boolean) is
   begin
      if not Has_Queue (Kernel, Update_View_Command_ID) then
         declare
            Command : constant Update_View_Command_Access :=
                        new Update_View_Command_Type;
         begin
            Command.Apply_Filters := Apply_Filters;

            Launch_Background_Command
              (Kernel            => Kernel,
               Command           => Command,
               Active            => False,
               Show_Bar          => True,
               Block_Exit        => False,
               Queue_Id          => Update_View_Command_ID,
               Start_Immediately => False);
         end;
      end if;
   end Update_View_Async;

   -------------------
   -- Message_Added --
   -------------------

   overriding procedure Message_Added
     (Self    : not null access Message_Listener;
      Message : not null access GPS.Kernel.Messages.Abstract_Message'Class)
   is
      View : constant Filters_View_Access := Filters_View_Access (Self.View);
   begin
      if Message.all in GNAThub_Message'Class then
         View.Update_Models;
      end if;
   end Message_Added;

   ---------------------
   -- Message_Removed --
   ---------------------

   overriding procedure Message_Removed
     (Self    : not null access Message_Listener;
      Message : not null access GPS.Kernel.Messages.Abstract_Message'Class)
   is
      View : constant Filters_View_Access := Filters_View_Access (Self.View);
   begin
      if Message.all in GNAThub_Message'Class
        and then Message.Get_Flags (Locations)
      then
         declare
            M        : constant GNAThub_Message_Access :=
                         GNAThub_Message_Access (Message);
            Rule     : constant Rule_Access := M.Get_Rule;
            Severity : constant Severity_Access := M.Get_Severity;
         begin
            --  Decrease the counter of the rule associated to the message
            --  being removed.

            if Rule.Count (Severity) > 0 then
               Rule.Count (Severity) := Rule.Count (Severity) - 1;
            end if;

            --  Update the view asynchronously so that it's updated after
            --  messages have been actually removed (which is not the case
            --  here).

            Update_View_Async
              (View.Kernel,
               Apply_Filters => True);
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
         GPS.Kernel.Messages.Listener_Access (Self.Listener));

      Interrupt_Queue (Self.Kernel, Queue_Id => Update_View_Command_ID);

      Free (Self.Listener);
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

   ----------------------
   -- On_Rules_Changed --
   ----------------------

   procedure On_Rules_Changed
     (View : access Filters_View_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      pragma Unreferenced (Path);
   begin
      if not View.On_Update then
         Apply_Filters (View);
      end if;
   end On_Rules_Changed;

   ---------------------------
   -- On_Severities_Changed --
   ---------------------------

   procedure On_Severities_Changed
     (View : access Filters_View_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      use type Rules_Editors.Criteria_Editor;
      use type Gtk.Tree_Model.Gtk_Tree_Path;

      Update   : constant Boolean := not View.On_Update;
      Severity : Severity_Access;
   begin
      if View.Rules_Editor = null then
         return;
      end if;

      if Update then
         View.On_Update := True;
      end if;

      if Path = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         --  Toggle select/unselect all
         if View.Severities_Editor.Get_Visible_Items.Is_Empty then
            --  Unselect all
            for Rule of GNAThub_Module.Rules loop
               if View.Rules_Editor.Get_Visible_Items.Contains (Rule) then
                  View.Rules_Editor.Unselect (Rule);
               end if;
            end loop;

         else
            --  Select all
            for Rule of GNAThub_Module.Rules loop
               if not View.Rules_Editor.Get_Visible_Items.Contains (Rule) then
                  View.Rules_Editor.Choose (Rule);
               end if;
            end loop;
         end if;

      else
         --  Single Selection
         Severity := View.Severities_Editor.Item_By_Path (Path);

         if View.Severities_Editor.Get_Visible_Items.Contains (Severity) then
            for Rule of GNAThub_Module.Rules loop
               if not View.Rules_Editor.Get_Visible_Items.Contains (Rule)
                 and then Rule.Count.Contains (Severity)
               then
                  View.Rules_Editor.Choose (Rule);
               end if;
            end loop;

         else
            for Rule of GNAThub_Module.Rules loop
               if View.Rules_Editor.Get_Visible_Items.Contains (Rule)
                 and then Rule.Count.Contains (Severity)
                 and then not Is_Rule_Visible
                   (Rule, Gtk.Widget.Gtk_Widget (View))
               then
                  View.Rules_Editor.Unselect (Rule);
               end if;
            end loop;
         end if;
      end if;

      View.Rules_Editor.Update;

      if Update then
         Apply_Filters (View);
      end if;

   exception
      when others =>
         if Update then
            View.On_Update := False;
         end if;
   end On_Severities_Changed;

   ----------------------
   -- On_Tools_Changed --
   ----------------------

   procedure On_Tools_Changed
     (View : access Filters_View_Record'Class;
      Path : Gtk.Tree_Model.Gtk_Tree_Path)
   is
      use type Severities_Editors.Criteria_Editor;
      use type Rules_Editors.Criteria_Editor;
      use type Gtk.Tree_Model.Gtk_Tree_Path;

      Update   : constant Boolean := not View.On_Update;
      Tool     : Tool_Access;
      Selected : Boolean;

   begin
      if View.Rules_Editor = null
        or else View.Severities_Editor = null
      then
         return;
      end if;

      if Update then
         View.On_Update := True;
      end if;

      if Path = Gtk.Tree_Model.Null_Gtk_Tree_Path then
         --  Toggle select/unselect all

         if View.Tools_Editor.Get_Visible_Items.Is_Empty then
            --  Unselect all
            for Rule of GNAThub_Module.Rules loop
               if View.Rules_Editor.Get_Visible_Items.Contains (Rule) then
                  View.Rules_Editor.Unselect (Rule);
               end if;
            end loop;

         else
            --  Select all
            for Rule of GNAThub_Module.Rules loop
               if not View.Rules_Editor.Get_Visible_Items.Contains (Rule) then
                  View.Rules_Editor.Choose (Rule);
               end if;
            end loop;
         end if;

      else
         --  Single selection
         Tool     := View.Tools_Editor.Item_By_Path (Path);
         Selected := View.Tools_Editor.Get_Visible_Items.Contains (Tool);

         if Selected then
            for Rule of GNAThub_Module.Rules loop
               if Rule.Tool = Tool
                 and then not View.Rules_Editor.
                   Get_Visible_Items.Contains (Rule)
               then
                  View.Rules_Editor.Choose (Rule);
               end if;
            end loop;

         else
            for Rule of GNAThub_Module.Rules loop
               if Rule.Tool = Tool
                 and then View.Rules_Editor.Get_Visible_Items.Contains (Rule)
               then
                  View.Rules_Editor.Unselect (Rule);
               end if;
            end loop;
         end if;
      end if;

      if Tool = null then
         --  Toggle select/unselect
         if View.Tools_Editor.Get_Visible_Items.Is_Empty then
            --  Unselect all
            for Severity of GNAThub_Module.Severities loop
               if View.Severities_Editor.
                 Get_Visible_Items.Contains (Severity)
               then
                  View.Severities_Editor.Unselect (Severity);
               end if;
            end loop;
         else
            --  Select all
            for Severity of GNAThub_Module.Severities loop
               if not View.Severities_Editor.
                 Get_Visible_Items.Contains (Severity)
               then
                  View.Severities_Editor.Choose (Severity);
               end if;
            end loop;
         end if;

      else
         --  Single selection
         if Selected then
            for Severity of GNAThub_Module.Severities loop
               if not View.Severities_Editor.
                 Get_Visible_Items.Contains (Severity)
                 and then Is_Severity_Visible (Severity, Tool)
               then
                  View.Severities_Editor.Choose (Severity);
               end if;
            end loop;

         else
            for Severity of GNAThub_Module.Severities loop
               if View.Severities_Editor.
                 Get_Visible_Items.Contains (Severity)
                 and then not Is_Severity_Visible
                   (Severity, Gtk.Widget.Gtk_Widget (View))
               then
                  View.Severities_Editor.Unselect (Severity);
               end if;
            end loop;
         end if;
      end if;

      View.Severities_Editor.Update;
      View.Rules_Editor.Update;

      if Update then
         Apply_Filters (View);
      end if;

   exception
      when others =>
         if Update then
            View.On_Update := False;
         end if;
   end On_Tools_Changed;

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
