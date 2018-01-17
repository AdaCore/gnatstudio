------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2018, AdaCore                  --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Interfaces.C.Strings;
with System;

with Glib;                  use Glib;
with Glib.Object;
with Gtk.Check_Button;
with Gtk.Enums;
with Gtk.Handlers;
with Gtk.Label;
with Gtk.Scrolled_Window;
with Gtk.Toggle_Button;

with GPS.Intl;              use GPS.Intl;
with Histories;

package body CodePeer.Lifeage_Criteria_Editors is

   Class_Record : Glib.Object.Ada_GObject_Class :=
      Glib.Object.Uninitialized_Class;

   Signals : constant Interfaces.C.Strings.chars_ptr_array :=
     (1 => Interfaces.C.Strings.New_String (String (Signal_Criteria_Changed)));

   Signal_Parameters : constant Glib.Object.Signal_Parameter_Types :=
     (1 => (1 => Glib.GType_None));

   procedure On_Show_Added_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Lifeage_Criteria_Editor);
   --  Handles change of state of 'added' check box.

   procedure On_Show_Unchanged_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Lifeage_Criteria_Editor);
   --  Handles change of state of 'unchanged' check box.

   procedure On_Show_Removed_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Lifeage_Criteria_Editor);
   --  Handles change of state of 'removed' check box.

   package Check_Button_Editor_Callbacks is new Gtk.Handlers.User_Callback
     (Gtk.Check_Button.Gtk_Check_Button_Record, Lifeage_Criteria_Editor);

   procedure Emit_By_Name
     (Object : System.Address;
      Name   : Glib.Signal_Name);
   pragma Import (C, Emit_By_Name, "ada_g_signal_emit_by_name");

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor         : out Lifeage_Criteria_Editor;
      Kernel         : GPS.Kernel.Kernel_Handle;
      Title          : String;
      History_Prefix : String) is
   begin
      Editor := new Lifeage_Criteria_Editor_Record;
      Initialize (Editor, Kernel, Title, History_Prefix);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self           : not null access Lifeage_Criteria_Editor_Record'Class;
      Kernel         : GPS.Kernel.Kernel_Handle;
      Title          : String;
      History_Prefix : String)
   is
      Scrolled : Gtk.Scrolled_Window.Gtk_Scrolled_Window;
      Box      : Gtk.Box.Gtk_Vbox;
      Label    : Gtk.Label.Gtk_Label;
      Check    : Gtk.Check_Button.Gtk_Check_Button;

   begin
      Glib.Object.Initialize_Class_Record
        (Ancestor     => Gtk.Box.Get_Vbox_Type,
         Signals      => Signals,
         Class_Record => Class_Record,
         Type_Name    => "CodePeerMessageLifeageCriteriaEditor",
         Parameters   => Signal_Parameters);
      Glib.Object.G_New (Self, Class_Record);

      Self.Kernel := Kernel;
      Self.History_Prefix := To_Unbounded_String (History_Prefix);

      --  Restore settings from histories.

      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all,
         Histories.History_Key (To_String (Self.History_Prefix) & "-added"),
         True);
      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all,
         Histories.History_Key
           (To_String (Self.History_Prefix) & "-unchanged"),
         True);
      Histories.Create_New_Boolean_Key_If_Necessary
        (Kernel.Get_History.all,
         Histories.History_Key (To_String (Self.History_Prefix) & "-removed"),
         False);

      Self.Criteria (Added)     :=
        Histories.Get_History
          (Kernel.Get_History.all,
           Histories.History_Key (To_String (Self.History_Prefix) & "-added"));
      Self.Criteria (Unchanged) :=
        Histories.Get_History
          (Kernel.Get_History.all,
           Histories.History_Key
             (To_String (Self.History_Prefix) & "-unchanged"));
      Self.Criteria (Removed)   := False;

      --  Initialize internal components.

      Gtk.Label.Gtk_New (Label, Title);
      Self.Pack_Start (Label, False);

      Gtk.Scrolled_Window.Gtk_New (Scrolled);
      Scrolled.Set_Policy
        (Gtk.Enums.Policy_Automatic, Gtk.Enums.Policy_Automatic);
      Self.Pack_Start (Scrolled);

      Gtk.Box.Gtk_New_Vbox (Box);
      Scrolled.Add (Box);

      Gtk.Check_Button.Gtk_New (Check, -"added");
      Check.Set_Active (Self.Criteria (Added));
      Box.Pack_Start (Check, False);
      Check_Button_Editor_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Editor_Callbacks.To_Marshaller
           (On_Show_Added_Messages_Toggled'Access),
         Lifeage_Criteria_Editor (Self));

      Gtk.Check_Button.Gtk_New (Check, -"unchanged");
      Check.Set_Active (Self.Criteria (Unchanged));
      Box.Pack_Start (Check, False);
      Check_Button_Editor_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Editor_Callbacks.To_Marshaller
           (On_Show_Unchanged_Messages_Toggled'Access),
         Lifeage_Criteria_Editor (Self));

      Gtk.Check_Button.Gtk_New (Check, -"removed");
      Check.Set_Active (Self.Criteria (Removed));
      Box.Pack_Start (Check, False);
      Check_Button_Editor_Callbacks.Connect
        (Check,
         Gtk.Toggle_Button.Signal_Toggled,
         Check_Button_Editor_Callbacks.To_Marshaller
           (On_Show_Removed_Messages_Toggled'Access),
         Lifeage_Criteria_Editor (Self));
   end Initialize;

   --------------------------
   -- Get_Visible_Lifeages --
   --------------------------

   function Get_Visible_Lifeages
     (Self : access Lifeage_Criteria_Editor_Record'Class)
      return CodePeer.Lifeage_Kinds_Flags is
   begin
      return Self.Criteria;
   end Get_Visible_Lifeages;

   ------------------------------------
   -- On_Show_Added_Messages_Toggled --
   ------------------------------------

   procedure On_Show_Added_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Lifeage_Criteria_Editor) is
   begin
      Self.Criteria (Added) := Object.Get_Active;
      Histories.Set_History
        (Self.Kernel.Get_History.all,
         Histories.History_Key (To_String (Self.History_Prefix) & "-added"),
         Self.Criteria (Added));
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Added_Messages_Toggled;

   --------------------------------------
   -- On_Show_Removed_Messages_Toggled --
   --------------------------------------

   procedure On_Show_Removed_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Lifeage_Criteria_Editor) is
   begin
      Self.Criteria (Removed) := Object.Get_Active;
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Removed_Messages_Toggled;

   ----------------------------------------
   -- On_Show_Unchanged_Messages_Toggled --
   ----------------------------------------

   procedure On_Show_Unchanged_Messages_Toggled
     (Object : access Gtk.Check_Button.Gtk_Check_Button_Record'Class;
      Self   : Lifeage_Criteria_Editor) is
   begin
      Self.Criteria (Unchanged) := Object.Get_Active;
      Histories.Set_History
        (Self.Kernel.Get_History.all,
         Histories.History_Key
           (To_String (Self.History_Prefix) & "-unchanged"),
         Self.Criteria (Unchanged));
      Emit_By_Name (Self.Get_Object, Signal_Criteria_Changed & ASCII.NUL);
   end On_Show_Unchanged_Messages_Toggled;

end CodePeer.Lifeage_Criteria_Editors;
