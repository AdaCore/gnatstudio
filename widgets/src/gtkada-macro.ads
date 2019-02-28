------------------------------------------------------------------------------
--                  GtkAda - Ada95 binding for Gtk+/Gnome                   --
--                                                                          --
--                     Copyright (C) 2000-2019, AdaCore                     --
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

--  This package provides low level support for event recording and replay.

with Glib;         use Glib;
with Gdk.Device;   use Gdk.Device;
with Gdk.Event;
with Gdk.Types;
with Gtk.Widget;   use Gtk.Widget;
with GNAT.Strings;
with Ada.Text_IO;

package Gtkada.Macro is

   type Identifier_Type is (None, Name, Title, Transient, Label);
   --  The different ways we have to identify a widget.
   --  To make the macros as robust as possible with regards to GUI changes,
   --  we try to associate each event with the closest widget (ie we try to
   --  go up in the widget tree as less as possible).
   --  For this, there are different strategies that can be used to identify
   --  each widget, and this type defines which strategy was used.
   --
   --  - None: No Identifier could be defined for the widget
   --  - Name: The Name of the widget was used. This is the prefered way,
   --          since the name, being hidden, won't have to change. However,
   --          no such name exist by default and the users have to explicitly
   --          set one.
   --  - Title: For top-level windows, it is possible to use the window's
   --          title. Although this will probably change if the application is
   --          translated to another language, it is easy to also translate
   --          the recorded macros.
   --  - Transient: Some top-level windows don't have a title (menus,...). They
   --          might, however, have a transient parent, whose title we could
   --          use to locate the window.
   --  - Label: Some widgets (buttons, menu items,...) generally contains a
   --          single label, whose text we can use to locate the widget.

   type Identifier is record
      Id_Type : Identifier_Type;
      Id      : GNAT.Strings.String_Access;
   end record;
   --  An identifier for a widget

   type File_Buffer is record
      Buffer : GNAT.Strings.String_Access;
      Index  : Natural;
   end record;
   --  Buffer used by Load_Macro procedures.

   ----------------
   -- Macro_Item --
   ----------------

   type Macro_Item;
   type Macro_Item_Access is access all Macro_Item'Class;

   type Macro_Item is abstract tagged record
      Event_Type : Gdk.Event.Gdk_Event_Type;
      Id         : Identifier := (None, null);
      Prev       : Macro_Item_Access;
      Next       : Macro_Item_Access;
      X          : Gint := 0;
      Y          : Gint := 0;
      Time       : Guint32 := 0;
      --  To ease concatenating macro lists, time is a relative time
      --  since the previous item.
   end record;

   function Play_Event
     (Item           : Macro_Item;
      Device         : not null access Gdk.Device.Gdk_Device_Record'Class;
      Default_Widget : Gtk_Widget := null) return Boolean is abstract;
   --  Play the event stored in item.
   --  Default_Widget is the default widget, if any, where the event should be
   --  replayed if it can't be retrieved automatically.
   --  Return True if the event could be replayed, False otherwise.

   procedure Free (Item : in out Macro_Item_Access);
   --  Free the memory associated with an item

   procedure Free_List (List : in out Macro_Item_Access);
   --  Free the memory associated with List and all its siblings.

   function Save_List (Name : String; Item : Macro_Item_Access) return Boolean;
   --  Save the list of macro items in file Name.
   --  Return whether the items could be saved successfully.

   procedure Load_List
     (Buffer  : String;
      Item    : out Macro_Item_Access;
      Success : out Boolean);
   --  Load a list of events from buffer.

   procedure Save_To_Disk (File : Ada.Text_IO.File_Type; Item : Macro_Item);
   --  Saves the item to the disk

   procedure Load_Macro (File : access File_Buffer; Item : out Macro_Item);
   --  Load an item from the disk

   ----------------------
   -- Macro_Item_Mouse --
   ----------------------

   type Macro_Item_Mouse is new Macro_Item with record
      Button : Guint;
      State  : Gdk.Types.Gdk_Modifier_Type;
      X_Root : Gint;
      Y_Root : Gint;
      Window : Gdk.Gdk_Window;
   end record;
   type Macro_Item_Mouse_Access is access all Macro_Item_Mouse'Class;

   function Create_Item
     (Event     : Gdk.Event.Gdk_Event_Button;
      Prev_Time : Guint32 := 0) return Macro_Item_Mouse_Access;
   --  Create a mouse item corresponding to Event.
   --  Prev_Time must be the time of the previous event recorded, if any so
   --  that Item.Time can be computed properly.

   overriding function Play_Event
     (Item           : Macro_Item_Mouse;
      Device         : not null access Gdk.Device.Gdk_Device_Record'Class;
      Default_Widget : Gtk_Widget := null) return Boolean;
   overriding procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type; Item : Macro_Item_Mouse);
   overriding procedure Load_Macro
     (File : access File_Buffer; Item : out Macro_Item_Mouse);

   --------------------
   -- Macro_Item_Key --
   --------------------

   type Macro_Item_Key is new Macro_Item with record
      State            : Gdk.Types.Gdk_Modifier_Type;
      Keyval           : Gdk.Types.Gdk_Key_Type;
      Group            : Guint8;
      Hardware_Keycode : Guint16;
   end record;
   type Macro_Item_Key_Access is access all Macro_Item_Key'Class;

   function Create_Item
     (Event     : Gdk.Event.Gdk_Event_Key;
      Prev_Time : Guint32 := 0) return Macro_Item_Key_Access;

   overriding function Play_Event
     (Item           : Macro_Item_Key;
      Device         : not null access Gdk.Device.Gdk_Device_Record'Class;
      Default_Widget : Gtk_Widget := null) return Boolean;
   overriding procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type;
      Item : Macro_Item_Key);
   overriding procedure Load_Macro
     (File : access File_Buffer;
      Item : out Macro_Item_Key);

   -----------------------
   -- Macro_Item_Motion --
   -----------------------

   type Macro_Item_Motion is new Macro_Item with record
      State  : Gdk.Types.Gdk_Modifier_Type;
   end record;
   type Macro_Item_Motion_Access is access all Macro_Item_Motion'Class;

   function Create_Item
     (Event     : Gdk.Event.Gdk_Event_Motion;
      Prev_Time : Guint32 := 0) return Macro_Item_Motion_Access;

   overriding function Play_Event
     (Item           : Macro_Item_Motion;
      Device         : not null access Gdk.Device.Gdk_Device_Record'Class;
      Default_Widget : Gtk_Widget := null) return Boolean;
   overriding procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type;
      Item : Macro_Item_Motion);
   overriding procedure Load_Macro
     (File : access File_Buffer;
      Item : out Macro_Item_Motion);

   -------------------------
   -- Macro_Item_Crossing --
   -------------------------

   type Macro_Item_Crossing is new Macro_Item with record
      Mode   : Gdk.Event.Gdk_Crossing_Mode;
      Detail : Gdk.Event.Gdk_Notify_Type;
      State  : Gdk.Types.Gdk_Modifier_Type;
   end record;
   type Macro_Item_Crossing_Access is access all Macro_Item_Crossing'Class;

   function Create_Item
     (Event     : Gdk.Event.Gdk_Event_Crossing;
      Prev_Time : Guint32 := 0) return Macro_Item_Crossing_Access;

   overriding function Play_Event
     (Item           : Macro_Item_Crossing;
      Device         : not null access Gdk.Device.Gdk_Device_Record'Class;
      Default_Widget : Gtk_Widget := null) return Boolean;
   overriding procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type;
      Item : Macro_Item_Crossing);
   overriding procedure Load_Macro
     (File : access File_Buffer;
      Item : out Macro_Item_Crossing);

   -----------------------
   -- Macro_Item_Scroll --
   -----------------------

   type Macro_Item_Scroll is new Macro_Item with record
      State     : Gdk.Types.Gdk_Modifier_Type;
      Direction : Gdk.Event.Gdk_Scroll_Direction;
   end record;
   type Macro_Item_Scroll_Access is access all Macro_Item_Scroll'Class;

   function Create_Item
     (Event     : Gdk.Event.Gdk_Event_Scroll;
      Prev_Time : Guint32 := 0) return Macro_Item_Scroll_Access;

   overriding function Play_Event
     (Item           : Macro_Item_Scroll;
      Device         : not null access Gdk.Device.Gdk_Device_Record'Class;
      Default_Widget : Gtk_Widget := null) return Boolean;
   overriding procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type; Item : Macro_Item_Scroll);
   overriding procedure Load_Macro
     (File : access File_Buffer; Item : out Macro_Item_Scroll);

end Gtkada.Macro;
