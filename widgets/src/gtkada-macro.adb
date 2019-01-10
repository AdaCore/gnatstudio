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

with System;
with Glib.Object;       use Glib.Object;
with Gdk.Window;

with Gdk;               use Gdk;
with Gdk.Event;         use Gdk.Event;
with Gdk.Types;         use Gdk.Types;
with Gdk.Types.Keysyms; use Gdk.Types.Keysyms;

with Gtk.Bin;           use Gtk.Bin;
with Gtk.Container;     use Gtk.Container;
with Gtk.Label;         use Gtk.Label;
with Gtk.Main;          use Gtk.Main;
with Gtk.Menu;          use Gtk.Menu;
with Gtk.Menu_Item;     use Gtk.Menu_Item;
with Gtk.Window;        use Gtk.Window;

with GNAT.OS_Lib;       use GNAT.OS_Lib;
with Ada.Text_IO;       use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with Ada.Unchecked_Conversion;

package body Gtkada.Macro is

   use Gdk;

   -----------------
   -- Local Types --
   -----------------

   type Address_Integer is mod System.Memory_Size;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function To_Window is new
     Ada.Unchecked_Conversion (Address_Integer, Gdk_Window);
   function To_Integer is new
     Ada.Unchecked_Conversion (Gdk_Window, Address_Integer);

   function Find_Widget (Item : Macro_Item'Class) return Gtk_Widget;
   --  Find the widget associated with Item.

   procedure Find_Named_Parent
     (Widget    : access Gtk_Widget_Record'Class;
      Parent    : out Gtk_Widget;
      Parent_Id : out Identifier);
   --  Returns the first widget in Widget's hierarchy that has a name.
   --  It might be Widget itself.
   --  Parent is null if there was no named parent.

   function Get_Id (Widget : access Gtk_Widget_Record'Class) return Identifier;
   --  Return an identifier that can be used for the widget.
   --  return value.Id is left to null if no specific identifier could be
   --  found.

   procedure Move_Pointer (X_Root, Y_Root : Gint) is null;
   --  Move mouse pointer to specified absolute location.
   --  ??? This function needs to wrap to XWarpPointer on X11 platforms.
   --  pragma Import (C, Move_Pointer, "ada_gdk_move_pointer");

   function Get_Focus_Widget return Gtk_Widget;
   --  Return the current top level widget having the focus

   Invalid_Line : exception;

   function Load_Line
     (File     : access File_Buffer;
      Name     : String;
      Optional : Boolean := False) return String;
   --  Read the next line in the file, check that the item name is Name,
   --  and return the value (i.e after ":=" ).
   --  Raise Invalid_Line if the item is incorrect.
   --  If Optional is True, then "" is returned if the current line doesn't
   --  match Name.

   ----------
   -- Free --
   ----------

   procedure Free (Item : in out Macro_Item_Access) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Macro_Item'Class, Macro_Item_Access);
   begin
      Internal (Item);
   end Free;

   ---------------
   -- Free_List --
   ---------------

   procedure Free_List (List : in out Macro_Item_Access) is
      Prev : Macro_Item_Access := List;
   begin
      while List /= null loop
         List := List.Next;
         Free (Prev);
         Prev := List;
      end loop;
   end Free_List;

   ----------------------
   -- Get_Focus_Widget --
   ----------------------

   function Get_Focus_Widget return Gtk_Widget is
      Current : Gtk.Widget.Widget_List.Glist;
      Widget  : Gtk_Widget;
      List    : Widget_List.Glist;
      use type Gtk.Widget.Widget_List.Glist;

   begin
      List := List_Toplevels;
      Current := Widget_List.First (List);

      while Current /= Widget_List.Null_List loop
         Widget := Widget_List.Get_Data (Current);

         if Widget.Has_Focus then
            Widget_List.Free (List);
            return Widget;
         end if;

         Current := Widget_List.Next (Current);
      end loop;

      Widget_List.Free (List);
      return Get_Toplevel (Widget);
   end Get_Focus_Widget;

   -----------------
   -- Find_Widget --
   -----------------

   function Find_Widget (Item : Macro_Item'Class) return Gtk_Widget is
      W    : Gtk_Widget;
      List : Widget_List.Glist;

      function Get_Widget_From_Id
        (Id : Identifier; List : Widget_List.Glist) return Gtk_Widget;
      --  Find the widget whose Id is ID in the application.

      ------------------------
      -- Get_Widget_From_Id --
      ------------------------

      function Get_Widget_From_Id
        (Id : Identifier; List : Widget_List.Glist) return Gtk_Widget
      is
         Current : Gtk.Widget.Widget_List.Glist := Widget_List.First (List);
         W       : Gtk_Widget;
         L       : Widget_List.Glist;
         W_Id    : Identifier;
         use type Gtk.Widget.Widget_List.Glist;

      begin
         while Current /= Widget_List.Null_List loop
            W := Widget_List.Get_Data (Current);
            W_Id := Get_Id (W);

            if W_Id.Id_Type = Id.Id_Type
              and then W_Id.Id /= null
              and then W_Id.Id.all = Id.Id.all
            then
               return W;

            --  Else we examine the children of W (except when we know the
            --  title of a top-level window, in which case there is no need to
            --  go down)

            elsif W.all in Gtk_Container_Record'Class
              and then Id.Id_Type /= Title
            then
               L := Get_Children (Gtk_Container (W));

               if L /= Widget_List.Null_List then
                  W := Get_Widget_From_Id (Id, L);

                  if W /= null then
                     return W;
                  end if;
               end if;
            end if;

            Current := Widget_List.Next (Current);
         end loop;

         return null;
      end Get_Widget_From_Id;

   begin
      List := List_Toplevels;
      W    := Get_Widget_From_Id (Item.Id, List);
      Widget_List.Free (List);

      if W = null or else Get_Window (W) = null then
         return null;
      end if;

      return W;
   end Find_Widget;

   ------------
   -- Get_Id --
   ------------

   function Get_Id
     (Widget : access Gtk_Widget_Record'Class) return Identifier
   is
      Widget_Name : constant UTF8_String := Get_Name (Widget);
      Class_Name  : constant UTF8_String := Type_Name (Get_Type (Widget));
   begin
      if Class_Name /= Widget_Name then
         --  Widget_Name is set equal to Class_Name when the widget does not
         --  have a name.
         return (Name, new String'(Widget_Name));

      elsif Widget.all in Gtk_Window_Record'Class then
         if Get_Title (Gtk_Window (Widget)) /= "" then
            return (Title, new String'(Get_Title (Gtk_Window (Widget))));

         elsif Get_Transient_For (Gtk_Window (Widget)) /= null then
            declare
               T : constant String :=
                 Get_Title (Get_Transient_For (Gtk_Window (Widget)));
            begin
               if T /= "" then
                  return (Transient, new String'(T));
               end if;
            end;
         end if;

      elsif Widget.all in Gtk_Bin_Record'Class then
         declare
            C : constant Gtk_Widget := Get_Child (Gtk_Bin (Widget));
         begin
            if C /= null and then C.all in Gtk_Label_Record'Class then
               return (Label, new String'(Get_Text (Gtk_Label (C))));
            end if;
         end;
      end if;

      return (None, null);
   end Get_Id;

   -----------------------
   -- Find_Named_Parent --
   -----------------------

   procedure Find_Named_Parent
     (Widget    : access Gtk_Widget_Record'Class;
      Parent    : out Gtk_Widget;
      Parent_Id : out Identifier) is
   begin
      Parent_Id := (None, null);
      Parent := Gtk_Widget (Widget);

      --  Stop either at the top-level widget, or at the first widget that is
      --  associated with an id.

      while Parent /= null loop
         if Get_Window (Parent) /= null then
            Parent_Id := Get_Id (Parent);
            exit when Parent_Id.Id /= null;
         end if;

         Parent := Get_Parent (Parent);
      end loop;
   end Find_Named_Parent;

   ---------------
   -- Load_Line --
   ---------------

   function Load_Line
     (File     : access File_Buffer;
      Name     : String;
      Optional : Boolean := False) return String
   is
      Last  : Natural;
      First : Natural;
   begin
      Last := File.Index;
      while Last <= File.Buffer'Last
        and then File.Buffer (Last) /= ASCII.LF
      loop
         Last := Last + 1;
      end loop;

      if File.Buffer (File.Index .. File.Index + Name'Length - 1) /= Name then
         if Optional then
            return "";
         else
            raise Invalid_Line;
         end if;
      end if;

      First := File.Index + Name'Length + 2;
      while First <= Last and then File.Buffer (First) = ' ' loop
         First := First + 1;
      end loop;

      File.Index := Last + 1;
      return File.Buffer (First .. Last - 1);
   end Load_Line;

   ---------------
   -- Save_List --
   ---------------

   function Save_List
     (Name : String; Item : Macro_Item_Access) return Boolean
   is
      File  : File_Type;
      Event : Macro_Item_Access := Item;
   begin
      Create (File, Name => Name);

      loop
         exit when Event = null;

         Save_To_Disk (File, Event.all);
         Event := Event.Next;
      end loop;

      Close (File);
      return True;

   exception
      when Status_Error | Use_Error | Name_Error =>
         return False;
   end Save_List;

   ---------------
   -- Load_List --
   ---------------

   procedure Load_List
     (Buffer  : String;
      Item    : out Macro_Item_Access;
      Success : out Boolean)
   is
      File : aliased File_Buffer;
      Prev : Macro_Item_Access;
      Next : Macro_Item_Access;
      Typ  : Gdk_Event_Type;

   begin
      Item        := null;
      Success     := True;
      File.Buffer := Buffer'Unrestricted_Access;
      File.Index  := Buffer'First;

      while File.Index <= File.Buffer'Last loop
         begin
            declare
               Str : constant String := Load_Line (File'Access, "Type");
            begin
               if Str /= "FIXED_TIME" then
                  Typ := Gdk_Event_Type'Value (Str);
                  case Typ is
                     when Enter_Notify | Leave_Notify =>
                        Next := new Macro_Item_Crossing;

                     when Button_Press | Button_Release
                       | Gdk_2button_Press
                       | Gdk_3button_Press
                       =>
                        Next := new Macro_Item_Mouse;

                     when Key_Press | Key_Release =>
                        Next := new Macro_Item_Key;

                     when Motion_Notify =>
                        Next := new Macro_Item_Motion;

                     when others =>
                        --  Ignore unknown events. Do nothing else so that we
                        --  are forward compatible.

                        Next    := null;
                        Success := False;
                  end case;

                  if Next /= null then
                     Next.Event_Type := Typ;
                     Load_Macro (File'Access, Next.all);

                     if Prev = null then
                        Item := Next;
                     else
                        Prev.Next := Next;
                        Next.Prev := Prev;
                     end if;

                     Prev := Next;
                  end if;
               end if;
            end;

         exception
            when Constraint_Error | Invalid_Line =>
               Success := False;
         end;
      end loop;
   end Load_List;

   ------------------
   -- Save_To_Disk --
   ------------------

   procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type;
      Item : Macro_Item) is
   begin
      Put_Line (File, "Type:=" & Gdk_Event_Type'Image (Item.Event_Type));
      Put_Line (File, "Id:=" & Item.Id.Id_Type'Img);

      if Item.Id.Id = null then
         Put_Line (File, "Name:=");
      else
         Put_Line (File, "Name:=" & Item.Id.Id.all);
      end if;

      Put_Line (File, "Time:=" & Guint32'Image (Item.Time));
   end Save_To_Disk;

   overriding procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type;
      Item : Macro_Item_Mouse) is
   begin
      Save_To_Disk (File, Macro_Item (Item));
      Put_Line (File, "X:=" & Gint'Image (Item.X));
      Put_Line (File, "Y:=" & Gint'Image (Item.Y));
      Put_Line (File, "Button:=" & Guint'Image (Item.Button));
      Put_Line (File, "State:=" & Gdk_Modifier_Type'Image (Item.State));
      Put_Line (File, "X_Root:=" & Gint'Image (Item.X_Root));
      Put_Line (File, "Y_Root:=" & Gint'Image (Item.Y_Root));
      Put_Line
        (File, "Window:=" & Address_Integer'Image (To_Integer (Item.Window)));
   end Save_To_Disk;

   overriding procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type;
      Item : Macro_Item_Key) is
   begin
      Save_To_Disk (File, Macro_Item (Item));
      Put_Line (File, "State:=" & Gdk_Modifier_Type'Image (Item.State));
      Put_Line (File, "Keyval:=" & Gdk_Key_Type'Image (Item.Keyval));
      Put_Line (File, "Hardware:=" & Guint16'Image (Item.Hardware_Keycode));
      Put_Line (File, "Group:=" & Guint8'Image (Item.Group));
   end Save_To_Disk;

   overriding procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type;
      Item : Macro_Item_Motion) is
   begin
      Save_To_Disk (File, Macro_Item (Item));
      Put_Line (File, "X:=" & Gint'Image (Item.X));
      Put_Line (File, "Y:=" & Gint'Image (Item.Y));
      Put_Line (File, "State:=" & Gdk_Modifier_Type'Image (Item.State));
   end Save_To_Disk;

   overriding procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type;
      Item : Macro_Item_Crossing) is
   begin
      Save_To_Disk (File, Macro_Item (Item));
      Put_Line (File, "X:=" & Gint'Image (Item.X));
      Put_Line (File, "Y:=" & Gint'Image (Item.Y));
      Put_Line (File, "Mode:=" & Gdk_Crossing_Mode'Image (Item.Mode));
      Put_Line (File, "Detail:=" & Gdk_Notify_Type'Image (Item.Detail));
      Put_Line (File, "State:=" & Gdk_Modifier_Type'Image (Item.State));
   end Save_To_Disk;

   overriding procedure Save_To_Disk
     (File : Ada.Text_IO.File_Type;
      Item : Macro_Item_Scroll) is
   begin
      Save_To_Disk (File, Macro_Item (Item));
      Put_Line (File, "X:=" & Gint'Image (Item.X));
      Put_Line (File, "Y:=" & Gint'Image (Item.Y));
      Put_Line (File, "State:=" & Gdk_Modifier_Type'Image (Item.State));
      Put_Line (File,
                "Direction:=" & Gdk_Scroll_Direction'Image (Item.Direction));
   end Save_To_Disk;

   ----------------
   -- Load_Macro --
   ----------------

   procedure Load_Macro
     (File : access File_Buffer;
      Item : out Macro_Item)  is
   begin
      Item.Id.Id_Type := Identifier_Type'Value (Load_Line (File, "Id"));
      Item.Id.Id := new String'(Load_Line (File, "Name"));
      Item.Time := Guint32'Value (Load_Line (File, "Time"));
   end Load_Macro;

   overriding procedure Load_Macro
     (File : access File_Buffer;
      Item : out Macro_Item_Mouse) is
   begin
      Load_Macro (File, Macro_Item (Item));
      Item.X := Gint'Value (Load_Line (File, "X"));
      Item.Y := Gint'Value (Load_Line (File, "Y"));
      Item.Button := Guint'Value (Load_Line (File, "Button"));
      Item.State  := Gdk_Modifier_Type'Value (Load_Line (File, "State"));
      Item.X_Root := Gint'Value (Load_Line (File, "X_Root"));
      Item.Y_Root := Gint'Value (Load_Line (File, "Y_Root"));
      Item.Window :=
        To_Window (Address_Integer'Value (Load_Line (File, "Window")));
   end Load_Macro;

   overriding procedure Load_Macro
     (File : access File_Buffer;
      Item : out Macro_Item_Key) is
   begin
      Load_Macro (File, Macro_Item (Item));
      Item.X := 0;
      Item.Y := 0;
      Item.State := Gdk_Modifier_Type'Value (Load_Line (File, "State"));
      Item.Keyval := Gdk_Key_Type'Value (Load_Line (File, "Keyval"));
      Item.Hardware_Keycode := Guint16'Value (Load_Line (File, "Hardware"));
      Item.Group := Guint8'Value (Load_Line (File, "Group"));
   end Load_Macro;

   overriding procedure Load_Macro
     (File : access File_Buffer;
      Item : out Macro_Item_Motion) is
   begin
      Load_Macro (File, Macro_Item (Item));
      Item.X := Gint'Value (Load_Line (File, "X"));
      Item.Y := Gint'Value (Load_Line (File, "Y"));
      Item.State := Gdk_Modifier_Type'Value (Load_Line (File, "State"));
   end Load_Macro;

   overriding procedure Load_Macro
     (File : access File_Buffer;
      Item : out Macro_Item_Crossing) is
   begin
      Load_Macro (File, Macro_Item (Item));
      Item.X      := Gint'Value (Load_Line (File, "X"));
      Item.Y      := Gint'Value (Load_Line (File, "Y"));
      Item.Mode   := Gdk_Crossing_Mode'Value (Load_Line (File, "Mode"));
      Item.Detail := Gdk_Notify_Type'Value (Load_Line (File, "Detail"));
      Item.State  := Gdk_Modifier_Type'Value (Load_Line (File, "State"));
   end Load_Macro;

   overriding procedure Load_Macro
     (File : access File_Buffer;
      Item : out Macro_Item_Scroll) is
   begin
      Load_Macro (File, Macro_Item (Item));
      Item.X         := Gint'Value (Load_Line (File, "X"));
      Item.Y         := Gint'Value (Load_Line (File, "Y"));
      Item.State     := Gdk_Modifier_Type'Value (Load_Line (File, "State"));
      Item.Direction :=
        Gdk_Scroll_Direction'Value (Load_Line (File, "Direction"));
   end Load_Macro;

   ----------------
   -- Play_Event --
   ----------------

   overriding function Play_Event
     (Item           : Macro_Item_Mouse;
      Device         : not null access Gdk.Device.Gdk_Device_Record'Class;
      Default_Widget : Gtk_Widget := null) return Boolean
   is
      pragma Unreferenced (Default_Widget);

      E              : Gdk_Event;
      X, Y           : Gint;
      Win            : Gdk_Window;
      W              : constant Gtk_Widget := Find_Widget (Item);
      Grab_Widget    : Gtk_Widget;
      Parent         : Gtk_Widget;
      Mouse_Press    : Macro_Item_Mouse_Access;
      Time_Elapsed   : Guint32;
      Min_Timeout_Ms : constant := 200;
      --  Minimum amount (in milliseconds) of time between a button press
      --  and a button release to consider it a 'close menu' action.

      procedure Find_Prev_Button_Press
        (Item : Macro_Item_Mouse;
         Prev : out Macro_Item_Mouse_Access;
         Time : out Guint32);
      --  Find previous button press item in the list of items.
      --  Time is the accumulated time difference between Item and Prev.

      procedure Find_Prev_Button_Press
        (Item : Macro_Item_Mouse;
         Prev : out Macro_Item_Mouse_Access;
         Time : out Guint32)
      is
         Tmp : Macro_Item_Access := Item.Prev;
      begin
         Prev := null;
         Time := Item.Time;

         while Tmp /= null loop
            Time := Time + Tmp.Time;

            if Tmp.Event_Type = Button_Press then
               Prev := Macro_Item_Mouse_Access (Tmp);
               return;

            elsif Tmp.Event_Type = Button_Release then
               return;
            end if;

            Tmp := Tmp.Prev;
         end loop;

         return;
      end Find_Prev_Button_Press;

   begin
      Move_Pointer (Item.X_Root, Item.Y_Root);
      Gdk.Window.At_Pointer (X, Y, Win);

      if W /= null then
         Parent := Get_Parent (W);
         Grab_Widget := Grab_Get_Current;

         --  Handling of menus seems to be particularly tricky.
         --  There are lots of possible combinations to take into account
         --  (top level menus, contextual menus, click-move-release or
         --  click-release-move-click, ...) so any change in the following
         --  section should be done with lots of care *and* testing.

         if Grab_Widget /= null
           and then Grab_Widget /= W
           and then Grab_Widget.all in Gtk_Menu_Record'Class
           and then W.all not in Gtk_Menu_Item_Record'Class
         then
            if Item.Event_Type = Button_Release then
               --  Close the menu since we clicked outside its area

               Deactivate (Gtk_Menu (Grab_Widget));
            end if;

            return True;
         end if;

         if W.all in Gtk_Menu_Item_Record'Class
           and then Parent /= null
           and then Parent.all in Gtk_Menu_Record'Class
         then
            if Item.Event_Type = Button_Release then
               Find_Prev_Button_Press (Item, Mouse_Press, Time_Elapsed);

               --  The following test differenciates between:
               --  - button press/release, move, button press/release
               --  - button press, move, button release
               --  and tries to avoid automatically selecting the first
               --  item in the first case.

               if Mouse_Press = null
                 or else Time_Elapsed > Min_Timeout_Ms
                 or else Mouse_Press.Window = Item.Window
               then
                  --  Simulate menu activation by sending the 'space' key
                  --  Sending the button event itself does not work for some
                  --  reason.
                  --  Alternatively, using Gtk.Menu_Shell.Activate_Item will
                  --  call the handler immediately, possibly blocking in
                  --  another event loop, so we have to send an event instead.

                  Gdk_New (E, Key_Press);
                  E.Key.Window := Win;
                  E.Key.State := Item.State;
                  E.Key.Keyval := GDK_KP_Space;
                  E.Key.Hardware_Keycode := 36;  --  is this portable ???
                  Put (E);
                  Free (E);

                  return True;
               end if;
            end if;
         end if;
      end if;

      Gdk_New (E, Item.Event_Type);
      E.Button.Window := Win;
      Gdk.Window.Ref (Win);
      E.Button.X := Gdouble (Item.X);
      E.Button.Y := Gdouble (Item.Y);
      E.Button.X_Root := Gdouble (Item.X_Root);
      E.Button.Y_Root := Gdouble (Item.Y_Root);
      E.Button.Button := Item.Button;
      E.Button.State := Item.State;
      Set_Device (E, Device);
      Put (E);
      Free (E);

      return True;
   end Play_Event;

   ----------------
   -- Play_Event --
   ----------------

   overriding function Play_Event
     (Item           : Macro_Item_Key;
      Device         : not null access Gdk.Device.Gdk_Device_Record'Class;
      Default_Widget : Gtk_Widget := null) return Boolean
   is
      E       : Gdk_Event;
      Widget  : Gtk_Widget := Grab_Get_Current;
   begin
      if Widget = null then
         Widget := Find_Widget (Item);

         if Widget = null then
            Widget := Default_Widget;

            if Widget = null then
               Widget := Get_Focus_Widget;

               if Widget = null then
                  return False;
               end if;
            end if;
         end if;
      end if;

      Gdk_New (E, Item.Event_Type);
      E.Key.Window := Get_Window (Widget);
      Gdk.Window.Ref (E.Key.Window);
      E.Key.State := Item.State;
      E.Key.Keyval := Item.Keyval;
      E.Key.Group := Item.Group;
      Set_Device (E, Device);
      E.Key.Hardware_Keycode := Item.Hardware_Keycode;
      Put (E);
      Free (E);
      return True;
   end Play_Event;

   ----------------
   -- Play_Event --
   ----------------

   overriding function Play_Event
     (Item           : Macro_Item_Motion;
      Device         : not null access Gdk.Device.Gdk_Device_Record'Class;
      Default_Widget : Gtk_Widget := null) return Boolean
   is
      pragma Unreferenced (Default_Widget);

      E    : Gdk_Event;
      Win  : Gdk_Window;
      X, Y : Gint;

   begin
      Move_Pointer (Item.X, Item.Y);
      Gdk.Window.At_Pointer (X, Y, Win);

      Gdk_New (E, Item.Event_Type);
      E.Motion.Window := Win;
      Gdk.Window.Ref (E.Motion.Window);
      E.Motion.X := Gdouble (Item.X - X);
      E.Motion.Y := Gdouble (Item.Y - Y);
      E.Motion.State := Item.State;
      Set_Device (E, Device);
      Put (E);
      Free (E);
      return True;
   end Play_Event;

   ----------------
   -- Play_Event --
   ----------------

   overriding function Play_Event
     (Item           : Macro_Item_Crossing;
      Device         : not null access Gdk.Device.Gdk_Device_Record'Class;
      Default_Widget : Gtk_Widget := null) return Boolean
   is
      pragma Unreferenced (Default_Widget);

      E    : Gdk_Event;
      X, Y : Gint;
      Win  : Gdk_Window;

   begin
      Move_Pointer (Item.X, Item.Y);

      if Item.Mode /= Crossing_Normal then
         Gdk.Window.At_Pointer (X, Y, Win);
         Gdk_New (E, Item.Event_Type);
         E.Crossing.Window := Win;
         Gdk.Window.Ref (E.Crossing.Window);
         E.Crossing.X := Gdouble (Item.X - X);
         E.Crossing.Y := Gdouble (Item.Y - Y);
         E.Crossing.X_Root := Gdouble (Item.X);
         E.Crossing.Y_Root := Gdouble (Item.Y);
         E.Crossing.Mode  := Item.Mode;
         E.Crossing.Detail := Item.Detail;
         E.Crossing.State := Item.State;
         Set_Device (E, Device);
         Put (E);
         Free (E);
      end if;

      return True;
   end Play_Event;

   ----------------
   -- Play_Event --
   ----------------

   overriding function Play_Event
     (Item           : Macro_Item_Scroll;
      Device         : not null access Gdk.Device.Gdk_Device_Record'Class;
      Default_Widget : Gtk_Widget := null) return Boolean
   is
      pragma Unreferenced (Default_Widget);

      E    : Gdk_Event;
      X, Y : Gint;
      Win  : Gdk_Window;

   begin
      Move_Pointer (Item.X, Item.Y);
      Gdk.Window.At_Pointer (X, Y, Win);

      Gdk_New (E, Item.Event_Type);
      E.Scroll.Window := Win;
      Gdk.Window.Ref (E.Scroll.Window);
      E.Scroll.X := Gdouble (Item.X - X);
      E.Scroll.Y := Gdouble (Item.Y - Y);
      E.Scroll.X_Root := Gdouble (Item.X);
      E.Scroll.Y_Root := Gdouble (Item.Y);
      E.Scroll.State := Item.State;
      E.Scroll.Direction := Item.Direction;
      Set_Device (E, Device);
      Put (E);
      Free (E);

      return True;
   end Play_Event;

   -----------------
   -- Create_Item --
   -----------------

   function Create_Item
     (Event     : Gdk_Event_Button;
      Prev_Time : Guint32 := 0) return Macro_Item_Mouse_Access
   is
      Parent : Gtk_Widget;
      Item   : Macro_Item_Mouse_Access;
   begin
      Item            := new Macro_Item_Mouse;
      Find_Named_Parent
        (Get_Event_Widget (To_Event (Event'Unrestricted_Access)),
         Parent, Item.Id);

      Item.Event_Type := Event.The_Type;
      Item.X          := Gint (Event.X);
      Item.Y          := Gint (Event.Y);
      Item.X_Root     := Gint (Event.X_Root);
      Item.Y_Root     := Gint (Event.Y_Root);
      Item.Button     := Event.Button;
      Item.State      := Event.State;

      if Prev_Time = 0 then
         Item.Time := 0;
      else
         Item.Time := Event.Time - Prev_Time;
      end if;

      Item.Window     := Event.Window;
      return Item;
   end Create_Item;

   function Create_Item
     (Event     : Gdk_Event_Key;
      Prev_Time : Guint32 := 0) return Macro_Item_Key_Access
   is
      Parent : Gtk_Widget;
      Item   : Macro_Item_Key_Access;
   begin
      Item                  := new Macro_Item_Key;
      Find_Named_Parent
        (Get_Event_Widget (To_Event (Event'Unrestricted_Access)),
         Parent, Item.Id);

      Item.Event_Type       := Event.The_Type;
      Item.State            := Event.State;

      if Prev_Time = 0 then
         Item.Time := 0;
      else
         Item.Time := Event.Time - Prev_Time;
      end if;

      Item.Keyval           := Event.Keyval;
      Item.Group            := Event.Group;
      Item.Hardware_Keycode := Event.Hardware_Keycode;
      return Item;
   end Create_Item;

   function Create_Item
     (Event     : Gdk_Event_Motion;
      Prev_Time : Guint32 := 0) return Macro_Item_Motion_Access
   is
      Item : Macro_Item_Motion_Access;
   begin
      Item            := new Macro_Item_Motion;
      Item.Event_Type := Event.The_Type;
      Item.X          := Gint (Event.X_Root);
      Item.Y          := Gint (Event.Y_Root);
      Item.State      := Event.State;

      if Prev_Time = 0 then
         Item.Time := 0;
      else
         Item.Time := Event.Time - Prev_Time;
      end if;

      return Item;
   end Create_Item;

   function Create_Item
     (Event     : Gdk_Event_Crossing;
      Prev_Time : Guint32 := 0) return Macro_Item_Crossing_Access
   is
      Item : Macro_Item_Crossing_Access;
   begin
      Item            := new Macro_Item_Crossing;
      Item.Event_Type := Event.The_Type;
      Item.X          := Gint (Event.X_Root);
      Item.Y          := Gint (Event.Y_Root);
      Item.Mode       := Event.Mode;
      Item.Detail     := Event.Detail;

      if Prev_Time = 0 then
         Item.Time := 0;
      else
         Item.Time := Event.Time - Prev_Time;
      end if;

      return Item;
   end Create_Item;

   function Create_Item
     (Event     : Gdk_Event_Scroll;
      Prev_Time : Guint32 := 0) return Macro_Item_Scroll_Access
   is
      Item : Macro_Item_Scroll_Access;
   begin
      Item            := new Macro_Item_Scroll;
      Item.Event_Type := Event.The_Type;
      Item.X          := Gint (Event.X_Root);
      Item.Y          := Gint (Event.Y_Root);
      Item.State      := Event.State;
      Item.Direction  := Event.Direction;

      if Prev_Time = 0 then
         Item.Time := 0;
      else
         Item.Time := Event.Time - Prev_Time;
      end if;

      return Item;
   end Create_Item;

end Gtkada.Macro;
