-----------------------------------------------------------------------
--          GtkAda - Ada95 binding for the Gimp Toolkit              --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Glib; use Glib;
with Gtk.Widget; use Gtk.Widget;
with Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);

package body Gtkada.Toolbar is

   use Gtk;
   use Gtk.Box;
   use Gtk.Toolbar;
   use Gtk.Handle_Box;
   use Widget_List;

   package Widget_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Gtkada_Toolbar);

   procedure Child_Attached
     (Widget  : access Gtk_Widget_Record'Class;
      Toolbar : Gtkada_Toolbar);
   --  Callback called when the handle box is attached to its parent

   procedure Child_Detached
     (Widget  : access Gtk_Widget_Record'Class;
      Toolbar : Gtkada_Toolbar);
   --  Callback called when the handle box is detached from its parent

   --------------------
   -- Child_Attached --
   --------------------

   procedure Child_Attached
     (Widget  : access Gtk_Widget_Record'Class;
      Toolbar : Gtkada_Toolbar)
   is
      List     : Glist;
      Position : Gint := 1;

   begin
      Set_Orientation (Toolbar, Orientation_Horizontal);
      List := First (Children (Toolbar.Toolbar_Right));

      while List /= Widget_List.Null_List loop
         Remove (Toolbar.Toolbar_Right, Get_Data (List));
         Insert_Widget (Toolbar, Get_Data (List), "", "", Position);
         Position := Position + 2;
         List := Next (List);
      end loop;

      Destroy (Toolbar.Toolbar_Right);
   end Child_Attached;

   --------------------
   -- Child_Detached --
   --------------------

   procedure Child_Detached
     (Widget  : access Gtk_Widget_Record'Class;
      Toolbar : Gtkada_Toolbar)
   is
      List    : Glist;
   begin
      Set_Orientation (Toolbar, Orientation_Vertical);
      Gtk_New (Toolbar.Toolbar_Right, Orientation_Vertical, Toolbar.Style);
      Pack_Start (Toolbar.Box, Toolbar.Toolbar_Right);
      List := Next (First (Children (Toolbar)));

      while List /= Widget_List.Null_List loop
         Remove (Toolbar, Get_Data (List));
         Append_Widget (Toolbar.Toolbar_Right, Get_Data (List));
         List := Next (List);

         exit when List = Widget_List.Null_List;

         List := Next (List);
      end loop;

      Show_All (Toolbar.Toolbar_Right);
   end Child_Detached;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Toolbar     : out Gtkada_Toolbar;
      Orientation : in Gtk_Orientation;
      Style       : in Gtk_Toolbar_Style) is
   begin
      Gtk_New (Toolbar, Style);
   end Gtk_New;

   procedure Gtk_New
     (Toolbar : out Gtkada_Toolbar;
      Style   : in Gtk_Toolbar_Style) is
   begin
      Toolbar := new Gtkada_Toolbar_Record;
      Initialize (Toolbar, Style);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Toolbar : access Gtkada_Toolbar_Record'Class;
      Style   : in Gtk_Toolbar_Style) is
   begin
      Initialize (Gtk_Toolbar (Toolbar), Orientation_Horizontal, Style);
      Toolbar.Style := Style;
      Gtk_New (Toolbar.Handle_Box);
      Gtk_New_Hbox (Toolbar.Box, True);
      Add (Toolbar.Handle_Box, Toolbar.Box);
      Pack_Start (Toolbar.Box, Toolbar);
      Show_All (Toolbar.Box);
      Widget_Callback.Connect
        (Toolbar.Handle_Box, "child_attached",
         Widget_Callback.To_Marshaller (Child_Attached'Access),
         Gtkada_Toolbar (Toolbar));
      Widget_Callback.Connect
        (Toolbar.Handle_Box, "child_detached",
         Widget_Callback.To_Marshaller (Child_Detached'Access),
         Gtkada_Toolbar (Toolbar));
   end Initialize;

   --------------------
   -- Get_Handle_Box --
   --------------------

   function Get_Handle_Box
     (Toolbar : access Gtkada_Toolbar_Record)
      return Gtk.Handle_Box.Gtk_Handle_Box is
   begin
      return Toolbar.Handle_Box;
   end Get_Handle_Box;

end Gtkada.Toolbar;
