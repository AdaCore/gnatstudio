-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2002                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides an extension of GVD.Menu when GVD is used in
--  stand alone mode (e.g not within an IDE)

with Glib; use Glib;
with Gtk.Item_Factory; use Gtk.Item_Factory;
with Factory_Data;

package GVD.Menu.Standalone is
   use Factory_Data;

   ---------------
   -- Callbacks --
   ---------------

   procedure On_Open_Debugger
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for File->New Debugger

   procedure On_Edit_Source
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for File->Edit Source

   procedure On_Open_Source
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for File->Open Source

   procedure On_Reload_Sources
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for File->Reload Sources

   procedure On_Open_Session
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for File->Open Session

   procedure On_Save_Session_As
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for File->Save Session As

   procedure On_Close
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for File->Close

   procedure On_Exit
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for File->Exit

   procedure On_Undo
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Edit->Undo

   procedure On_Redo
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Edit->Redo

   procedure On_Cut
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Edit->Cut

   procedure On_Copy
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Edit->Copy

   procedure On_Paste
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Edit->Paste

   procedure On_Select_All
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Edit->Select All

   procedure On_Preferences
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Edit->Preferences

   procedure On_Manual
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Help->Manual

   procedure On_About_GVD
     (Object : Data_Type_Access;
      Action : Guint;
      Widget : Limited_Widget);
   --  Callback for Help->About

   --------------------
   -- GVD_Menu_Items --
   --------------------

   type Gtk_Item_Factory_Entry_Access is access Gtk_Item_Factory_Entry_Array;

   function GVD_Menu_Items return Gtk_Item_Factory_Entry_Access;
   --  Return a pointer to the Factory_Entry_Array needed to create the
   --  GVD menu items.

end GVD.Menu.Standalone;
