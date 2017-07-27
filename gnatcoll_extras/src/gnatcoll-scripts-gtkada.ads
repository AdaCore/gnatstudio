------------------------------------------------------------------------------
--                             G N A T C O L L                              --
--                                                                          --
--                     Copyright (C) 2003-2017, AdaCore                     --
--                                                                          --
-- This library is free software;  you can redistribute it and/or modify it --
-- under terms of the  GNU General Public License  as published by the Free --
-- Software  Foundation;  either version 3,  or (at your  option) any later --
-- version. This library is distributed in the hope that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE.                            --
--                                                                          --
-- As a special exception under Section 7 of GPL version 3, you are granted --
-- additional permissions described in the GCC Runtime Library Exception,   --
-- version 3.1, as published by the Free Software Foundation.               --
--                                                                          --
-- You should have received a copy of the GNU General Public License and    --
-- a copy of the GCC Runtime Library Exception along with this program;     --
-- see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
-- <http://www.gnu.org/licenses/>.                                          --
--                                                                          --
------------------------------------------------------------------------------

--  This package provides subprograms related to the use of the scripts
--  interface in a GtkAda environment.
--  In particular, it provides a number of subprograms to properly associate
--  a GtkAda object with a script object, properly taking care of reference
--  counting in both cases.

with Glib.Object;
with Gtk.Handlers;
with Gtk.Widget;

package GNATCOLL.Scripts.Gtkada is

   package Subprogram_Callback is new Gtk.Handlers.User_Callback
     (Widget_Type => Gtk.Widget.Gtk_Widget_Record,
      User_Type   => Subprogram_Type);
   --  Callbacks that can be used to associate a GUI object (menu, toolbar
   --  button,...) with a script subprogram.
   --  This can for instance be used to make a menu item execute a python
   --  subprogram when it is selected.

   -------------------------------------------
   -- Class instances and graphical objects --
   -------------------------------------------
   --  Often, some of the classes exported by an application to a scripting
   --  language represent graphical objects from GtkAda.
   --  Such objects are reference-counted, and need proper handling if we want
   --  to avoid memory leaks.
   --  In addition, we want a given GUI object to always be represented by the
   --  same class instance. Some scripting languages, in particular python,
   --  allow users to store their own user data in the instance, and we want
   --  them to retrieve that data whenever they access that GUI object. For
   --  instance:
   --      menu = get ("/File/Open")  # returns the class instance for menu
   --      menu.data = "foo"          # create own user data
   --      ....
   --      menu = get ("/File/Open")  # Must return same class instance
   --      print menu.data            # must print "foo"
   --
   --  The following subprograms will take care of these aspects. They make
   --  sure that the script's class_instance lives at least as long as the GUI
   --  object, so that if the user users the same GUI object, we always get the
   --  same Class_Instance.
   --
   --  However, when the GUI object is destroyed, we do not necessarily free
   --  the class_instance, since it might be referenced in other contexts.
   --  But calling any method of the class_instance should raise an exception,
   --  since there is no more real associated object.
   --      e = get ("/File/Open")
   --      e.destroy()  ## widget destroyed, but e is still valid
   --      e.hide()     ## error, since e is no longer associated with a widget

   GUI_Data_Name : constant String := "__gui";
   --  The default name of user data. This is mostly for convenience when
   --  calling the subprograms below, and you can easily replace it by some
   --  other string, in particular if multiple GUI objects need to be
   --  associated with a given class instance.

   procedure Set_Data
     (Instance : Class_Instance;
      Widget   : Glib.Object.GObject;
      Name     : String := GUI_Data_Name);
   --  Associate Widget and Instance, so that one can be retrieved from the
   --  other.

   function Get_Instance
     (Script : access Scripting_Language_Record'Class;
      Widget : access Glib.Object.GObject_Record'Class)
      return Class_Instance;
   --  Return the class_instance associated with Widget in the given scripting
   --  language. No_Class_Instance is returned if Widget is not already
   --  associated, in which case you should create a new class instance and
   --  use Set_Data above.

   function Get_Data
     (Instance : Class_Instance;
      Name     : String := GUI_Data_Name) return Glib.Object.GObject;
   --  Return the object associated with Instance.

end GNATCOLL.Scripts.Gtkada;
