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

--  Addition subprograms to support python scripting in graphical mode.
--  In particular, these provide additional support for pygtk

with Glib.Object;
with Gdk;

package GNATCOLL.Scripts.Python.Gtkada is

   procedure Init_PyGtk_Support
     (Script : access Scripting_Language_Record'Class);
   --  Initialize the support for pygtk.
   --  If this fails, the calls to the subprograms below will have no effect.
   --  This suprogram only succeeds when the scripts package was compiled with
   --  pygtk, and the latter is found in the python installation when the
   --  application is run.

   procedure Add_PyWidget_Method
     (Repo   : access Scripts_Repository_Record'Class;
      Class  : Class_Type);
   --  Adds a new method to Class:
   --     Class.pywidget
   --         Returns the pywidget corresponding to the GtkAda widget stored
   --         in Class. It is assumed that Scripts.Gtkada.Get_Data will
   --         return a valid GObject (or null) when called on this instance

   function From_PyGtk
     (Data : Callback_Data'Class;
      N    : Positive) return Glib.Object.GObject;
   function From_PyGtk
     (Data : Callback_Data'Class;
      N    : Positive) return Gdk.Gdk_Window;
   --  Return the Gtk object encapsulated inside the pygtk object given in the
   --  N-th argument

end GNATCOLL.Scripts.Python.Gtkada;
