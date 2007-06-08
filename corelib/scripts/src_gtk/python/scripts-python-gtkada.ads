
--  Addition subprograms to support python scripting in graphical mode.
--  In particular, these provide additional support for pygtk

with Glib.Object;

package Scripts.Python.Gtkada is

   procedure Init_PyGtk_Support
     (Script : access Scripting_Language_Record'Class);
   --  Initialize the support for pygtk.
   --  If this fails, the calls to the subprograms below will have no effect.
   --  This suprogram only succeeds when the scripts package was compiled with
   --  pygtk, and the latter is found in the python installation when the
   --  application is run.

   procedure Add_PyWidget_Method
     (Script : access Scripting_Language_Record'Class;
      Class  : Class_Type);
   --  Adds a new method to Class:
   --     Class.pywidget
   --         Returns the pywidget corresponding to the GtkAda widget stored
   --         in Class. It is assumed that Scripts.Gtkada.Get_Data will
   --         return a valid GObject (or null) when called on this instance

   function From_PyGtk
     (Data : Callback_Data'Class;
      N    : Positive) return Glib.Object.GObject;
   --  Return the Gtk object encapsulated inside the pygtk object given in the
   --  N-th argument

end Scripts.Python.Gtkada;
