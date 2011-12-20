------------------------------------------------------------------------------
--                      GVD - The GNU Visual Debugger                       --
--                                                                          --
--                     Copyright (C) 2005-2012, AdaCore                     --
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

--  This package provides a number of subprograms to ease the creation of views
--  in the context of GVD.
--  This is a generic package. Because of elaboration circularity issues,
--  several of the formal parameters can really just be passed from
--  gvd-process and gvd_module.ads.
--
--  See default instanciations in GVD.Views

with Glib;
with XML_Utils;
with GPS.Kernel.Hooks;
with GPS.Kernel.Modules;
with Gtk.Widget;
with Gtkada.MDI;

generic
   type Base_Type is new Gtk.Widget.Gtk_Widget_Record with private;
   type Base_Type_Access is access all Base_Type'Class;
   --  The type used as a parent for the view, and used to store the view in
   --  a Visual_Debugger.

   type Visual_Debugger_Record is tagged private;
   type Visual_Debugger is access all Visual_Debugger_Record'Class;
   --  The visual debugger class. We cannot with GVD.Process in this package,
   --  since otherwise this creates elaboration circularities.

   with function Get_Module return GPS.Kernel.Modules.Module_ID is <>;
   --  The module to be associated with the MDI child, in particular when
   --  creating contextual menus. This needs to be a function instead of a
   --  simple variable, since the package is instanciated before the module is
   --  created.

   with function Get_Num (Process : Visual_Debugger) return Glib.Gint is <>;
   --  Return the debugger identifier associated with Process

   with function Get_Process
     (Data : access GPS.Kernel.Hooks.Hooks_Data'Class)
      return Visual_Debugger is <>;
   --  Return the debugger stored in Data

   with function Get_State
     (Data : access GPS.Kernel.Hooks.Hooks_Data'Class)
     return Debugger_State is <>;
   --  Return the state stored in Data

   with function Command_In_Process
     (Process : access Visual_Debugger_Record'Class) return Boolean is <>;
   --  Whether a command is currently being processed by the debugger

   with function Get_Kernel
     (Process : access Visual_Debugger_Record'Class)
     return GPS.Kernel.Kernel_Handle is <>;
   --  Return the kernel

package GVD.Generic_View is
   type Process_View_Record is abstract new Base_Type with private;

   function Get_Process
     (View : access Process_View_Record) return Visual_Debugger;
   --  Return the debugger associated with that view, or null if the view
   --  is not associated currently.

   procedure Unset_Process (View : access Process_View_Record);
   --  Disconnect View from any process

   procedure On_Attach
     (View    : access Process_View_Record;
      Process : access Visual_Debugger_Record'Class) is null;
   --  Called when the view is being attached to Process. This procedure should
   --  typically be used to connect to specific events on Process. However, it
   --  doesn't need to force a refresh of the view, which is done
   --  automatically.
   --  By default, it does nothing.

   procedure Update (View : access Process_View_Record) is null;
   --  Refresh the view by getting up-to-date information from the debugger.
   --  Nothing is done when the view is not associated with a debugger.
   --  It does nothing by default.

   procedure On_Process_Terminated (View : access Process_View_Record) is null;
   --  Called when the debugged process has terminated

   procedure On_State_Changed
     (View      : access Process_View_Record;
      New_State : Debugger_State) is null;
   --  Called when the debugger state has changed. One possibly use is to
   --  clear the view while the debugger is executing because its information
   --  might be confusing in such a case.

   function Save_To_XML
     (View : access Process_View_Record) return XML_Utils.Node_Ptr;
   --  Return an XML represention of the view. This is used to save the view
   --  to the desktop, and possibly for debug purposes.
   --  By default, this returns null.

   procedure Load_From_XML
     (View : access Process_View_Record; XML : XML_Utils.Node_Ptr) is null;
   --  Initialize View from XML. XML is the contents of the desktop node for
   --  the View, and was generated by Save_To_XML.
   --  By default, this procedure does nothing.

   generic
      Module_Name : String;
      --  The name of the module, and name used in the desktop file. It mustn'y
      --  contain any space.

      View_Name   : String;
      --  Name of the menu, in tools, that is used to create the view. It is
      --  also used as the name for the MDI window.

      type Formal_View_Record is new Process_View_Record with private;
      --  The base type of the view

      with function Get_View
        (Process : access Visual_Debugger_Record'Class)
         return Base_Type_Access is <>;
      with procedure Set_View
        (Process : access Visual_Debugger_Record'Class;
         View    : Base_Type_Access) is <>;
      --  Get or set the view in the visual debugger structure, so that it is
      --  closely associated with that process.
      --  The function Set_View is a good place to save the current state of
      --  the view in the properties if need be. It always occurs before the
      --  widget is actually destroyed, and while the debugger still exists
      --  (you should save the contents of the current view, before setting the
      --  new one). When Set_View is called with a null View, it means the
      --  process is being detached. At that point, it is a good idea to clear
      --  graphically the contents of the old view. At that point the
      --  view is still attached to a process with which you can interact.

      with function Initialize
        (View   : access Formal_View_Record'Class;
         Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
        return Gtk.Widget.Gtk_Widget is <>;
      --  Function used to create the view itself.
      --  This function should return the Focus_Child in the MDI sense, ie the
      --  widget that should receive the focus when the MDI child receives the
      --  focus.

      Group    : Gtkada.MDI.Child_Group;
      Position : Gtkada.MDI.Child_Position := Gtkada.MDI.Position_Right;
      --  How should the MDI child be created?

      MDI_Child_Flags : Gtkada.MDI.Child_Flags := Gtkada.MDI.All_Buttons;
      --  Flags used when creating the MDI Child

   package Simple_Views is

      procedure Attach_To_View
        (Process             : access Visual_Debugger_Record'Class;
         Create_If_Necessary : Boolean);
      --  Attach the process to an instance of the view.
      --  If an unattached view exists in the desktop, it is reused.
      --  If none exists, one is created if Create_If_Necessary is true.
      --  Nothing is done when Process is already attached to a view.
      --
      --  The debugger console should be created already. When it is closed (ie
      --  the debugger exits), the view will be destroyed.

      procedure Register_Desktop_Functions
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class);
      --  Register the functions needed to load and save the desktop

   private
      procedure On_Destroy
        (View : access Gtk.Widget.Gtk_Widget_Record'Class);
      --  Callback for the "destroy_event" signal on the Call Stack window.
      --  This needs to be in the spec since it is used as a callback in the
      --  body.

      procedure On_Debugger_Terminate
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
         Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
      --  Callback when the debugger is terminated.
      --  This needs to be in the spec since it is used as a callback in the
      --  body.

      function Save_Desktop
        (Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
         Kernel : GPS.Kernel.Kernel_Handle) return XML_Utils.Node_Ptr;
      function Load_Desktop
        (MDI    : Gtkada.MDI.MDI_Window;
         Node   : XML_Utils.Node_Ptr;
         Kernel : GPS.Kernel.Kernel_Handle) return Gtkada.MDI.MDI_Child;
      --  Functions to save and load desktops

      procedure On_Update
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
         Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
      --  Hook called when the view needs to be refreshed

      procedure State_Changed
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
         Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
      --  Hook called when the state of the debugger changes

      procedure Process_Terminated
        (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
         Data   : access GPS.Kernel.Hooks.Hooks_Data'Class);
      --  Called when the process has terminated

   end Simple_Views;

private
   type Process_View_Record is abstract new Base_Type with record
      Process : Visual_Debugger;
         --  The process associated with the view
   end record;
end GVD.Generic_View;
