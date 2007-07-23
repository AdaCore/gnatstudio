-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2005-2007, AdaCore              --
--                                                                   --
-- GPS is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this program; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package contains various constants and subprograms used for the
--  GPS-specific usage of the MDI.

with Commands;
with Gtkada.MDI;         use Gtkada.MDI;
with GPS.Kernel.Modules; use GPS.Kernel.Modules;

package GPS.Kernel.MDI is

   ---------------------
   -- Child positions --
   ---------------------

   --  This is a list of predefined Child_Positions used by various elements
   --  in GPS.

   Group_Graphs         : constant Child_Group := 101;
   Group_VCS_Explorer   : constant Child_Group := 102;
   Group_Debugger_Stack : constant Child_Group := 103;
   Group_Debugger_Data  : constant Child_Group := 104;
   Group_VCS_Activities : constant Child_Group := 105;
   Group_View           : constant Child_Group := 106;
   Group_Consoles       : constant Child_Group := 107;

   function Get_Current_Window
     (Handle : access Kernel_Handle_Record'Class) return Gtk.Window.Gtk_Window;
   --  Return the window containing the current MDI Child.
   --  The main usage for this function should be to display the dialogs
   --  centered with regards to this window.

   type GPS_MDI_Child_Record is new Gtkada.MDI.MDI_Child_Record with private;
   type GPS_MDI_Child is access all GPS_MDI_Child_Record'Class;
   --  Base record for all MDI children that go into the MDI

   procedure Gtk_New
     (Child               : out GPS_MDI_Child;
      Widget              : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags               : Child_Flags := All_Buttons;
      Group               : Child_Group := Group_Default;
      Focus_Widget        : Gtk.Widget.Gtk_Widget := null;
      Default_Width, Default_Height : Glib.Gint := -1;
      Module              : access Module_ID_Record'Class; --  can be null
      Desktop_Independent : Boolean := False);
   --  Recommended version of Gtk_New to use, instead of the one in
   --  GtkAda.MDI. This version has several new parameters:
   --    - Module : used to associate a module with a widget. This is used to
   --               get the current context for instance
   --    - Desktop_Independent: if this is true, then the window will not be
   --               closed  when a new desktop is loaded.
   --    - Use_Scrolled : if this is true, then the widget will be included
   --               inside a scrolled window

   procedure Initialize
     (Child               : access GPS_MDI_Child_Record'Class;
      Widget              : access Gtk.Widget.Gtk_Widget_Record'Class;
      Flags               : Child_Flags := All_Buttons;
      Group               : Child_Group := Group_Default;
      Focus_Widget        : Gtk.Widget.Gtk_Widget := null;
      Default_Width, Default_Height : Glib.Gint := -1;
      Module              : access Module_ID_Record'Class;
      Desktop_Independent : Boolean := False);
   --  Internal version of Gtk_New

   function Get_MDI
     (Handle : access Kernel_Handle_Record'Class)
      return Gtkada.MDI.MDI_Window;
   --  Return the MDI associated with Handle.
   --  Use the Put function below instead of the one in GtkAda.MDI to
   --  associated a widget with a GPS module

   function Get_Module_From_Child
     (Child : Gtkada.MDI.MDI_Child) return Module_ID;
   --  Return the module that created Child, or null if no module was found.

   function Get_File_Editor
     (Handle : access Kernel_Handle_Record'Class;
      File   : VFS.Virtual_File) return Gtkada.MDI.MDI_Child;
   --  Return the first MDI child associated to an editor for File.
   --  Return null if no such editor was found.

   function Save_MDI_Children
     (Handle   : access Kernel_Handle_Record'Class;
      Children : Gtkada.MDI.MDI_Child_Array := Gtkada.MDI.No_Children;
      Force    : Boolean := False) return Boolean;
   --  Save all the MDI children, as well as the current project
   --  If Force is False, ask the user first.
   --  If Children is specified, only ask to save these specific children.
   --  The return value is False if the user has cancelled the action, True if
   --  the user has selected OK (whatever the number of children that were
   --  saved).

   procedure Close_All_Children (Handle : access Kernel_Handle_Record'Class);
   --  Close all the MDI children. No confirmation is asked, call
   --  Save_All_MDI_Children first if needed.

   function Get_Command_Queue
     (Child : access GPS_MDI_Child_Record) return Commands.Command_Queue;
   --  Return the command queue associated with the current context. In
   --  particular, this can be used for undo, for instance through
   --     Start_Group (Get_Command_Queue (Child))
   --  By default, it returns Null_Command_Queue

private

   type GPS_MDI_Child_Record is new Gtkada.MDI.MDI_Child_Record with record
      Module              : Abstract_Module_ID;
      Desktop_Independent : Boolean;
   end record;

end GPS.Kernel.MDI;
