-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
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

with Breakpoints_Pkg; use Breakpoints_Pkg;
with Advanced_Breakpoint_Pkg; use Advanced_Breakpoint_Pkg;
with GVD.Types; use GVD.Types;
with GVD.Process;
with Gdk.Bitmap;
with Gdk.Pixmap;

package Breakpoints_Editor is

   type Breakpoint_Editor_Record is new Breakpoints_Record with record
      Advanced_Breakpoints_Location    : Advanced_Breakpoint_Access;
      Advanced_Breakpoints_Watchpoints : Advanced_Breakpoint_Access;
      Advanced_Breakpoints_Exceptions  : Advanced_Breakpoint_Access;
      Process                          : GVD.Process.Debugger_Process_Tab;
      Enabled_Pixmap                   : Gdk.Pixmap.Gdk_Pixmap;
      Enabled_Mask                     : Gdk.Bitmap.Gdk_Bitmap;
   end record;
   type Breakpoint_Editor_Access is access all Breakpoint_Editor_Record'Class;

   procedure Breakpoint_Editor
     (Editor     : in out Breakpoint_Editor_Access;
      Process    : access GVD.Process.Debugger_Process_Tab_Record'Class);
   --  Open a breakpoint editor and launch a main loop until the ok or cancel
   --  button has been pressed.
   --  Editor if null is set to the created window, that is hidden on return.
   --  If non null, Breakpoint_Editor will show it instead of creating a new
   --  one.
   --  Return the breakpoint descriptor.
   --  Note that this is your responsibility to free the memory associated with
   --  Descriptor, using Free below.

   function Get_Selection_Index
     (Editor : access Breakpoint_Editor_Record) return Integer;
   --  Return the index of the currently selected line in the breakpoint
   --  editor. The index is the element in Editor.Process.Breakpoints, or
   --  -1 if there is no selection

   procedure Set_Process
     (Editor  : access Breakpoint_Editor_Record;
      Process : access GVD.Process.Debugger_Process_Tab_Record'Class);
   --  Change the process on which the dialogs applies.
   --  The list of breakpoints is automatically updated for the new process.

   procedure Toggle_Advanced_Dialog
     (Editor : access Breakpoint_Editor_Record'Class);
   --  Toggle the visibility of the advanced breakpoints dialog

   procedure Update_Breakpoint_List
     (Editor : access Breakpoint_Editor_Record'Class);
   --  Update the list of breakpoints in the dialog.
   --  The list is taken from the one stored in the current debugger session.

   function Set_Location_Breakpoint
     (Editor : access Breakpoint_Editor_Record'Class; Current : Integer := -1)
      return Breakpoint_Identifier;
   --  Set the breakpoint that is currently described in the location page.
   --  If Current is not -1, then the breakpoint currently displayed at line
   --  Current is updated (first removed if some of the information has
   --  changed).

   function Set_Exception_Breakpoint
     (Editor : access Breakpoint_Editor_Record'Class; Current : Integer := -1)
      return Breakpoint_Identifier;
   --  Set the breakpoint that is currently described in the exception page.

   procedure Set_Advanced
     (Editor : access Breakpoint_Editor_Record'Class;
      Adv    : Advanced_Breakpoint_Access;
      Bpt    : Breakpoint_Identifier);
   --  Set the advanced options for the breakpoint Bpt, based on the contents
   --  of the advanced breakpoint editor Adv.

end Breakpoints_Editor;
