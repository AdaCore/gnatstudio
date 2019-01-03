------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

--  This packages provides utility subprograms to display a welcome dialog.
--  This dialog can display different options, like creating a a new project
--  or opening an existing one.

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gtk.Window;            use Gtk.Window;
with GPS.Kernel;            use GPS.Kernel;

package Welcome_Dialogs is

   type Welcome_Dialog_Response is (Project_Loaded, Quit_GPS);
   --  Type representing the different responses of a welcome dialog.
   --
   --  . Project_Loaded is sent when a project (either a new one or an existing
   --    one) has been loaded.
   --
   --  . Quit_GPS is sent when the welcome dialog has been cancelled by the
   --    user.

   type Welcome_Dialog_Action is tagged private;
   type Welcome_Dialog_Action_Array is
     array (Integer range <>) of Welcome_Dialog_Action;
   --  Type representing actions that can be displayed in a welcome dialog
   --  (e.g: Open a project).

   type Welcome_Dialog_Action_Callback is access function
     (Kernel : not null access Kernel_Handle_Record'Class;
      Parent : not null access Gtk_Window_Record'Class) return Boolean;
   --  Type of the callbacks called when a given action is selected by the
   --  user (e.g: a callback that displays a dialog used to open a project).
   --
   --  Parent is the actual welcome dialog from which this callback has been
   --  caled. Call Set_Transient_Window with it if the callkack opens a dialog
   --  so that the created dialog stays on top of the welcome dialog.
   --
   --  Return False if the action was cancelled by the user, True otherwise.

   function Create
     (Callback  : not null Welcome_Dialog_Action_Callback;
      Label     : String;
      Icon_Name : String) return Welcome_Dialog_Action;
   --  Create a new welcome dialog action.

   function Display_Welcome_Dialog
     (Kernel  : not null access Kernel_Handle_Record'Class;
      Actions : Welcome_Dialog_Action_Array)
      return Welcome_Dialog_Response;
   --  Display a welcome dialog listing the given Actions and return the user's
   --  response.

private

   type Welcome_Dialog_Action is tagged record
      Callback  : Welcome_Dialog_Action_Callback;
      Label     : Unbounded_String;
      Icon_Name : Unbounded_String;
   end record;

end Welcome_Dialogs;
