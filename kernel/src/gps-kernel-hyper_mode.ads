-----------------------------------------------------------------------
--                              G P S                                --
--                                                                   --
--                Copyright (C) 2009, AdaCore                        --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
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

--  This package allows adding hyper-mode functionality to widgets
--
--  Hyper Mode works the following way:
--
--   . The pressing of a certain set of keys (currently hard-coded to the
--     "control" keys) causes GPS to enter a mode called hyper-mode.
--   . Releasing the key causes GPS to leave the hyper-mode.
--
--  Any widget in GPS can decide to react to hyper-mode by changing their
--  behavior when hyper-mode is active. They do this through a call to
--  GPS.Kernel.Hyper_Mode.Enable_Hyper_Mode.

with Gtk.Widget; use Gtk.Widget;

package GPS.Kernel.Hyper_Mode is

   type Simple_Callback is access procedure
     (Widget : access Gtk_Widget_Record'Class);

   procedure Enable_Hyper_Mode
     (Kernel              : Kernel_Handle;
      Widget              : access Gtk_Widget_Record'Class;
      On_Hyper_Mode_Enter : Simple_Callback;
      On_Hyper_Mode_Leave : Simple_Callback);
   --  Enable hyper mode on Widget.
   --  On_Hyper_Mode_Enter will called on the widget when hyper mode is
   --  activated, and On_Hyper_Mode_Leave will be called when the hyper
   --  mode stops or the widget is destroyed.

end GPS.Kernel.Hyper_Mode;
