-----------------------------------------------------------------------
--              GtkAda - Ada95 binding for Gtk+/Gnome                --
--                                                                   --
--               Copyright (C) 2008, AdaCore                         --
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
with Gtk.Check_Button;

package Gtkada.Check_Button is

   type Gtkada_Check_Button_Record is new
     Gtk.Check_Button.Gtk_Check_Button_Record with private;
   type Gtkada_Check_Button is access all Gtkada_Check_Button_Record'Class;
   --  This version of the Check Button is a variant of the original gtk+
   --  widget, in that is handles 3 states instead of two: Checked/Unchecked/
   --  Checked_By_Default. This allows to have a visual indication of
   --  check buttons that are activated by default, but can be still forced by
   --  the user (thus remaining active even if its default state change).

   procedure Gtk_New
     (Check   : out Gtkada_Check_Button;
      Label   : UTF8_String := "";
      Default : Boolean := False);
   --  Create a new Gtkada_Check_Button

   procedure Initialize
     (Check : access Gtkada_Check_Button_Record'Class;
      Label : UTF8_String := "";
      Default : Boolean := False);
   --  Initialize an existing Check_Button_Record

   procedure Set_Default
     (Check : access Gtkada_Check_Button_Record;
      State : Boolean);
   --  Set the default state of the check button

   function Get_Default
     (Check : access Gtkada_Check_Button_Record) return Boolean;
   --  Get the default state of the check button

   procedure Set_Active
     (Check : access Gtkada_Check_Button_Record;
      State : Boolean);
   --  Force the state of the check button

   type State_Type is (State_Checked, State_Checked_Default, State_Unchecked);

   function Get_State
     (Check : access Gtkada_Check_Button_Record) return State_Type;
   --  Get the current button state.

   procedure Clicked
     (Check : access Gtkada_Check_Button_Record);
   --  Sends the 'clicked' signal.

private

   type Gtkada_Check_Button_Record is new
     Gtk.Check_Button.Gtk_Check_Button_Record with record
      State     : State_Type;
      Default   : Boolean;
   end record;

end Gtkada.Check_Button;
