------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2008-2019, AdaCore                     --
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

--  This package is a wrapper for Gtkada.Combo_Tool_Button, which provides
--  support for creating a button that executes GPS actions.

with Gtkada.Combo_Tool_Button;   use Gtkada.Combo_Tool_Button;
with GPS.Kernel;                 use GPS.Kernel;

package Gtkada.Action_Combo_Tool is

   type Action_Combo_Tool_Record is new Gtkada_Combo_Tool_Button_Record
      with private;
   type Action_Combo_Tool is access all Action_Combo_Tool_Record'Class;

   procedure Gtk_New
     (Self   : out Action_Combo_Tool;
      Kernel : not null access Kernel_Handle_Record'Class;
      Initial_Label  : String;
      Initial_Action : String);
   --  Create a new button with a default icon

   procedure Add_Action
     (Self    : not null access Action_Combo_Tool_Record'Class;
      Label   : String;
      Action  : String);
   --  Add a new action to the list of actions executable through Self

   procedure Remove_Action
     (Self    : not null access Action_Combo_Tool_Record'Class;
      Action  : String);
   --  Remove the action from the list

private

   type Action_Combo_Tool_Record is new Gtkada_Combo_Tool_Button_Record with
      record
         Kernel : access Kernel_Handle_Record'Class;
      end record;

end Gtkada.Action_Combo_Tool;
