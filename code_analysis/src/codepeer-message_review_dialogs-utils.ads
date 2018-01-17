------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                       Copyright (C) 2016-2018, AdaCore                   --
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
--  Common utilities for message review dialogs.

with Gtk.Combo_Box;

package CodePeer.Message_Review_Dialogs.Utils is

   Status_Model_Label_Column : constant := 0;
   Status_Model_Value_Column : constant := 1;
   --  Positions of columns in Status_Combo_Box's model

   function Create_Status_Combo_Box
     (Active : Audit_Status_Kinds)
      return Gtk.Combo_Box.Gtk_Combo_Box;
   --  Create Combo_Box and fill it by Audit_Status_Kinds.
   --  Set box's iterator to Active

end CodePeer.Message_Review_Dialogs.Utils;
