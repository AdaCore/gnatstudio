------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2017, AdaCore                     --
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

package Creation_Wizard.Full is
   procedure Add_Full_Wizard_Pages
     (Wiz          : access Project_Wizard_Record'Class;
      Name_And_Loc : access Creation_Wizard.Name_And_Location_Page'Class;
      Context      : String;
      Allow_Page   : access function (Page : String) return Boolean := null);
   --  Add the required pages to allow wizard to edit all the properties of
   --  a project.
   --  New pages can be registered through XML pages.
   --  Context indicates what project attributes should be included from the
   --  XML file describing projects (see the "hide_in" XML attribute)
   --  If Allow_Page provided use it to filter pages out

end Creation_Wizard.Full;
