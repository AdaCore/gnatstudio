------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2018-2019, AdaCore                     --
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

--  Gtk/GUI support for Project templates
--  NOTE: this should remain independent from the GPS Kernel, so that
--  it can be reused in a stand-alone executable.

with Gtk.Window;                       use Gtk.Window;

with Project_Templates;                use Project_Templates;
with Project_Templates.Script_Objects; use Project_Templates.Script_Objects;

package Project_Templates.GUI is

   type Template_Script_Object is record
      Project  : Project_Template;
      Object   : Script_Object;
   end record;

   package Templates_Script_Objects_List is
      new Ada.Containers.Doubly_Linked_Lists (Template_Script_Object);

   procedure Install_Template
     (Templates     : Templates_Script_Objects_List.List;
      Parent        : not null access Gtk_Window_Record'Class;
      Chosen        : out Template_Script_Object;
      Installed     : out Boolean;
      Dir           : out Virtual_File;
      Project       : out Virtual_File;
      Errors        : out Unbounded_String;
      Default_Label : String := "");
   --  Read templates in Templates, offer a dialog to select a template and
   --  fill in the fields, and select a target directory.
   --  Parent is set to be the transient window of the spawned dialog.
   --  If the user validates the choice, then install the template in the
   --  selected location, and Installed is set to True, Dir contains
   --  the target directory, Project the deployed project file, and
   --  Chosen the chosen template.
   --  If any errors are encountered, they are listed in Errors.
   --  If Default_Label is specified and if a template has the same label, it
   --  gets selected by default.

end Project_Templates.GUI;
