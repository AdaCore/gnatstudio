-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

--  This package provides facilities for customizing menus,
--  keyboard shortcuts, and so on, in GPS.

--  Here is the format for the custom menus file :
--
--  <Custom>
--
--    <Menu>
--      <Title>Title for toplevel menu</Title>
--
--      <Menuitem>
--        <Title>title 1</Title>
--        <Shortcut>keyboard shortcut</Shortcut>
--        <Action>command 1</Action>
--        <Action>command 2</Action>
--      </Menuitem>
--
--      <Submenu>
--        <Title>title for submenu</Title>
--        <Menuitem>
--          <Title> (etc..)
--        </Menuitem>
--
--        <Submenu>
--               (...)
--        </Submenu>
--      </Submenu>
--    </Menu>
--
--    <Menu>
--      <Title>Title for toplevel menu number 2</Title>
--             (...)
--    </Menu>
--
--  </Custom>

--  ??? Here is a non-exhaustive list of possible enhancements here
--    -> facilities for creating toolbar buttons
--    -> be able to add custom menu items to existing menus
--    -> be able to position the custom menus relatively to
--       other menus
--    -> add icons to the menus ?
--    -> make the file syntax a little more flexible
--       (right now it's very case-sensitive, and so on)
--    -> Use GPS commands instead of shell commands
--    -> implement a GUI to create those menus graphically

with Glide_Kernel;

package Custom_Module is

   Custom_Module_ID   : Glide_Kernel.Module_ID;
   Custom_Module_Name : constant String := "Unit_Testing";

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

end Custom_Module;
