-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
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

--  This package provides facilities for customizing menus,
--  keyboard shortcuts, and so on, in GPS.

--  Here is the format for the custom menus file :
--
--  <Custom>
--
--    <Submenu>
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
--    </Submenu>
--
--    <Submenu>
--      <Title>Title for toplevel menu number 2</Title>
--             (...)
--    </Submenu>
--
--    <Button>
--      <Title>Button label/tooltip</Title>
--      <Pixmap>location of pixmap (xpm) file</Pixmap>
--      <Action>command 1</Action>
--    </Button>
--
--  </Custom>
--
--  Titles like "_File", "Open from _Project", ... can now be used,
--  The underscores identify the accelerator keys.
--
--  You can add items or submenus to existing menus :
--
--   This adds an item named "Robert" to the "Edit" menu.
--      <Submenu>
--         <Title>Edit</Title>
--         <Menuitem>
--           <Title>Robert</Title>
--         </Menuitem>
--      </Submenu>
--
--  This adds a submenu named "Olga" to the "Edit" menu.
--  <Submenu>
--     <Title>Edit/Olga</Title>
--     <Menuitem>
--       <Title>title 1</Title>
--     </Menuitem>
--  </Submenu>
--
--  Adding an item with an empty title or a null title will insert
--  a menu separator.
--
--  Here is a non-exhaustive list of possible enhancements here
--    -> facilities for creating toolbar buttons
--    -> be able to position the custom menus relatively to
--       other menus
--    -> add icons to the menus (using stock icons)
--    -> make the file syntax a little more flexible
--       (right now it's very case-sensitive, and so on)
--    -> Use GPS commands as well as shell commands
--    -> implement a GUI to create those menus graphically

with Glide_Kernel;

package Custom_Module is

   Custom_Module_ID   : Glide_Kernel.Module_ID;
   Custom_Module_Name : constant String := "Custom";

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Register the module into the list

end Custom_Module;
