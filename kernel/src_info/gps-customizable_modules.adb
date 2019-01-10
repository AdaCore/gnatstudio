------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                        Copyright (C) 2013-2019, AdaCore                  --
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

package body GPS.Customizable_Modules is

   ----------------------------------
   -- Execute_Customization_String --
   ----------------------------------

   procedure Execute_Customization_String
     (Kernel : access Core_Kernel_Record'Class;
      File   : GNATCOLL.VFS.Virtual_File;
      Node   : Node_Ptr;
      Level  : Customization_Level)
   is
      use Abstract_Module_List;
      List    : constant Abstract_Module_List.List :=
        Kernel.Module_List (Customizable_Module_Record'Tag);
      Current : Abstract_Module_List.Cursor;
      Tmp     : Node_Ptr := Node;
      Tmp2    : Node_Ptr;

   begin
      --  Parse the nodes in the XML file one after the other, and for each
      --  traverse the list of modules for it.
      --  It is less efficient than passing the whole file to each module, but
      --  is necessary to properly handle themes:
      --  If we have    <action>...</action>
      --                <theme> ... ref to the action ... </theme>
      --  we need to be sure that the action has been created before processing
      --  the theme itself, and that would depend on the order in which the
      --  modules were registered (DA15-003)

      while Tmp /= null loop
         Tmp2 := Tmp.Next;
         Tmp.Next := null;

         Current := Abstract_Module_List.First (List);
         while Has_Element (Current) loop
            Customizable_Module_Record'Class
              (Abstract_Module_List.Element (Current).all).Customize
                (File, Tmp, Level);

            Current := Abstract_Module_List.Next (Current);
         end loop;

         Tmp.Next := Tmp2;
         Tmp := Tmp.Next;
      end loop;
   end Execute_Customization_String;

end GPS.Customizable_Modules;
