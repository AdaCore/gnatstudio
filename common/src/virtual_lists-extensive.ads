-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
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

--  This package provide a simple-minded implementation of a virtual list
--  component, using a generic list.

generic

package Virtual_Lists.Extensive is

   ------------------------------
   -- Extensive_List_Component --
   ------------------------------

   package Extensive_List_Pckg is new Generic_List (Data_Type);
   --  May be used to create list for Data_Type.

   type Extensive_List_Component is new Virtual_List_Component with private;

   type Extensive_List_Iterator is new Virtual_List_Component_Iterator
   with private;

   function To_Extensive_List (L : Extensive_List_Pckg.List)
      return Extensive_List_Component;
   --  Return an extensive list corresponding to the list given in parameter.
   --  Note that no hard copy is done, so the two lists will share the same
   --  nodes.

   function First (List : Extensive_List_Component)
      return Virtual_List_Component_Iterator'Class;
   --  See inherited documentation

   function At_End (It : Extensive_List_Iterator) return Boolean;
   --  See inherited documentation

   procedure Next (It : in out Extensive_List_Iterator);
   --  See inherited documentation

   function Get
     (It : Extensive_List_Iterator) return Data_Type;
   --  See inherited documentation

   procedure Free (List : in out Extensive_List_Component);
   --  See inherited documentation

private
   type Extensive_List_Component is new Virtual_List_Component with record
      Content : Extensive_List_Pckg.List;
   end record;

   type Extensive_List_Iterator is new Virtual_List_Component_Iterator with
      record
         It : Extensive_List_Pckg.List_Node;
      end record;

end Virtual_Lists.Extensive;
