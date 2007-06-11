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

package body Virtual_Lists.Extensive is

   use Extensive_List_Pckg;

   -----------------------
   -- To_Extensive_List --
   -----------------------

   function To_Extensive_List (L : Extensive_List_Pckg.List)
      return Extensive_List_Component
   is
   begin
      return Extensive_List_Component'(Content => L);
   end To_Extensive_List;

   -----------
   -- First --
   -----------

   function First (List : Extensive_List_Component)
      return Virtual_List_Component_Iterator'Class
   is
   begin
      return Extensive_List_Iterator'(It => First (List.Content));
   end First;

   ------------
   -- At_End --
   ------------

   function At_End (It : Extensive_List_Iterator) return Boolean is
   begin
      return It.It = Null_Node;
   end At_End;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Extensive_List_Iterator) is
   begin
      It.It := Next (It.It);
   end Next;

   ---------
   -- Get --
   ---------

   function Get
     (It : Extensive_List_Iterator) return Data_Type
   is
   begin
      return Data (It.It);
   end Get;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Extensive_List_Component) is
   begin
      Free (List.Content);
   end Free;

end Virtual_Lists.Extensive;
