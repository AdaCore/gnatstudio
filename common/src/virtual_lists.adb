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

with Ada.Unchecked_Deallocation;

package body Virtual_Lists is

   use Components_Pckg;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Virtual_List) is
   begin
      Free (This.Contents);
   end Free;

   ------------
   -- Concat --
   ------------

   procedure Concat (This : in out Virtual_List; List : Virtual_List) is
   begin
      Concat (This.Contents, List.Contents);
   end Concat;

   ------------
   -- Append --
   ------------

   procedure Append
     (List : in out Virtual_List; Component : Virtual_List_Component'Class) is
   begin
      Append
        (List.Contents, new Virtual_List_Component'Class'(Component));
   end Append;

   ----------
   -- Free --
   ----------

   procedure Free (Component : in out Virtual_List_Component) is
      pragma Unreferenced (Component);
   begin
      null;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (It : in out Virtual_List_Component_Iterator) is
      pragma Unreferenced (It);
   begin
      null;
   end Free;

   -----------
   -- First --
   -----------

   function First (List : Virtual_List) return Virtual_List_Iterator is
      It : Virtual_List_Iterator;
   begin
      It.Current_Component := First (List.Contents);

      if It.Current_Component /= Components_Pckg.Null_Node then
         It.Current_Iterator := new Virtual_List_Component_Iterator'Class'
           (First (Data (It.Current_Component).all));
      end if;

      while It.Current_Component /= Components_Pckg.Null_Node
        and then At_End (It.Current_Iterator.all)
      loop
         Free (It.Current_Iterator);

         It.Current_Component := Next (It.Current_Component);

         if It.Current_Component /= Components_Pckg.Null_Node then
            It.Current_Iterator := new Virtual_List_Component_Iterator'Class'
              (First (Data (It.Current_Component).all));
         end if;
      end loop;

      return It;
   end First;

   ------------
   -- At_End --
   ------------

   function At_End (It : Virtual_List_Iterator) return Boolean is
   begin
      return It.Current_Component = Components_Pckg.Null_Node;
   end At_End;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Virtual_List_Iterator) is
   begin
      pragma Assert (not At_End (It));

      Next (It.Current_Iterator.all);

      while It.Current_Component /= Components_Pckg.Null_Node
        and then At_End (It.Current_Iterator.all)
      loop
         Free (It.Current_Iterator);

         It.Current_Component := Next (It.Current_Component);

         if It.Current_Component /= Components_Pckg.Null_Node then
            It.Current_Iterator := new Virtual_List_Component_Iterator'Class'
              (First (Data (It.Current_Component).all));
         end if;
      end loop;
   end Next;

   ---------
   -- Get --
   ---------

   function Get (It : Virtual_List_Iterator) return Data_Type is
   begin
      return Get (It.Current_Iterator.all);
   end Get;

   ----------
   -- Free --
   ----------

   procedure Free (It : in out Virtual_List_Iterator) is
   begin
      Free (It.Current_Iterator);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Virtual_List_Component_Access) is
      procedure Internal_Free is new Ada.Unchecked_Deallocation
        (Virtual_List_Component'Class, Virtual_List_Component_Access);
   begin
      Free (This.all);
      Internal_Free (This);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Virtual_List_Component_Iterator_Access) is
      procedure Internal_Free is new Ada.Unchecked_Deallocation
        (Virtual_List_Component_Iterator'Class,
         Virtual_List_Component_Iterator_Access);
   begin
      Free (This.all);
      Internal_Free (This);
   end Free;

end Virtual_Lists;
