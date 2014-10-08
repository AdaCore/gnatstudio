------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2014, AdaCore                     --
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

package body Language.Abstract_Language_Tree is

   ------------
   -- Create --
   ------------

   function Create (Node : Semantic_Node'Class) return Semantic_Tree_Iterator
   is (Sem_Node_Holders.To_Holder (Node), False);

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Semantic_Tree_Iterator) is
   begin
      if It.Visited_Children then
         declare
            N : constant Semantic_Node'Class := It.Sem_Node.Element.Next;
         begin
            if N = No_Semantic_Node then
               It.Sem_Node.Replace_Element (It.Sem_Node.Element.Parent);
               It.Visited_Children := True;
            else
               It.Sem_Node.Replace_Element (N);
               It.Visited_Children := False;
            end if;
         end;
      else
         declare
            First_Child : constant Semantic_Node'Class :=
              It.Sem_Node.Element.First_Child;
         begin
            if First_Child = No_Semantic_Node then
               It.Visited_Children := True;
               Next (It);
            else
               It.Visited_Children := False;
               It.Sem_Node.Replace_Element (It.Sem_Node.Element.First_Child);
            end if;
         end;
      end if;
   end Next;

   -------------
   -- Element --
   -------------

   function Element
     (It : Semantic_Tree_Iterator) return Semantic_Node'Class is
   begin
      return It.Sem_Node.Element;
   end Element;

   --------------
   -- Has_Next --
   --------------

   function Has_Element (It : Semantic_Tree_Iterator) return Boolean is
   begin
      return (not It.Sem_Node.Is_Empty)
        and then It.Sem_Node.Element /= No_Semantic_Node;
   end Has_Element;

end Language.Abstract_Language_Tree;
