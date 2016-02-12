------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2006-2016, AdaCore                     --
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

   overriding function First (List : Extensive_List_Component)
      return Virtual_List_Component_Iterator'Class
   is
   begin
      return Extensive_List_Iterator'(It => First (List.Content));
   end First;

   ------------
   -- At_End --
   ------------

   overriding function At_End (It : Extensive_List_Iterator) return Boolean is
   begin
      return It.It = Null_Node;
   end At_End;

   ----------
   -- Next --
   ----------

   overriding procedure Next (It : in out Extensive_List_Iterator) is
   begin
      It.It := Next (It.It);
   end Next;

   ---------
   -- Get --
   ---------

   overriding function Get
     (It : Extensive_List_Iterator) return Data_Type
   is
      Result : Data_Type := Data (It.It);
   begin
      Copy_On_Get (Result);

      return Result;
   end Get;

   ----------
   -- Free --
   ----------

   overriding procedure Free (List : in out Extensive_List_Component) is
   begin
      Free (List.Content);
   end Free;

end Virtual_Lists.Extensive;
