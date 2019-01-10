-----------------------------------------------------------------------
--                  Copyright (C) 2009-2019, AdaCore                 --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Ada.Command_Line;         use Ada.Command_Line;
with Ada.Containers.Ordered_Maps;
with Ada.Containers.Ordered_Sets;
with Ada.Text_IO.Text_Streams; use Ada.Text_IO;
with Input_Sources.File;       use Input_Sources.File;
with DOM.Core.Documents;       use DOM.Core, DOM.Core.Documents;
with DOM.Core.Nodes;           use DOM.Core.Nodes;
with DOM.Readers;              use DOM.Readers;
with System;                   use System;

procedure SortXML is
   File_Name : constant String := Argument (1);
   Attr_Name : constant String := Argument (2);

   function Before (N1, N2 : Node) return Boolean;
   function Get_Attribute (N : Node; Attribute : String) return String;

   procedure Sort_Children (N : Node; Offset : in out Natural);
   --  Sort the children of N.
   --  Offset is number of nodes in XML before N, including subnodes.
   --  After call Offset = Offset + 1 + number of subnodes of N

   package Position_Maps is new Ada.Containers.Ordered_Maps
     (Key_Type     => Address,
      Element_Type => Positive);
   use Position_Maps;

   Position_Map : Map;
   --  This map contains position of node for each Node address

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute (N : Node; Attribute : String) return String is
      Attr : constant Node := Get_Named_Item (Attributes (N), Attribute);
   begin
      if Attr = null then
         return "";
      else
         return Node_Value (Attr);
      end if;
   end Get_Attribute;

   ------------
   -- Before --
   ------------

   function Before (N1, N2 : Node) return Boolean is
      Name1 : constant String := Node_Name (N1);
      Name2 : constant String := Node_Name (N2);
   begin
      if Name1 /= Name2 then
         return Name1 < Name2;

      else
         declare
            Att1 : constant String := Get_Attribute (N1, Attr_Name);
            Att2 : constant String := Get_Attribute (N2, Attr_Name);
         begin
            if Att1 = Att2 then
               if First_Child (N1) /= null
                 and then First_Child (N2) /= null
               then
                  return Before (First_Child (N1), First_Child (N2));
               else
                  --  Compare original positions of each node
                  return Position_Map.Element (N1.all'Address)
                    < Position_Map.Element (N2.all'Address);
               end if;
            else
               return Att1 < Att2;
            end if;
         end;
      end if;
   end Before;

   package Node_Sets is new Ada.Containers.Ordered_Sets (Node, Before, "=");
   use Node_Sets;

   -------------------
   -- Sort_Children --
   -------------------

   procedure Sort_Children (N : Node; Offset : in out Natural) is
      Child : Node := First_Child (N);
      Pos   : Positive := Offset + 1;  --  Offset for Child
      Set   : Node_Sets.Set;
      C     : Node_Sets.Cursor;
   begin
      while Child /= null loop
         Position_Map.Insert (Child.all'Address, Pos);
         if Node_Type (Child) = Element_Node then
            Sort_Children (Child, Pos);
         else
            Pos := Pos + 1;
         end if;
         Set.Insert (Child);
         Child := Next_Sibling (Child);
      end loop;

      C := First (Set);
      while Has_Element (C) loop
         Child := Node_Sets.Element (C);
         Child := Append_Child (N, Remove_Child (N, Child));
         Next (C);
      end loop;

      Offset := Pos;
   end Sort_Children;

   Input  : File_Input;
   Reader : Tree_Reader;
   Root   : Node;
   File   : File_Type;
   Offset : Natural := 0;
begin
   Open (File_Name, Input);
   Parse (Reader, Input);
   Close (Input);

   Root := Get_Element (Get_Tree (Reader));
   Sort_Children (Root, Offset);

   Create (File, Out_File, File_Name);
   Write
     (Stream                => Ada.Text_IO.Text_Streams.Stream (File),
      N                     => Get_Tree (Reader),
      Print_Comments        => True,
      Print_XML_Declaration => True,
      With_URI              => False,
      Pretty_Print          => True);
   Close (File);

   Free (Reader);
end SortXML;
