-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                    Copyright (C) 2010, AdaCore                    --
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

package body GPS.Kernel.Messages.Legacy is

   use Ada.Strings.Unbounded;
   use Basic_Types;
   use Category_Maps;
   use File_Maps;
   use Node_Vectors;

   ---------------------
   -- Add_Action_Item --
   ---------------------

   procedure Add_Action_Item
     (Kernel    : not null access Kernel_Handle_Record'Class;
      Category  : String;
      File      : GNATCOLL.VFS.Virtual_File;
      Line      : Natural;
      Column    : Natural;
      Message   : String;
      Action    : GPS.Kernel.Standard_Hooks.Action_Item)
   is
      Container          : constant Messages_Container_Access :=
        Get_Messages_Container (Kernel);
      Category_Position  : Category_Maps.Cursor;
      Category_Node      : Node_Access;
      File_Position      : File_Maps.Cursor;
      File_Node          : Node_Access;
      Message_Position   : Node_Vectors.Cursor;
      Message_Node       : Message_Access;
      Secondary_Position : Node_Vectors.Cursor;

   begin
      --  Resolve category node

      Category_Position :=
        Container.Category_Map.Find (To_Unbounded_String (Category));

      if not Has_Element (Category_Position) then
         return;
      end if;

      Category_Node := Element (Category_Position);

      --  Resolve file node

      File_Position := Category_Node.File_Map.Find (File);

      if not Has_Element (File_Position) then
         return;
      end if;

      File_Node := Element (File_Position);

      --  Look for message at the specified position with spceified text

      Message_Position := File_Node.Children.First;

      Primary_Messages_Loop :
      while Has_Element (Message_Position) loop
         Message_Node := Message_Access (Element (Message_Position));

         if Message_Node.Line = Line
           and then Message_Node.Column = Visible_Column_Type (Column)
         then
            exit Primary_Messages_Loop when Message_Node.Get_Text = Message;

            --  Looking around secondary messages.

            Secondary_Position := Message_Node.Children.First;

            while Has_Element (Secondary_Position) loop
               exit Primary_Messages_Loop when
                 Message_Access (Element (Secondary_Position)).Get_Text
                   = Message;

               Next (Secondary_Position);
            end loop;

         end if;

         Message_Node := null;
         Next (Message_Position);
      end loop Primary_Messages_Loop;

      if Message_Node /= null then
         Message_Node.Set_Action (Action_Item (Action));
      end if;
   end Add_Action_Item;

   --------------------
   -- Category_Count --
   --------------------

   function Category_Count
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Category : String) return Natural
   is
      Container         : constant Messages_Container_Access :=
        Get_Messages_Container (Kernel);
      Category_Position : constant Category_Maps.Cursor :=
        Container.Category_Map.Find (To_Unbounded_String (Category));

   begin
      if Has_Element (Category_Position) then
         return Natural (Element (Category_Position).Children.Length);

      else
         return 0;
      end if;
   end Category_Count;

   --------------------
   -- Get_Message_At --
   --------------------

   function Get_Message_At
     (Self     : not null access constant Messages_Container'Class;
      Category : String;
      File     : GNATCOLL.VFS.Virtual_File;
      Line     : Natural;
      Column   : Basic_Types.Visible_Column_Type)
      return Message_Access
   is
      Category_Position : constant Category_Maps.Cursor :=
        Self.Category_Map.Find (To_Unbounded_String (Category));
      File_Position     : File_Maps.Cursor;
      Message_Position  : Node_Vectors.Cursor;
      Message           : Node_Access;

   begin
      if not Has_Element (Category_Position) then
         return null;
      end if;

      File_Position := Element (Category_Position).File_Map.Find (File);

      if not Has_Element (File_Position) then
         return null;
      end if;

      Message_Position := Element (File_Position).Children.Last;
      --  Go from the last message to first one to satisfy subprogram's
      --  semantic.

      while Has_Element (Message_Position) loop
         Message := Element (Message_Position);

         if Message.Line = Line
           and then (Column = 0 or else Message.Column = Column)
         then
            return Message_Access (Message);
         end if;

         Previous (Message_Position);
      end loop;

      return null;
   end Get_Message_At;

end GPS.Kernel.Messages.Legacy;
