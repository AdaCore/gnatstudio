-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
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

with Glib.Xml_Int;   use Glib.Xml_Int;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;    use GNAT.OS_Lib;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;    use Ada.Text_IO;

procedure GPS2Custom_1_3 is

   Home : constant String := Getenv ("HOME").all;
   Custom_Dir : constant String := Home & "/.gps/customize/";
   Dir : Dir_Type;
   File : String (1 .. 1024);
   Last : Integer;
   File_Node, Child : Node_Ptr;

   Actions : Node_Ptr;

   procedure Process_Child (Node : Node_Ptr);
   --  Process a given XML node

   -------------------
   -- Process_Child --
   -------------------

   procedure Process_Child (Node : Node_Ptr) is
      Current_Child, Current_Child_Child,
         Action_Child, Next, Previous : Node_Ptr;
      Action : String_Access;
   begin
      if To_Lower (Node.Tag.all) = "language" then
         null;

      elsif To_Lower (Node.Tag.all) = "submenu" then
         Current_Child := Node.Child;

         if To_Lower (Current_Child.Tag.all) = "title" then
            Current_Child_Child := Current_Child.Next;

            while Current_Child_Child /= null loop
               Process_Child (Current_Child_Child);
               Current_Child_Child := Current_Child_Child.Next;
            end loop;
         end if;

      elsif To_Lower (Node.Tag.all) = "menu_item"
        or else To_Lower (Node.Tag.all) = "toolbar_item"
      then
         if To_Lower (Node.Tag.all) = "menu_item" then
            Free (Node.Tag);
            Node.Tag := new String'("menu");
         else
            Free (Node.Tag);
            Node.Tag := new String'("button");
         end if;

         Actions := new Glib.Xml_Int.Node'
           (Tag => new String'("action"),
            Attributes => null,
            Value => null,
            Parent => File_Node,
            Child => null,
            Next => Actions,
            Specific_Data => 1);
         Action_Child := null;

         Current_Child := Node.Child;
         Previous := null;
         Action := null;

         while Current_Child /= null loop
            Next := Current_Child.Next;

            if To_Lower (Current_Child.Tag.all) = "title" then
               Free (Action);
               Action := new String'(Current_Child.Value.all);
               Set_Attribute (Actions, "name", Action.all);
               Previous := Current_Child;
               Previous.Next := null;
               Set_Attribute (Node, "action", Action.all);

            elsif To_Lower (Current_Child.Tag.all) = "action"
              or else To_Lower (Current_Child.Tag.all) = "gps_action"
            then
               if To_Lower (Current_Child.Tag.all) = "action" then
                  Free (Current_Child.Tag);
                  Current_Child.Tag := new String'("external");
               else
                  Free (Current_Child.Tag);
                  Current_Child.Tag := new String'("shell");
               end if;

               if Action_Child = null then
                  Actions.Child := Current_Child;
               else
                  Action_Child.Next := Current_Child;
               end if;

               Action_Child := Current_Child;
            end if;

            Current_Child := Next;
         end loop;
      end if;
   end Process_Child;

begin
   Open (Dir, Custom_Dir);

   loop
      Read (Dir, File, Last);
      exit when Last = 0;

      if Is_Regular_File (Custom_Dir & File (1 .. Last)) then
         Put_Line ("Parsing " & Custom_Dir & File (1 .. Last));
         File_Node := Parse (Custom_Dir & File (1 .. Last));
         Actions := null;

         if File_Node = null then
            Put_Line ("Error while parsing file " & File (1 .. Last));
            Put_Line ("No conversion done for that file");
         else
            Child := File_Node.Child;

            while Child /= null loop
               Process_Child (Child);
               Child := Child.Next;
            end loop;

            if Actions /= null then
               declare
                  A : Node_Ptr := Actions;
               begin
                  while A.Next /= null loop
                     A := A.Next;
                  end loop;

                  A.Next := File_Node.Child;
                  File_Node.Child := Actions;
               end;
            end if;

            Print (File_Node, Custom_Dir & File (1 .. Last));
            Free (File_Node);
         end if;
      end if;
   end loop;

   Close (Dir);
end GPS2Custom_1_3;
