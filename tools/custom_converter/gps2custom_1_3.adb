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

with Glib.Xml_Int;              use Glib.Xml_Int;
with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Text_IO;               use Ada.Text_IO;
with Interfaces.C.Strings;      use Interfaces.C.Strings;

procedure GPS2Custom_1_3 is

   function Executable_Location return String;
   --  Return the name of the parent directory where the executable is stored.
   --  The executable must be located in a directory called "bin". Thus, if
   --  the executable is stored in directory "/foo/bar/bin", this routine
   --  returns "/foo/bar/". If the executable is not stored in a directory
   --  "bin" (casing is unimportant) then a null string is returned.

   File_Node, Child  : Node_Ptr;
   Actions           : Node_Ptr;

   function Is_Directory_Separator (C : Character) return Boolean;
   --  Returns True if C is a directory separator

   procedure Parse_Dir (Directory : String);
   --  Convert all writable custom files found in Directory

   procedure Process_Child (Node : Node_Ptr);
   --  Process a given XML node

   ----------------------------
   -- Is_Directory_Separator --
   ----------------------------

   function Is_Directory_Separator (C : Character) return Boolean is
   begin
      return C = Directory_Separator or else C = '/';
   end Is_Directory_Separator;

   -------------------------
   -- Executable_Location --
   -------------------------

   type chars_ptr_ptr is access all chars_ptr;

   Argv : chars_ptr_ptr;
   pragma Import (C, Argv, "gnat_argv");

   function Executable_Location return String is
      Exec_Name : constant String := Value (Argv.all);

      function Get_Install_Dir (S : String) return String;
      --  S is the executable name preceeded by the absolute or relative
      --  path, e.g. "c:\usr\bin\gcc.exe" or "..\bin\gcc". Returns the absolute
      --  or relative directory where "bin" lies (in the example "C:\usr"
      --  or ".."). If the executable is not a "bin" directory, return "".

      ---------------------
      -- Get_Install_Dir --
      ---------------------

      function Get_Install_Dir (S : String) return String is
         Exec      : String  := S;
         Path_Last : Integer := 0;

      begin
         for J in reverse Exec'Range loop
            if Is_Directory_Separator (Exec (J)) then
               Path_Last := J - 1;
               exit;
            end if;
         end loop;

         if Path_Last >= Exec'First + 2 then
            To_Lower (Exec (Path_Last - 2 .. Path_Last));
         end if;

         if Path_Last < Exec'First + 2
           or else Exec (Path_Last - 2 .. Path_Last) /= "bin"
           or else (Path_Last - 3 >= Exec'First
                    and then not Is_Directory_Separator (Exec (Path_Last - 3)))
         then
            return "";
         end if;

         return Exec (Exec'First .. Path_Last - 4);
      end Get_Install_Dir;

   --  Beginning of Executable_Location

   begin
      --  First determine if a path prefix was placed in front of the
      --  executable name.

      for J in reverse Exec_Name'Range loop
         if Is_Directory_Separator (Exec_Name (J)) then
            return Get_Install_Dir (Exec_Name);
         end if;
      end loop;

      --  If you are here, the user has typed the executable name with no
      --  directory prefix.

      return Get_Install_Dir (GNAT.OS_Lib.Locate_Exec_On_Path (Exec_Name).all);
   end Executable_Location;

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

   ---------------
   -- Parse_Dir --
   ---------------

   procedure Parse_Dir (Directory : String) is
      Dir               : Dir_Type;
      File              : String (1 .. 1024);
      Last              : Integer;
      A                 : Node_Ptr;

   begin
      Open (Dir, Directory);

      loop
         Read (Dir, File, Last);
         exit when Last = 0;

         declare
            F : constant String := Directory & File (1 .. Last);
         begin
            if Is_Regular_File (F) and then Is_Writable_File (F) then
               Put_Line ("Parsing " & F);
               File_Node := Parse (F);
               Actions := null;

               if File_Node = null then
                  Put_Line ("Error while parsing file " & F);
                  Put_Line ("No conversion done for that file");
               else
                  Child := File_Node.Child;

                  while Child /= null loop
                     Process_Child (Child);
                     Child := Child.Next;
                  end loop;

                  if Actions /= null then
                     A := Actions;

                     while A.Next /= null loop
                        A := A.Next;
                     end loop;

                     A.Next := File_Node.Child;
                     File_Node.Child := Actions;
                  end if;

                  Print (File_Node, F);
                  Free (File_Node);
               end if;
            end if;
         end;
      end loop;

      Close (Dir);

   exception
      when Directory_Error =>
         Put_Line ("Error while opening directory " & Directory);
         Put_Line ("No conversion done for that directory");
   end Parse_Dir;

   Home              : constant String := Getenv ("HOME").all;
   Custom_Dir        : constant String := Home & "/.gps/customize/";
   Global_Custom_Dir : constant String :=
     Executable_Location & "/share/gps/customize/";

begin
   Parse_Dir (Custom_Dir);
   Parse_Dir (Global_Custom_Dir);
end GPS2Custom_1_3;
