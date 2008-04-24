-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2007, AdaCore                 --
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

with GNAT.Regpat;        use GNAT.Regpat;
with Filesystem.Queries; use Filesystem.Queries;

with Machine_Descriptors; use Machine_Descriptors;

package body Shell_Descriptors is

   Shell_Descriptor_List   : Shell_Descriptor_Access := null;
   --  ??? Should get rid of this global variable

   --------------------------
   -- Get_Shell_Descriptor --
   --------------------------

   function Get_Shell_Descriptor
     (Nickname : String) return Shell_Descriptor_Access
   is
      Desc       : Shell_Descriptor_Access := Shell_Descriptor_List;
      Shell_Name : constant String :=
        Get_Machine_Descriptor_Access (Nickname).Desc.Shell_Name.all;
   begin
      while Desc /= null loop
         if Desc.Name.all = Shell_Name then
            return Desc;
         end if;

         Desc := Desc.Next;
      end loop;

      raise Invalid_Nickname;
   end Get_Shell_Descriptor;

   --------------------------
   -- Add_Shell_Descriptor --
   --------------------------

   procedure Add_Shell_Descriptor
     (Name                : String;
      Start_Command       : String             := "";
      Generic_Prompt      : String             := "";
      Configured_Prompt   : String             := "";
      FS                  : Filesystem_Record'Class;
      No_Echo_Command     : String             := "";
      Init_Commands       : String_List        := Null_String_List;
      Exit_Commands       : String_List        := Null_String_List;
      Cd_Command          : String             := "";
      Get_Status_Command  : String             := "";
      Get_Status_Ptrn     : String             := "")
   is
      Shell : constant Shell_Descriptor_Access := new Shell_Descriptor_Record;
      Item  : Shell_Descriptor_Access;
   begin
      Shell.all :=
        (Name             => new String'(Name),
         Filesystem       => new Filesystem_Record'Class'(FS),
         Start_Cmd        => new String'(Start_Command),
         No_Echo_Cmd      => new String'(No_Echo_Command),
         Init_Cmds        => new String_List'(Init_Commands),
         Exit_Cmds        => new String_List'(Exit_Commands),
         Cd_Cmd           => new String'(Cd_Command),
         Get_Status_Cmd   => new String'(Get_Status_Command),
         Get_Status_Ptrn  => new Pattern_Matcher'
           (Compile (Get_Status_Ptrn, Single_Line + Multiple_Lines)),
         Generic_Prompt   => new Pattern_Matcher'
           (Compile (Generic_Prompt, Single_Line + Multiple_Lines)),
         Prompt           => new Pattern_Matcher'
           (Compile (Configured_Prompt, Single_Line + Multiple_Lines)),
         Next             => null);

      Item := Shell_Descriptor_List;

      if Item = null or else Item.Name.all > Name then
         Shell.Next := Shell_Descriptor_List;
         Shell_Descriptor_List := Shell;
      else
         while Item /= null loop
            if Item.Next = null or else Item.Next.Name.all > Name then
               Shell.Next := Item.Next;
               Item.Next := Shell;
               exit;
            end if;
            Item := Item.Next;
         end loop;
      end if;
   end Add_Shell_Descriptor;

   -----------------------------
   -- Get_Nb_Shell_Descriptor --
   -----------------------------

   function Get_Nb_Shell_Descriptor return Natural is
      N : Natural;
      Desc : Shell_Descriptor_Access;
   begin
      N := 0;

      Desc := Shell_Descriptor_List;

      while Desc /= null loop
         N := N + 1;
         Desc := Desc.Next;
      end loop;

      return N;
   end Get_Nb_Shell_Descriptor;

   -------------------------------
   -- Get_Shell_Descriptor_Name --
   -------------------------------

   function Get_Shell_Descriptor_Name (N : Natural) return String is
      Desc : Shell_Descriptor_Access;
   begin
      Desc := Shell_Descriptor_List;

      for J in 2 .. N loop
         Desc := Desc.Next;
      end loop;

      return Desc.Name.all;
   end Get_Shell_Descriptor_Name;

   ------------------------------
   -- Get_Descriptor_From_Name --
   ------------------------------

   function Get_Descriptor_From_Name
     (Name : String) return Shell_Descriptor_Access
   is
      Shell_Desc : Shell_Descriptor_Access  := Shell_Descriptor_List;
   begin
      while Shell_Desc /= null loop
         exit when Shell_Desc.Name.all = Name;
         Shell_Desc := Shell_Desc.Next;
      end loop;

      return Shell_Desc;
   end Get_Descriptor_From_Name;

   -------------------------------
   -- Get_Filesystem_From_Shell --
   -------------------------------

   function Get_Filesystem_From_Shell
     (Shell : String) return Filesystem_Record'Class
   is
      Desc : Shell_Descriptor_Access := Shell_Descriptor_List;
   begin
      while Desc /= null loop
         if Desc.Name.all = Shell then
            return Desc.Filesystem.all;
         end if;

         Desc := Desc.Next;
      end loop;

      return Get_Local_Filesystem;
   end Get_Filesystem_From_Shell;

end Shell_Descriptors;
