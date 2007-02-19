-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006-2007                      --
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

with Password_Manager;     use Password_Manager;

with GNAT.Regpat;          use GNAT.Regpat;
with GNAT.OS_Lib;

package body Remote_Descriptors is

   Remote_Descriptor_List  : Remote_Descriptor_Access := null;
   --  ??? Should get rid of this global variable

   Login_Regexp : constant Pattern_Matcher :=
                    Compile ("^[^\n]*([Ll]ogin|[Nn]ame)[^\n]*: *$",
                             Multiple_Lines or Single_Line);
   --  Default regexp for login prompt

   ----------------------------------
   -- Add_Remote_Access_Descriptor --
   ----------------------------------

   procedure Add_Remote_Access_Descriptor
     (Name                      : String;
      Start_Command             : String;
      Start_Command_Common_Args : String_List;
      Start_Command_User_Args   : String_List;
      User_Prompt_Ptrn          : String_Access;
      Password_Prompt_Ptrn      : String_Access;
      Passphrase_Prompt_Ptrn    : String_Access;
      Extra_Prompt_Array        : Extra_Prompts := Null_Extra_Prompts;
      Use_Cr_Lf                 : Boolean := False;
      Use_Pipes                 : Boolean := False)
   is
      --  ??? Add max_password_prompt in parameters
      Remote          : constant Remote_Descriptor_Access :=
                          new Remote_Descriptor_Record;
      Full_Exec       : String_Access;
      Password_Ptrn   : Pattern_Matcher_Access;
      Passphrase_Ptrn : Pattern_Matcher_Access;
      Login_Ptrn      : Pattern_Matcher_Access;

   begin
      Full_Exec := GNAT.OS_Lib.Locate_Exec_On_Path (Start_Command);

      if Full_Exec = null then
         return;
      end if;

      if User_Prompt_Ptrn = null then
         Login_Ptrn := new Pattern_Matcher'(Login_Regexp);
      else
         Login_Ptrn := new Pattern_Matcher'(Compile (
           User_Prompt_Ptrn.all,
           Single_Line + Multiple_Lines));
      end if;

      if Password_Prompt_Ptrn = null then
         Password_Ptrn := new Pattern_Matcher'(Get_Default_Password_Regexp);
      else
         Password_Ptrn := new Pattern_Matcher'(Compile (
           Password_Prompt_Ptrn.all,
           Single_Line + Multiple_Lines));
      end if;

      if Passphrase_Prompt_Ptrn = null then
         Passphrase_Ptrn :=
           new Pattern_Matcher'(Get_Default_Passphrase_Regexp);
      else
         Passphrase_Ptrn := new Pattern_Matcher'(Compile (
           Passphrase_Prompt_Ptrn.all,
           Single_Line + Multiple_Lines));
      end if;

      Remote.all :=
        (Name => new String'(Name),
         Start_Cmd              => Full_Exec,
         Start_Cmd_Common_Args  => new String_List'(Start_Command_Common_Args),
         Start_Cmd_User_Args    => new String_List'(Start_Command_User_Args),
         User_Prompt_Ptrn       => Login_Ptrn,
         Password_Prompt_Ptrn   => Password_Ptrn,
         Passphrase_Prompt_Ptrn => Passphrase_Ptrn,
         Extra_Prompt_Array     => new Extra_Prompts'(Extra_Prompt_Array),
         Use_Cr_Lf              => Use_Cr_Lf,
         Use_Pipes              => Use_Pipes,
         Max_Password_Prompt    => 3,
         Next                   => Remote_Descriptor_List);
      Remote_Descriptor_List := Remote;
   end Add_Remote_Access_Descriptor;

   ------------------------------
   -- Get_Descriptor_From_Name --
   ------------------------------

   function Get_Descriptor_From_Name
     (Name : String) return Remote_Descriptor_Access
   is
      Remote_Desc : Remote_Descriptor_Access;
   begin
      Remote_Desc := Remote_Descriptor_List;

      while Remote_Desc /= null loop
         exit when Remote_Desc.Name.all = Name;

         Remote_Desc := Remote_Desc.Next;
      end loop;

      return Remote_Desc;
   end Get_Descriptor_From_Name;

   -------------------------------------
   -- Get_Nb_Remote_Access_Descriptor --
   -------------------------------------

   function Get_Nb_Remote_Access_Descriptor return Natural is
      N : Natural;
      Desc : Remote_Descriptor_Access;
   begin
      N := 0;
      Desc := Remote_Descriptor_List;

      while Desc /= null loop
         N := N + 1;
         Desc := Desc.Next;
      end loop;

      return N;
   end Get_Nb_Remote_Access_Descriptor;

   ----------------------------
   -- Get_Remote_Access_Name --
   ----------------------------

   function Get_Remote_Access_Name (N : Natural) return String is
      Desc : Remote_Descriptor_Access;
   begin
      Desc := Remote_Descriptor_List;

      for J in 2 .. N loop
         Desc := Desc.Next;
      end loop;

      return Desc.Name.all;
   end Get_Remote_Access_Name;

end Remote_Descriptors;
