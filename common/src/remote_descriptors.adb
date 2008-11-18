-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2008, AdaCore                 --
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

with Password_Manager;           use Password_Manager;
with Ada.Characters.Handling;    use Ada.Characters.Handling;
with Ada.Unchecked_Deallocation;
with Ada.Strings.Fixed;          use Ada.Strings.Fixed;
with Basic_Types;                use Basic_Types;
with Toolchains;
with GNAT.Regpat;                use GNAT.Regpat;

package body Remote_Descriptors is

   Remote_Descriptor_List  : Remote_Descriptor_Access := null;
   --  ??? Should get rid of this global variable

   Login_Regexp : constant Pattern_Matcher :=
                    Compile ("^[^\n]*([Ll]ogin|[Nn]ame)[^\n]*: *$",
                             Multiple_Lines or Single_Line);
   --  Default regexp for login prompt

   procedure Free (Descr : in out Remote_Descriptor_Access);
   procedure Free (Prompt : in out Extra_Prompt);
   procedure Free (Prompt : in out Extra_Prompts);
   --  Free the parameter

   ----------
   -- Free --
   ----------

   procedure Free (Prompt : in out Extra_Prompt) is
   begin
      Unchecked_Free (Prompt.Ptrn);
      case Prompt.Auto_Answer is
         when True =>
            Free (Prompt.Answer);
         when False =>
            Free (Prompt.Question);
      end case;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Prompt : in out Extra_Prompts) is
   begin
      for P in Prompt'Range loop
         Free (Prompt (P));
      end loop;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Descr : in out Remote_Descriptor_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Remote_Descriptor_Record, Remote_Descriptor_Access);
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Extra_Prompts, Extra_Prompts_Access);
   begin
      if Descr /= null then
         Free (Descr.Name);
         Free (Descr.Start_Cmd);
         Free (Descr.Start_Cmd_Common_Args);
         Free (Descr.Start_Cmd_User_Args);
         Free (Descr.Send_Interrupt);
         Unchecked_Free (Descr.User_Prompt_Ptrn);
         Unchecked_Free (Descr.Password_Prompt_Ptrn);
         Unchecked_Free (Descr.Passphrase_Prompt_Ptrn);
         Free (Descr.Extra_Prompt_Array.all);
         Unchecked_Free (Descr.Extra_Prompt_Array);
         Unchecked_Free (Descr);
      end if;
   end Free;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      N : Remote_Descriptor_Access;
   begin
      while Remote_Descriptor_List /= null loop
         N := Remote_Descriptor_List.Next;
         Free (Remote_Descriptor_List);
         Remote_Descriptor_List := N;
      end loop;
   end Finalize;

   ----------------------------------
   -- Add_Remote_Access_Descriptor --
   ----------------------------------

   procedure Add_Remote_Access_Descriptor
     (Name                      : String;
      Start_Command             : String;
      Start_Command_Common_Args : String_List;
      Start_Command_User_Args   : String_List;
      Send_Interrupt            : String_Access;
      User_Prompt_Ptrn          : String_Access;
      Password_Prompt_Ptrn      : String_Access;
      Passphrase_Prompt_Ptrn    : String_Access;
      Extra_Prompt_Array        : Extra_Prompts := Null_Extra_Prompts;
      Use_Cr_Lf                 : Boolean := False;
      Use_Pipes                 : Boolean := False)
   is
      --  ??? Add max_password_prompt in parameters
      Remote          : Remote_Descriptor_Access;
      Full_Exec       : String_Access;
      Send_Intr       : String_Access;
      Password_Ptrn   : Pattern_Matcher_Access;
      Passphrase_Ptrn : Pattern_Matcher_Access;
      Login_Ptrn      : Pattern_Matcher_Access;

   begin
      Full_Exec := Toolchains.Locate_Tool_Executable (Start_Command);

      Remote := new Remote_Descriptor_Record;

      if Send_Interrupt = null or else Send_Interrupt.all = "" then
         Send_Intr := null;
      else
         Send_Intr := new String'(Send_Interrupt.all);
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
         Send_Interrupt         => Send_Intr,
         User_Prompt_Ptrn       => Login_Ptrn,
         Password_Prompt_Ptrn   => Password_Ptrn,
         Passphrase_Prompt_Ptrn => Passphrase_Ptrn,
         Extra_Prompt_Array     => new Extra_Prompts'(Extra_Prompt_Array),
         Use_Cr_Lf              => Use_Cr_Lf,
         Use_Pipes              => Use_Pipes,
         Max_Password_Prompt    => 3,
         Next                   => Remote_Descriptor_List);

      --  Only test this after creating Remote, so that we can appropriately
      --  free the memory. Otherwise we would have to duplicate the code for
      --  Free here to be sure we free all the fields.

      if Full_Exec = null
        or else Index (To_Lower (Full_Exec.all), "system32") >= Full_Exec'First
      then
         Free (Remote);
         return;
      end if;

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
