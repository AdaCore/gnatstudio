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

--  This package contains remote access descriptors.

with GNAT.Expect;     use GNAT.Expect;
with GNAT.Strings;    use GNAT.Strings;

package Remote_Descriptors is

   type Extra_Prompt (Auto_Answer : Boolean := False) is record
      Ptrn : Pattern_Matcher_Access;

      case Auto_Answer is
         when True =>
            Answer : String_Access;
         when False =>
            Question : String_Access;
      end case;
   end record;
   --  Used to handle extra prompts received from a remote access tool
   --  Auto_Answer: Tells if we shall automatically send an answer to this
   --   prompt.
   --  Ptrn: The pattern to expect
   --  Answer: The automatic answer to send to the remote access tool.
   --  Question: The question GPS will ask to the user. The user's response
   --   will then be sent to the remote access tool.

   Null_Extra_Prompt : constant Extra_Prompt :=
     (Auto_Answer => True,
      Ptrn        => null,
      Answer      => null);

   --  Tool specific prompt. If Auto_Answer is set, then the GPS user will not
   --  be asked for anything, and Answer will be sent to the tool.
   --  Else, the User will be asked the User_Question.
   type Extra_Prompts is array (Natural range <>) of Extra_Prompt;
   Null_Extra_Prompts : constant Extra_Prompts (1 .. 0)
     := (others => Null_Extra_Prompt);

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
      Use_Pipes                 : Boolean := False);
   --  Adds a new Remote Access Descriptor
   --  Name : identifier of this descriptor
   --  Start_Command : command used to launch the remote access utility
   --  Start_Command_Common_Args : arguments always provided to this utility
   --  Start_Command_User_Arg    : if user is specified, this argument will
   --   be used (%u replaced by actual user)
   --  User_Prompt_Ptrn          : regular expression for user prompt
   --                              if null, the default user prompt is used
   --  Password_Prompt_Ptrn      : regular expression for password prompt
   --                              if null, the default password prompt is used
   --  Passphrase_Prompt_Ptrn    : regular expression for passphrases. This
   --                              expression shall isolate the key_id with
   --                              parenthesis
   --  Extra_Prompt_Array        : extra specific prompts.
   --  Use_Cr_Lf                 : tell if CR character needs to be added when
   --                               sending commands to the tool.
   --  Use_Pipes                 : tell if the tool is launched in pipe or tty
   --                               mode. Only applicable on Windows (no effect
   --                               on other machines)

   type Extra_Prompts_Access is access all Extra_Prompts;

   type Remote_Descriptor_Record;
   type Remote_Descriptor_Access is access all Remote_Descriptor_Record;

   type Remote_Descriptor_Record is record
      Name                   : String_Access            := null;
      Start_Cmd              : String_Access            := null;
      Start_Cmd_Common_Args  : String_List_Access       := null;
      Start_Cmd_User_Args    : String_List_Access       := null;
      Send_Interrupt         : String_Access            := null;
      User_Prompt_Ptrn       : Pattern_Matcher_Access   := null;
      Password_Prompt_Ptrn   : Pattern_Matcher_Access   := null;
      Passphrase_Prompt_Ptrn : Pattern_Matcher_Access   := null;
      Extra_Prompt_Array     : Extra_Prompts_Access     := null;
      Use_Cr_Lf              : Boolean                  := False;
      Use_Pipes              : Boolean                  := False;
      Max_Password_Prompt    : Natural                  := 3;
      Next                   : Remote_Descriptor_Access := null;
   end record;

   function Get_Descriptor_From_Name
     (Name : String) return Remote_Descriptor_Access;
   --  Return a descriptor from its name.
   --  Return null if no descriptor was found.
   --  Caller must not free the result.

   function Get_Nb_Remote_Access_Descriptor return Natural;
   --  Get the total number of remote access descriptor configured

   function Get_Remote_Access_Name (N : Natural) return String;
   --  Get the Nth remote access descriptor name

end Remote_Descriptors;
