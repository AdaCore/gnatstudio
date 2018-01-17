------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

--  This package defines the required interface for g-exttre operations.
--  This is used for uncoupling the GPS remote mode configuration (in the
--  remote project) and GNAT.Expect.TTY.Remote (in the common project).

with GNAT.Strings;         use GNAT.Strings;
with GNAT.Expect;          use GNAT.Expect;

with GNATCOLL.VFS_Types;   use GNATCOLL.VFS_Types;

package Gexpect is

   --------------------------------
   --  Various type declarations --
   --------------------------------

   Null_String_List : constant String_List (1 .. 0) := (others => null);
   --  Null string list

   type Cr_Lf_Handling is (CRLF, LF, Auto);

   type Extra_Prompt (Auto_Answer : Boolean := False) is record
      Ptrn : Pattern_Matcher_Access;

      case Auto_Answer is
         when True =>
            Answer   : String_Access;
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

   type Extra_Prompt_Array is array (Natural range <>) of Extra_Prompt;
   --  See Free procedure in remote-db.ads

   Null_Extra_Prompt : constant Extra_Prompt :=
                         (Auto_Answer => True,
                          Ptrn        => null,
                          Answer      => null);

   ------------------------
   -- Machine definition --
   ------------------------

   type Machine_Type is interface;
   type Machine_Access is access all Machine_Type'Class;

   procedure Ref (Machine : in out Machine_Type) is abstract;
   procedure Unref (Machine : access Machine_Type) is abstract;

   --  Machine accessors

   --  Basic Machine properties
   function Nickname
     (Machine : Machine_Type) return String is abstract;
   function Network_Name
     (Machine : Machine_Type) return String is abstract;
   function Access_Tool
     (Machine : Machine_Type) return String is abstract;
   function Shell
     (Machine : Machine_Type) return String is abstract;

   function Sync_Tool
     (Machine : Machine_Type) return String is abstract;
   --  The name of the synchronization tool
   function Sync_Tool_Args
     (Machine : Machine_Type) return String_List is abstract;
   --  Arguments used by synchronization tool

   function Extra_Init_Commands
     (Machine : Machine_Type) return String_List is abstract;
   function User_Name
     (Machine : Machine_Type) return String is abstract;
   procedure Set_User_Name
     (Machine   : in out Machine_Type;
      User_Name : String) is abstract;
   function Max_Nb_Connections
     (Machine : Machine_Type) return Natural is abstract;
   function Timeout
     (Machine : Machine_Type) return Natural is abstract;
   function Cr_Lf
     (Machine : Machine_Type) return Cr_Lf_Handling is abstract;
   function Use_Dbg
     (Machine : Machine_Type) return Boolean is abstract;
   type Mode_Type is (Input, Output);
   procedure Dbg
     (Machine : access Machine_Type;
      Str     : String;
      Mode    : Mode_Type) is abstract;

   --  Machine's shell properties
   function Shell_Command
     (Machine : Machine_Type) return String is abstract;
   function Shell_Generic_Prompt
     (Machine : Machine_Type) return Pattern_Matcher_Access is abstract;
   function Shell_Configured_Prompt
     (Machine : Machine_Type) return Pattern_Matcher_Access is abstract;
   function Shell_FS
     (Machine : Machine_Type) return FS_Type is abstract;
   function Shell_No_Echo_Cmd
     (Machine : Machine_Type) return String is abstract;
   function Shell_Init_Cmds
     (Machine : Machine_Type) return String_List is abstract;
   function Shell_Exit_Cmds
     (Machine : Machine_Type) return String_List is abstract;
   function Shell_Cd_Cmd
     (Machine : Machine_Type) return String is abstract;
   function Shell_Get_Status_Cmd
     (Machine : Machine_Type) return String is abstract;
   function Shell_Get_Status_Pattern
     (Machine : Machine_Type) return Pattern_Matcher_Access is abstract;

   --  Machine's access tool properties

   function Access_Tool_Command
     (Machine : Machine_Type) return String is abstract;
   function Access_Tool_Common_Args
     (Machine : Machine_Type) return String_List is abstract;
   function Access_Tool_User_Args
     (Machine : Machine_Type) return String_List is abstract;
   function Access_Tool_Send_Interrupt
     (Machine : Machine_Type) return String is abstract;
   function Access_Tool_User_Prompt_Ptrn
     (Machine : Machine_Type) return Pattern_Matcher_Access is abstract;
   function Access_Tool_Password_Prompt_Ptrn
     (Machine : Machine_Type) return Pattern_Matcher_Access is abstract;
   function Access_Tool_Passphrase_Prompt_Ptrn
     (Machine : Machine_Type) return Pattern_Matcher_Access is abstract;
   function Access_Tool_Extra_Prompts
     (Machine : Machine_Type) return Extra_Prompt_Array is abstract;
   function Access_Tool_Use_Pipes
     (Machine : Machine_Type) return Boolean is abstract;

   ------------------------------------
   -- internally used g-exttre datas --
   ------------------------------------

   type Machine_User_Data_Type is abstract tagged null record;
   type Machine_User_Data_Access is access all
     Machine_User_Data_Type'Class;

   procedure Free (Machine : in out Machine_User_Data_Type) is null;
   --  Should free memory used by Machine.

   procedure Set_Data
     (Machine : in out Machine_Type;
      Data    : Machine_User_Data_Access) is abstract;
   function Get_Data
     (Machine : Machine_Type) return Machine_User_Data_Access is abstract;

end Gexpect;
