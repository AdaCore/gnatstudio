-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                            ACT-Europe                             --
--                                                                   --
-- GPS is free  software; you can  redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Glide_Kernel;         use Glide_Kernel;
with Glide_Kernel.Modules; use Glide_Kernel.Modules;
with Glide_Kernel.Timeout; use Glide_Kernel.Timeout;

package body Commands.Custom is

   ------------
   -- Create --
   ------------

   procedure Create
     (Item         : out Custom_Command_Access;
      Kernel       : Kernel_Handle;
      Command      : String;
      Args         : Argument_List_Access) is
   begin
      Item := new Custom_Command;
      Item.Kernel := Kernel;
      Item.Command := new String' (Command);
      Item.Args := Args;
   end Create;

   -------------
   -- Execute --
   -------------

   function Execute (Command : access Custom_Command) return Boolean is
      Context : Selection_Context_Access
        := Get_Current_Context (Command.Kernel);
      File    : File_Selection_Context_Access;
      Success : Boolean;
      No_Args : String_List (1 .. 0);
   begin
      --  Perform arguments substitutions for the command.

      if Command.Args /= null then
         for J in Command.Args'Range loop
            if Context /= null
              and then Context.all in File_Selection_Context'Class
            then
               File := File_Selection_Context_Access (Context);

               if Has_File_Information (File) then
                  if Command.Args (J).all = "%f" then
                     Free (Command.Args (J));
                     Command.Args (J) := new String' (File_Information (File));
                  elsif Command.Args (J).all = "%F" then
                     Free (Command.Args (J));
                     Command.Args (J) := new String'
                       (Directory_Information (File)
                          & File_Information (File));
                  end if;
               end if;

            elsif Command.Args (J).all = "%f"
              or else Command.Args (J).all = "%F"
            then
               return False;
            end if;
         end loop;

         --  Arguments have been substituted, launch the command.

         Launch_Process
           (Command.Kernel,
            Command.Command.all,
            Command.Args.all,
            null,
            "",
            Success);
      else
         Launch_Process
           (Command.Kernel,
            Command.Command.all,
            No_Args,
            null,
            "",
            Success);
      end if;

      return Success;
   end Execute;

end Commands.Custom;
