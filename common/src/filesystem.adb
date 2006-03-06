-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2006                            --
--                             AdaCore                               --
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

with GNAT.Expect;            use GNAT.Expect;
pragma Warnings (Off);
with GNAT.Expect.TTY.Remote; use GNAT.Expect.TTY.Remote;
pragma Warnings (On);
with GNAT.OS_Lib;            use GNAT.OS_Lib;
with GNAT.Regpat;            use GNAT.Regpat;

with String_Utils; use String_Utils;
with Traces;       use Traces;

package body Filesystem is

   Me : constant Debug_Handle := Create ("Filesystem");

   Request_Obj : constant Request_User_Object := (Main_Window => null);
   --  Used for remote spawn, in case of request to the final user.

   ----------------
   -- Is_Subtree --
   ----------------

   function Is_Subtree
     (FS        : Filesystem_Record;
      Directory : String;
      Full_Path : String)
      return Boolean
   is
   begin
      --  Path length shall be greater or equal to directory length
      if Directory'Length > Full_Path'Length then
         return False;
      end if;

      --  Do not try to compare last character: on VMS, you will compare
      --  a closing bracket with a dot (disk:[path] with disk:[path.subpath])
      return Equal
        (Full_Path (Full_Path'First .. Full_Path'First + Directory'Length - 1),
         Directory,
         Is_Case_Sensitive (Filesystem_Record'Class (FS)));
   end Is_Subtree;

   --------------------
   -- File_Extension --
   --------------------

   function File_Extension (FS   : Filesystem_Record;
                            Path : String) return String is
      pragma Unreferenced (FS);
   begin
      for J in reverse Path'Range loop
         if Path (J) = '.' then
            return Path (J + 1 .. Path'Last);
         end if;
      end loop;
      return "";
   end File_Extension;

   ------------
   -- Concat --
   ------------

   function Concat (FS   : Filesystem_Record;
                    Root : String;
                    Sub  : String) return String
   is
      pragma Unreferenced (FS);
   begin
      return Root & Sub;
   end Concat;

   procedure Internal_Sync_Execute
     (Host                  : String;
      Args                  : GNAT.OS_Lib.Argument_List;
      Execution_Directory   : String;
      Get_Output            : Boolean;
      Out_Value             : out GNAT.OS_Lib.String_Access;
      Status                : out Integer;
      Success               : out Boolean);
   --  Execute the command synchronously.

   ------------------
   -- Sync_Execute --
   ------------------

   procedure Sync_Execute
     (Host                  : String;
      Args                  : GNAT.OS_Lib.Argument_List;
      Out_Value             : out GNAT.OS_Lib.String_Access;
      Status                : out Boolean;
      Execution_Directory   : String  := "")
   is
      Status_Nb : Integer;
   begin
      Internal_Sync_Execute
        (Host, Args, Execution_Directory, True, Out_Value, Status_Nb, Status);
   end Sync_Execute;

   ------------------
   -- Sync_Execute --
   ------------------

   procedure Sync_Execute
     (Host                  : String;
      Args                  : GNAT.OS_Lib.Argument_List;
      Status                : out Boolean;
      Execution_Directory   : String  := "")
   is
      Out_Value : GNAT.OS_Lib.String_Access;
      Status_Nb : Integer;
   begin
      Internal_Sync_Execute
        (Host, Args, Execution_Directory, False, Out_Value, Status_Nb, Status);
   end Sync_Execute;

   ------------------
   -- Sync_Execute --
   ------------------

   procedure Internal_Sync_Execute
     (Host                  : String;
      Args                  : GNAT.OS_Lib.Argument_List;
      Execution_Directory   : String;
      Get_Output            : Boolean;
      Out_Value             : out GNAT.OS_Lib.String_Access;
      Status                : out Integer;
      Success               : out Boolean)
   is
      Fd     : Process_Descriptor_Access;
      Result : Expect_Match;
      Regexp : constant Pattern_Matcher := Compile (".*$", Single_Line);

   begin
      Remote_Spawn (Fd,
                    Host,
                    Args,
                    Execution_Directory,
                    Err_To_Out            => True,
                    Request_User_Instance => Request_Obj);
      loop
         Expect (Fd.all, Result, Regexp, Timeout => 5);
         if Result /= Expect_Timeout and then Get_Output then
            declare
               Output : constant String := Strip_CR (Expect_Out (Fd.all));
               Tmp    : String_Access;
            begin
               if Active (Me) then
                  Trace (Me, "Output: '" & Output & "'");
               end if;
               if Out_Value /= null then
                  Tmp := new String'(Out_Value.all & Output);
                  Free (Out_Value);
                  Out_Value := Tmp;
               else
                  Out_Value := new String'(Output);
               end if;
            end;
         end if;

      end loop;
   exception
      when Process_Died =>
         Close (Process_Descriptor'Class (Fd.all), Status);
         if Status = 0 then
            Success := True;
         else
            Success := False;
         end if;
   end Internal_Sync_Execute;

end Filesystem;
