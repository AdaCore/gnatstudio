-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with String_Utils;              use String_Utils;
with String_List_Utils;         use String_List_Utils;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;

with GNAT.OS_Lib;
with GNAT.Expect;               use GNAT.Expect;

pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect.TTY;
pragma Warnings (On);

with VCS_View_Pkg;              use VCS_View_Pkg;

with Commands;                  use Commands;

with Ada.Text_IO; use Ada.Text_IO;

package body VCS.ClearCase is

   use String_List;
   type VCS_Clearcase_Module_ID_Record is new Module_ID_Record with record
      ClearCase_Reference : VCS_Access;
   end record;
   type VCS_Clearcase_Module_ID_Access is access all
     VCS_Clearcase_Module_ID_Record'Class;

   VCS_ClearCase_Module_Name : constant String := "ClearCase_Connectivity";
   VCS_ClearCase_Module_ID   : VCS_Clearcase_Module_ID_Access;
   ClearCase_Identifier      : constant String := "ClearCase";

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Destroy (Id : in out VCS_Clearcase_Module_ID_Record);
   --  Free the memory occupied by this module

   function Identify_VCS (S : String) return VCS_Access;
   --  Return an access to VCS_Record if S describes a ClearCase system.

   function Command
     (Command : String;
      Args    : String_List.List)
     return String_List.List;
   --  Spawn a command until the end of execution, and return its output
   --  as a list.

   -------------
   -- Command --
   -------------

   function Command
     (Command : String;
      Args    : String_List.List)
     return String_List.List
   is
      Match     : Expect_Match := 1;
      Fd        : TTY_Process_Descriptor;
      Result    : String_List.List;
      The_Args  : GNAT.OS_Lib.Argument_List (1 .. String_List.Length (Args));
      Temp_Args : List_Node := First (Args);
   begin
      for J in The_Args'Range loop
         The_Args (J) := new String' (Data (Temp_Args));
         Temp_Args := Next (Temp_Args);
      end loop;

      Non_Blocking_Spawn
        (Fd,
         Command,
         The_Args,
         Err_To_Out => True,
         Buffer_Size => 0);

      for J in The_Args'Range loop
         GNAT.OS_Lib.Free (The_Args (J));
      end loop;

      declare
      begin
         loop
            Expect (Fd, Match, "\n", 10);

            case Match is
               when Expect_Timeout =>
                  null;
               when others =>
                  declare
                     S : String := Expect_Out (Fd);
                  begin
                     if S (S'Last) = ASCII.LF then
                        String_List.Append (Result, S (S'First .. S'Last - 1));
                     else
                        String_List.Append (Result, S);
                     end if;
                  end;
            end case;
         end loop;

      exception
         when Process_Died =>
            Close (Fd);
            return Result;
      end;
   end Command;

   ----------
   -- Name --
   ----------

   function Name (Ref : access ClearCase_Record) return String is
      pragma Unreferenced (Ref);
   begin
      return ClearCase_Identifier;
   end Name;

   ------------------
   -- Identify_VCS --
   ------------------

   function Identify_VCS (S : String) return VCS_Access is
      Id         : String := S;
      Identifier : String := ClearCase_Identifier;
   begin
      Lower_Case (Id);
      Lower_Case (Identifier);

      if Strip_Quotes (Id) = Identifier then
         return VCS_ClearCase_Module_ID.ClearCase_Reference;
      end if;

      return null;
   end Identify_VCS;

   ----------------
   -- Get_Status --
   ----------------

   procedure Get_Status
     (Rep         : access ClearCase_Record;
      Filenames   : String_List.List)
   is
   begin
      --  ??? To be improved.

      Put_Line ("ClearCase : Getting Status");

      Display_File_Status
        (Rep.Kernel,
         Local_Get_Status (Rep, Filenames),
         VCS_ClearCase_Module_ID.ClearCase_Reference,
         True,
         True);
   end Get_Status;

   ----------------------
   -- Local_Get_Status --
   ----------------------

   function Local_Get_Status
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List)
     return File_Status_List.List
   is
      pragma Unreferenced (Rep);

      Result     : File_Status_List.List;
      Args       : String_List.List;
      List_Temp  : List_Node := First (Filenames);
      Output     : String_List.List;

   begin
      if Is_Empty (Filenames) then
         return Result;
      end if;

      Append (Args, "ls");

      while List_Temp /= Null_Node loop
         Append (Args, Data (List_Temp));
         List_Temp := Next (List_Temp);
      end loop;

      Output := Command ("cleartool", Args);

      List_Temp := First (Output);

      while List_Temp /= Null_Node loop
         declare
            S              : String := Data (List_Temp);
            Current_Status : File_Status_Record;
            Index          : Integer;
            Last_Index     : Integer;
         begin
            --  Determine Status

            Index := S'First;
            Skip_To_String (S, Index, "Rule:");

            if Index > S'Last - 5 then
               Current_Status.Status := Not_Registered;

            else
               if S (Index .. S'Last) = "Rule: CHECKEDOUT" then
                  Current_Status.Status := Modified;

               elsif S (S'Last - 5 .. S'Last) = "LATEST" then
                  Current_Status.Status := Up_To_Date;
               else
                  Current_Status.Status := Unknown;
               end if;
            end if;

            --  Determine file name and version.
            --  ??? The following will not work with file names containing
            --  "@@"

            Last_Index := Index;

            if Current_Status.Status = Modified then
               --  Find the last occurence of " from "

               while S (Index .. Index + 5) /= " from " loop
                  Index := Index - 1;
               end loop;

               Append (Current_Status.Working_Revision,
                       Strip_Quotes (S (Index + 5 .. Last_Index - 1)));

               Index := S'First;
               Skip_To_String (S, Index, "@@");
               Append (Current_Status.File_Name,
                       Strip_Quotes (S (S'First .. Index - 1)));

            elsif Current_Status.Status = Not_Registered then
               Append (Current_Status.File_Name, S);
               Append (Current_Status.Working_Revision, "n/a");

            else
               Index := S'First;
               Skip_To_String (S, Index, "@@");
               Append (Current_Status.File_Name,
                       Strip_Quotes (S (S'First .. Index - 1)));

               Append (Current_Status.Working_Revision,
                       Strip_Quotes (S (Index + 2 .. Last_Index - 1)));
            end if;

            Append (Current_Status.Repository_Revision, "n/a");

            File_Status_List.Append (Result, Current_Status);
         end;

         List_Temp := Next (List_Temp);
      end loop;

      Free (Output);
      return Result;
   end Local_Get_Status;

   ----------
   -- Open --
   ----------

   function Checkout_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   pragma Unreferenced (Checkout_Handler);

   function Checkout_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean
   is
      pragma Unreferenced (Kernel, Head, List);
   begin
      return False;
   end Checkout_Handler;

   procedure Open
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List;
      User_Name : String := "")
   is
      pragma Unreferenced (Rep, Filenames, User_Name);
   begin
      Put_Line ("ClearCase : Open");
      null;
   end Open;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List;
      Logs      : String_List.List)
   is
      pragma Unreferenced (Rep, Filenames, Logs);
   begin
      null;
   end Commit;

   ------------
   -- Update --
   ------------

   procedure Update
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep, Filenames);
   begin
      null;
   end Update;

   -----------
   -- Merge --
   -----------

   procedure Merge
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep, Filenames);
   begin
      null;
   end Merge;

   ---------
   -- Add --
   ---------

   procedure Add
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep, Filenames);
   begin
      null;
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep, Filenames);
   begin
      null;
   end Remove;

   ------------
   -- Revert --
   ------------

   procedure Revert
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List)
   is
      pragma Unreferenced (Rep, Filenames);
   begin
      null;
   end Revert;

   ----------
   -- Diff --
   ----------

   procedure Diff
     (Rep       : access ClearCase_Record;
      File      : String;
      Version_1 : String := "";
      Version_2 : String := "")
   is
      pragma Unreferenced (Rep, File, Version_1, Version_2);
   begin
      null;
   end Diff;

   ---------
   -- Log --
   ---------

   procedure Log
     (Rep  : access ClearCase_Record;
      File : String)
   is
      pragma Unreferenced (Rep, File);
   begin
      null;
   end Log;

   --------------
   -- Annotate --
   --------------

   procedure Annotate
     (Rep  : access ClearCase_Record;
      File : String)
   is
      pragma Unreferenced (Rep, File);
   begin
      null;
   end Annotate;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out VCS_Clearcase_Module_ID_Record) is
   begin
      Free (Id.ClearCase_Reference);
      Unregister_VCS_Identifier (Identify_VCS'Access);
   end Destroy;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
   begin
      VCS_ClearCase_Module_ID := new VCS_Clearcase_Module_ID_Record;
      Register_VCS_Identifier (Identify_VCS'Access);
      Register_Module
        (Module                  => Module_ID (VCS_ClearCase_Module_ID),
         Kernel                  => Kernel,
         Module_Name             => VCS_ClearCase_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => null);

      VCS_ClearCase_Module_ID.ClearCase_Reference := new ClearCase_Record;
      VCS_ClearCase_Module_ID.ClearCase_Reference.Kernel
        := Kernel_Handle (Kernel);
      VCS_ClearCase_Module_ID.ClearCase_Reference.Queue  := New_Queue;

      Register_VCS (Kernel, ClearCase_Identifier);
   end Register_Module;

end VCS.ClearCase;
