-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2004                       --
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

with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Task_Manager; use Glide_Kernel.Task_Manager;
with Generic_List;
with Unchecked_Deallocation;
with Commands;             use Commands;
with Commands.VCS;         use Commands.VCS;

package body VCS is

   procedure Free (Identifier : in out VCS_Id_Identifier);
   --  Dummy function used to instanciate Identifiers list.

   package Identifiers is new Generic_List (VCS_Id_Identifier);

   Identifiers_List : Identifiers.List;
   --  Global variable to store all the registered handlers.

   ----------------------
   -- Copy_File_Status --
   ----------------------

   function Copy_File_Status
     (F : in File_Status_Record) return File_Status_Record
   is
      Result : File_Status_Record;
   begin
      Result.File := F.File;
      Result.Working_Revision := Copy_String_List (F.Working_Revision);
      Result.Repository_Revision
        := Copy_String_List (F.Repository_Revision);
      Result.Tags := Copy_String_List (F.Tags);
      Result.Users := Copy_String_List (F.Users);
      Result.Status := F.Status;
      return Result;
   end Copy_File_Status;

   ----------
   -- Free --
   ----------

   procedure Free (Identifier : in out VCS_Id_Identifier) is
      pragma Unreferenced (Identifier);
   begin
      null;
   end Free;

   -----------------------------
   -- Register_VCS_Identifier --
   -----------------------------

   procedure Register_VCS_Identifier (Identifier : VCS_Id_Identifier) is
   begin
      Identifiers.Append (Identifiers_List, Identifier);
   end Register_VCS_Identifier;

   -------------------------------
   -- Unregister_VCS_Identifier --
   -------------------------------

   procedure Unregister_VCS_Identifier (Identifier : VCS_Id_Identifier) is
      use Identifiers;
      Prev, Current : Identifiers.List_Node;
   begin
      Current := First (Identifiers_List);
      while Current /= Null_Node loop
         if Data (Current) = Identifier then
            Remove_Nodes (Identifiers_List, Prev, Current);
            return;
         end if;

         Prev := Current;
         Current := Next (Current);
      end loop;
   end Unregister_VCS_Identifier;

   ---------------------
   -- Get_VCS_From_Id --
   ---------------------

   function Get_VCS_From_Id (Id : String) return VCS_Access is
      use Identifiers;

      Result : VCS_Access;
      Temp   : List_Node  := First (Identifiers_List);

   begin
      while Temp /= Null_Node loop
         Result := Data (Temp) (Id);

         if Result /= null then
            return Result;
         end if;

         Temp := Next (Temp);
      end loop;

      return Result;
   end Get_VCS_From_Id;

   ---------------
   -- Set_Error --
   ---------------

   procedure Set_Error
     (Rep            : access VCS_Record;
      Message        : String;
      Add_LF         : Boolean := True) is
   begin
      if Rep.Kernel = null then
         return;
      end if;

      Insert (Rep.Kernel, Message, Add_LF, Mode => Error);
   end Set_Error;

   ----------
   -- Free --
   ----------

   procedure Free (Ref : in out VCS_Record) is
   begin
      Free_Queue (Ref.Queue);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Ref : in out VCS_Access) is
      procedure Unchecked_Free is new Unchecked_Deallocation
        (VCS_Record'Class, VCS_Access);
   begin
      Free (Ref.all);
      Unchecked_Free (Ref);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (F : in out File_Status_Record) is
      use String_List;
      use File_Status_List;
   begin
      Free (F.Working_Revision);
      Free (F.Repository_Revision);
      Free (F.Tags);
      Free (F.Users);
   end Free;

   ---------------
   -- Get_Queue --
   ---------------

   function Get_Queue
     (VCS : access VCS_Record) return Commands.Command_Queue is
   begin
      return VCS.Queue;
   end Get_Queue;

   -----------------
   -- Check_Files --
   -----------------

   procedure Check_Files
     (Rep       : access VCS_Record;
      Filenames : String_List_Utils.String_List.List)
   is
      C : Update_Files_Command_Access;
   begin
      Create (C, Rep.Kernel, Filenames);
      Launch_Background_Command
        (Rep.Kernel, Command_Access (C), False, True, Name (VCS_Access (Rep)));
   end Check_Files;

   ------------------
   -- Parse_Status --
   ------------------

   function Parse_Status
     (Rep   : access VCS_Record;
      Text  : String;
      Local : Boolean) return File_Status_List.List
   is
      pragma Unreferenced (Rep, Text, Local);
      Result : File_Status_List.List;
   begin
      return Result;
   end Parse_Status;

   -----------------------
   -- Parse_Annotations --
   -----------------------

   procedure Parse_Annotations
     (Rep   : access VCS_Record;
      File  : VFS.Virtual_File;
      Text  : String)
   is
      pragma Unreferenced (Rep, File, Text);
   begin
      null;
   end Parse_Annotations;

   ----------------------------
   -- Get_Identified_Actions --
   ----------------------------

   function Get_Identified_Actions
     (Rep : access VCS_Record) return Action_Array
   is
      pragma Unreferenced (Rep);
      Result : Action_Array;
   begin
      return Result;
   end Get_Identified_Actions;

end VCS;
