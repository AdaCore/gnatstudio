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
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Intl;                use Glide_Intl;

with GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Expect;               use GNAT.Expect;
with Ada.Text_IO;               use Ada.Text_IO;

pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect.TTY;
pragma Warnings (On);

with VCS_View_Pkg;              use VCS_View_Pkg;
with VCS_Module;                use VCS_Module;

with Commands;                  use Commands;
with Commands.External;         use Commands.External;
with Commands.Console;          use Commands.Console;
with Commands.Locations;        use Commands.Locations;

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

   procedure Insert
     (L    : List;
      Mode : Message_Type := Info);
   --  Display L in the console with mode Mode, with a small indentation.

   function Checkin_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Check that List corresponds to the output of a ClearCase checkin.

   function Checkout_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Check that List corresponds to the output of a ClearCase checkout
   --  or a ClearCase mkelem command.

   function Remove_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Check that List corresponds to the output of a ClearCase remove.

   function Diff_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Display the visual differences listed in List in the standard diff
   --  format.
   --  Head contains the name of the file for which the differences are shown.
   --  ??? This is a copy of VCS.CVS.Diff_Handler.

   function Display_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Display Head and List, and return True.

   ------------------
   -- Diff_Handler --
   ------------------

   function Diff_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean
   is
      use String_List;

      L_Temp  : List_Node := First (List);
      Success : Boolean;

      Current_File : constant String := String_List.Head (Head);
      Base         : constant String := Base_Name (Current_File);
      Patch_File   : constant String :=
        String_Utils.Name_As_Directory (Get_Pref (Kernel, Tmp_Dir)) &
        Base & "$difs";
      File         : File_Type;

   begin
      Create (File, Name => Patch_File);

      while L_Temp /= Null_Node loop
         Put (File, Data (L_Temp));
         L_Temp := Next (L_Temp);
      end loop;

      Close (File);
      Insert (Kernel,
              -"ClearCase: Got comparison for file " & Current_File,
              Highlight_Sloc => False,
              Mode => Verbose);

      Display_Differences
        (Kernel, New_File => Current_File, Diff_File => Patch_File);
      GNAT.OS_Lib.Delete_File (Patch_File, Success);

      return True;
   end Diff_Handler;

   ---------------------
   -- Display_Handler --
   ---------------------

   function Display_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean
   is
      pragma Unreferenced (Kernel);
   begin
      Insert (Head, Info);
      Insert (List, Verbose);

      return True;
   end Display_Handler;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (L    : List;
      Mode : Message_Type := Info)
   is
      Node   : List_Node := First (L);
      Kernel : Kernel_Handle
        renames VCS_ClearCase_Module_ID.ClearCase_Reference.Kernel;
   begin
      while Node /= Null_Node loop
         Insert (Kernel,
                 "   " & Data (Node),
                 False,
                 True,
                 Mode);

         Node := Next (Node);
      end loop;
   end Insert;

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
                     S : constant String := Expect_Out (Fd);
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
      Filenames   : String_List.List;
      Clear_Logs  : Boolean := False)
   is
   begin
      --  ??? To be improved.

      Display_File_Status
        (Rep.Kernel,
         Local_Get_Status (Rep, Filenames),
         VCS_ClearCase_Module_ID.ClearCase_Reference,
         True, True, Clear_Logs);
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
      Kernel     : Kernel_Handle
        renames VCS_ClearCase_Module_ID.ClearCase_Reference.Kernel;

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

      Free (Args);

      List_Temp := First (Output);

      --  Detect a possible error in the output.
      if List_Temp /= Null_Node then
         declare
            S             : constant String := Data (List_Temp);
            Error_Pattern : constant String := "Error";
            Index         : Integer := S'First;
         begin
            Skip_To_String (S, Index, Error_Pattern);

            if Index < S'Last - Error_Pattern'Length then
               Insert
                 (Kernel,
                  -"ClearCase error:",
                  Highlight_Sloc => False,
                  Mode           => Error);

               while List_Temp /= Null_Node loop
                  Insert
                    (Kernel,
                     "    " & Data (List_Temp),
                     Highlight_Sloc => False,
                     Mode           => Error);

                  List_Temp := Next (List_Temp);
               end loop;
            end if;
         end;
      end if;


      while List_Temp /= Null_Node loop
         declare
            S              : constant String := Data (List_Temp);
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

   procedure Open
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List;
      User_Name : String := "")
   is
      pragma Unreferenced (User_Name);

      Kernel : Kernel_Handle
        renames VCS_ClearCase_Module_ID.ClearCase_Reference.Kernel;

      File_Node : List_Node := First (Filenames);
   begin
      while File_Node /= Null_Node loop
         declare
            Args     : List;
            Head     : List;
            File     : constant String := Data (File_Node);

            Checkout_File_Command : External_Command_Access;

            Fail_Message    : Console_Command_Access;
            Success_Message : Console_Command_Access;

            Open_File_Command : Source_Location_Command;

         begin
            Insert (Kernel,
                    -"Clearcase: Checking out element: "
                      & File & " ...",
                    False, True, Info);

            --  Create the end of the message.

            Create (Fail_Message,
                    Kernel,
                    -("Checkout of ") & File & (-" failed."),
                    False,
                    True,
                    Info);

            Create (Success_Message,
                    Kernel,
                    -"... done.",
                    False,
                    True,
                    Info);

            Append (Args, "co");
            Append (Args, "-c");

            --  ??? Must provide a way for the user to change this
            --  log message !
            Append (Args, -"Checking out " & File);
            Append (Args, File);

            Append (Head, -"ClearCase error: could not checkout " & File);

            Create (Checkout_File_Command,
                    Kernel,
                    "cleartool",
                    "",
                    Args,
                    Head,
                    Checkout_Handler'Access);

            Free (Args);
            Free (Head);

            --  Create the "Open File" command that will be executed
            --  after the checkout is made.

            Create (Open_File_Command,
                    Kernel,
                    File,
                    Highlight_Line => False);

            --  Enqueue the actions.

            Add_Consequence_Action
              (Checkout_File_Command,
               Success_Message);

            Add_Alternate_Action
              (Checkout_File_Command,
               Fail_Message);

            Enqueue (Rep.Queue, Checkout_File_Command);
            Enqueue (Rep.Queue, Open_File_Command);
         end;

         File_Node := Next (File_Node);
      end loop;
   end Open;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List;
      Logs      : String_List.List)
   is
      Kernel : Kernel_Handle
        renames VCS_ClearCase_Module_ID.ClearCase_Reference.Kernel;

      File_Node : List_Node := First (Filenames);
      Logs_Node : List_Node := First (Logs);
   begin
      while File_Node /= Null_Node loop
         declare
            Args     : List;
            Head     : List;
            File     : constant String := Data (File_Node);

            Checkin_File_Command : External_Command_Access;

            Fail_Message    : Console_Command_Access;
            Success_Message : Console_Command_Access;

         begin
            Insert (Kernel,
                    -"Clearcase: Checking-in element: "
                      & File & " ...",
                    False, True, Info);

            --  Create the end of the message.

            Create (Fail_Message,
                    Kernel,
                    -("check-in of ") & File & (-" failed."),
                    False,
                    True,
                    Info);

            Create (Success_Message,
                    Kernel,
                    -"... done.",
                    False,
                    True,
                    Info);

            Append (Args, "ci");
            Append (Args, "-c");

            Append (Args, Data (Logs_Node));
            Append (Args, File);

            Append (Head, -"ClearCase error: could not check-in " & File);

            Create (Checkin_File_Command,
                    Kernel,
                    "cleartool",
                    "",
                    Args,
                    Head,
                    Checkin_Handler'Access);

            Free (Args);
            Free (Head);

            --  Enqueue the actions.

            Add_Consequence_Action
              (Checkin_File_Command,
               Success_Message);

            Add_Alternate_Action
              (Checkin_File_Command,
               Fail_Message);

            Enqueue (Rep.Queue, Checkin_File_Command);
         end;

         File_Node := Next (File_Node);
         Logs_Node := Next (Logs_Node);
      end loop;
   end Commit;

   ------------
   -- Update --
   ------------

   procedure Update
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List)
   is
      Kernel : Kernel_Handle
        renames VCS_ClearCase_Module_ID.ClearCase_Reference.Kernel;

      File_Node : List_Node := First (Filenames);
   begin
      while File_Node /= Null_Node loop
         declare
            Args     : List;
            File     : constant String := Data (File_Node);

            Update_Command  : External_Command_Access;
            Success_Message : Console_Command_Access;

         begin
            Insert (Kernel,
                    -"Clearcase: updating "
                      & File & " ...",
                    False, True, Info);

            --  Create the end of the message.

            Create (Success_Message,
                    Kernel,
                    -"... done.",
                    False,
                    True,
                    Info);

            Append (Args, "update");
            Append (Args, File);

            Create (Update_Command,
                    Kernel,
                    "cleartool",
                    "",
                    Args,
                    Null_List,
                    Display_Handler'Access);

            Free (Args);

            --  Enqueue the actions.

            Enqueue (Rep.Queue, Update_Command);
            Enqueue (Rep.Queue, Success_Message);
         end;

         File_Node := Next (File_Node);
      end loop;
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

   ---------------------
   -- Checkin_Handler --
   ---------------------

   function Checkin_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean
   is
      pragma Unreferenced (Kernel);

      Last_Line : constant String := Data (Last (List));
      Pattern   : constant String := "Checked in ";
   begin
      if Last_Line'Length < Pattern'Length
        or else Last_Line
        (Last_Line'First
           .. Last_Line'First + Pattern'Length - 1) /= Pattern
      then
         Insert (Head, Error);
         Insert (List, Verbose);

         return False;
      end if;

      return True;
   end Checkin_Handler;

   ----------------------
   -- Checkout_Handler --
   ----------------------

   function Checkout_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean
   is
      pragma Unreferenced (Kernel);

      Last_Line : constant String := Data (Last (List));
      Pattern   : constant String := "Checked out ";
   begin
      if Last_Line'Length < Pattern'Length
        or else Last_Line
        (Last_Line'First
           .. Last_Line'First + Pattern'Length - 1) /= Pattern
      then
         Insert (Head, Error);
         Insert (List, Verbose);

         return False;
      end if;

      return True;
   end Checkout_Handler;

   --------------------
   -- Remove_Handler --
   --------------------

   function Remove_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean
   is
      pragma Unreferenced (Kernel);

      Last_Line : constant String := Data (Last (List));
      Pattern   : constant String := "Removed ";
   begin
      if Last_Line'Length < Pattern'Length
        or else Last_Line
        (Last_Line'First
           .. Last_Line'First + Pattern'Length - 1) /= Pattern
      then
         Insert (Head, Error);
         Insert (List, Verbose);

         return False;
      end if;

      return True;
   end Remove_Handler;

   ---------
   -- Add --
   ---------

   procedure Add
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List)
   is
      Kernel : Kernel_Handle
        renames VCS_ClearCase_Module_ID.ClearCase_Reference.Kernel;

      File_Node : List_Node := First (Filenames);

   begin
      while File_Node /= Null_Node loop
         declare
            Args     : List;
            Head     : List;
            File     : constant String := Data (File_Node);
            Dir      : constant String := Dir_Name (Data (File_Node));

            Checkout_Dir_Command : External_Command_Access;
            Make_Element_Command : External_Command_Access;
            Checkin_Element_Command : External_Command_Access;
            Checkin_Dir_Command  : External_Command_Access;

            Fail_Message    : Console_Command_Access;
            Success_Message : Console_Command_Access;

         begin
            Insert (Kernel,
                    -"Clearcase: Adding element: "
                      & File & " ...",
                    False, True, Info);

            --  Create the end of the message.

            Create (Fail_Message,
                    Kernel,
                    -("Adding of ") & File & (-" failed."),
                    False,
                    True,
                    Info);

            Create (Success_Message,
                    Kernel,
                    -"... done.",
                    False,
                    True,
                    Info);

            --  Check out the directory.

            Append (Args, "co");
            Append (Args, "-c");
            Append (Args, -"Adding " & File);
            Append (Args, Dir);

            Append (Head, -"ClearCase error: could not checkout " & Dir);

            Create (Checkout_Dir_Command,
                    Kernel,
                    "cleartool",
                    "",
                    Args,
                    Head,
                    Checkout_Handler'Access);

            Free (Args);
            Free (Head);

            --  Add the file

            Append (Args, "mkelem");
            Append (Args, "-c");
            Append (Args, -"Initial revision");
            Append (Args, File);

            Append
              (Head,
               -"ClearCase error: could not create the repository element "
                 & File);

            Create (Make_Element_Command,
                    Kernel,
                    "cleartool",
                    "",
                    Args,
                    Head,
                    Checkout_Handler'Access);

            Free (Args);
            Free (Head);

            --  Check in the file

            Append (Args, "ci");
            Append (Args, "-c");
            Append (Args, -"Initial check-in");
            Append (Args, File);

            Append (Head, -"ClearCase error: could not checkin " & File);

            Create (Checkin_Element_Command,
                    Kernel,
                    "cleartool",
                    "",
                    Args,
                    Head,
                    Checkin_Handler'Access);

            Free (Args);
            Free (Head);

            --  Check in the directory.
            Append (Args, "ci");
            Append (Args, "-c");
            Append (Args, -"Added element: " & File);
            Append (Args, Dir);

            Append (Head, -"ClearCase error: could not checkin " & Dir);

            Create (Checkin_Dir_Command,
                    Kernel,
                    "cleartool",
                    "",
                    Args,
                    Head,
                    Checkin_Handler'Access);

            Free (Args);
            Free (Head);

            --  If the directory checkout was successful, create the element.
            Add_Consequence_Action
              (Checkout_Dir_Command,
               Make_Element_Command);

            Add_Alternate_Action
              (Checkout_Dir_Command,
               Fail_Message);

            --  If the element was successfully created, check it in.
            Add_Consequence_Action
              (Make_Element_Command,
               Checkin_Element_Command);

            Add_Alternate_Action
              (Make_Element_Command,
               Copy (Fail_Message));

            Add_Consequence_Action
              (Checkin_Dir_Command,
               Success_Message);

            Enqueue (Rep.Queue, Checkout_Dir_Command);
            Enqueue (Rep.Queue, Checkin_Dir_Command);
         end;

         File_Node := Next (File_Node);
      end loop;
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List)
   is
      Kernel : Kernel_Handle
        renames VCS_ClearCase_Module_ID.ClearCase_Reference.Kernel;

      File_Node : List_Node := First (Filenames);

   begin
      while File_Node /= Null_Node loop
         declare
            Args     : List;
            Head     : List;
            File     : constant String := Data (File_Node);
            Dir      : constant String := Dir_Name (Data (File_Node));

            Checkout_Dir_Command   : External_Command_Access;
            Remove_Element_Command : External_Command_Access;
            Checkin_Dir_Command    : External_Command_Access;

            Fail_Message    : Console_Command_Access;
            Success_Message : Console_Command_Access;

         begin
            Insert (Kernel,
                    -"Clearcase: Removing element: "
                      & File & " ...",
                    False, True, Info);

            --  Create the end of the message.

            Create (Fail_Message,
                    Kernel,
                    -("Removing of ") & File & (-" failed."),
                    False,
                    True,
                    Info);

            Create (Success_Message,
                    Kernel,
                    -"... done.",
                    False,
                    True,
                    Info);

            --  Check out the directory.

            Append (Args, "co");
            Append (Args, "-c");
            Append (Args, -"Removing " & File);
            Append (Args, Dir);

            Append (Head, -"ClearCase error: could not checkout " & Dir);

            Create (Checkout_Dir_Command,
                    Kernel,
                    "cleartool",
                    "",
                    Args,
                    Head,
                    Checkout_Handler'Access);

            Free (Args);
            Free (Head);

            --  Add the file

            Append (Args, "rm");
            Append (Args, "-c");
            Append (Args, -"Removing this element.");
            Append (Args, File);

            Append
              (Head,
               -"ClearCase error: could not remove the element "
                 & File);

            Create (Remove_Element_Command,
                    Kernel,
                    "cleartool",
                    "",
                    Args,
                    Head,
                    Remove_Handler'Access);

            Free (Args);
            Free (Head);

            --  Check in the directory.
            Append (Args, "ci");
            Append (Args, "-c");
            Append (Args, -"Removed element: " & File);
            Append (Args, Dir);

            Append (Head, -"ClearCase error: could not checkin " & Dir);

            Create (Checkin_Dir_Command,
                    Kernel,
                    "cleartool",
                    "",
                    Args,
                    Head,
                    Checkin_Handler'Access);

            Free (Args);
            Free (Head);

            --  If the directory checkout was successful, create the element.
            Add_Consequence_Action
              (Checkout_Dir_Command,
               Remove_Element_Command);

            Add_Alternate_Action
              (Checkout_Dir_Command,
               Fail_Message);

            Add_Alternate_Action
              (Remove_Element_Command,
               Copy (Fail_Message));

            Add_Consequence_Action
              (Checkin_Dir_Command,
               Success_Message);

            Enqueue (Rep.Queue, Checkout_Dir_Command);
            Enqueue (Rep.Queue, Checkin_Dir_Command);
         end;

         File_Node := Next (File_Node);
      end loop;
   end Remove;

   ------------
   -- Revert --
   ------------

   procedure Revert
     (Rep       : access ClearCase_Record;
      Filenames : String_List.List)
   is
      Kernel : Kernel_Handle
        renames VCS_ClearCase_Module_ID.ClearCase_Reference.Kernel;

      File_Node : List_Node := First (Filenames);
   begin
      while File_Node /= Null_Node loop
         declare
            Args     : List;
            File     : constant String := Data (File_Node);

            Revert_Command  : External_Command_Access;

         begin
            Append (Args, "uncheckout");
            Append (Args, "-keep");
            Append (Args, File);

            Create (Revert_Command,
                    Kernel,
                    "cleartool",
                    "",
                    Args,
                    Null_List,
                    Display_Handler'Access);

            Free (Args);

            --  Enqueue the actions.

            Enqueue (Rep.Queue, Revert_Command);
         end;

         File_Node := Next (File_Node);
      end loop;
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
      Kernel : Kernel_Handle
        renames VCS_ClearCase_Module_ID.ClearCase_Reference.Kernel;

      Args     : List;
      Head     : List;

      Diff_File_Command : External_Command_Access;

      Fail_Message      : Console_Command_Access;
      Success_Message   : Console_Command_Access;

   begin
      Insert (Kernel,
              -"Clearcase: getting differences for "
                & File & " ...",
              False, True, Info);

      --  Create the end of the message.

      Create (Fail_Message,
              Kernel,
              -("comparison of ") & File & (-" failed."),
              False,
              True,
              Info);

      Create (Success_Message,
              Kernel,
              -"... done.",
              False,
              True,
              Info);

      Append (Args, "diff");
      Append (Args, "-diff_format");

      if Version_1 = ""
        and then Version_2 = ""
      then
         --  ??? If no version is specified, we assume that
         --  we want differences with the main branch, is that the
         --  right behaviour ?
         Append (Args, File & "@@/main/LATEST");
         Append (Args, File);

      else
         if Version_2 = "" then
            Append (Args, File);
         else
            Append (Args, File & "@@" & Version_2);
         end if;

         if Version_1 = "" then
            Append (Args, File);
         else
            Append (Args, File & "@@" & Version_1);
         end if;
      end if;

      Append (Head, File);

      Create (Diff_File_Command,
              Kernel,
              "cleartool",
              "",
              Args,
              Head,
              Diff_Handler'Access);

      Free (Args);
      Free (Head);

      --  Enqueue the action.

      Enqueue (Rep.Queue, Diff_File_Command);
      Enqueue (Rep.Queue, Success_Message);
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

      Register_VCS (VCS_Module_ID, ClearCase_Identifier);
   end Register_Module;

end VCS.ClearCase;
