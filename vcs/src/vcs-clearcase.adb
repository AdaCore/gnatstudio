-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2003                       --
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
with Glide_Kernel.Task_Manager; use Glide_Kernel.Task_Manager;
with Glide_Intl;                use Glide_Intl;

with GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Expect;               use GNAT.Expect;
with Ada.Text_IO;               use Ada.Text_IO;

pragma Warnings (Off);
with GNAT.Expect.TTY;           use GNAT.Expect.TTY;
pragma Warnings (On);

with VCS_View_Pkg;              use VCS_View_Pkg;
with VCS_Module;                use VCS_Module;
with OS_Utils;                  use OS_Utils;

with Commands;                  use Commands;
with Commands.External;         use Commands.External;
with Commands.Console;          use Commands.Console;
with Commands.Locations;        use Commands.Locations;
with File_Utils;                use File_Utils;
with Basic_Types;               use Basic_Types;
with VFS;                       use VFS;


package body VCS.ClearCase is

   use String_List;
   type VCS_ClearCase_Module_ID_Record is new Module_ID_Record with record
      ClearCase_Reference : VCS_Access;
   end record;
   type VCS_ClearCase_Module_ID_Access is access all
     VCS_ClearCase_Module_ID_Record'Class;

   VCS_ClearCase_Module_Name : constant String := "ClearCase_Connectivity";
   VCS_ClearCase_Module_ID   : VCS_ClearCase_Module_ID_Access;
   ClearCase_Identifier      : constant String := "ClearCase";

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Destroy (Id : in out VCS_ClearCase_Module_ID_Record);
   --  Free the memory occupied by this module

   function Identify_VCS (S : String) return VCS_Access;
   --  Return an access to VCS_Record if S describes a ClearCase system.

   procedure Insert
     (L    : List;
      Mode : Message_Type := Info);
   --  Display L in the console with mode Mode, with a small indentation.

   function Checkin_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Check that List corresponds to the output of a ClearCase checkin.
   --  If Head is not empty, first item in head contains an error message to
   --  be displayed in case of failure, and the second item, if not empty,
   --  contains a file corresponding to a log file.

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

   function Status_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Parse the output of the command "describe -fmt "%Vn;%f;\n"".
   --  (-fmt is an option that is used to specify the output format:
   --   %Vn corresponds to the version name, and %f corresponds to the
   --   checked-out version, if any).

   procedure Parse_Describe
     (Kernel     : Kernel_Handle;
      Files      : String_List.List;
      List       : String_List.List;
      Clear_Logs : Boolean);
   --  Parse the output from the "describe" command, contained in List.
   --  Files contains the absolute names of files being described, in
   --  the same order as List.

   procedure Report_Error
     (Kernel  : Kernel_Handle;
      Message : String;
      List    : String_List.List := Null_List);
   --  Report a ClearCase error.

   function Text_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Create a file with the information from List, and display it in an
   --  editor. File name is based on the information found in Head.
   --  ??? This is code duplication from vcs-cvs.adb

   function Annotation_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Display the annotations for the file.

   -------------------------------
   -- Annotation_Output_Handler --
   -------------------------------

   function Annotation_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean
   is
      use String_List;

      L_Temp       : List_Node := First (List);
      Length       : constant Natural := String_List.Length (List);
      Current_File : constant Virtual_File :=
        Create (Full_Filename => String_List.Head (Head));
      A            : Line_Information_Array (1 .. Length);

   begin
      --  ??? This assumes that the file currently opened is identical to the
      --  file first checked-out from VCS. It would be necessary here to
      --  force a save of the file, and then to get a diff between that file
      --  and the one on VCS (which is given in the output of the annotation,
      --  by the way), and to compute from that diff which lines correspond to
      --  the annotated ones.

      Open_File_Editor (Kernel, Current_File);

      for J in 1 .. Length loop
         declare
            S : constant String := Data (L_Temp);
         begin
            if S'Length > 70 then
               A (J).Text := new String'(S (S'First .. S'First + 69));
            else
               A (J).Text := new String'(S);
            end if;
            --  The index is linked to the annotation format.
         end;

         L_Temp := Next (L_Temp);
      end loop;

      Add_Line_Information
        (Kernel,
         Current_File,
         Annotation_Id,
         new Line_Information_Array'(A));

      return True;
   end Annotation_Output_Handler;

   -------------------------
   -- Text_Output_Handler --
   -------------------------

   function Text_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean
   is
      use String_List;

      L_Temp  : List_Node := First (List);

      Current_File : constant String := String_List.Head (Head);
      Text_File    : constant Virtual_File :=
        Create (Full_Filename => Get_Tmp_Dir & Base_Name (Current_File));
      File         : File_Type;

   begin
      Create (File, Name => Full_Name (Text_File).all);

      while L_Temp /= Null_Node loop
         Put (File, Data (L_Temp));
         L_Temp := Next (L_Temp);
      end loop;

      Close (File);
      Open_File_Editor (Kernel, Text_File);
      Delete (Text_File);

      return True;
   end Text_Output_Handler;

   ------------------
   -- Report_Error --
   ------------------

   procedure Report_Error
     (Kernel  : Kernel_Handle;
      Message : String;
      List    : String_List.List := Null_List)
   is
      Node   : List_Node := First (List);
   begin
      Insert (Kernel, -"ClearCase error: " & Message, Mode => Error);

      while Node /= Null_Node loop
         Insert (Kernel, "   " & Data (Node), Mode => Error);
         Node := Next (Node);
      end loop;
   end Report_Error;

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

      Current_File : constant Virtual_File :=
        Create (Full_Filename => String_List.Head (Head));
      Patch_File   : constant Virtual_File := Create
        (Full_Filename =>
           Get_Tmp_Dir & Base_Name (Current_File) & "$difs");
      File         : File_Type;

   begin
      Create (File, Name => Full_Name (Patch_File).all);

      while L_Temp /= Null_Node loop
         Put (File, Data (L_Temp));
         L_Temp := Next (L_Temp);
      end loop;

      Close (File);
      Insert
        (Kernel,
           -"ClearCase: Got comparison for file "
           & Full_Name (Current_File).all,
         Mode => Verbose);

      Display_Differences
        (Kernel, New_File => Current_File, Diff_File => Patch_File);
      Delete (Patch_File);

      return True;
   end Diff_Handler;

   ---------------------------
   -- Status_Output_Handler --
   ---------------------------

   function Status_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean
   is
      Clear_Logs : Boolean;
      Head_Node  : List_Node;
      Files      : String_List.List;
   begin
      pragma Assert (not Is_Empty (Head));

      Head_Node := First (Head);
      Clear_Logs := Boolean'Value (Data (Head_Node));

      Head_Node := Next (Head_Node);

      while Head_Node /= Null_Node loop
         Append (Files, Data (Head_Node));
         Head_Node := Next (Head_Node);
      end loop;

      Parse_Describe (Kernel, Files, List, Clear_Logs);

      Free (Files);

      return True;
   end Status_Output_Handler;

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
                 Mode => Mode);

         Node := Next (Node);
      end loop;
   end Insert;

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
      To_Lower (Id);
      To_Lower (Identifier);

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
      C            : External_Command_Access;
      Command_Head : List;
      Args         : List;
      List_Temp    : List_Node := First (Filenames);

      procedure Status (File : in String);
      --  Append necessary data to local variables to query the status for
      --  File.

      procedure Status (File : in String) is
      begin
         Append (Args, File);
         Append (Command_Head, File);

         Insert
           (Rep.Kernel,
              -"ClearCase: Querying status for " & File,
            Mode => Verbose);
      end Status;

   begin
      if Is_Empty (Filenames) then
         Report_Error
           (Rep.Kernel,  -"Attempting to get the status of no file.");
         return;
      end if;

      Append (Command_Head, Boolean'Image (Clear_Logs));

      Append (Args, "describe");
      Append (Args, "-fmt");
      Append (Args, "%Vn;%f;\n");

      while List_Temp /= Null_Node loop
         if GNAT.OS_Lib.Is_Directory (Data (List_Temp)) then
            declare
               S : File_Array_Access :=
                 Read_Files_From_Dirs (Data (List_Temp));
            begin
               for J in S'Range loop
                  if S (J) /= No_File
                    and then not GNAT.OS_Lib.Is_Directory
                      (Full_Name (S (J)).all)
                  then
                     Status (Full_Name (S (J)).all);
                  end if;
               end loop;

               Unchecked_Free (S);
            end;

         else
            Status (Data (List_Temp));
         end if;

         List_Temp := Next (List_Temp);
      end loop;

      Create
        (C,
         Rep.Kernel,
         Get_Pref (Rep.Kernel, ClearCase_Command),
         "",
         Args,
         Command_Head,
         Status_Output_Handler'Access,
         -"ClearCase : Querying status");

      Launch_Background_Command
        (Rep.Kernel, Command_Access (C), False, ClearCase_Identifier);

      Free (Command_Head);
      Free (Args);
   end Get_Status;

   --------------------
   -- Parse_Describe --
   --------------------

   procedure Parse_Describe
     (Kernel     : Kernel_Handle;
      Files      : String_List.List;
      List       : String_List.List;
      Clear_Logs : Boolean)
   is
      Node       : List_Node;
      Files_Node : List_Node;
      Result     : File_Status_List.List;
   begin
      --  Browse the output for a line not beginning with a blank space.

      Node := First (List);
      Files_Node := First (Files);

      if Node = Null_Node then
         Report_Error
           (Kernel, -"Could not read the output from the ClearCase command.");

         return;
      end if;

      while Node /= Null_Node loop
         if Files_Node = Null_Node then
            Report_Error
              (Kernel,
               -"Output from the ClearCase command does not match files list",
               Files);
            Report_Error (Kernel, -"Output:", List);
            File_Status_List.Free (Result);

            return;
         end if;

         declare
            Line        : constant String := Data (Node);
            Begin_Index : Natural;
            End_Index   : Natural;
            Current_Status : File_Status_Record;
            Version     : String_Access;
            Rep_Version : String_Access;
         begin
            Append (Current_Status.File_Name, Data (Files_Node));

            End_Index := Line'First;
            Skip_To_Char (Line, End_Index, ';');

            if End_Index > Line'Last then
               Report_Error
                 (Kernel, -"Could not parse ClearCase output:", List);

               File_Status_List.Free (Result);
               return;
            end if;

            Version := new String'(Line (Line'First .. End_Index - 1));

            Begin_Index := End_Index + 1;
            End_Index   := Begin_Index;

            Skip_To_Char (Line, End_Index, ';');

            if End_Index > Line'Last then
               Report_Error
                 (Kernel, -"Could not parse ClearCase output:", List);

               File_Status_List.Free (Result);
               return;
            end if;

            Rep_Version := new String'(Line (Begin_Index .. End_Index - 1));

            Append (Current_Status.Working_Revision, Version.all);

            if Rep_Version.all = "" then
               Append (Current_Status.Repository_Revision, Version.all);
            else
               Append (Current_Status.Repository_Revision, Rep_Version.all);
            end if;

            if Version'Length >= 10
              and then Version
                (Version'Last - 9 .. Version'Last) = "CHECKEDOUT"
            then
               Current_Status.Status := Modified;

            elsif Rep_Version.all = "" and then Version.all = "" then
               Current_Status.Status := Not_Registered;

            elsif Rep_Version.all = Version.all
              or else Rep_Version.all = ""
            then
               Current_Status.Status := Up_To_Date;

            else
               Current_Status.Status := Needs_Update;
            end if;

            Free (Rep_Version);
            Free (Version);

            File_Status_List.Append (Result, Current_Status);
         end;

         Files_Node := Next (Files_Node);
         Node := Next (Node);
      end loop;

      if File_Status_List.Is_Empty (Result) then
         Report_Error
           (Kernel,
              -"Did not find element descriptions in ClearCase output:",
            List);

      else
         Display_File_Status
           (Kernel,
            Result,
            VCS_ClearCase_Module_ID.ClearCase_Reference,
            True, True, Clear_Logs);

         File_Status_List.Free (Result);
      end if;
   end Parse_Describe;

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
      List_Temp  : List_Node := First (Filenames);

      function Status (File : in String) return File_Status_Record;
      --  Return the local file status for File.

      function Status (File : in String) return File_Status_Record is
         Result : File_Status_Record;
      begin
         Append (Result.File_Name, File);
         return Result;
      end Status;

   begin
      while List_Temp /= Null_Node loop
         if GNAT.OS_Lib.Is_Directory (Data (List_Temp)) then
            declare
               S : File_Array_Access :=
                 Read_Files_From_Dirs (Data (List_Temp));
            begin
               for J in S'Range loop
                  if S (J) /= No_File
                    and then not GNAT.OS_Lib.Is_Directory
                      (Full_Name (S (J)).all)
                  then
                     File_Status_List.Append
                       (Result, Status (Full_Name (S (J)).all));
                  end if;
               end loop;

               Unchecked_Free (S);
            end;

         else
            File_Status_List.Append (Result, Status (Data (List_Temp)));
         end if;

         List_Temp := Next (List_Temp);
      end loop;

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
            File     : constant Virtual_File :=
              Create (Full_Filename => Data (File_Node));

            Checkout_File_Command : External_Command_Access;

            Fail_Message    : Console_Command_Access;
            Success_Message : Console_Command_Access;

            Open_File_Command : Source_Location_Command;

         begin
            Insert (Kernel,
                    -"ClearCase: Checking out element: "
                    & Full_Name (File).all & " ...", Mode => Info);

            --  Create the end of the message.

            Create (Fail_Message,
                    Kernel,
                    -("Checkout of ") & Full_Name (File).all & (-" failed."),
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
            Append (Args, -"GPS checking out " & Full_Name (File).all);
            Append (Args, Full_Name (File).all);

            Append (Head, -"ClearCase error: could not checkout "
                    & Full_Name (File).all);

            Create (Checkout_File_Command,
                    Kernel,
                    Get_Pref (Rep.Kernel, ClearCase_Command),
                    "",
                    Args,
                    Head,
                    Checkout_Handler'Access,
                    -"ClearCase: Checking out");

            Free (Args);
            Free (Head);

            --  Create the "Open File" command that will be executed
            --  after the checkout is made.

            Create (Open_File_Command, Kernel, File);

            --  Enqueue the actions.

            Add_Consequence_Action
              (Checkout_File_Command,
               Success_Message);

            Add_Alternate_Action
              (Checkout_File_Command,
               Fail_Message);

            Launch_Background_Command
              (Rep.Kernel,
               Command_Access (Checkout_File_Command),
               False,
               ClearCase_Identifier);

            Launch_Background_Command
              (Rep.Kernel,
               Command_Access (Open_File_Command),
               False,
               ClearCase_Identifier);
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
            Args                 : List;
            Head                 : List;
            File                 : constant String := Data (File_Node);
            Checkin_File_Command : External_Command_Access;
            Fail_Message         : Console_Command_Access;
            Success_Message      : Console_Command_Access;
            Log_File             : constant String := Get_Tmp_Dir &
              "clearcase_log_" & Base_Name (File);
            Fd                   : GNAT.OS_Lib.File_Descriptor;

         begin
            Insert
              (Kernel,
               -"ClearCase: Checking-in element: "
               & File & " ...", Mode => Info);

            --  Create the end of the message.

            Create
              (Fail_Message,
               Kernel,
               -("ClearCase: check-in of ") & File & (-" failed."),
               False,
               True,
               Info);

            Create
              (Success_Message,
               Kernel,
               -("ClearCase: check-in of ") & File & (-" done."),
               False,
               True,
               Info);

            Append (Args, "ci");
            Append (Args, "-cfile");

            Fd := GNAT.OS_Lib.Create_File (Log_File, GNAT.OS_Lib.Binary);

            declare
               Log           : aliased constant String := Data (Logs_Node);
               Bytes_Written : Integer;
               pragma Unreferenced (Bytes_Written);
            begin
               Bytes_Written :=
                 GNAT.OS_Lib.Write (Fd, Log (Log'First)'Address, Log'Length);
            end;

            GNAT.OS_Lib.Close (Fd);

            Append (Args, Log_File);

            Append (Args, File);

            Append (Head, -"ClearCase error: could not check-in " & File);
            Append (Head, Log_File);

            Create
              (Checkin_File_Command,
               Kernel,
               Get_Pref (Rep.Kernel, ClearCase_Command),
               "",
               Args,
               Head,
               Checkin_Handler'Access,
               -"ClearCase: Checking in");

            Free (Args);
            Free (Head);

            --  Enqueue the actions.

            Add_Consequence_Action
              (Checkin_File_Command,
               Success_Message);

            Add_Alternate_Action
              (Checkin_File_Command,
               Fail_Message);

            Launch_Background_Command
              (Rep.Kernel,
               Command_Access (Checkin_File_Command),
               False,
               ClearCase_Identifier);
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
                    -"ClearCase: updating "
                      & File & " ...", Mode => Info);

            --  Create the end of the message.

            Create (Success_Message,
                    Kernel,
                    -("ClearCase: update of ") & File & (-" done."),
                    False,
                    True,
                    Info);

            Append (Args, "update");
            Append (Args, File);

            Create (Update_Command,
                    Kernel,
                    Get_Pref (Rep.Kernel, ClearCase_Command),
                    "",
                    Args,
                    Null_List,
                    Display_Handler'Access,
                    -"ClearCase: Updating");

            Free (Args);

            --  Enqueue the actions.

            Launch_Background_Command
              (Rep.Kernel,
               Command_Access (Update_Command),
               False,
               ClearCase_Identifier);

            Launch_Background_Command
              (Rep.Kernel,
               Command_Access (Success_Message),
               False,
               ClearCase_Identifier);
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

      Node      : String_List.List_Node;
      Pattern   : constant String := "Checked in ";
      Head_Node : String_List.List_Node;
      Success   : Boolean;
   begin
      Node := First (List);

      --  Delete the associated log file, if it exists.

      if not Is_Empty (Head) then
         Head_Node := String_List.First (Head);
         Head_Node := String_List.Next (Head_Node);

         if Head_Node /= Null_Node then
            GNAT.OS_Lib.Delete_File (String_List.Data (Head_Node), Success);
         end if;
      end if;

      --  Check for error messages in the output.

      while Node /= Null_Node loop
         declare
            Line : constant String := Data (Node);
         begin
            if Line'Length < Pattern'Length
              or else Line
                (Line'First .. Line'First + Pattern'Length - 1) = Pattern
            then
               return True;
            end if;
         end;

         Node := Next (Node);
      end loop;

      Insert (Head, Error);
      Insert (List, Verbose);
      return False;
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

      Node      : String_List.List_Node;
      Pattern   : constant String := "Checked out ";
   begin
      Node := First (List);

      while Node /= Null_Node loop
         declare
            Line : constant String := Data (Node);
         begin
            if Line'Length < Pattern'Length
              or else Line
                (Line'First .. Line'First + Pattern'Length - 1) = Pattern
            then
               return True;
            end if;
         end;

         Node := Next (Node);
      end loop;

      Insert (Head, Error);
      Insert (List, Verbose);
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

      Node      : String_List.List_Node;
      Pattern   : constant String := "Removed ";
   begin
      Node := First (List);

      while Node /= Null_Node loop
         declare
            Line : constant String := Data (Node);
         begin
            if Line'Length < Pattern'Length
              or else Line
                (Line'First .. Line'First + Pattern'Length - 1) = Pattern
            then
               return True;
            end if;
         end;

         Node := Next (Node);
      end loop;

      Insert (Head, Error);
      Insert (List, Verbose);
      return False;
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
                    -"ClearCase: Adding element: "
                      & File & " ...", Mode => Info);

            --  Create the end of the message.

            Create (Fail_Message,
                    Kernel,
                    -("ClearCase error: Adding of ") & File & (-" failed."),
                    False,
                    True,
                    Info);

            Create (Success_Message,
                    Kernel,
                    ("ClearCase: Adding of ") & File & (-" done."),
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
                    Get_Pref (Rep.Kernel, ClearCase_Command),
                    "",
                    Args,
                    Head,
                    Checkout_Handler'Access,
                    -"ClearCase: Checking out");

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
                    Get_Pref (Rep.Kernel, ClearCase_Command),
                    "",
                    Args,
                    Head,
                    Checkout_Handler'Access,
                    -"ClearCase: Making element");

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
                    Get_Pref (Rep.Kernel, ClearCase_Command),
                    "",
                    Args,
                    Head,
                    Checkin_Handler'Access,
                    -"ClearCase: Checking in");

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
                    Get_Pref (Rep.Kernel, ClearCase_Command),
                    "",
                    Args,
                    Head,
                    Checkin_Handler'Access,
                    -"ClearCase: Checking in");

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

            Launch_Background_Command
              (Rep.Kernel,
               Command_Access (Checkout_Dir_Command),
               False,
               ClearCase_Identifier);

            Launch_Background_Command
              (Rep.Kernel,
               Command_Access (Checkin_Dir_Command),
               False,
               ClearCase_Identifier);
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
                    -"ClearCase: Removing element: "
                      & File & " ...", Mode => Info);

            --  Create the end of the message.

            Create (Fail_Message,
                    Kernel,
                    -("Removing of ") & File & (-" failed."),
                    False,
                    True,
                    Info);

            Create (Success_Message,
                    Kernel,
                    -"ClearCase: ",
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
                    Get_Pref (Rep.Kernel, ClearCase_Command),
                    "",
                    Args,
                    Head,
                    Checkout_Handler'Access,
                    -"ClearCase: Checking out");

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
                    Get_Pref (Rep.Kernel, ClearCase_Command),
                    "",
                    Args,
                    Head,
                    Remove_Handler'Access,
                    -"ClearCase: Removing element");

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
                    Get_Pref (Rep.Kernel, ClearCase_Command),
                    "",
                    Args,
                    Head,
                    Checkin_Handler'Access,
                    -"ClearCase: Checking in directory");

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

            Launch_Background_Command
              (Rep.Kernel,
               Command_Access (Checkout_Dir_Command),
               False,
               ClearCase_Identifier);

            Launch_Background_Command
              (Rep.Kernel,
               Command_Access (Checkin_Dir_Command),
               False,
               ClearCase_Identifier);
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
                    Get_Pref (Rep.Kernel, ClearCase_Command),
                    "",
                    Args,
                    Null_List,
                    Display_Handler'Access,
                    -"ClearCase: Reverting");

            Free (Args);

            --  Enqueue the actions.

            Launch_Background_Command
              (Rep.Kernel,
               Command_Access (Revert_Command),
               False,
               ClearCase_Identifier);
         end;

         File_Node := Next (File_Node);
      end loop;
   end Revert;

   ----------
   -- Diff --
   ----------

   procedure Diff
     (Rep       : access ClearCase_Record;
      File      : VFS.Virtual_File;
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
              -"ClearCase: getting differences for "
                & Full_Name (File).all & " ...", Mode => Info);

      --  Create the end of the message.

      Create (Fail_Message,
              Kernel,
              -("ClearCase error: comparison of ")
              & Full_Name (File).all & (-" failed."),
              False,
              True,
              Info);

      Create (Success_Message,
              Kernel,
              -("ClearCase: comparison of ")
              & Full_Name (File).all & (-" done."),
              False,
              True,
              Info);

      Append (Args, "diff");
      Append (Args, "-diff_format");

      if Version_1 = ""
        and then Version_2 = ""
      then
         --  If no version is specified, we assume that
         --  we want differences with the latest version.
         Append (Args, Full_Name (File).all);
         Append (Args, Full_Name (File).all & "@@/main/LATEST");

      else
         if Version_2 = "" then
            Append (Args, Full_Name (File).all);
         else
            Append (Args, Full_Name (File).all & "@@" & Version_2);
         end if;

         if Version_1 = "" then
            Append (Args, Full_Name (File).all);
         else
            Append (Args, Full_Name (File).all & "@@" & Version_1);
         end if;
      end if;

      Append (Head, Full_Name (File).all);

      Create (Diff_File_Command,
              Kernel,
              Get_Pref (Rep.Kernel, ClearCase_Command),
              "",
              Args,
              Head,
              Diff_Handler'Access,
              -"ClearCase: Querying differences");

      Free (Args);
      Free (Head);

      --  Enqueue the action.

      Launch_Background_Command
        (Rep.Kernel,
         Command_Access (Diff_File_Command),
         False,
         ClearCase_Identifier);

      Launch_Background_Command
        (Rep.Kernel,
         Command_Access (Success_Message),
         False,
         ClearCase_Identifier);
   end Diff;

   ---------
   -- Log --
   ---------

   procedure Log
     (Rep  : access ClearCase_Record;
      File : Virtual_File;
      Rev  : String)
   is
      C            : External_Command_Access;
      Command_Head : List;
      Args         : List;

      pragma Unreferenced (Rev);
      --  ??? Should implement handling of specific revision.
   begin
      Append (Args, "lshistory");
      Append (Args, Full_Name (File).all);

      Append (Command_Head, Base_Name (File) & "$changelog");

      Create
        (C,
         Rep.Kernel,
         Get_Pref (Rep.Kernel, ClearCase_Command),
         Dir_Name (File).all,
         Args,
         Command_Head,
         Text_Output_Handler'Access,
         -"ClearCase: Querying revision history");

      Launch_Background_Command
        (Rep.Kernel,
         Command_Access (C),
         False,
         ClearCase_Identifier);

      Free (Command_Head);
      Free (Args);
   end Log;

   --------------
   -- Annotate --
   --------------

   procedure Annotate
     (Rep  : access ClearCase_Record;
      File : Virtual_File)
   is
      use String_List;

      C            : External_Command_Access;
      Command_Head : List;
      Args         : List;
   begin
      Append (Args, "annotate");
      Append (Args, "-nheader");
      Append (Args, "-force");
      Append (Args, "-fmt");
      Append (Args, "%BAd\040%Sd\040%-16.16u\040%-40.40Vn");
      Append (Args, "-out");
      Append (Args, "-");

      Append (Args, Full_Name (File).all);
      Append (Command_Head, Full_Name (File).all);

      Create
        (C,
         Rep.Kernel,
         Get_Pref (Rep.Kernel, ClearCase_Command),
         Dir_Name (File).all,
         Args,
         Command_Head,
         Annotation_Output_Handler'Access,
         -"ClearCase: Querying annotations");


      Launch_Background_Command
        (Rep.Kernel,
         Command_Access (C),
         False,
         ClearCase_Identifier);

      Free (Command_Head);
      Free (Args);
   end Annotate;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out VCS_ClearCase_Module_ID_Record) is
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
      VCS_ClearCase_Module_ID := new VCS_ClearCase_Module_ID_Record;
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
