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

with Glide_Intl;                use Glide_Intl;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Console;      use Glide_Kernel.Console;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Preferences;  use Glide_Kernel.Preferences;
with Glide_Kernel.Task_Manager; use Glide_Kernel.Task_Manager;

with GNAT.OS_Lib;
with GNAT.Case_Util;            use GNAT.Case_Util;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Ada.Text_IO;               use Ada.Text_IO;
with Ada.Strings;               use Ada.Strings;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;

with String_Utils;              use String_Utils;
with OS_Utils;                  use OS_Utils;

with Src_Info;                  use Src_Info;

with VCS_View_Pkg;              use VCS_View_Pkg;
with VCS_Module;                use VCS_Module;

with Commands;                  use Commands;
with Commands.External;         use Commands.External;
with Traces;                    use Traces;
with VFS;                       use VFS;

package body VCS.CVS is

   type VCS_CVS_Module_ID_Record is new Module_ID_Record with record
      CVS_Reference : VCS_Access;
   end record;
   type VCS_CVS_Module_ID_Access is access all VCS_CVS_Module_ID_Record'Class;

   VCS_CVS_Module_Name : constant String := "CVS_Connectivity";
   VCS_CVS_Module_ID   : VCS_CVS_Module_ID_Access;
   CVS_Identifier      : constant String := "CVS";

   Me : constant Debug_Handle := Create (CVS_Identifier);

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Destroy (Id : in out VCS_CVS_Module_ID_Record);
   --  Free the memory occupied by this module

   function Identify_VCS (S : String) return VCS_Access;
   --  Return an access to VCS_Record if S describes a CVS system.

   procedure Handle_Error
     (Rep : access CVS_Record;
      S   : String);
   --  Append S at the end of current message.

   procedure Real_Get_Status
     (Rep         : access CVS_Record;
      Filenames   : String_List.List;
      Clear_Logs  : Boolean := False);
   --  Just like Get_Status, but assuming that Filenames is not empty
   --  and that all files in Filenames are from the same directory.

   function Real_Local_Get_Status
     (Rep         : access CVS_Record;
      Filenames   : String_List.List) return File_Status_List.List;
   --  Just like Local_Get_Status, but assuming that Filenames is not
   --  empty and that all files in Filenames are from the same directory.

   procedure Simple_Action
     (Rep       : access CVS_Record;
      Filenames : String_List.List;
      Arguments : String_List.List);
   --  Launch a simple cvs command, ie "cvs" followed by Arguments then
   --  Filenames. User must free Filenames and Arguments.

   procedure Real_Simple_Action
     (Rep       : access CVS_Record;
      Filenames : String_List.List;
      Arguments : String_List.List);
   --  Just like Simple_Action, but assuming that Filenames is not
   --  empty and that all files in Filenames are from the same directory.

   --------------
   -- Handlers --
   --------------

   function Text_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Create a file with the information from List, and display it in an
   --  editor.

   function Annotation_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Display the annotations for the file.

   function Message_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Insert the output in the console.

   function Status_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Parse the status information from List and display it in the VCS
   --  explorer.

   function Intermediate_Diff_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Store List in a file given by Head.

   function Diff_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Display comparison between file given in Head and patch
   --  given in List.

   function Checkin_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Remove the log file stored in the first element of Head.

   ---------------------
   -- Checkin_Handler --
   ---------------------

   function Checkin_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean
   is
      Success : Boolean;

   begin
      --  Delete the associated log file, if it exists.

      if not String_List.Is_Empty (Head) then
         GNAT.OS_Lib.Delete_File (String_List.Head (Head), Success);
      end if;

      Success := Message_Output_Handler (Kernel, Head, List);

      return True;
   end Checkin_Handler;

   ----------
   -- Name --
   ----------

   function Name (Ref : access CVS_Record) return String is
      pragma Unreferenced (Ref);
   begin
      return CVS_Identifier;
   end Name;

   ----------------------------
   -- Message_Output_Handler --
   ----------------------------

   function Message_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean
   is
      use String_List;

      L_Temp  : List_Node := First (List);
      H_Temp  : List_Node := First (Head);

   begin
      if L_Temp /= Null_Node then
         Insert (Kernel, -"CVS output:", Mode => Info);

         while H_Temp /= Null_Node loop
            Insert (Kernel,
                    Data (H_Temp),
                    Mode => Verbose);
            H_Temp := Next (H_Temp);
         end loop;

         while L_Temp /= Null_Node loop
            Insert (Kernel,
                    Data (L_Temp),
                    Mode => Info,
                    Add_LF => False);
            L_Temp := Next (L_Temp);
         end loop;
      end if;

      return True;
   end Message_Output_Handler;

   ------------------------
   -- Real_Simple_Action --
   ------------------------

   procedure Real_Simple_Action
     (Rep               : access CVS_Record;
      Filenames         : String_List.List;
      Arguments         : String_List.List)
   is
      use String_List;

      C    : External_Command_Access;
      Args : List;

   begin
      declare
         Args_Temp : List_Node := First (Arguments);
      begin
         while Args_Temp /= Null_Node loop
            Append (Args, Data (Args_Temp));
            Args_Temp := Next (Args_Temp);
         end loop;
      end;

      if Head (Filenames) /= Dir_Name (Head (Filenames)) then
         declare
            Files_Temp : List_Node := First (Filenames);
         begin
            while Files_Temp /= Null_Node loop
               Append (Args, Base_Name (Data (Files_Temp)));
               Files_Temp := Next (Files_Temp);
            end loop;
         end;
      end if;

      Create
        (C,
         Rep.Kernel,
         Get_Pref (Rep.Kernel, CVS_Command),
         Dir_Name (Head (Filenames)),
         Args,
         Null_List,
         Message_Output_Handler'Access,
         -"CVS: Basic query");

      Launch_Background_Command
        (Rep.Kernel, Command_Access (C), False, CVS_Identifier);

      Free (Args);
   end Real_Simple_Action;

   -------------------
   -- Simple_Action --
   -------------------

   procedure Simple_Action
     (Rep               : access CVS_Record;
      Filenames         : String_List.List;
      Arguments         : String_List.List)
   is
      use String_List;

      Current_Filename : List_Node := First (Filenames);
   begin
      if Current_Filename = Null_Node then
         --  ??? Set an error here.
         return;
      end if;

      while Current_Filename /= Null_Node loop
         --  Extract a list of files that belong to the same directory.

         declare
            Current_Directory : constant String :=
              Dir_Name (Data (Current_Filename));
            Current_List      : List;
         begin
            while Current_Filename /= Null_Node
              and then Dir_Name (Data (Current_Filename)) = Current_Directory
            loop
               Append (Current_List, Data (Current_Filename));
               Current_Filename := Next (Current_Filename);
            end loop;

            --  At this point, Current_List should not be empty and
            --  all its element are files from Current_Directory.

            Real_Simple_Action (Rep, Current_List, Arguments);
            Free (Current_List);
         end;
      end loop;
   end Simple_Action;

   ---------------------------
   -- Status_Output_Handler --
   ---------------------------

   function Status_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean
   is
      Result         : File_Status_List.List := File_Status_List.Null_List;
      Blank_Status   : File_Status_Record;
      Current_Status : File_Status_Record := Blank_Status;

      use String_List;

      Output         : List_Node := First (List);
      New_Dir        : constant String := String_List.Head (Head);
      Head_Node      : List_Node := String_List.First (Head);
      Clear_Logs     : Boolean;

   begin
      Head_Node := String_List.Next (Head_Node);
      Clear_Logs := Boolean'Value (Data (Head_Node));

      while Output /= Null_Node loop
         declare
            Line       : constant String := Data (Output);
            Index      : Natural;
            Next_Index : Natural;
            First      : constant Integer := Line'First;
            Last       : Integer := Line'Last;
            Length     : Integer := Line'Length;
         begin
            if Length /= 0 and then Line (Last) = ASCII.LF then
               Last := Last - 1;
               Length := Length - 1;
            end if;

            if Length > 4
              and then Line (First .. First + 3) = "===="
            then
               --  Upon encounter of "====", append the status to the result.

               if Current_Status /= Blank_Status then
                  File_Status_List.Append (Result, Current_Status);
               end if;

               Current_Status := Blank_Status;

            elsif Length > 5
              and then Line (First .. First + 4) = "File:"
            then
               --  Upon encounter of "File:", parse the status of the file.

               Index := First + 6;
               Skip_To_Char (Line, Index, ASCII.HT);
               Append (Current_Status.File_Name,
                       New_Dir & Strip_Quotes (Line (7 .. Index - 1)));
               --  ??? Maybe we should use Strip_Blanks.

               Index := First;
               Skip_To_String (Line, Index, "Status:");
               Index := Index + 8;

               if Last >= Index + 6
                 and then Line (Index .. Index + 6) = "Unknown"
               then
                  Current_Status.Status := Not_Registered;

               elsif Last >= Index + 15
                 and then Line (Index .. Index + 15) = "Locally Modified"
               then
                  Current_Status.Status := Modified;

               elsif Last >= Index + 14
                 and then Line (Index .. Index + 14) = "Locally Removed"
               then
                  Current_Status.Status := Not_Registered;

               elsif Last >= Index + 10
                 and then Line (Index .. Index + 10) = "Needs Merge"
               then
                  Current_Status.Status := Needs_Merge;

               elsif Last >= Index + 10
                 and then Line (Index .. Index + 10) = "Needs Patch"
               then
                  Current_Status.Status := Needs_Update;

               elsif Last >= Index + 9
                 and then Line (Index .. Index + 9) =  "Up-to-date"
               then
                  Current_Status.Status := Up_To_Date;

                  declare
                     S : constant String :=
                       String_List.Head (Current_Status.File_Name);
                  begin
                     if S'Length > New_Dir'Length + 7
                       and then
                         S (S'First + New_Dir'Length
                                .. S'First + New_Dir'Length + 7) = "no file "
                     then
                        Free (Current_Status.File_Name);
                        Append (Current_Status.File_Name,
                                New_Dir &
                                S (S'First + New_Dir'Length + 8 .. S'Last));
                        Current_Status.Status := Removed;
                     end if;
                  end;

               elsif Last >= Index + 13
                 and then Line (Index .. Index + 13) = "Needs Checkout"
               then
                  Current_Status.Status := Needs_Update;

               elsif Last > Index + 13
                 and then Line (Index .. Index + 13) = "File had confl"
               then
                  Current_Status.Status := Modified;
               end if;

               declare
                  S : constant String :=
                    String_List.Head (Current_Status.File_Name);
               begin
                  if S'Length > New_Dir'Length + 7
                    and then S
                      (S'First + New_Dir'Length
                           .. S'First + New_Dir'Length + 7) = "no file "
                  then
                     Free (Current_Status.File_Name);
                     Append (Current_Status.File_Name,
                             New_Dir &
                             S (S'First + New_Dir'Length + 8 .. S'Last));
                  end if;
               end;


            elsif Length > 14
              and then Line (First .. First + 13) = "   Working rev"
            then
               Index := First + 10;
               Skip_To_Char (Line, Index, ASCII.HT);

               if Current_Status.Status /= Unknown
                 and then Current_Status.Status /= Not_Registered
               then
                  Skip_Blanks (Line (Index .. Last), Index);
                  Next_Index := Index + 1;
                  Skip_To_Blank (Line (Index .. Last), Next_Index);

                  if Next_Index > Last then
                     Next_Index := Last;
                  end if;

                  Append (Current_Status.Working_Revision,
                          Trim (Line (Index .. Next_Index), Both));
               end if;

            elsif Length > 15
              and then Line (First .. First + 14) = "   Repository r"
            then
               Index := First + 10;
               Skip_To_Char (Line, Index, ASCII.HT);

               if Current_Status.Status /= Unknown
                 and then Current_Status.Status /= Not_Registered
               then
                  Skip_Blanks (Line (Index .. Last), Index);
                  Next_Index := Index + 1;
                  Skip_To_Blank (Line (Index .. Last), Next_Index);

                  if Next_Index > Last then
                     Next_Index := Last;
                  end if;

                  Append (Current_Status.Repository_Revision,
                          Trim (Line (Index .. Next_Index), Both));
               end if;
            end if;

            Output := Next (Output);
         end;
      end loop;

      --  Append the last status.

      if not Is_Empty (Current_Status.File_Name) then
         File_Status_List.Append (Result, Current_Status);
      end if;

      if not File_Status_List.Is_Empty (Result) then
         Insert (Kernel,
                 -"CVS: Status obtained for files in " & New_Dir,
                 Mode => Verbose);
      end if;

      Display_File_Status
        (Kernel, Result, VCS_CVS_Module_ID.CVS_Reference, True, True,
         Clear_Logs);
      File_Status_List.Free (Result);

      return True;
   end Status_Output_Handler;

   ---------------------
   -- Real_Get_Status --
   ---------------------

   procedure Real_Get_Status
     (Rep         : access CVS_Record;
      Filenames   : String_List.List;
      Clear_Logs  : Boolean := False)
   is
      use String_List;

      Files           : List_Node := First (Filenames);
      C               : External_Command_Access;
      Command_Head    : List;
      Args            : List;
      Dir             : constant String := Dir_Name (Data (Files));

   begin
      Append (Command_Head, Dir_Name (Data (Files)));
      Append (Command_Head, Boolean'Image (Clear_Logs));

      --  Generate arguments list.
      --  If the first argument is a directory, do a simple query for
      --  all files in that directory.

      Insert (Rep.Kernel,
              -"CVS: Querying status for files in "
              & Dir_Name (Data (Files)),
              Mode => Verbose);

      if Data (Files) = Dir_Name (Data (Files)) then
         Append (Args, "status");
         Append (Args, "-l");

      else
         Append (Args, "status");

         while Files /= Null_Node loop
            Append (Args, Base_Name (Data (Files)));
            Files := Next (Files);
         end loop;
      end if;

      Create (C,
              Rep.Kernel,
              Get_Pref (Rep.Kernel, CVS_Command),
              Dir,
              Args,
              Command_Head,
              Status_Output_Handler'Access,
              -"CVS: Querying status");

      Launch_Background_Command
        (Rep.Kernel, Command_Access (C), False, CVS_Identifier);

      Check_Files (Rep, Filenames);

      Free (Command_Head);
      Free (Args);
   end Real_Get_Status;

   ---------------------------
   -- Real_Local_Get_Status --
   ---------------------------

   function Real_Local_Get_Status
     (Rep         : access CVS_Record;
      Filenames   : String_List.List) return File_Status_List.List
   is
      pragma Unreferenced (Rep);

      use String_List;
      use File_Status_List;

      Result  : File_Status_List.List;

      Old_Dir : constant Dir_Name_Str := Get_Current_Dir;
      New_Dir : constant Dir_Name_Str := Dir_Name (Head (Filenames));

      Blank_Status   : File_Status_Record;
      Current_Status : File_Status_Record := Blank_Status;

      File   : File_Type;
      Buffer : String (1 .. 8192);
      Last   : Integer := 1;

      Index  : Natural;
      Next_Index : Natural;

      Entries_Timestamp : Timestamp;
      File_Timestamp    : Timestamp;

   begin
      --  ??? Do we really have to change the current (global) directory ?
      --  This may have unexpected side effects, so would be nice to avoid it.

      Change_Dir (New_Dir);

      --  Open and parse the Entries file.

      Open (File, In_File, Format_Pathname (New_Dir & "CVS/Entries"));

      Entries_Timestamp :=
        To_Timestamp (GNAT.OS_Lib.File_Time_Stamp (New_Dir & "CVS/Entries"));

      while Last >= 0
        and then not End_Of_File (File)
      loop
         Get_Line (File, Buffer, Last);

         if Buffer (1) = '/' then
            Index := 2;
            Skip_To_Char (Buffer (1 .. Last), Index, '/');
            Next_Index := Index + 1;
            Skip_To_Char (Buffer (1 .. Last), Next_Index, '/');

            Append (Current_Status.File_Name,
                    New_Dir & Buffer (2 .. Index - 1));

            File_Timestamp :=
              To_Timestamp
                (GNAT.OS_Lib.File_Time_Stamp
                     (New_Dir & Buffer (2 .. Index - 1)));

            if File_Timestamp > Entries_Timestamp then
               Current_Status.Status := Modified;
            end if;

            Append (Current_Status.Working_Revision,
                    Trim (Buffer (Index + 1 .. Next_Index - 1), Both));

            if Index + 1 < Next_Index - 1 then
               File_Status_List.Append (Result, Current_Status);
            else
               Free (Current_Status.File_Name);
               Free (Current_Status.Working_Revision);
            end if;
         end if;

         Current_Status := Blank_Status;
      end loop;

      Close (File);
      Change_Dir (Old_Dir);

      --  We have gathered information about all files in the directory,
      --  now we build a list corresponding to what the user wants.

      if Head (Filenames) = New_Dir then
         return Result;
      else
         declare
            The_Result     : File_Status_List.List;
            Filenames_Temp : String_List.List_Node := First (Filenames);
            Status_Temp    : File_Status_List.List_Node;
            Found          : Boolean;

         begin
            while Filenames_Temp /= String_List.Null_Node loop
               Status_Temp := File_Status_List.First (Result);
               Found       := False;

               while Status_Temp /= File_Status_List.Null_Node
                 and then not Found
               loop
                  if Head (Data (Status_Temp).File_Name)
                    = Data (Filenames_Temp)
                  then
                     Found := True;
                     Append (The_Result,
                             Copy_File_Status (Data (Status_Temp)));
                  end if;

                  Status_Temp := Next (Status_Temp);
               end loop;

               Filenames_Temp := Next (Filenames_Temp);
            end loop;

            Free (Result);
            return The_Result;
         end;
      end if;

   exception
      when End_Error =>
         Close (File);
         return Result;
      when Use_Error =>
         return Result;
      when Name_Error =>
         return Result;
      when Directory_Error =>
         return Result;
   end Real_Local_Get_Status;

   ----------------
   -- Get_Status --
   ----------------

   procedure Get_Status
     (Rep         : access CVS_Record;
      Filenames   : String_List.List;
      Clear_Logs  : Boolean := False)
   is
      use String_List;

      Current_Filename : List_Node := First (Filenames);
   begin
      if Current_Filename = Null_Node then
         return;
      end if;

      while Current_Filename /= Null_Node loop
         --  Extract a list of files that belong to the same directory.

         declare
            Current_Directory : constant String :=
              Dir_Name (Data (Current_Filename));
            Current_List      : String_List.List;

         begin
            while Current_Filename /= Null_Node
              and then Dir_Name (Data (Current_Filename)) = Current_Directory
            loop
               Append (Current_List, Data (Current_Filename));
               Current_Filename := Next (Current_Filename);
            end loop;

            --  At this point, Current_List should not be empty and
            --  all its element are files from Current_Directory.

            Real_Get_Status (Rep, Current_List, Clear_Logs);
            Free (Current_List);
         end;
      end loop;
   end Get_Status;

   ----------------------
   -- Local_Get_Status --
   ----------------------

   function Local_Get_Status
     (Rep         : access CVS_Record;
      Filenames   : String_List.List) return File_Status_List.List
   is
      use String_List;

      Result           : File_Status_List.List;
      Current_Filename : List_Node := First (Filenames);

   begin
      if Current_Filename = Null_Node then
         return Result;
      end if;

      while Current_Filename /= Null_Node loop
         --  Extract a list of files that belong to the same directory.

         declare
            Current_Directory : constant String :=
              Dir_Name (Data (Current_Filename));
            Current_List      : String_List.List;

         begin
            while Current_Filename /= Null_Node
              and then Dir_Name (Data (Current_Filename)) = Current_Directory
            loop
               Append (Current_List, Data (Current_Filename));
               Current_Filename := Next (Current_Filename);
            end loop;

            --  At this point, Current_List should not be empty and
            --  all its element are files from Current_Directory.

            File_Status_List.Concat
              (Result, Real_Local_Get_Status (Rep, Current_List));
            Free (Current_List);
         end;
      end loop;

      return Result;
   end Local_Get_Status;

   ----------
   -- Open --
   ----------

   procedure Open
     (Rep       : access CVS_Record;
      Filenames : String_List.List;
      User_Name : String           := "")
   is
      pragma Unreferenced (User_Name);

      use String_List;

      Arguments : String_List.List;
      Node      : String_List.List_Node := First (Filenames);
   begin
      String_List.Append (Arguments, "edit");

      Simple_Action (Rep, Filenames, Arguments);

      Free (Arguments);

      while Node /= Null_Node loop
         Open_File_Editor
           (Rep.Kernel, Create (Full_Filename => Data (Node)));
         Node := Next (Node);
      end loop;
   end Open;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (Rep       : access CVS_Record;
      Filenames : String_List.List;
      Logs      : String_List.List)
   is
      use String_List;

      Checkin_File_Command : External_Command_Access;

      Arguments      : String_List.List;
      Filenames_Temp : List_Node := First (Filenames);
      Logs_Temp      : List_Node := First (Logs);

   begin
      while Filenames_Temp /= Null_Node loop
         declare
            File     : constant String := Data (Filenames_Temp);
            Head     : List;
            Log_File : constant String := Get_Tmp_Dir
              & "cvs_log_" & Base_Name (File);
            Fd       : GNAT.OS_Lib.File_Descriptor;

         begin
            Append (Arguments, "commit");
            Append (Arguments, "-F");
            Append (Arguments, Log_File);

            Fd := GNAT.OS_Lib.Create_File (Log_File, GNAT.OS_Lib.Binary);

            declare
               Log           : aliased constant String := Data (Logs_Temp);
               Bytes_Written : Integer;
               pragma Unreferenced (Bytes_Written);
            begin
               Bytes_Written :=
                 GNAT.OS_Lib.Write (Fd, Log (Log'First)'Address, Log'Length);
            end;

            GNAT.OS_Lib.Close (Fd);

            Append (Arguments, Base_Name (File));

            Append (Head, Log_File);

            Create
              (Checkin_File_Command,
               Rep.Kernel,
               Get_Pref (Rep.Kernel, CVS_Command),
               Dir_Name (File),
               Arguments,
               Head,
               Checkin_Handler'Access,
               -"CVS: Committing");

            Launch_Background_Command
              (Rep.Kernel,
               Command_Access (Checkin_File_Command),
               False, CVS_Identifier);

            Free (Arguments);
            Free (Head);

            Logs_Temp      := Next (Logs_Temp);
            Filenames_Temp := Next (Filenames_Temp);
         end;
      end loop;

   exception
      when List_Empty =>
         Handle_Error (Rep, -"Log list incomplete !");
   end Commit;

   ------------
   -- Update --
   ------------

   procedure Update
     (Rep       : access CVS_Record;
      Filenames : String_List.List)
   is
      Arguments : String_List.List;
   begin
      String_List.Append (Arguments, "update");
      String_List.Append (Arguments, "-d");
      Simple_Action (Rep, Filenames, Arguments);
      String_List.Free (Arguments);

      Check_Files (Rep, Filenames);
   end Update;

   -----------
   -- Merge --
   -----------

   procedure Merge
     (Rep       : access CVS_Record;
      Filenames : String_List.List)
   is
      Arguments : String_List.List;
   begin
      String_List.Append (Arguments, "update");
      Simple_Action (Rep, Filenames, Arguments);
      String_List.Free (Arguments);
   end Merge;

   ---------
   -- Add --
   ---------

   procedure Add
     (Rep       : access CVS_Record;
      Filenames : String_List.List)
   is
      Arguments   : String_List.List;
      Arguments_2 : String_List.List;
   begin
      String_List.Append (Arguments, "add");

      Simple_Action (Rep, Filenames, Arguments);
      String_List.Free (Arguments);

      String_List.Append (Arguments_2, "commit");
      String_List.Append (Arguments_2, "-m");

      String_List.Append (Arguments_2, -"Initial revision for this file.");
      --  ??? This should be customizable.

      Simple_Action (Rep, Filenames, Arguments_2);
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Rep       : access CVS_Record;
      Filenames : String_List.List)
   is
      Arguments : String_List.List;
      Arguments_2 : String_List.List;
   begin
      String_List.Append (Arguments, "remove");
      String_List.Append (Arguments, "-f");

      Simple_Action (Rep, Filenames, Arguments);

      String_List.Append (Arguments_2, "commit");
      String_List.Append (Arguments_2, "-m");

      String_List.Append (Arguments_2, -"Remove this file.");
      --  ??? This should be customizable.

      Simple_Action (Rep, Filenames, Arguments_2);
   end Remove;

   ------------
   -- Revert --
   ------------

   procedure Revert
     (Rep       : access CVS_Record;
      Filenames : String_List.List)
   is
      Arguments : String_List.List;
   begin
      String_List.Append (Arguments, "update");
      String_List.Append (Arguments, "-C");

      Simple_Action (Rep, Filenames, Arguments);
   end Revert;

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
        (Full_Filename => Get_Tmp_Dir & Base_Name (Current_File) & "$difs");
      Full       : constant String := Full_Name (Current_File);
      Num_Lines    : Natural := 0;
      File         : File_Type;

   begin
      Create (File, Name => Full_Name (Patch_File));

      while L_Temp /= Null_Node loop
         Num_Lines := Num_Lines + 1;
         Put (File, Data (L_Temp));
         L_Temp := Next (L_Temp);
      end loop;

      Trace (Me, "got" & Num_Lines'Img & " lines of diff");

      Close (File);
      Insert (Kernel,
              -"CVS: Got comparison for file " & Full,
              Mode => Verbose);

      Display_Differences
        (Kernel, New_File => Current_File, Diff_File => Patch_File);
      Delete (Patch_File);

      if Full'Length > 5
        and then Full (Full'Last - 4 .. Full'Last) = "$orig"
      then
         Delete (Current_File);
      end if;

      return True;
   end Diff_Handler;

   -------------------------------
   -- Intermediate_Diff_Handler --
   -------------------------------

   function Intermediate_Diff_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean
   is
      pragma Unreferenced (Kernel);
      use String_List;

      Orig_File    : constant String := String_List.Head (Head);
      File         : File_Type;
      L_Temp       : List_Node := First (List);
   begin
      Create (File, Name => Orig_File);

      while L_Temp /= Null_Node loop
         Put (File, Data (L_Temp));
         L_Temp := Next (L_Temp);
      end loop;

      Close (File);

      return True;
   end Intermediate_Diff_Handler;

   ----------
   -- Diff --
   ----------

   procedure Diff
     (Rep       : access CVS_Record;
      File      : VFS.Virtual_File;
      Version_1 : String := "";
      Version_2 : String := "")
   is
      use String_List, GNAT.OS_Lib;

      C               : External_Command_Access;
      Command_Head    : List;
      Args            : List;
      Diff_Args       : Argument_List_Access :=
        Argument_String_To_List (Get_Pref (Rep.Kernel, Diff_Cmd));

   begin
      --  Use -f to avoid e.g. -u or -c options set in ~/.cvsrc

      Append (Args, "-f");
      Append (Args, "diff");

      --  Take into account diff arguments (e.g. -b).
      --  Try to avoid obviously incompatible switches: -u, -c

      for J in Diff_Args'First + 1 .. Diff_Args'Last loop
         if Diff_Args (J).all /= "-u"
           and then Diff_Args (J).all /= "-c"
         then
            Append (Args, Diff_Args (J).all);
         end if;
      end loop;

      Free (Diff_Args);

      if Version_1 /= "" then
         Append (Args, "-r" & Version_1);

         declare
            C_2       : External_Command_Access;
            Args_2    : List;
            Orig_File : constant String :=
              Get_Tmp_Dir & Base_Name (File) & "$orig";

         begin
            Append (Command_Head, Orig_File);

            Append (Args_2, "-q");
            Append (Args_2, "update");
            Append (Args_2, "-p");
            Append (Args_2, Base_Name (File));

            Create (C_2,
                    Rep.Kernel,
                    Get_Pref (Rep.Kernel, CVS_Command),
                    Dir_Name (File),
                    Args_2,
                    Command_Head,
                    Intermediate_Diff_Handler'Access,
                    -"CVS: Querying differences");

            Launch_Background_Command
              (Rep.Kernel, Command_Access (C_2), False, CVS_Identifier);

            Free (Args_2);
         end;
      end if;

      if Version_2 /= "" then
         Append (Args, "-r" & Version_2);
      else
         Append (Args, "-rHEAD");
      end if;

      Append (Args, Base_Name (File));

      if Is_Empty (Command_Head) then
         Append (Command_Head, Full_Name (File));
      end if;

      Insert (Rep.Kernel,
              -"CVS: Getting comparison for file "
              & Full_Name (File) & "...",
              Mode => Verbose);

      Create (C,
              Rep.Kernel,
              Get_Pref (Rep.Kernel, CVS_Command),
              Dir_Name (File),
              Args,
              Command_Head,
              Diff_Handler'Access,
              -"CVS: Querying differences");

      Launch_Background_Command
        (Rep.Kernel, Command_Access (C), False, CVS_Identifier);

      Free (Command_Head);
      Free (Args);
   end Diff;

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
      Length       : constant Integer := String_List.Length (List) - 2;
      Current_File : constant Virtual_File :=
        Create (Full_Filename => String_List.Head (Head));
      A            : Line_Information_Array (1 .. Length);
      Index        : Natural;

   begin
      --  ??? This assumes that the file currently opened is identical to the
      --  file first checked-out from CVS. It would be necessary here to
      --  force a save of the file, and then to get a diff between that file
      --  and the one on CVS (which is given in the output of the annotation,
      --  by the way), and to compute from that diff which lines correspond to
      --  the annotated ones.

      if Length <= 0 then
         Insert
           (Kernel,
            -"CVS: No annotations available for file "
            & Full_Name (Current_File),
            Mode => Verbose);
         return False;
      end if;

      if Is_Open (Kernel, Current_File) then
         Open_File_Editor (Kernel, Current_File, Line => 0);
      else
         Open_File_Editor (Kernel, Current_File);
      end if;

      L_Temp := Next (Next (L_Temp));

      declare
         S : constant String := Data (L_Temp);
      begin
         Index := S'First;
         Skip_To_String (S, Index, "): ");
         Index := Index - S'First + 2;
      end;

      for J in 1 .. Length loop
         declare
            S : constant String := Data (L_Temp);
         begin
            A (J).Text := new String'(S (S'First .. S'First + Index));
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
      Current_File : constant Virtual_File :=
        Create (Full_Filename => String_List.Head (Head));
      Text_File    : constant Virtual_File :=
        Create (Full_Filename => Get_Tmp_Dir & Base_Name (Current_File));
      File         : File_Type;

   begin
      Create (File, Name => Full_Name (Text_File));

      while L_Temp /= Null_Node loop
         Put (File, Data (L_Temp));
         L_Temp := Next (L_Temp);
      end loop;

      Close (File);
      Open_File_Editor (Kernel, Text_File);
      Delete (Text_File);

      return True;
   end Text_Output_Handler;

   ---------
   -- Log --
   ---------

   procedure Log
     (Rep  : access CVS_Record;
      File : VFS.Virtual_File)
   is
      use String_List;

      C            : External_Command_Access;
      Command_Head : List;
      Args         : List;

   begin
      Append (Args, "log");
      Append (Args, Base_Name (File));
      Append (Command_Head, Base_Name (File) & "$changelog");

      Create
        (C,
         Rep.Kernel,
         Get_Pref (Rep.Kernel, CVS_Command),
         Dir_Name (File),
         Args,
         Command_Head,
         Text_Output_Handler'Access,
         -"CVS: Querying revision history");

      Launch_Background_Command
        (Rep.Kernel, Command_Access (C), False, CVS_Identifier);

      Free (Command_Head);
      Free (Args);
   end Log;

   --------------
   -- Annotate --
   --------------

   procedure Annotate
     (Rep  : access CVS_Record;
      File : VFS.Virtual_File)
   is
      use String_List;

      C            : External_Command_Access;
      Command_Head : List;
      Args         : List;

   begin
      Append (Args, "annotate");
      Append (Args, Base_Name (File));
      Append (Command_Head, Full_Name (File));

      Create
        (C,
         Rep.Kernel,
         Get_Pref (Rep.Kernel, CVS_Command),
         Dir_Name (File),
         Args,
         Command_Head,
         Annotation_Output_Handler'Access,
         -"CVS: Querying annotations");

      Launch_Background_Command
        (Rep.Kernel, Command_Access (C), False, CVS_Identifier);

      Free (Command_Head);
      Free (Args);
   end Annotate;

   ------------------
   -- Handle_Error --
   ------------------

   procedure Handle_Error
     (Rep : access CVS_Record;
      S   : String) is
   begin
      Set_Error (Rep, S);
   end Handle_Error;

   ------------------
   -- Identify_VCS --
   ------------------

   function Identify_VCS (S : String) return VCS_Access is
      Id         : String := S;
      Identifier : String := CVS_Identifier;

   begin
      To_Lower (Id);
      To_Lower (Identifier);

      if Strip_Quotes (Id) = Identifier then
         return VCS_CVS_Module_ID.CVS_Reference;
      end if;

      return null;
   end Identify_VCS;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (Id : in out VCS_CVS_Module_ID_Record) is
   begin
      Unregister_VCS_Identifier (Identify_VCS'Access);
      Free (Id.CVS_Reference);
   end Destroy;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class) is
   begin
      VCS_CVS_Module_ID := new VCS_CVS_Module_ID_Record;
      Register_VCS_Identifier (Identify_VCS'Access);
      Register_Module
        (Module                  => Module_ID (VCS_CVS_Module_ID),
         Kernel                  => Kernel,
         Module_Name             => VCS_CVS_Module_Name,
         Priority                => Default_Priority,
         Contextual_Menu_Handler => null);

      VCS_CVS_Module_ID.CVS_Reference := new CVS_Record;
      VCS_CVS_Module_ID.CVS_Reference.Kernel := Kernel_Handle (Kernel);
      VCS_CVS_Module_ID.CVS_Reference.Queue  := New_Queue;

      Register_VCS (VCS_Module_ID, CVS_Identifier);
   end Register_Module;

end VCS.CVS;
