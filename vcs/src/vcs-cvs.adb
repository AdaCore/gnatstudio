-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                        Copyright (C) 2001                         --
--                            ACT-Europe                             --
--                                                                   --
-- GLIDE is free software; you can redistribute it and/or modify  it --
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

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Expect;               use GNAT.Expect;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Ada.Text_IO;               use Ada.Text_IO;

with String_Utils;              use String_Utils;

package body VCS.CVS is

   --  ??? Should we make commands customizable ?
   --  ??? Should we make expect timeouts customizable ?

   CVS_Command : constant String := "cvs";
   --  <preferences>

   The_CVS_Access : VCS_Access := new CVS_Record;

   -----------------------
   -- Local Subprograms --
   -----------------------

   function Idendify_VCS (S : String) return VCS_Access;
   --  Return an access to VCS_Record if S describes a CVS system.

   procedure Handle_Error
     (Rep : access CVS_Record;
      S   : String);
   --  Appends S at the end of current message.

   procedure Handle_Error
     (Rep : access CVS_Record;
      L   : String_List.List);
   --  Concats L at the end of current message.

   function Command
     (Rep               : access CVS_Record;
      Command           : String;
      Arguments         : Argument_List;
      Output_To_Message : Boolean := False) return String_List.List;
   --  Executes command Command with arguments Arguments and returns the result
   --  as a string list with one element per line of output.

   function Get_CVSROOT (Filename : String) return String;
   --  Return the CVSROOT corresponding to a file name.
   --  Filename must be an absolute file name.

   function Get_Path (Filename : String) return String;
   --  Returns the path to Filename.
   --  Filename is an absolute file name.
   --  Returns "" if no satisfactory path could be found.
   --  ??? Maybe this function should be implemented elsewhere.

   function Real_Get_Status
     (Rep         : access CVS_Record;
      Filenames   : String_List.List;
      Get_Status  : Boolean := True;
      Get_Version : Boolean := True;
      Get_Tags    : Boolean := False;
      Get_Users   : Boolean := False) return File_Status_List.List;
   --  Just like Get_Status, but assuming that Filenames is not empty
   --  and that all files in Filenames are from the same directory.

   function Real_Local_Get_Status
     (Rep         : access CVS_Record;
      Filenames   : String_List.List) return File_Status_List.List;
   --  Just like Local_Get_Status, but assuming that Filenames is not
   --  empty and that all files in Filenames are from the same directory.

   procedure Simple_Action
     (Rep               : access CVS_Record;
      Filenames         : String_List.List;
      Arguments         : String_List.List;
      Output_To_Message : Boolean := False);

   procedure Real_Simple_Action
     (Rep               : access CVS_Record;
      Filenames         : String_List.List;
      Arguments         : String_List.List;
      Output_To_Message : Boolean := False);
   --  Just like Simple_Action, but assuming that Filenames is not
   --  empty and that all files in Filenames are from the same directory.

   ----------
   -- Free --
   ----------

   procedure Free (Ref : access CVS_Record) is
   begin
      null;
   end Free;

   --------------
   -- Get_Path --
   --------------

   function Get_Path (Filename : String) return String is
   begin
      for J in reverse Filename'First .. Filename'Last loop
         if Filename (J) = Directory_Separator then
            return Filename (1 .. J);
         end if;
      end loop;

      return "";
   end Get_Path;

   -----------------
   -- Get_CVSROOT --
   -----------------

   function Get_CVSROOT (Filename : String) return String is
      File   : File_Type;
      Buffer : String (1 .. 1024);
      Last   : Integer;

   begin
      Open (File, In_File,
            Get_Path (Filename)
            & Directory_Separator & "CVS"
            & Directory_Separator & "Root");
      Get_Line (File, Buffer, Last);
      Close (File);

      return Buffer (1 .. Last);

   exception
      when Use_Error =>
         return "";
      when Name_Error =>
         return "";
   end Get_CVSROOT;

   -------------
   -- Command --
   -------------

   function Command
     (Rep               : access CVS_Record;
      Command           : String;
      Arguments         : Argument_List;
      Output_To_Message : Boolean := False) return String_List.List
   is
      Result : String_List.List;
      Fd     : Process_Descriptor;
      Match  : Expect_Match := 1;

   begin
      Non_Blocking_Spawn (Fd, Command, Arguments, Err_To_Out => True);

      begin
         if The_Idle_Function = null then
            while Match = 1 loop
               Expect (Fd, Match, "\n");
               declare
                  S : String := Expect_Out (Fd);
               begin
                  if Output_To_Message then
                     Set_Error (Rep, S (S'First .. S'Last - 1));
                  else
                     String_List.Prepend (Result, S (S'First .. S'Last - 1));
                  end if;
               end;
            end loop;
         else
            while Match = 1 loop
               Expect (Fd, Match, "\n",  Timeout);

               case Match is
                  when Expect_Timeout =>
                     The_Idle_Function.all;
                     Match := 1;
                  when others =>
                     declare
                        S : String := Expect_Out (Fd);
                     begin
                        if Output_To_Message then
                           Set_Error (Rep, S (S'First .. S'Last - 1));
                        else
                           String_List.Prepend
                             (Result, S (S'First .. S'Last - 1));
                        end if;
                     end;
               end case;
            end loop;
         end if;

      exception
         when Process_Died =>
            null;
      end;

      String_List.Rev (Result);
      return Result;
   end Command;

   ------------------------
   -- Real_Simple_Action --
   ------------------------

   procedure Real_Simple_Action
     (Rep               : access CVS_Record;
      Filenames         : String_List.List;
      Arguments         : String_List.List;
      Output_To_Message : Boolean := False)
   is
      use String_List;

      Filenames_Temp   : List := Filenames;
      Arguments_Temp   : List := Arguments;

      Output  : List;

      Filenames_Length : Natural := Length (Filenames);
      Args_Length      : Natural := Length (Arguments);

      Old_Dir : Dir_Name_Str := Get_Current_Dir;
      New_Dir : Dir_Name_Str := Get_Path (Head (Filenames));

   begin
      Change_Dir (New_Dir);

      --  Build arguments list.

      if Head (Filenames) = New_Dir then
         declare
            Args : Argument_List (1 .. Args_Length);
         begin
            for J in 1 .. Args_Length loop
               Args (J) := new String'(Head (Arguments_Temp));
               Arguments_Temp := Next (Arguments_Temp);
            end loop;

            Output := Command (Rep, CVS_Command, Args, Output_To_Message);

            for J in Args'Range loop
               Free (Args (J));
            end loop;
         end;

      else
         declare
            Args : Argument_List (1 .. Filenames_Length + Args_Length);
         begin
            for J in 1 .. Args_Length loop
               Args (J) := new String'(Head (Arguments_Temp));
               Arguments_Temp := Next (Arguments_Temp);
            end loop;

            for J in Args_Length + 1 .. Args_Length + Filenames_Length loop
               Args (J) := new String'(Base_Name (Head (Filenames_Temp)));
               Filenames_Temp := Next (Filenames_Temp);
            end loop;
            Output := Command (Rep, CVS_Command, Args, Output_To_Message);

            for J in Args'Range loop
               Free (Args (J));
            end loop;
         end;
      end if;

      --  Treat any output as an error, since all actions are
      --  supposed to be quiet.

      Handle_Error (Rep, Output);

      Change_Dir (Old_Dir);

   exception
      when Directory_Error =>
         Handle_Error (Rep, "Could not open directory");

   end Real_Simple_Action;

   -------------------
   -- Simple_Action --
   -------------------

   procedure Simple_Action
     (Rep               : access CVS_Record;
      Filenames         : String_List.List;
      Arguments         : String_List.List;
      Output_To_Message : Boolean := False)
   is
      use String_List;

      Current_Filename : String_List.List := Filenames;
   begin
      if Is_Empty (Current_Filename) then
         --  ??? Set an error here.
         return;
      end if;

      while not Is_Empty (Current_Filename) loop

         --  Extract a list of files that belong to the same directory.
         declare
            Current_Directory : String := Get_Path (Head (Current_Filename));
            Current_List      : String_List.List;
         begin
            while not Is_Empty (Current_Filename)
              and then Get_Path (Head (Current_Filename)) = Current_Directory
            loop
               Append (Current_List, Head (Current_Filename));
               Current_Filename := Next (Current_Filename);
            end loop;

            --  At this point, Current_List should not be empty and
            --  all its element are files from Current_Directory.
            Real_Simple_Action (Rep, Current_List,
                                Arguments, Output_To_Message);
            Free (Current_List);
         end;
      end loop;
   end Simple_Action;

   ---------------------
   -- Real_Get_Status --
   ---------------------

   function Real_Get_Status
     (Rep         : access CVS_Record;
      Filenames   : String_List.List;
      Get_Status  : Boolean := True;
      Get_Version : Boolean := True;
      Get_Tags    : Boolean := False;
      Get_Users   : Boolean := False) return File_Status_List.List
   is
      use String_List;

      Files   : List := Filenames;

      Filenames_Length : Natural := Length (Filenames);
      Result  : File_Status_List.List;
      Old_Dir : Dir_Name_Str := Get_Current_Dir;
      New_Dir : Dir_Name_Str := Get_Path (Head (Filenames));

      Output  : List;

      Blank_Status   : File_Status_Record;
      Current_Status : File_Status_Record := Blank_Status;
   begin
      --  ??? Need to take parameters into account and fill the
      --  corresponding information accordingly.

      Change_Dir (New_Dir);

      --  Generate arguments list.
      --  If the first argument is a directory, do a simple query for
      --  all files in that directory.

      if Head (Filenames) = New_Dir then
         declare
            Args    : Argument_List (1 .. 2);
         begin
            Args (1) := new String' ("status");
            Args (2) := new String' ("-l");

            --  Spawn command.
            Output := Command (Rep, CVS_Command, Args);

            Free (Args (1));
         end;
      else
         declare
            Args    : Argument_List (1 .. Filenames_Length + 1);
         begin
            Args (1) := new String' ("status");

            for J in 2 .. Filenames_Length + 1 loop
               Args (J) := new String'(Base_Name (Head (Files)));
               Files := Next (Files);
            end loop;

            --  Spawn command.
            Output := Command (Rep, CVS_Command, Args);

            --  Free arguments list.
            for J in Args'Range loop
               Free (Args (J));
            end loop;
         end;
      end if;

      --  Parse output of the command.

      while not Is_Empty (Output) loop
         declare
            Line       : String := Head (Output);
            Index      : Natural;
            Next_Index : Natural;
         begin
            if Line'Length > 4
              and then Line (Line'First .. Line'First + 3) = "===="
            then
               --  Upon encounter of "====", append the status to the result.
               if Current_Status /= Blank_Status then
                  File_Status_List.Append (Result, Current_Status);
               end if;
               Current_Status := Blank_Status;

            elsif Line'Length > 5
              and then Line (Line'First .. Line'First + 4) = "File:"
            then
               --  Upon encounter of "File:", parse the status of the file.

               Index := Line'First + 6;
               Skip_To_Char (Line, Index, ASCII.HT);
               Append (Current_Status.File_Name,
                       New_Dir & Strip_Quotes (Line (7 .. Index - 1)));
               --  ??? Maybe we should use Strip_Blanks.

               Index := Line'First;
               Skip_To_String (Line, Index, "Status:");
               Index := Index + 8;

               if Line'Last >= Index + 6
                 and then Line (Index .. Index + 6) = "Unknown"
               then
                  Current_Status.Status := Not_Registered;
               elsif Line'Last >= Index + 15
                 and then Line (Index .. Index + 15) = "Locally Modified"
               then
                  Current_Status.Status := Modified;
               elsif Line'Last >= Index + 14
                 and then Line (Index .. Index + 14) = "Locally Removed"
               then
                  Current_Status.Status := Not_Registered;
                  declare
                     S : String := Head (Current_Status.File_Name);
                  begin
                     if S (S'First + New_Dir'Length
                           .. S'First + New_Dir'Length + 7) = "no file "
                     then
                        Free (Current_Status.File_Name);
                        Append (Current_Status.File_Name,
                                New_Dir &
                                S (S'First + New_Dir'Length + 8 .. S'Last));
                     end if;
                  end;
               elsif Line'Last >= Index + 10
                 and then Line (Index .. Index + 10) = "Needs Merge"
               then
                  Current_Status.Status := Needs_Merge;
               elsif Line'Last >= Index + 10
                 and then Line (Index .. Index + 10) = "Needs Patch"
               then
                  Current_Status.Status := Needs_Update;
               elsif Line'Last >= Index + 9
                 and then Line (Index .. Index + 9) =  "Up-to-date"
               then
                  Current_Status.Status := Up_To_Date;
               elsif Line'Last >= Index + 13
                 and then Line (Index .. Index + 13) = "Needs Checkout"
               then
                  Current_Status.Status := Needs_Update;
               elsif Line'Last > Index + 13
                 and then Line (Index .. Index + 13) = "File had confl"
               then
                  Current_Status.Status := Modified;
               end if;

            elsif Line'Length > 14
              and then Line (Line'First .. Line'First + 13) = "   Working rev"
            then
               Index := Line'First + 10;
               Skip_To_Char (Line, Index, ASCII.HT);
               if Current_Status.Status /= Unknown
                 and then Current_Status.Status /= Not_Registered
               then
                  Skip_Blanks (Line (Index .. Line'Last), Index);
                  Next_Index := Index + 1;
                  Skip_To_Blank (Line (Index .. Line'Last), Next_Index);
                  if Next_Index > Line'Last then
                     Next_Index := Line'Last;
                  end if;
                  Append (Current_Status.Working_Revision,
                          Line (Index .. Next_Index));
               end if;

            elsif Line'Length > 15
              and then Line (Line'First .. Line'First + 14) = "   Repository r"
            then
               Index := Line'First + 10;
               Skip_To_Char (Line, Index, ASCII.HT);
               if Current_Status.Status /= Unknown
                 and then Current_Status.Status /= Not_Registered
               then
                  Skip_Blanks (Line (Index .. Line'Last), Index);
                  Next_Index := Index + 1;
                  Skip_To_Blank (Line (Index .. Line'Last), Next_Index);
                  if Next_Index > Line'Last then
                     Next_Index := Line'Last;
                  end if;
                  Append (Current_Status.Repository_Revision,
                          Line (Index .. Next_Index));
               end if;
            end if;

            Tail (Output);
         end;
      end loop;

      --  Append the last status.
      File_Status_List.Append (Result, Current_Status);

      Change_Dir (Old_Dir);
      return Result;

   exception
      when Directory_Error =>
         Handle_Error (Rep, "Could not open directory");
         return Result;
   end Real_Get_Status;

   ---------------------------
   -- Real_Local_Get_Status --
   ---------------------------

   function Real_Local_Get_Status
     (Rep         : access CVS_Record;
      Filenames   : String_List.List) return File_Status_List.List
   is
      use String_List;

      Result  : File_Status_List.List;

      Old_Dir : Dir_Name_Str := Get_Current_Dir;
      New_Dir : Dir_Name_Str := Get_Path (Head (Filenames));

      Blank_Status   : File_Status_Record;
      Current_Status : File_Status_Record := Blank_Status;

      File   : File_Type;
      Buffer : String (1 .. 8192);
      Last   : Integer := 1;

      Index  : Natural;
      Next_Index : Natural;

   begin
      Change_Dir (New_Dir);

      --  Open and parse the Entries file.

      Open (File, In_File, New_Dir & "CVS" & Directory_Separator & "Entries");


      while Last >= 0
        and then not End_Of_File (File)
      loop
         Get_Line (File, Buffer, Last);
         Index := 2;
         Skip_To_Char (Buffer (1 .. Last), Index, '/');
         Next_Index := Index + 1;
         Skip_To_Char (Buffer (1 .. Last), Next_Index, '/');

         Append (Current_Status.File_Name, New_Dir & Buffer (2 .. Index - 1));
         Append (Current_Status.Working_Revision,
                 Buffer (Index + 1 .. Next_Index - 1));

         if Index + 1 < Next_Index - 1 then
            File_Status_List.Append (Result, Current_Status);
         else
            Free (Current_Status.File_Name);
            Free (Current_Status.Working_Revision);
         end if;

         Current_Status := Blank_Status;
      end loop;

      Close (File);
      Change_Dir (Old_Dir);
      return Result;

   exception
      when End_Error =>
         Close (File);
         return Result;
      when Use_Error =>
         return Result;
      when Name_Error =>
         return Result;
   end Real_Local_Get_Status;

   ----------------
   -- Get_Status --
   ----------------

   function Get_Status
     (Rep         : access CVS_Record;
      Filenames   : String_List.List;
      Get_Status  : Boolean := True;
      Get_Version : Boolean := True;
      Get_Tags    : Boolean := False;
      Get_Users   : Boolean := False) return File_Status_List.List
   is
      Result           : File_Status_List.List;
      Current_Filename : String_List.List := Filenames;

      use String_List;
   begin
      if Is_Empty (Current_Filename) then
         return Result;
      end if;

      while not Is_Empty (Current_Filename) loop
         --  Extract a list of files that belong to the same directory.

         declare
            Current_Directory : String := Get_Path (Head (Current_Filename));
            Current_List      : String_List.List;

         begin
            while not Is_Empty (Current_Filename)
              and then Get_Path (Head (Current_Filename)) = Current_Directory
            loop
               Append (Current_List, Head (Current_Filename));
               Current_Filename := Next (Current_Filename);
            end loop;

            --  At this point, Current_List should not be empty and
            --  all its element are files from Current_Directory.

            File_Status_List.Concat
              (Result,
               Real_Get_Status
                 (Rep,
                  Current_List,
                  Get_Status,
                  Get_Version,
                  Get_Tags,
                  Get_Users));

            Free (Current_List);
         end;
      end loop;

      return Result;
   end Get_Status;

   ----------------------
   -- Local_Get_Status --
   ----------------------

   function Local_Get_Status
     (Rep         : access CVS_Record;
      Filenames   : String_List.List) return File_Status_List.List
   is
      Result           : File_Status_List.List;
      Current_Filename : String_List.List := Filenames;

      use String_List;

   begin
      if Is_Empty (Current_Filename) then
         return Result;
      end if;

      while not Is_Empty (Current_Filename) loop

         --  Extract a list of files that belong to the same directory.
         declare
            Current_Directory : String := Get_Path (Head (Current_Filename));
            Current_List      : String_List.List;

         begin
            while not Is_Empty (Current_Filename)
              and then Get_Path (Head (Current_Filename)) = Current_Directory
            loop
               Append (Current_List, Head (Current_Filename));
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
      Arguments : String_List.List;
   begin
      String_List.Append (Arguments, "-Q");
      String_List.Append (Arguments, "edit");

      Simple_Action (Rep, Filenames, Arguments);

      String_List.Free (Arguments);
   end Open;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (Rep       : access CVS_Record;
      Filenames : String_List.List;
      Logs      : String_List.List)
   is
      Arguments      : String_List.List;
      Filenames_Temp : String_List.List := Filenames;
      Logs_Temp      : String_List.List := Logs;
      Single_File    : String_List.List;

      use String_List;

   begin
      while not Is_Empty (Filenames_Temp) loop
         Append (Arguments, "-Q");
         Append (Arguments, "commit");
         Append (Arguments, "-m");
         Append (Arguments, Head (Logs_Temp));

         Append (Single_File, Head (Filenames_Temp));

         Simple_Action (Rep, Single_File, Arguments);

         Free (Arguments);
         Free (Single_File);

         Logs_Temp      := Next (Logs_Temp);
         Filenames_Temp := Next (Filenames_Temp);
      end loop;

   exception
      when List_Empty =>
         Handle_Error (Rep, "Log list incomplete !");
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

      Simple_Action (Rep, Filenames, Arguments, True);
      String_List.Free (Arguments);
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
      Simple_Action (Rep, Filenames, Arguments, True);
      String_List.Free (Arguments);
   end Merge;

   ---------
   -- Add --
   ---------

   procedure Add
     (Rep       : access CVS_Record;
      Filenames : String_List.List)
   is
      Arguments : String_List.List;
   begin
      String_List.Append (Arguments, "-Q");
      String_List.Append (Arguments, "add");

      Simple_Action (Rep, Filenames, Arguments);

      String_List.Free (Arguments);
      String_List.Append (Arguments, "-Q");
      String_List.Append (Arguments, "commit");
      String_List.Append (Arguments, "-m");

      String_List.Append (Arguments, "Initial revision for this file.");
      --  ??? This should be customizable.

      Simple_Action (Rep, Filenames, Arguments);
      String_List.Free (Arguments);
   end Add;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (Rep       : access CVS_Record;
      Filenames : String_List.List)
   is
      Arguments : String_List.List;
   begin
      String_List.Append (Arguments, "-Q");
      String_List.Append (Arguments, "remove");
      String_List.Append (Arguments, "-f");

      Simple_Action (Rep, Filenames, Arguments);
      String_List.Free (Arguments);
   end Remove;

   ----------
   -- Diff --
   ----------

   function Diff
     (Rep       : access CVS_Record;
      File      : String;
      Version_1 : String := "";
      Version_2 : String := "") return String_List.List
   is
      Result : String_List.List;
   begin
      if Version_1 = ""
        and then Version_2 = ""
      then
         declare
            Old_Dir : Dir_Name_Str := Get_Current_Dir;
            New_Dir : Dir_Name_Str := Get_Path (File);

            Args    : Argument_List (1 .. 2);

         begin
            Change_Dir (New_Dir);
            Args (1) := new String' ("diff");
            Args (2) := new String' (Base_Name (File));

            Result := Command (Rep, CVS_Command, Args);

            Change_Dir (Old_Dir);

            for J in Args'Range loop
               Free (Args (J));
            end loop;
         end;
      end if;

      --  ??? deal with other cases

      return Result;
   end Diff;

   ---------
   -- Log --
   ---------

   function Log
     (Rep  : access CVS_Record;
      File : String) return String_List.List
   is
      Result  : String_List.List;
      Old_Dir : Dir_Name_Str := Get_Current_Dir;
      New_Dir : Dir_Name_Str := Get_Path (File);
      Args    : Argument_List (1 .. 2);

   begin
      Change_Dir (New_Dir);
      Args (1) := new String' ("log");
      Args (2) := new String' (Base_Name (File));

      Result := Command (Rep, CVS_Command, Args);

      Change_Dir (Old_Dir);

      for J in Args'Range loop
         Free (Args (J));
      end loop;

      return Result;

   exception
      when Directory_Error =>
         Set_Error (Rep, "Directory error : cannot access " & New_Dir);
      return Result;
   end Log;

   --------------
   -- Annotate --
   --------------

   function Annotate
     (Rep  : access CVS_Record;
      File : String) return String_List.List
   is
      Result  : String_List.List;
      Old_Dir : Dir_Name_Str := Get_Current_Dir;
      New_Dir : Dir_Name_Str := Get_Path (File);
      Args    : Argument_List (1 .. 2);

   begin
      Change_Dir (New_Dir);
      Args (1) := new String' ("annotate");
      Args (2) := new String' (Base_Name (File));

      Result := Command (Rep, CVS_Command, Args);

      Change_Dir (Old_Dir);

      for J in Args'Range loop
         Free (Args (J));
      end loop;

      return Result;

   exception
      when Directory_Error =>
         Set_Error (Rep, "Directory error : cannot access " & New_Dir);
      return Result;
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

   procedure Handle_Error
     (Rep : access CVS_Record;
      L   : String_List.List)
   is
      Temp_L : String_List.List := L;
   begin
      while not String_List.Is_Empty (Temp_L) loop
         Set_Error (Rep, String_List.Head (Temp_L));

         Temp_L := String_List.Next (Temp_L);
      end loop;

   end Handle_Error;

   ------------------
   -- Idendify_VCS --
   ------------------

   function Idendify_VCS (S : String) return VCS_Access is
      Id : String := S;
   begin
      Lower_Case (Id);
      if Strip_Quotes (Id) = "cvs" then
         return The_CVS_Access;
      end if;

      return null;
   end Idendify_VCS;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module is
   begin
      Register_VCS_Identifier (Idendify_VCS'Access);
   end Register_Module;

end VCS.CVS;
