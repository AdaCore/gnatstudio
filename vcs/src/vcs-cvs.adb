-----------------------------------------------------------------------
--                          G L I D E  I I                           --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
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

with Gtk.Main;                  use Gtk.Main;
with Glide_Intl;                use Glide_Intl;
with Glide_Kernel;              use Glide_Kernel;
with Glide_Kernel.Modules;      use Glide_Kernel.Modules;
with Glide_Kernel.Console;      use Glide_Kernel.Console;

with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Ada.Text_IO;               use Ada.Text_IO;

with String_Utils;              use String_Utils;

with VCS_View_Pkg;              use VCS_View_Pkg;

package body VCS.CVS is

   --  ??? Should we make commands customizable ?
   --  ??? Should we make expect timeouts customizable ?

   CVS_Command : constant String := "cvs";
   --  <preferences>

   Tmp_Dir : constant String := "/tmp/";
   --  <preferences>

   CVS_Reference : VCS_Access;

   VCS_CVS_Module_ID : Module_ID;

   -----------------------
   -- Local Subprograms --
   -----------------------

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialize module information.

   function Identify_VCS (S : String) return VCS_Access;
   --  Return an access to VCS_Record if S describes a CVS system.

   procedure Handle_Error
     (Rep : access CVS_Record;
      S   : String);
   --  Appends S at the end of current message.

   procedure Handle_Error
     (Rep : access CVS_Record;
      L   : String_List.List);
   --  Concats L at the end of current message.

   function Atomic_Command (D : String_List_And_Handler_Access) return Boolean;

   procedure Command (Rep         : CVS_Access;
                      New_Command : in out Command_Record);
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

   procedure Real_Get_Status
     (Rep         : access CVS_Record;
      Filenames   : String_List.List);
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
      Arguments         : String_List.List);

   procedure Real_Simple_Action
     (Rep               : access CVS_Record;
      Filenames         : String_List.List;
      Arguments         : String_List.List);
   --  Just like Simple_Action, but assuming that Filenames is not
   --  empty and that all files in Filenames are from the same directory.

   ----------
   -- Free --
   ----------

   procedure Free (Ref : access CVS_Record) is
      pragma Unreferenced (Ref);
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

   --------------------
   -- Atomic_Command --
   --------------------

   function Atomic_Command
     (D : String_List_And_Handler_Access)
     return Boolean
   is
      Match  : Expect_Match := 1;
   begin
      Expect (D.Rep.Fd, Match, "\n",  10);

      case Match is
         when Expect_Timeout =>
            return True;
         when others =>
            declare
               S : String := Expect_Out (D.Rep.Fd);
            begin
               String_List.Prepend
                 (D.List, S (S'First .. S'Last - 1));
            end;

            return True;
      end case;

   exception
      when Process_Died =>
         Close (D.Rep.Fd);
         String_List.Rev (D.List);
         D.Handler (D.Rep.Kernel, D.Head, D.List);

         D.Rep.Command_In_Progress := False;

         if not Command_List.Is_Empty (D.Rep.Command_Queue) then
            declare
               New_Command : Command_Record
                 := Command_List.Head (D.Rep.Command_Queue);
            begin
               Command (D.Rep, New_Command);
            end;

            Command_List.Tail (D.Rep.Command_Queue);
         end if;

         Pop_State (D.Rep.Kernel);
         Destroy (D);

         return False;
   end Atomic_Command;

   -------------
   -- Command --
   -------------

   procedure Command (Rep         : CVS_Access;
                      New_Command : in out Command_Record)
   is
      R  : String_List_And_Handler_Access := new String_List_And_Handler;
      Id : Timeout_Handler_Id;
   begin
      if Rep.Command_In_Progress then
         Command_List.Append (Rep.Command_Queue, New_Command);
         return;
      end if;

      Push_State (Rep.Kernel, Processing);

      Rep.Command_In_Progress := True;

      R.Rep := CVS_Access (Rep);
      R.Head := New_Command.Head;
      R.Handler := New_Command.Handler;

      declare
         Args : Argument_List (1 .. String_List.Length (New_Command.Args));
         Temp_Args : String_List.List := New_Command.Args;

         Old_Dir : Dir_Name_Str := Get_Current_Dir;
         New_Dir : Dir_Name_Str := String_List.Head (New_Command.Dir);
      begin
         Change_Dir (New_Dir);

         for J in Args'Range loop
            Args (J) := new String' (String_List.Head (Temp_Args));
            Temp_Args := String_List.Next (Temp_Args);
         end loop;

         Non_Blocking_Spawn (Rep.Fd,
                             String_List.Head (New_Command.Command),
                             Args,
                             Err_To_Out => True);

         for J in Args'Range loop
            Free (Args (J));
         end loop;

         Temp_Args := New_Command.Args;
         String_List.Free (Temp_Args);

         Change_Dir (Old_Dir);

      exception
         when Directory_Error =>
            Set_Error (Rep, "Directory error : cannot access " & New_Dir);
      end;

      Id := String_List_Idle.Add (50, Atomic_Command'Access, R);
   end Command;

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
      exception
         when Process_Died =>
            null;
      end;

      String_List.Rev (Result);
      return Result;
   end Command;

   --------------------------
   -- Error_Output_Handler --
   --------------------------

   procedure Error_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List);

   procedure Error_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List)
   is
      L_Temp  : String_List.List := List;
      H_Temp  : String_List.List := Head;
   begin
      if not String_List.Is_Empty (List) then

         Insert (Kernel,
                 -"CVS output :",
                 Highlight_Sloc => False,
                 Mode => Info);

         while not String_List.Is_Empty (H_Temp) loop
            Insert (Kernel,
                    String_List.Head (H_Temp),
                    Highlight_Sloc => False,
                    Mode => Verbose);
            H_Temp := String_List.Next (H_Temp);
         end loop;

         while not String_List.Is_Empty (L_Temp) loop
            Insert (Kernel,
                    String_List.Head (L_Temp),
                    Highlight_Sloc => False,
                    Mode => Info);
            L_Temp := String_List.Next (L_Temp);
         end loop;
      end if;
   end Error_Output_Handler;

   ------------------------
   -- Real_Simple_Action --
   ------------------------

   procedure Real_Simple_Action
     (Rep               : access CVS_Record;
      Filenames         : String_List.List;
      Arguments         : String_List.List)
   is
      C : Command_Record;
   begin
      String_List.Append (C.Dir, Get_Path (String_List.Head (Filenames)));

      declare
         Args_Temp : String_List.List := Arguments;
      begin
         while not String_List.Is_Empty (Args_Temp) loop
            String_List.Append (C.Args, String_List.Head (Args_Temp));
            Args_Temp := String_List.Next (Args_Temp);
         end loop;
      end;

      String_List.Append (C.Command, CVS_Command);

      if String_List.Head (Filenames)
        /=  Get_Path (String_List.Head (Filenames))
      then
         declare
            Files_Temp : String_List.List := Filenames;
         begin
            while not String_List.Is_Empty (Files_Temp) loop
               String_List.Append (C.Args,
                                   Base_Name (String_List.Head (Files_Temp)));
               Files_Temp := String_List.Next (Files_Temp);
            end loop;
         end;
      end if;

      C.Handler := Error_Output_Handler'Access;
      Command (CVS_Access (Rep), C);
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
            Real_Simple_Action (Rep, Current_List, Arguments);
            Free (Current_List);
         end;
      end loop;
   end Simple_Action;

   --------------------------
   -- Status_Output_Handler --
   --------------------------

   procedure Status_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List);

   procedure Status_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List)
   is
      Result         : File_Status_List.List := File_Status_List.Null_List;
      Output         : String_List.List := List;
      Blank_Status   : File_Status_Record;
      Current_Status : File_Status_Record := Blank_Status;

      New_Dir        : String := String_List.Head (Head);

      use String_List;
   begin
      while not Is_Empty (Output) loop
         declare
            Line       : String := String_List.Head (Output);
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
                     S : String := String_List.Head (Current_Status.File_Name);
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

            Output := Next (Output);
         end;
      end loop;

      --  Append the last status.

      if not Is_Empty (Current_Status.File_Name) then
         File_Status_List.Append (Result, Current_Status);
      end if;

      Display_File_Status (Kernel, Result, True);
      File_Status_List.Free (Result);
   end Status_Output_Handler;

   ---------------------
   -- Real_Get_Status --
   ---------------------

   procedure Real_Get_Status
     (Rep         : access CVS_Record;
      Filenames   : String_List.List)
   is
      Files   : String_List.List := Filenames;

      C : Command_Record;
   begin
      --  ??? Need to take parameters into account and fill the
      --  corresponding information accordingly.

      String_List.Append (C.Dir, Get_Path (String_List.Head (Filenames)));
      String_List.Append (C.Command, CVS_Command);
      String_List.Append (C.Head, Get_Path (String_List.Head (Filenames)));

      --  Generate arguments list.
      --  If the first argument is a directory, do a simple query for
      --  all files in that directory.

      if String_List.Head (Filenames)
        = Get_Path (String_List.Head (Filenames))
      then
         String_List.Append (C.Args, "status");
         String_List.Append (C.Args, "-l");
      else
         String_List.Append (C.Args, "status");

         while not String_List.Is_Empty (Files) loop
            String_List.Append (C.Args, Base_Name (String_List.Head (Files)));
            Files := String_List.Next (Files);
         end loop;
      end if;

      C.Handler := Status_Output_Handler'Access;
      Command (CVS_Access (Rep), C);
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
      when Directory_Error =>
         Set_Error (Rep, "CVS: Could not open directory: " & New_Dir);
         return Result;
   end Real_Local_Get_Status;

   ----------------
   -- Get_Status --
   ----------------

   procedure Get_Status
     (Rep         : access CVS_Record;
      Filenames   : String_List.List)
   is
      Current_Filename : String_List.List := Filenames;

      use String_List;
   begin
      if Is_Empty (Current_Filename) then
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

            Real_Get_Status
              (Rep,
               Current_List);

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
      pragma Unreferenced (User_Name);
      Arguments : String_List.List;
   begin
      String_List.Append (Arguments, "-Q");
      String_List.Append (Arguments, "edit");

      Simple_Action (Rep, Filenames, Arguments);
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
      String_List.Append (Arguments, "-d");

      Simple_Action (Rep, Filenames, Arguments);
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
      String_List.Append (Arguments, "-Q");
      String_List.Append (Arguments, "add");

      Simple_Action (Rep, Filenames, Arguments);

      String_List.Append (Arguments_2, "-Q");
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
      String_List.Append (Arguments, "-Q");
      String_List.Append (Arguments, "remove");
      String_List.Append (Arguments, "-f");

      Simple_Action (Rep, Filenames, Arguments);

      String_List.Append (Arguments_2, "-Q");
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
      String_List.Append (Arguments, "-Q");
      String_List.Append (Arguments, "update");
      String_List.Append (Arguments, "-C");

      Simple_Action (Rep, Filenames, Arguments);
   end Revert;

   ------------------
   -- Diff_Handler --
   ------------------

   procedure Diff_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List);

   procedure Diff_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List)
   is
      L_Temp  : String_List.List := List;

      Success : Boolean;

      Current_File : constant String := String_List.Head (Head);
      Base         : constant String := Base_Name (Current_File);
      Patch_File   : constant String := Tmp_Dir & Base & "_difs";
      File         : File_Type;
   begin
      Create (File, Name => Patch_File);

      while not String_List.Is_Empty (L_Temp) loop
         Put_Line (File, String_List.Head (L_Temp));
         L_Temp := String_List.Next (L_Temp);
      end loop;

      String_List.Free (L_Temp);

      Close (File);

      Insert (Kernel,
              -"CVS: Got differences for file " & Current_File & ".",
              Highlight_Sloc => False,
              Mode => Verbose);

      Display_Differences
        (Kernel, New_File => Current_File, Diff_File => Patch_File);
      Delete_File (Patch_File, Success);
   end Diff_Handler;

   ----------
   -- Diff --
   ----------

   procedure Diff
     (Rep       : access CVS_Record;
      File      : String;
      Version_1 : String := "";
      Version_2 : String := "")
   is
      C       : Command_Record;
   begin
      if Version_1 = ""
        and then Version_2 = ""
      then
         String_List.Append (C.Dir, Get_Path (File));
         String_List.Append (C.Command, CVS_Command);
         String_List.Append (C.Args, "diff");
         String_List.Append (C.Args, Base_Name (File));
         String_List.Append (C.Head, File);
         C.Handler := Diff_Handler'Access;

         Insert (Rep.Kernel,
                 -"CVS: Getting differences for file " & File & "...",
                 Highlight_Sloc => False,
                 Mode => Verbose);

         Command (CVS_Access (Rep), C);
      end if;

      --  ??? deal with other cases (different versions)
   end Diff;

   -------------------------
   -- Text_Output_Handler --
   -------------------------

   procedure Text_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List);

   procedure Text_Output_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List)
   is
      L_Temp  : String_List.List := List;

      Success : Boolean;

      Current_File : constant String := String_List.Head (Head);
      Text_File    : constant String := Tmp_Dir & Base_Name (Current_File);
      File         : File_Type;
   begin
      Create (File, Name => Text_File);

      while not String_List.Is_Empty (L_Temp) loop
         Put_Line (File, String_List.Head (L_Temp));
         L_Temp := String_List.Next (L_Temp);
      end loop;

      String_List.Free (L_Temp);

      Close (File);

      Open_File_Editor (Kernel, Text_File);

      Delete_File (Text_File, Success);
   end Text_Output_Handler;

   ---------
   -- Log --
   ---------

   procedure Log
     (Rep  : access CVS_Record;
      File : String)
   is
      C       : Command_Record;
   begin
      String_List.Append (C.Dir, Get_Path (File));
      String_List.Append (C.Command, CVS_Command);
      String_List.Append (C.Args, "log");
      String_List.Append (C.Args, Base_Name (File));
      String_List.Append (C.Head, Base_Name (File) & "_changelog");
      C.Handler := Text_Output_Handler'Access;
      Command (CVS_Access (Rep), C);
   end Log;

   --------------
   -- Annotate --
   --------------

   procedure Annotate
     (Rep  : access CVS_Record;
      File : String)
   is
      C       : Command_Record;
   begin
      String_List.Append (C.Dir, Get_Path (File));
      String_List.Append (C.Command, CVS_Command);
      String_List.Append (C.Args, "annotate");
      String_List.Append (C.Args, Base_Name (File));
      String_List.Append (C.Head, Base_Name (File) & "_annotations");
      C.Handler := Text_Output_Handler'Access;
      Command (CVS_Access (Rep), C);
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
   -- Identify_VCS --
   ------------------

   function Identify_VCS (S : String) return VCS_Access is
      Id : String := S;
   begin
      Lower_Case (Id);

      if Strip_Quotes (Id) = "cvs" then
         return CVS_Reference;
      end if;

      return null;
   end Identify_VCS;

   -----------------------
   -- Initialize_Module --
   -----------------------

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
   begin
      CVS_Reference := new CVS_Record;
      CVS_Reference.Kernel := Kernel_Handle (Kernel);
   end Initialize_Module;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module is
   begin
      Register_VCS_Identifier (Identify_VCS'Access);
      VCS_CVS_Module_ID := Register_Module
        (Module_Name             => VCS_CVS_Module_Name,
         Priority                => Default_Priority,
         Initializer             => Initialize_Module'Access,
         Contextual_Menu_Handler => null);
   end Register_Module;

   -------------
   -- Destroy --
   -------------

   procedure Destroy (D : in String_List_And_Handler_Access)
   is
      D_Copy : String_List_And_Handler_Access := D;
   begin
      String_List.Free (D_Copy.List);
      String_List.Free (D_Copy.Head);
      Free (D_Copy);
   end Destroy;

   ----------
   -- Free --
   ----------

   procedure Free (D : in out Command_Record) is
      D_Copy : Command_Record := D;
   begin
      String_List.Free (D_Copy.Command);
      String_List.Free (D_Copy.Dir);

      --  We deliberately do not free D.Head here, since this list
      --  is passed to the
      --  String_List.Free (D.Head);
   end Free;

end VCS.CVS;
