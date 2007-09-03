-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2002-2007, AdaCore                 --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;

with GNAT.Calendar.Time_IO;     use GNAT.Calendar.Time_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;
with GNAT.Strings;

with Gtk.Enums;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with Templates_Parser;          use Templates_Parser;

with Basic_Mapper;              use Basic_Mapper;
with Commands;                  use Commands;
with Commands.VCS;              use Commands.VCS;
with Commands.External;         use Commands.External;
with File_Utils;                use File_Utils;
with String_Utils;              use String_Utils;
with GPS.Kernel.Console;        use GPS.Kernel.Console;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Task_Manager;   use GPS.Kernel.Task_Manager;
with GPS.Kernel.Project;        use GPS.Kernel.Project;
with GPS.Intl;                  use GPS.Intl;
with GPS.Location_View;         use GPS.Location_View;
with Projects;                  use Projects;
with Projects.Registry;         use Projects.Registry;
with Traces;                    use Traces;
with VFS;                       use VFS;
with VCS_Module;                use VCS_Module;
with VCS_Status;                use VCS_Status;
with VCS_Utils;                 use VCS_Utils;
with VCS_View;                  use VCS_View;

package body Log_Utils is

   function Check_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean;
   --  Display Head in the console, then return True if List is not
   --  empty, otherwise display List in the console and return False.

   --------------------------
   -- Action_To_Log_Suffix --
   --------------------------

   function Action_To_Log_Suffix (Action : VCS_Action) return String is
   begin
      case Action is
         when Commit =>
            return "$log";

         when others =>
            return "$" & Action'Img & "$log";
      end case;
   end Action_To_Log_Suffix;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Kernel : access Kernel_Handle_Record'Class) is
      use GNAT.OS_Lib;

      --  The format for the mappings file is as follows :
      --
      --      File_1
      --      Log_1
      --      File_2
      --      Log_2
      --      File_3
      --      Log_3
      --
      --  and so on.

      Logs_Dir : constant String := Get_Home_Dir (Kernel) & "log_files";
      Mapping  : constant Virtual_File :=
                   Create (Full_Filename => Logs_Dir & "/mapping");
      Mapper   : File_Mapper_Access;
      Button   : Message_Dialog_Buttons;
      Success  : Boolean;
      pragma Unreferenced (Button);

      --  Create the mappings file and read it

   begin
      if not Is_Directory (Logs_Dir) then
         Make_Dir (Logs_Dir);
      end if;

      if not Is_Regular_File (Mapping) then
         declare
            File : File_Descriptor;
         begin
            File := Create_New_File (Locale_Full_Name (Mapping), Text);
            Close (File);
         end;
      end if;

      begin
         Load_Mapper (Mapper, Full_Name (Mapping).all);
      exception
         when E : others =>
            Trace (Exception_Handle, E);
            Button := Message_Dialog
              (Msg     =>
                 (-"The file") & ASCII.LF & Full_Name (Mapping).all & ASCII.LF
                 & (-"is corrupted, and will be deleted."),
               Dialog_Type => Warning,
               Title   => -"Corrupted file.",
               Buttons => Button_OK,
               Parent  => Get_Current_Window (Kernel));

            Delete (Mapping, Success);

            declare
               File : File_Descriptor;
            begin
               File := Create_New_File (Locale_Full_Name (Mapping), Text);
               Close (File);
            end;

            Empty_Mapper (Mapper);
      end;

      Set_Logs_Mapper (Kernel, Mapper);
   end Initialize;

   -----------------------------
   -- Get_ChangeLog_From_File --
   -----------------------------

   function Get_ChangeLog_From_File
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : VFS.Virtual_File) return VFS.Virtual_File
   is
      use GNAT.OS_Lib;

      procedure Add_Header (Pos : Positive; Date_Header : Boolean);
      --  Add ChangeLog headers at position POS in the file buffer content.
      --  If Date_Header is True, adds also the ISO date tag.

      function Get_GPS_User return String;
      --  Returns the global ChangeLog user name and e-mail to use. It returns
      --  GPS_CHANGELOG_USER environement variable value or "name  <e-mail>"
      --  if not found or "name  <user@>" if USER environment variable is set.

      ChangeLog   : aliased String  := Dir_Name (File_Name).all & "ChangeLog";
      Date_Tag    : constant String := Image (Clock, ISO_Date);
      Base_Name   : constant String := VFS.Base_Name (File_Name);
      CL_File     : Virtual_File;   -- ChangeLog file
      CL          : String_Access;  -- ChangeLog content
      W_File      : Writable_File;  -- ChangeLog write access
      First, Last : Natural;
      F           : Natural;

      ------------------
      -- Get_GPS_User --
      ------------------

      function Get_GPS_User return String is
         User_Ptr : String_Access   := Getenv ("USER");
         GCU_Ptr  : String_Access   := Getenv ("GPS_CHANGELOG_USER");
         User     : constant String := User_Ptr.all;
         GCU      : constant String := GCU_Ptr.all;
      begin
         Free (User_Ptr);
         Free (GCU_Ptr);

         if GCU = "" then
            if User = "" then
               return "name  <e-mail>";
            else
               return "name  <" & User & "@>";
            end if;
         else
            if GCU (GCU'First) = '"' and then GCU (GCU'Last) = '"' then
               --  If this is quoted, remove the quotes. On Windows this will
               --  be quoted to avoid < and > to be interpreted.
               return GCU (GCU'First + 1 .. GCU'Last - 1);
            else
               return GCU;
            end if;
         end if;
      end Get_GPS_User;

      ----------------
      -- Add_Header --
      ----------------

      procedure Add_Header (Pos : Positive; Date_Header : Boolean) is
         Header   : constant String :=
                      Date_Tag & "  " & Get_GPS_User & ASCII.LF;
         F_Header : constant String :=
                      ASCII.HT & "* " & Base_Name & ':' & ASCII.LF &
                      ASCII.HT & ASCII.LF;

         Old      : String_Access := CL;
         New_Size : Natural;
      begin
         if CL = null then
            --  In this case Date_Header is always true
            CL := new String'(Header & ASCII.LF & F_Header);

         else
            New_Size := CL'Length + F_Header'Length + 1;

            if Date_Header then
               New_Size := New_Size + Header'Length + 1;

               CL := new String (1 .. New_Size);

               CL (1 .. Pos - 1) := Old (1 .. Pos - 1);
               CL (Pos .. Pos + Header'Length + F_Header'Length + 1) :=
                 Header & ASCII.LF & F_Header & ASCII.LF;
               CL (Pos + Header'Length + F_Header'Length + 2 .. CL'Last) :=
                 Old (Pos .. Old'Last);

            else
               CL := new String (1 .. New_Size);

               CL (1 .. Pos - 1) := Old (1 .. Pos - 1);
               CL (Pos .. Pos + F_Header'Length) := F_Header & ASCII.LF;
               CL (Pos + F_Header'Length + 1 .. CL'Last) :=
                 Old (Pos .. Old'Last);
            end if;
         end if;

         Free (Old);
      end Add_Header;

   begin
      --  Makes sure that the ChangeLog buffer is saved before continuing
      --  otherwise part of the ChangeLog file could be lost.

      Execute_GPS_Shell_Command
        (Kernel, "Editor.save_buffer", (1 => ChangeLog'Unchecked_Access));
      Execute_GPS_Shell_Command
        (Kernel, "Editor.close", (1 => ChangeLog'Unchecked_Access));

      --  Get ChangeLog content

      CL_File := Create (ChangeLog);
      CL      := Read_File (CL_File);
      W_File  := Write_File (CL_File);

      if CL = null then
         --  No ChangeLog content, add headers
         Add_Header (1, True);

      else
         First := Index (CL.all, Date_Tag);

         if First = 0 then
            --  No entry for this date
            Add_Header (1, True);

         else
            --  We have an entry for this date, look for file entry

            Last := First;

            while Last < CL.all'Last
              and then not (CL (Last) = ASCII.LF
                            and then CL (Last + 1) in '0' .. '9')
            loop
               Last := Last + 1;
            end loop;

            F := Index (CL (First .. Last), Base_Name);

            if F = 0 then
               --  No file entry for this date

               F := First;

               while CL (F) /= ASCII.LF and then F < CL'Last loop
                  F := F + 1;
               end loop;

               F := F + 1;

               while CL (F) = ASCII.CR or else CL (F) = ASCII.LF loop
                  F := F + 1;
               end loop;

               Add_Header (F, False);
            end if;
         end if;
      end if;

      Write (W_File, CL.all);
      Close (W_File);
      Free (CL);

      return CL_File;
   end Get_ChangeLog_From_File;

   -----------------------
   -- Get_Log_From_File --
   -----------------------

   function Get_Log_From_File
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : VFS.Virtual_File;
      Create    : Boolean;
      Suffix    : String := "$log") return VFS.Virtual_File
   is
      use GNAT.OS_Lib;

      Mapper      : File_Mapper_Access := Get_Logs_Mapper (Kernel);
      Real_Name   : constant String :=
                      Full_Name (File_Name, Normalize => True).all;
      Return_Name : constant String := Get_Other_Text (Mapper, Real_Name);
   begin
      --  ??? Right now, we save the mapping every time that we add
      --  an entry. This is a bit inefficient, we should save the mapping
      --  on disk only on exit.

      if Return_Name = ""
        and then Create
      then
         declare
            Logs_Dir : constant String := Get_Home_Dir (Kernel) & "log_files";
            File     : File_Descriptor;
            S        : Virtual_File := VFS.Create
              (Full_Filename =>
                 Logs_Dir & Directory_Separator & Base_Name (Real_Name)
                   & Suffix);
            --  In case there are multiple files with the same base name, see
            --  the loop below to use an alternate name and store it in the
            --  mapping file.

         begin
            if not Is_Regular_File (S) then
               File := Create_New_File (Locale_Full_Name (S), Text);
               Close (File);
               Add_Entry (Mapper,
                          Real_Name,
                          Full_Name (S, Normalize => True).all);
               Save_Mapper
                 (Mapper, Normalize_Pathname (Logs_Dir & "/mapping"));
               return S;

            else
               for J in Natural loop
                  S := VFS.Create
                    (Full_Filename =>
                       Logs_Dir & Directory_Separator
                       & Base_Name (Real_Name) & "$" & Image (J) & Suffix);

                  if not Is_Regular_File (S) then
                     File := Create_New_File (Locale_Full_Name (S), Text);
                     Close (File);
                     Add_Entry
                       (Mapper,
                        Real_Name,
                        Full_Name (S, Normalize => True).all);
                     Save_Mapper
                       (Mapper, Normalize_Pathname (Logs_Dir & "/mapping"));
                     return S;
                  end if;
               end loop;

               return VFS.No_File;
            end if;
         end;

      elsif Return_Name = "" then
         return VFS.No_File;

      else
         return VFS.Create (Full_Filename => Return_Name);
      end if;
   end Get_Log_From_File;

   -----------------------
   -- Get_File_From_Log --
   -----------------------

   function Get_File_From_Log
     (Kernel   : access Kernel_Handle_Record'Class;
      Log_Name : Virtual_File) return Virtual_File
   is
      Mapper : constant File_Mapper_Access := Get_Logs_Mapper (Kernel);
      F_Name : constant String := Get_Other_Text
        (Mapper, Full_Name (Log_Name, Normalize => True).all);
   begin
      if F_Name = "" then
         return No_File;
      else
         return Create (Full_Filename => F_Name);
      end if;
   end Get_File_From_Log;

   -------------
   -- Get_Log --
   -------------

   function Get_Log
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : VFS.Virtual_File) return String
   is
      use GNAT.OS_Lib;

      R : String_Access;
   begin
      R := Read_File (Get_Log_From_File (Kernel, File_Name, False));

      if R = null then
         return "";

      else
         declare
            S : constant String := R.all;
         begin
            Free (R);
            return S;
         end;
      end if;
   end Get_Log;

   ----------------------------
   -- Get_Log_From_ChangeLog --
   ----------------------------

   procedure Get_Log_From_ChangeLog
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : VFS.Virtual_File;
      Suffix    : String := "$log")
   is
      use GNAT.OS_Lib;

      ChangeLog : constant String := Dir_Name (File_Name).all & "ChangeLog";
      Log_File  : constant Virtual_File :=
                    Get_Log_From_File (Kernel, File_Name, False);
   begin
      if Log_File = VFS.No_File then
         declare
            Log_File     : constant Virtual_File :=
                             Get_Log_From_File
                               (Kernel, File_Name, True, Suffix);
            CL_File      : constant Virtual_File := Create (ChangeLog);
            Date_Tag     : constant String := Image (Clock, ISO_Date);
            Base_Name    : constant String := VFS.Base_Name (File_Name);
            CL           : String_Access := Read_File (CL_File);
            W_File       : Writable_File := Write_File (Log_File);
            First, Last  : Natural;
            F, L, P1, P2 : Natural;
         begin
            --  Create the log file and fill it with the log entry from the
            --  global ChangeLog.

            if CL = null then
               Close (W_File);
               return;
            end if;

            --  Now we parse the ChangeLog file to get the RH, a global
            --  ChangeLog has the following format:
            --
            --  <ISO-DATE>  <name>  <<e-mail>>
            --  <HT>* filename[, filename]:
            --  <HT>revision history
            --
            --  where:
            --
            --  <ISO-DATE>   A date with format YYYY-MM-DD
            --  <name>       A name, generally the developer name
            --  <<e-mail>>   The e-mail address of the developer surrounded
            --               with '<' and '>' characters.
            --  <HT>         Horizontal tabulation (or 8 spaces)

            First := Index (CL.all, Date_Tag);

            if First /= 0 then
               --  There is some ChangeLog entry for this date
               --  First check for Last character for log entries at this date

               Last := First;

               while Last < CL'Last
                 and then not (CL (Last) = ASCII.LF
                               and then CL (Last + 1) in '0' .. '9')
               loop
                  Last := Last + 1;
               end loop;

               --  Look for filename between '*' and ':'

               L := First;

               Fill_Log : while L < Last loop
                  F := L;

                  P1 := Index (CL (F .. Last), ASCII.HT & "*");

                  if P1 = 0 then
                     P1 := Index (CL (F .. Last), "        *");
                  end if;

                  if P1 = 0 then
                     exit Fill_Log;

                  else
                     P2 := P1;

                     for K in P1 .. Last loop
                        if CL (K) = ':' then
                           P2 := K;
                           exit;
                        end if;
                     end loop;

                     --  CL (P1 .. P2) defines a ChangeLog entry, look
                     --  for filename inside this slice

                     if Index (CL (P1 .. P2), Base_Name) /= 0 then
                        --  This is really the ChangeLog entry for this file

                        Write_RH : while P2 < Last loop
                           P1 := P2;

                           --  Look for first line

                           while CL (P1) /= ASCII.HT
                             and then CL (P1) /= ' '
                             and then P1 < Last
                           loop
                              P1 := P1 + 1;

                              exit Write_RH when
                                CL (P1) = ASCII.LF
                                and then
                                  (CL (P1 - 1) = ASCII.LF
                                   or else (P1 > 2
                                            and then CL (P1 - 1) = ASCII.CR
                                            and then CL (P1 - 2) = ASCII.LF));
                           end loop;

                           --  Now we have either an HT or 8 spaces

                           if CL (P1) = ASCII.HT then
                              P1 := P1 + 1;

                           else
                              --  Skip at most 8 spaces at the start of the
                              --  line.

                              for K in 1 .. 8 loop
                                 exit when CL (P1) /= ' ' or else P1 = Last;
                                 P1 := P1 + 1;
                              end loop;
                           end if;

                           P2 := P1;

                           --  Look for end of line

                           while CL (P2) /= ASCII.LF and then P2 < Last loop
                              P2 := P2 + 1;
                           end loop;

                           Write (W_File, CL (P1 .. P2));
                        end loop Write_RH;

                        exit Fill_Log;
                     end if;
                  end if;

                  L := P2 + 1;
               end loop Fill_Log;

            end if;

            Close (W_File);
            Free (CL);
         end;
      end if;
   end Get_Log_From_ChangeLog;

   ------------------------------
   -- Remove_File_From_Mapping --
   ------------------------------

   procedure Remove_File_From_Mapping
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : Virtual_File)
   is
      --  Need to call Name_As_Directory below, to properly handle windows
      --  directories.
      Logs_Dir : constant String :=
                   Name_As_Directory (Get_Home_Dir (Kernel) & "log_files");
      Mapper   : constant File_Mapper_Access := Get_Logs_Mapper (Kernel);
   begin
      Remove_Entry (Mapper, Full_Name (File_Name, Normalize => True).all);
      Save_Mapper (Mapper, Logs_Dir & "mapping");
   end Remove_File_From_Mapping;

   -------------------
   -- Check_Handler --
   -------------------

   function Check_Handler
     (Kernel : Kernel_Handle;
      Head   : String_List.List;
      List   : String_List.List) return Boolean
   is
      use String_List;

      List_Temp : String_List.List_Node := First (List);
      Head_Temp : String_List.List_Node := First (Head);
      Length    : Integer := 0;

   begin
      if not String_List.Is_Empty (List) then
         while Head_Temp /= Null_Node loop
            Console.Insert (Kernel, Data (Head_Temp), Mode => Console.Error);
            Head_Temp := Next (Head_Temp);
         end loop;
      end if;

      while List_Temp /= Null_Node loop
         declare
            S : constant String := Data (List_Temp);
         begin
            Console.Insert (Kernel, S, Mode => Console.Error);
            Length := Length + S'Length;
         end;

         List_Temp := Next (List_Temp);
      end loop;

      if Length /= 0 then
         declare
            S : String (1 .. Length);
         begin
            Length := 1;
            List_Temp := First (List);

            while List_Temp /= Null_Node loop
               declare
                  D : constant String := Data (List_Temp);
               begin
                  S (Length .. Length - 1 + D'Length) := D;
                  Length := Length + D'Length;
               end;

               List_Temp := Next (List_Temp);
            end loop;

            Parse_File_Locations (Kernel, S, -"Style/Log Check");
         end;
      end if;

      return String_List.Is_Empty (List);
   end Check_Handler;

   ----------------------
   -- Log_Action_Files --
   ----------------------

   procedure Log_Action_Files
     (Kernel   : Kernel_Handle;
      Ref      : VCS_Access;
      Action   : VCS_Action;
      Files    : String_List.List;
      Activity : Activity_Id)
   is
      use String_List;

      function Add_LF_To_Log (Log : in String) return String;
      --  Returns Log with LF added if there is no ending line separator. We
      --  call this routine to ensure that the catenated logs won't be put on
      --  the same line.

      -------------------
      -- Add_LF_To_Log --
      -------------------

      function Add_LF_To_Log (Log : in String) return String is
      begin
         if Log'Length > 0
           and then Log (Log'Last) /= ASCII.LF
           and then Log (Log'Last) /= ASCII.CR
         then
            return Log & ASCII.LF;
         else
            return Log;
         end if;
      end Add_LF_To_Log;

      Logs                   : String_List.List;
      Files_Temp             : List_Node := First (Files);

      Commit_Command         : Log_Action_Command_Access;
      Get_Status_Command     : Get_Status_Command_Access;
      Check_Activity_Command : Check_Activity_Command_Access;

      Project                : Project_Type;

      Success                : Boolean;
      pragma Unreferenced (Success);

      Cancel_All             : Boolean := False;

      Log_Checks             : External_Command_Access;
      File_Checks            : External_Command_Access;
      File                   : Virtual_File;

      First_Check, Last_Check : Command_Access := null;

   begin
      if not Save_Files (Kernel, Files, Activity, Save_Logs => True) then
         return;
      end if;

      --  Build the log for the commit

      if Activity = No_Activity then
         while Files_Temp /= Null_Node loop
            --  Save any open log editors, and then get the corresponding logs

            File := Create (Full_Filename => Data (Files_Temp));
            Append (Logs, Get_Log (Kernel, File));
            Files_Temp := Next (Files_Temp);
         end loop;

      else
         --  Create the Check_Activity command

         if Action = Commit then
            Create (Check_Activity_Command, Kernel, Activity);
         end if;

         if Get_Group_Commit (Activity) then
            declare
               T_Set : Translate_Set :=
                         Get_Activity_Template_Tags (Kernel, Activity);
            begin
               Insert (T_Set, Assoc ("IS_PATCH", False));
               Append
                 (Logs, Parse (Get_Activity_Log_Template (Kernel), T_Set));
            end;

         else
            while Files_Temp /= Null_Node loop
               --  Save any open log editors, and then get the corresponding
               --  logs.

               File := Create (Full_Filename => Data (Files_Temp));

               if Get_Log_From_File (Kernel, File, False) = VFS.No_File then
                  --  No individual logs
                  Append (Logs, Get_Log (Kernel, Activity));

               else
                  Append
                    (Logs,
                     Add_LF_To_Log (Get_Log (Kernel, File))
                     & Get_Log (Kernel, Activity));
               end if;

               Files_Temp := Next (Files_Temp);
            end loop;
         end if;
      end if;

      --  Create the Commit command

      Create (Commit_Command, Kernel, Ref, Action, Files, Logs);

      --  Create the Get_Status command

      Create (Get_Status_Command, Ref, Files);

      --  The Get_Status command is a consequence of the Commit command

      Add_Consequence_Action
        (Command_Access (Commit_Command),
         Command_Access (Get_Status_Command));

      --  The Check_Activity command is a consequence of the Get_Status command

      if Activity /= No_Activity and then Action = Commit then
         Add_Consequence_Action
           (Command_Access (Get_Status_Command),
            Command_Access (Check_Activity_Command));
      end if;

      --  Check if the activity log is not empty

      if Activity /= No_Activity and then Get_Log (Kernel, Activity) = "" then
         if Message_Dialog
           ((-"The activity log file is empty,")
            & ASCII.LF &
            (-"Commit anyway ?"),
            Confirmation,
            Button_Yes or Button_No,
            Button_Yes,
            "", -"Empty log detected",
            Gtk.Enums.Justify_Left) = Button_No
         then
            Cancel_All := True;
         end if;
      end if;

      --  Create the file checks and the log checks

      --  ??? Should we add check for the activity log and in this case which
      --  check script should be used.

      --  First clear the current category, this is done here and not in
      --  Check_Handler as we want to keep the output of multiple check
      --  commands.

      Remove_Location_Category (Kernel, -"Style/Log Check");

      Files_Temp := First (Files);

      while not Cancel_All and then Files_Temp /= Null_Node loop
         File := Create (Full_Filename => Data (Files_Temp));
         Project := Get_Project_From_File (Get_Registry (Kernel).all, File);

         if Project /= No_Project then
            declare
               use GNAT;

               File_Check_Command : constant String := Get_Attribute_Value
                 (Project, VCS_File_Check);
               Log_Check_Command  : constant String := Get_Attribute_Value
                 (Project, VCS_Log_Check);
               Log_File           : constant Virtual_File :=
                                      Get_Log_From_File
                                        (Kernel, File, False,
                                         Action_To_Log_Suffix (Action));
               File_Args          : String_List.List;
               Log_Args           : String_List.List;
               Head_List          : String_List.List;
               S                  : Strings.String_Access;
               C_Args             : OS_Lib.Argument_List_Access;

               use type GNAT.Strings.String_Access;

            begin
               if Has_Status
                   (Get_Status_Cache,
                    Create (Data (Files_Temp)), Ref, Removed_Id)
               then
                  --  Remove this file from the explorer now, we do not have to
                  --  check it as it is removed.

                  Remove_File
                    (Get_Explorer (Kernel, False, False),
                     Create (Data (Files_Temp)));

               else
                  --  Record a check command if necessary

                  if File_Check_Command /= "" then
                     C_Args := OS_Lib.Argument_String_To_List
                       (File_Check_Command);

                     --  Add all arguments from the command string

                     for K in C_Args'First + 1 .. C_Args'Last loop
                        Append (File_Args, C_Args (K).all);
                     end loop;

                     --  Add filename

                     Append (File_Args, Data (Files_Temp));

                     Create (File_Checks,
                             Kernel,
                             C_Args (C_Args'First).all,
                             "",
                             File_Args,
                             Null_List,
                             Check_Handler'Access,
                             -"Version Control: Checking files");

                     if First_Check = null then
                        First_Check := Command_Access (File_Checks);
                     else
                        Add_Consequence_Action (Last_Check, File_Checks);
                     end if;

                     Last_Check := Command_Access (File_Checks);
                     OS_Lib.Free (C_Args);
                  end if;
               end if;

               if Log_Check_Command /= ""
                 and then Log_File /= No_File
               then
                  --  Check that the log file is not empty

                  S := Read_File (Log_File);

                  if S = null then
                     Cancel_All := True;
                     Insert (Kernel,
                             (-"File could not be read: ")
                             & Full_Name (Log_File).all);

                     Free (File_Args);
                     Free (Log_Args);
                     Free (Head_List);
                     exit;

                  elsif S.all = "" then
                     if Message_Dialog
                       ((-"File: ") & Full_Name (File).all
                        & ASCII.LF & ASCII.LF &
                          (-"The revision log for this file is empty,")
                        & ASCII.LF &
                          (-"Commit anyway ?"),
                        Confirmation,
                        Button_Yes or Button_No,
                        Button_Yes,
                        "", -"Empty log detected",
                        Gtk.Enums.Justify_Left) = Button_No
                     then
                        Cancel_All := True;

                        GNAT.OS_Lib.Free (S);
                        Free (File_Args);
                        Free (Log_Args);
                        Free (Head_List);
                        exit;
                     end if;
                  end if;

                  GNAT.OS_Lib.Free (S);

                  C_Args := OS_Lib.Argument_String_To_List
                    (Log_Check_Command);

                  --  Add all arguments form the command string

                  for K in C_Args'First + 1 .. C_Args'Last loop
                     Append (Log_Args, C_Args (K).all);
                  end loop;

                  --  Add filename

                  Append (Log_Args, Full_Name (Log_File).all);
                  Append
                    (Head_List, -"File: " & Full_Name (File).all & ASCII.LF
                     & (-"The revision log does not pass the checks."));

                  Create
                    (Log_Checks,
                     Kernel,
                     C_Args (C_Args'First).all,
                     "",
                     Log_Args,
                     Head_List,
                     Check_Handler'Access,
                     -"Version Control: Checking file changelogs");

                  if First_Check = null then
                     First_Check := Command_Access (Log_Checks);
                  else
                     Add_Consequence_Action (Last_Check, Log_Checks);
                  end if;

                  Last_Check := Command_Access (Log_Checks);
                  OS_Lib.Free (C_Args);
               end if;

               Free (File_Args);
               Free (Log_Args);
               Free (Head_List);
            end;
         end if;

         Files_Temp := Next (Files_Temp);
      end loop;

      --  Execute the commit command after the last file check or log check
      --  command.

      if Last_Check /= null then
         Add_Consequence_Action (Last_Check, Commit_Command);
      else
         First_Check := Command_Access (Commit_Command);
      end if;

      if Cancel_All then
         Destroy (First_Check);
      else
         Launch_Background_Command
           (Kernel, First_Check, True, True, Name (Ref));
      end if;

      Free (Logs);
   end Log_Action_Files;

end Log_Utils;
