-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002-2003                    --
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

with Ada.Calendar;              use Ada.Calendar;
with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Fixed;         use Ada.Strings.Fixed;
with Traces;                    use Traces;

with GNAT.Calendar.Time_IO;     use GNAT.Calendar.Time_IO;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

with Basic_Mapper;              use Basic_Mapper;
with File_Utils;                use File_Utils;
with String_Utils;              use String_Utils;
with Glide_Intl;                use Glide_Intl;
with Gtkada.Dialogs;            use Gtkada.Dialogs;
with VFS;                       use VFS;

package body Log_Utils is

   Me : constant Debug_Handle := Create ("Log_Utils");

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

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (Kernel : access Kernel_Handle_Record'Class) is
      Logs_Dir : constant String := Get_Home_Dir (Kernel) & "log_files";
      Mapping  : constant Virtual_File :=
        Create (Full_Filename => Logs_Dir & "/mapping");
      Mapper   : File_Mapper_Access;
      Button   : Message_Dialog_Buttons;
      pragma Unreferenced (Button);

      --  Create the mappings file and read it.

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
            Trace (Me, "unexpected exception: " & Exception_Information (E));

            Button := Message_Dialog
              (Msg     =>
                 (-"The file") & ASCII.LF & Full_Name (Mapping).all & ASCII.LF
                 & (-"is corrupted, and will be deleted."),
               Dialog_Type => Warning,
               Title   => -"Corrupted file.",
               Buttons => Button_OK,
               Parent  => Get_Main_Window (Kernel));

            Delete (Mapping);

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
     (File_Name : VFS.Virtual_File) return VFS.Virtual_File
   is
      ChangeLog : constant String := Dir_Name (File_Name).all & "ChangeLog";
   begin
      return VFS.Create (ChangeLog);
   end Get_ChangeLog_From_File;

   -----------------------
   -- Get_Log_From_File --
   -----------------------

   function Get_Log_From_File
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : VFS.Virtual_File;
      Create    : Boolean) return VFS.Virtual_File
   is
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
                   & "$log");
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
                       & Base_Name (Real_Name) & "$" & Image (J) & "$log");

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
   begin
      return Create
        (Full_Filename => Get_Other_Text
           (Mapper, Full_Name (Log_Name, Normalize => True).all));
   end Get_File_From_Log;

   -------------
   -- Get_Log --
   -------------

   function Get_Log
     (Kernel    : access Kernel_Handle_Record'Class;
      File_Name : VFS.Virtual_File) return String
   is
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
      File_Name : VFS.Virtual_File)
   is
      ChangeLog : constant String := Dir_Name (File_Name).all & "ChangeLog";
      Log_File  : constant Virtual_File :=
        Get_Log_From_File (Kernel, File_Name, False);
   begin
      if Log_File = No_File then
         declare
            Log_File     : constant Virtual_File :=
              Get_Log_From_File (Kernel, File_Name, True);
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
            --  <name>       A name, generally the developper name
            --  <<e-mail>>   The e-mail address of the developper surrounder
            --               with '<' and '>' characters.
            --  <HT>         Horizontal tabulation (or 8 spaces)

            First := Index (CL.all, Date_Tag);

            if First /= 0 then
               --  There is some ChangeLog entry for this date
               --  First check for Last character for log entries at this date

               Last := First;

               while Last < CL.all'Last
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

                           P1 := P1 + 1;

                           --  Skip spaces at the start of the line

                           while CL (P1) = ' ' and then P1 < Last loop
                              P1 := P1 + 1;
                           end loop;

                           P2 := P1;

                           --  Look for end of line

                           while CL (P2) /= ASCII.LF and then P2 < Last loop
                              P2 := P2 + 1;
                           end loop;

                           Write (W_File, CL (P1 .. P2));
                        end loop Write_RH;

                        Close (W_File);
                        exit Fill_Log;
                     end if;
                  end if;

                  L := P2 + 1;
               end loop Fill_Log;

            end if;

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
      Mapper   : File_Mapper_Access := Get_Logs_Mapper (Kernel);
   begin
      Remove_Entry (Mapper, Full_Name (File_Name, Normalize => True).all);
      Save_Mapper (Mapper, Logs_Dir & "mapping");
   end Remove_File_From_Mapping;

end Log_Utils;
