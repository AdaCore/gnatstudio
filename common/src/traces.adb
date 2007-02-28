-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2001-2006                       --
--                              AdaCore                              --
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
with Ada.Characters.Handling;   use Ada.Characters.Handling;
with Ada.Text_IO;               use Ada.Text_IO;

with GNAT.Calendar;             use GNAT.Calendar;
with GNAT.Calendar.Time_IO;     use GNAT.Calendar.Time_IO;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.IO_Aux;               use GNAT.IO_Aux;
with GNAT.OS_Lib;               use GNAT.OS_Lib;
with GNAT.Task_Lock;            use GNAT.Task_Lock;
with GNAT.Traceback;            use GNAT.Traceback;

with System.Address_Image;
with System.Assertions;         use System.Assertions;

with Unchecked_Conversion;
with Unchecked_Deallocation;

with String_Utils;              use String_Utils;

package body Traces is

   --  Note: rev 1.5 of this file has a (disabled) support for symbolic
   --  tracebacks.

   --  ??? We could display the stack pointer with
   --  procedure Print_Sp is
   --     start : aliased Integer;
   --  begin
   --     Put_Line (System.Address_Image (Start'Address));
   --  end;

   --  Red_Fg     : constant String := ASCII.ESC & "[31m";
   --  Green_Fg   : constant String := ASCII.ESC & "[32m";
   Brown_Fg   : constant String := ASCII.ESC & "[33m";
   --  Blue_Fg    : constant String := ASCII.ESC & "[34m";
   Purple_Fg  : constant String := ASCII.ESC & "[35m";
   Cyan_Fg    : constant String := ASCII.ESC & "[36m";
   --  Grey_Fg    : constant String := ASCII.ESC & "[37m";
   Default_Fg : constant String := ASCII.ESC & "[39m";

   Red_Bg     : constant String := ASCII.ESC & "[41m";
   --  Green_Bg   : constant String := ASCII.ESC & "[42m";
   --  Brown_Bg   : constant String := ASCII.ESC & "[43m";
   --  Blue_Bg    : constant String := ASCII.ESC & "[44m";
   --  Purple_Bg  : constant String := ASCII.ESC & "[45m";
   --  Cyan_Bg    : constant String := ASCII.ESC & "[46m";
   --  Grey_Bg    : constant String := ASCII.ESC & "[47m";
   Default_Bg : constant String := ASCII.ESC & "[49m";

   function Convert is new Unchecked_Conversion
     (File_Access, File_Type_Access);

   procedure Unchecked_Free is new Unchecked_Deallocation
     (Debug_Handle_Record, Debug_Handle);
   procedure Unchecked_Free is new Unchecked_Deallocation
     (File_Type, File_Type_Access);

   Handles_List : Debug_Handle := null;
   --  The global list of all defined handles.
   --  Accesses to this list are protected by called to
   --  System.Soft_Links.Lock_Task.

   Default_Output : File_Type_Access := Convert (Ada.Text_IO.Standard_Output);
   --  The default stream where output is sent

   Default_Activation : Boolean := False;
   --  Default activation status for debug handles

   function Find_Handle (Unit_Name_Upper_Case : String) return Debug_Handle;
   --  Return the debug handle associated with Unit_Name_Upper_Case,
   --  or null if there is none. The case of Unit_Name_Upper_Case is
   --  not changed.
   --  Note: this subprogram doesn't do any locking, it is the
   --  responsability of the called to make sure that not two tasks
   --  can access it at the same time.

   procedure Log
     (Handle        : Debug_Handle;
      Message       : String;
      Location      : String := GNAT.Source_Info.Source_Location;
      Entity        : String := GNAT.Source_Info.Enclosing_Entity;
      Message_Color : String := Default_Fg);
   --  Log a message to Handle unconditionally.

   procedure Put_Absolute_Time (Handle : Debug_Handle);
   --  Print the absolute time in Handle. No locking is done, this is the
   --  responsability of the caller. No colors is modified either.

   procedure Put_Elapsed_Time (Handle : Debug_Handle);
   --  Print the elapsed time the last call to Trace for this Handle. No
   --  locking done.

   procedure Put_Stack_Trace (Handle : Debug_Handle);
   --  Print the stack trace for this handle. No locking done.

   function Config_File
     (Filename : String;
      Default  : String) return String;
   --  Return the name of the config file to use.
   --  If Filename is specified, this is the file to use, providing it exists.
   --  Otherwise, we use a .gnatdebug in the current directory, and if there is
   --  none, Default if it exists.
   --  The empty string is returned if no such file was found.

   function Get_Process_Id return Integer;
   --  Return the process ID of the current process.
   pragma Import (C, Get_Process_Id, "getpid");

   -----------------
   -- Find_Handle --
   -----------------

   function Find_Handle (Unit_Name_Upper_Case : String) return Debug_Handle is
      Tmp : Debug_Handle := Handles_List;
   begin
      while Tmp /= null
        and then Tmp.Name.all /= Unit_Name_Upper_Case
      loop
         Tmp := Tmp.Next;
      end loop;
      return Tmp;
   end Find_Handle;

   ------------------------
   -- Show_Configuration --
   ------------------------

   procedure Show_Configuration (Output : Output_Proc) is
      Tmp : Debug_Handle := Handles_List;
   begin
      while Tmp /= null loop
         if Tmp.Active then
            Output (Tmp.Name.all & "=yes");
         else
            Output (Tmp.Name.all & "=no");
         end if;
         Tmp := Tmp.Next;
      end loop;
   end Show_Configuration;

   ------------
   -- Create --
   ------------

   function Create
     (Unit_Name : String;
      Default   : Default_Activation_Status := From_Config;
      Finalize  : Boolean := True) return Debug_Handle
   is
      Tmp        : Debug_Handle    := null;
      Upper_Case : constant String := To_Upper (Unit_Name);
   begin
      if Debug_Mode then
         Lock;

         Tmp := Find_Handle (Upper_Case);
         if Tmp = null then
            Tmp := new Debug_Handle_Record'
              (Name          => new String'(Upper_Case),
               Active        => Default_Activation,
               Forced_Active => False,
               Stream        => Default_Output,
               Timer         => Ada.Calendar.Clock,
               Count         => 1,
               Next          => Handles_List,
               Finalize      => Finalize);

            if Default = On then
               Tmp.Active := True;
               Tmp.Forced_Active := True;
            elsif Default = Off then
               Tmp.Active := False;
               Tmp.Forced_Active := True;
            end if;

            Handles_List := Tmp;
         end if;

         Unlock;
      end if;
      return Tmp;
   exception
      when others =>
         Unlock;
         raise;
   end Create;

   ------------------------
   -- Predefined handles --
   ------------------------
   --  This must be done after the body of Create has been seen

   Absolute_Time    : constant Debug_Handle := Create ("DEBUG.ABSOLUTE_TIME");
   Elapsed_Time     : constant Debug_Handle := Create ("DEBUG.ELAPSED_TIME");
   Stack_Trace      : constant Debug_Handle := Create ("DEBUG.STACK_TRACE");
   Colors           : constant Debug_Handle := Create ("DEBUG.COLORS");
   Enclosing_Entity : constant Debug_Handle :=
     Create ("DEBUG.ENCLOSING_ENTITY");
   Location         : constant Debug_Handle := Create ("DEBUG.LOCATION");
   Count            : constant Debug_Handle := Create ("DEBUG.COUNT");

   Finalize_Traces  : constant Debug_Handle :=
     Create ("DEBUG.FINALIZE_TRACES", On);
   --  If set to Off, this module will not be finalized, and traces will still
   --  be activated when the program itself is finalized by GNAT

   ---------------
   -- Unit_Name --
   ---------------

   function Unit_Name (Handle : Debug_Handle) return String is
   begin
      return Handle.Name.all;
   end Unit_Name;

   -----------
   -- Trace --
   -----------

   procedure Trace
     (Handle   : Debug_Handle;
      Message  : String;
      Location : String := GNAT.Source_Info.Source_Location;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      if Debug_Mode
        and then Handles_List /= null  --  module not terminated
        and then Handle.Active
      then
         Log (Handle, Message, Location, Entity);
      end if;
   end Trace;

   ------------
   -- Assert --
   ------------

   procedure Assert
     (Handle             : Debug_Handle;
      Condition          : Boolean;
      Error_Message      : String;
      Message_If_Success : String := "";
      Raise_Exception    : Boolean := True;
      Location           : String := GNAT.Source_Info.Source_Location;
      Entity             : String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      if Debug_Mode and then Handles_List /= null and then Handle.Active then
         if not Condition then
            Log (Handle, Error_Message, Location, Entity, Red_Bg & Default_Fg);

            if Raise_Exception then
               Raise_Assert_Failure
                 (Error_Message & " (" & Entity & " at " &
                  Location & ")");
            end if;

         elsif Message_If_Success'Length /= 0 then
            Log (Handle, Message_If_Success, Location, Entity);
         end if;
      end if;
   end Assert;

   ----------------
   -- Set_Active --
   ----------------

   procedure Set_Active (Handle : Debug_Handle; Active : Boolean) is
   begin
      Handle.Active := Active;
   end Set_Active;

   ------------
   -- Active --
   ------------

   function Active (Handle : Debug_Handle) return Boolean is
   begin
      if Handles_List = null then
         --  If this module has been finalized, we always display the traces.
         --  These traces are generally when GNAT finalizes controlled types...
         return True;

      elsif Handle = null then
         --  In case Handle hasn't been initialized yet
         return False;

      else
         return Handle.Active;
      end if;
   end Active;

   -----------------------
   -- Put_Absolute_Time --
   -----------------------

   procedure Put_Absolute_Time (Handle : Debug_Handle) is
      T  : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Ms : constant String := Integer'Image (Integer (Sub_Second (T) * 1000));
   begin
      Put (Handle.Stream.all, "(" & Image (T, ISO_Date & " %T.")
           & Ms (Ms'First + 1 .. Ms'Last) & ')');
   end Put_Absolute_Time;

   ----------------------
   -- Put_Elapsed_Time --
   ----------------------

   procedure Put_Elapsed_Time (Handle : Debug_Handle) is
      T   : constant Ada.Calendar.Time := Ada.Calendar.Clock;
      Dur : Integer;
   begin
      Dur := Integer ((T - Handle.Timer) * 1000);
      Put (Handle.Stream.all, "(elapsed:" & Integer'Image (Dur) & "ms)");
      Handle.Timer := T;
   end Put_Elapsed_Time;

   ---------------------
   -- Put_Stack_Trace --
   ---------------------

   procedure Put_Stack_Trace (Handle : Debug_Handle) is
      Tracebacks : Tracebacks_Array (1 .. 50);
      Len        : Natural;
   begin
      Call_Chain (Tracebacks, Len);
      Put (Handle.Stream.all, "(callstack: ");
      for J in Tracebacks'First .. Len loop
         Put (Handle.Stream.all, System.Address_Image (Tracebacks (J)) & ' ');
      end loop;
      Put (Handle.Stream.all, ')');
   end Put_Stack_Trace;

   ---------
   -- Log --
   ---------

   procedure Log
     (Handle        : Debug_Handle;
      Message       : String;
      Location      : String := GNAT.Source_Info.Source_Location;
      Entity        : String := GNAT.Source_Info.Enclosing_Entity;
      Message_Color : String := Default_Fg)
   is
      Start, Last  : Positive;
      Continuation : constant String := '_' & Handle.Name.all & "_ ";
   begin
      if Message'Length = 0 then
         return;
      end if;

      Lock;

      if Colors.Active then
         Put (Handle.Stream.all, Cyan_Fg);
      end if;
      Put (Handle.Stream.all, '[' & Handle.Name.all & "] ");

      if Count.Active then
         declare
            C : constant String := Integer'Image (Count.Count);
            H : constant String := Integer'Image (Handle.Count);
         begin
            Put (Handle.Stream.all, H (H'First + 1 .. H'Last)
                 & '/' & C (C'First + 1 .. C'Last) & ' ');
         end;
         Count.Count := Count.Count + 1;
         Handle.Count := Handle.Count + 1;
      end if;

      if Colors.Active then
         Put (Handle.Stream.all, Message_Color);
      end if;

      Start := Message'First;
      loop
         Last := Start;
         while Last <= Message'Last
           and then Message (Last) /= ASCII.LF
           and then Message (Last) /= ASCII.CR
         loop
            Last := Last + 1;
         end loop;

         Put (Handle.Stream.all, Message (Start .. Last - 1));

         Start := Last + 1;
         exit when Start > Message'Last;

         New_Line (Handle.Stream.all);
         if Colors.Active then
            Put (Handle.Stream.all, Purple_Fg & Default_Bg);
         end if;
         Put (Handle.Stream.all, Continuation);
         if Colors.Active then
            Put (Handle.Stream.all, Message_Color);
         end if;
      end loop;

      if Colors.Active then
         Put (Handle.Stream.all, Brown_Fg & Default_Bg);
      end if;

      Put (Handle.Stream.all, ' ');

      if Absolute_Time.Active then
         Put_Absolute_Time (Handle);
      end if;

      if Elapsed_Time.Active then
         Put_Elapsed_Time (Handle);
      end if;

      if Traces.Location.Active then
         Put (Handle.Stream.all, "(loc: " & Location & ')');
      end if;

      if Enclosing_Entity.Active then
         Put (Handle.Stream.all, "(entity:" & Entity & ')');
      end if;

      if Stack_Trace.Active then
         Put_Stack_Trace (Handle);
      end if;

      if Colors.Active then
         Put (Handle.Stream.all, Default_Fg);
      end if;

      New_Line (Handle.Stream.all);

      Flush (Handle.Stream.all);

      Unlock;

   exception
      when Ada.Text_IO.Device_Error =>
         Unlock;
      when others =>
         Unlock;
         raise;
   end Log;

   -----------------
   -- Config_File --
   -----------------

   function Config_File
     (Filename : String;
      Default  : String) return String
   is
      Env  : String_Access := Getenv (Config_File_Environment);
      Home : String_Access;
   begin
      if Filename /= "" and then File_Exists (Filename) then
         return Filename;
      end if;

      --  First test the file described in the environment variable
      if Env /= null and then Env.all /= "" then
         if File_Exists (Env.all) then
            declare
               N : constant String := Env.all;
            begin
               Free (Env);
               return N;
            end;
         end if;

         Free (Env);
         return "";
      end if;

      Free (Env);

      --  Then the file in the current directory

      if File_Exists (Default_Config_File) then
         return Default_Config_File;
      end if;

      --  Then the file in the user's home directory
      Home := Getenv ("HOME");

      if Home /= null and then Home.all /= "" then
         declare
            N : constant String :=
              Format_Pathname (Home.all & '/' & Default_Config_File);
         begin
            Free (Home);

            if File_Exists (N) then
               return N;
            end if;
         end;
      end if;

      Free (Home);

      --  Finally the default file
      if Default /= "" and then File_Exists (Default) then
         return Default;
      end if;

      return "";
   end Config_File;

   -----------------------
   -- Parse_Config_File --
   -----------------------

   procedure Parse_Config_File
     (Filename : String := "";
      Default  : String := "")
   is
      File_Name  : aliased constant String := Config_File (Filename, Default);
      Buffer     : String_Access;
      F          : File_Descriptor;
      Length     : Long_Integer;
      Index, First, Last : Natural;
      Handle     : Debug_Handle;

      procedure Skip_Spaces (Skip_Newline : Boolean := True);
      --  Skip the spaces (including possibly newline), and leave Index on the
      --  first non blank character.

      procedure Skip_To_Newline (Stop_At_First_Blank : Boolean := False);
      --  Set Index after the last significant character on the line (either
      --  the ASCII.LF or after the last character in the buffer).

      function Parse_Stream return File_Type_Access;
      --  Return the file described at index.
      --  Index should point at the '>' sign.

      -----------------
      -- Skip_Spaces --
      -----------------

      procedure Skip_Spaces (Skip_Newline : Boolean := True) is
      begin
         while Index <= Buffer'Last
           and then (Buffer (Index) = ' '
                     or else (Buffer (Index) = ASCII.LF
                              and then Skip_Newline)
                     or else Buffer (Index) = ASCII.HT)
         loop
            Index := Index + 1;
         end loop;
      end Skip_Spaces;

      ---------------------
      -- Skip_To_Newline --
      ---------------------

      procedure Skip_To_Newline (Stop_At_First_Blank : Boolean := False) is
      begin
         while Index <= Buffer'Last
           and then Buffer (Index) /= ASCII.LF
           and then (not Stop_At_First_Blank
                     or else (Buffer (Index) /= ' '
                              and then Buffer (Index) /= ASCII.HT))
         loop
            Index := Index + 1;
         end loop;
      end Skip_To_Newline;

      ------------------
      -- Parse_Stream --
      ------------------

      function Parse_Stream return File_Type_Access is
         File  : File_Type_Access;
         First : Natural;
         Last  : Natural;

      begin
         Index := Index + 1;
         Skip_Spaces;
         First := Index;

         Skip_To_Newline;
         Last := Index - 1;

         while Buffer (Last) = ' '
           or else Buffer (Last) = ASCII.HT
         loop
            Last := Last - 1;
         end loop;

         if Buffer (First .. Last) = "&2" then
            return Convert (Ada.Text_IO.Standard_Error);

         else
            Skip_To_Newline (Stop_At_First_Blank => True);
            File := new File_Type;

            if Buffer (Last - 1 .. Last) = "$$" then
               Create
                 (File.all, Out_File,
                  Normalize_Pathname
                    (Buffer (First .. Last - 2) & Image (Get_Process_Id),
                     Dir_Name (File_Name)));
            else
               Create
                 (File.all, Out_File,
                  Normalize_Pathname
                    (Buffer (First .. Last), Dir_Name (File_Name)));
            end if;

            return File;
         end if;
      end Parse_Stream;

   begin
      if File_Name /= "" then
         F := Open_Read (File_Name, Text);

         if F = Invalid_FD then
            return;
         end if;

         Length := File_Length (F);
         Buffer := new String (1 .. Positive (Length));
         Length := Long_Integer
           (Read (F, Buffer.all'Address, Integer (Length)));
         Close (F);

         Index := Buffer'First;

         loop
            Skip_Spaces;
            exit when Index > Buffer'Last;

            if Index + 1 <= Buffer'Last
              and then Buffer (Index .. Index + 1) = "--"
            then
               Skip_To_Newline;

            else
               case Buffer (Index) is
                  when '>' =>
                     Default_Output := Parse_Stream;
                     Skip_To_Newline;
                     Handle := Handles_List;
                     while Handle /= null loop
                        Handle.Stream := Default_Output;
                        Handle := Handle.Next;
                     end loop;

                  when '+' =>
                     Default_Activation := True;
                     Skip_To_Newline;
                     Handle := Handles_List;
                     while Handle /= null loop
                        if not Handle.Forced_Active
                          and then Handle /= Absolute_Time
                          and then Handle /= Elapsed_Time
                          and then Handle /= Stack_Trace
                          and then Handle /= Colors
                          and then Handle /= Enclosing_Entity
                          and then Handle /= Location
                        then
                           Handle.Active := True;
                        end if;
                        Handle := Handle.Next;
                     end loop;

                  when others =>
                     First := Index;
                     while Index <= Buffer'Last
                       and then Buffer (Index) /= '='
                       and then Buffer (Index) /= '>'
                       and then Buffer (Index) /= '-'
                       and then Buffer (Index) /= ASCII.LF
                     loop
                        Index := Index + 1;
                     end loop;

                     Last := Index - 1;
                     while Last >= Buffer'First
                       and then (Buffer (Last) = ' '
                                 or else Buffer (Last) = ASCII.HT)
                     loop
                        Last := Last - 1;
                     end loop;

                     Handle := Create (Buffer (First .. Last));

                     if Index > Buffer'Last
                       or else Buffer (Index) /= '='
                     then
                        Handle.Active := True;
                     else
                        Index := Index + 1;
                        Skip_Spaces;
                        Handle.Active :=
                          Index + 1 > Buffer'Last
                          or else Buffer (Index .. Index + 1) /= "no";
                     end if;

                     while Index <= Buffer'Last
                       and then Buffer (Index) /= '>'
                       and then Buffer (Index) /= ASCII.LF
                     loop
                        Index := Index + 1;
                     end loop;

                     if Index <= Buffer'Last
                       and then Buffer (Index) = '>'
                     then
                        Handle.Stream := Parse_Stream;
                     end if;

                     Skip_To_Newline;
               end case;
            end if;
         end loop;

         Free (Buffer);
      end if;
   end Parse_Config_File;

   --------------
   -- Finalize --
   --------------

   procedure Finalize is
      Tmp  : Debug_Handle := Handles_List;
      Next : Debug_Handle;
      Tmp2 : Debug_Handle;
   begin
      if Active (Finalize_Traces) then
         Trace
           (Finalize_Traces,
            "Finalize traces -- no more traces after this point");
         while Tmp /= null loop
            Next := Tmp.Next;

            if Tmp.Finalize then
               Free (Tmp.Name);

               if Tmp.Stream /= null
                 and then Tmp.Stream /= Default_Output
               then
                  --  Streams can be shared, so avoid freeing and closing them
                  --  multiple times.
                  Tmp2 := Tmp.Next;

                  while Tmp2 /= null loop
                     if Tmp2.Stream = Tmp.Stream then
                        Tmp2.Stream := null;
                     end if;

                     Tmp2 := Tmp2.Next;
                  end loop;

                  Close (Tmp.Stream.all);
                  Unchecked_Free (Tmp.Stream);
               end if;

               Unchecked_Free (Tmp);
            end if;

            Tmp := Next;
         end loop;

         Handles_List := null;

         if Default_Output /= Convert (Ada.Text_IO.Standard_Output) then
            Close (Default_Output.all);
         end if;
      end if;
   end Finalize;

begin
   Exception_Handle := Create ("UNEXPECTED_EXCEPTION", Default => On);
   Testsuite_Handle := Create ("TESTSUITE", Default => Off);
end Traces;
