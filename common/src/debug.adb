-----------------------------------------------------------------------
--                                                                   --
--                     Copyright (C) 2001                            --
--                          ACT-Europe                               --
--                                                                   --
-- This library is free software; you can redistribute it and/or     --
-- modify it under the terms of the GNU General Public               --
-- License as published by the Free Software Foundation; either      --
-- version 2 of the License, or (at your option) any later version.  --
--                                                                   --
-- This library is distributed in the hope that it will be useful,   --
-- but WITHOUT ANY WARRANTY; without even the implied warranty of    --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details.                          --
--                                                                   --
-- You should have received a copy of the GNU General Public         --
-- License along with this library; if not, write to the             --
-- Free Software Foundation, Inc., 59 Temple Place - Suite 330,      --
-- Boston, MA 02111-1307, USA.                                       --
--                                                                   --
-- As a special exception, if other files instantiate generics from  --
-- this unit, or you link this unit with other files to produce an   --
-- executable, this  unit  does not  by itself cause  the resulting  --
-- executable to be covered by the GNU General Public License. This  --
-- exception does not however invalidate any other reasons why the   --
-- executable file  might be covered by the  GNU Public License.     --
-----------------------------------------------------------------------

with Ada.Calendar;            use Ada.Calendar;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Text_IO;             use Ada.Text_IO;
with GNAT.Calendar;           use GNAT.Calendar;
with GNAT.Calendar.Time_IO;   use GNAT.Calendar.Time_IO;
with GNAT.IO_Aux;             use GNAT.IO_Aux;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with GNAT.Traceback;          use GNAT.Traceback;
with GNAT.Traceback.Symbolic; use GNAT.Traceback.Symbolic;
with System.Address_Image;
with System.Assertions;       use System.Assertions;
with Unchecked_Conversion;

package body Debug is

   Red_Fg     : constant String := ASCII.Esc & "[31m";
   Green_Fg   : constant String := ASCII.Esc & "[32m";
   Brown_Fg   : constant String := ASCII.Esc & "[33m";
   Blue_Fg    : constant String := ASCII.Esc & "[34m";
   Purple_Fg  : constant String := ASCII.Esc & "[35m";
   Cyan_Fg    : constant String := ASCII.Esc & "[36m";
   Grey_Fg    : constant String := ASCII.Esc & "[37m";
   Default_Fg : constant String := ASCII.Esc & "[39m";

   Red_Bg     : constant String := ASCII.Esc & "[41m";
   Green_Bg   : constant String := ASCII.Esc & "[42m";
   Brown_Bg   : constant String := ASCII.Esc & "[43m";
   Blue_Bg    : constant String := ASCII.Esc & "[44m";
   Purple_Bg  : constant String := ASCII.Esc & "[45m";
   Cyan_Bg    : constant String := ASCII.Esc & "[46m";
   Grey_Bg    : constant String := ASCII.Esc & "[47m";
   Default_Bg : constant String := ASCII.Esc & "[49m";

   type File_Type_Access is access File_Type;
   function Convert is new Unchecked_Conversion
     (File_Access, File_Type_Access);

   type Debug_Handle_Record is record
      Name   : String_Access;
      Active : Boolean;
      Stream : File_Type_Access;
      Timer  : Ada.Calendar.Time;
      Next   : Debug_Handle;
   end record;
   --  ... Should Be protected So That Streams Are Correctly Closed On exit

   Handles_List : Debug_Handle := null;
   --  The global list of all defined handles.
   --  Accesses to this list are protected by called to
   --  System.Soft_Links.Lock_Task.

   Default_Output : File_Type_Access := Convert (Ada.Text_IO.Standard_Output);
   --  The default stream where output is sent

   Default_Activation : Boolean := False;
   --  Default activation status for debug handles

   procedure Null_Procedure;
   --  Does nothing, this is used to provide locking in the non-tasking case.

   type No_Param_Proc is access procedure;
   Lock   : No_Param_Proc := Null_Procedure'Access;
   Unlock : No_Param_Proc := Null_Procedure'Access;
   --  Should protect access to internal data, in case we are using
   --  tasking. However, we do not want to drag all the tasking runtime.
   --  Would be nice to have access to System.Soft_Links, but this is
   --  GNAT specific

   function Find_Handle (Unit_Name_Upper_Case : String)
      return Debug_Handle;
   --  Return the debug handle associated with Unit_Name_Upper_Case,
   --  or null if there is none. The case of Unit_Name_Upper_Case is
   --  not changed.
   --  Note: this subprogram doesn't do any locking, it is the
   --  responsability of the called to make sure that not two tasks
   --  can access it at the same time.

   procedure Log
     (Handle : Debug_Handle;
      Message : String;
      Location : String := GNAT.Source_Info.Source_Location;
      Entity   : String := GNAT.Source_Info.Enclosing_Entity;
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

   procedure Put_Symbolic_Stack_Trace
     (Handle : Debug_Handle; Line_Prefix : String);
   --  Print the symbolic stack trace for this handle. No locking done.

   function Config_File return String;
   --  Return the name of the config file to use.
   --  The empty string is returned if no such file was found.

   procedure Parse_Config_File;
   --  Parse the config file

   --------------------
   -- Null_Procedure --
   --------------------

   procedure Null_Procedure is
   begin
      null;
   end Null_Procedure;

   -----------------
   -- Find_Handle --
   -----------------

   function Find_Handle (Unit_Name_Upper_Case : String)
      return Debug_Handle
   is
      Tmp : Debug_Handle := Handles_List;
   begin
      while Tmp /= null
        and then Tmp.Name.all /= Unit_Name_Upper_Case
      loop
         Tmp := Tmp.Next;
      end loop;
      return Tmp;
   end Find_Handle;

   ------------
   -- Create --
   ------------

   function Create (Unit_Name : String) return Debug_Handle is
      Tmp : Debug_Handle := null;
      Upper_Case : constant String := To_Upper (Unit_Name);
   begin
      if Debug_Mode then
         Lock.all;

         Tmp := Find_Handle (Upper_Case);
         if Tmp = null then
            Tmp := new Debug_Handle_Record'
              (Name   => new String' (Upper_Case),
               Active => Default_Activation,
               Stream => Default_Output,
               Timer  => Ada.Calendar.Clock,
               Next   => Handles_List);
            Handles_List := Tmp;
         end if;

         Unlock.all;
      end if;
      return Tmp;
   exception
      when others =>
         Unlock.all;
         raise;
   end Create;

   ------------------------
   -- Predefined handles --
   ------------------------
   --  This must be done after the body of Create has been seen

   Absolute_Time : Debug_Handle := Create ("DEBUG.ABSOLUTE_TIME");
   Elapsed_Time  : Debug_Handle := Create ("DEBUG.ELAPSED_TIME");
   Stack_Trace   : Debug_Handle := Create ("DEBUG.STACK_TRACE");
   Stack_Trace_Symbolic : Debug_Handle :=
     Create ("DEBUG.STACK_TRACE_SYMBOLIC");
   Colors        : Debug_Handle := Create ("DEBUG.COLORS");
   Enclosing_Entity : Debug_Handle := Create ("DEBUG.ENCLOSING_ENTITY");
   Location : Debug_Handle := Create ("DEBUG.LOCATION");

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

   procedure Trace (Handle   : Debug_Handle;
                    Message  : String;
                    Location : String := GNAT.Source_Info.Source_Location;
                    Entity   : String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      if Debug_Mode and then Handle.Active then
         Log (Handle, Message, Location, Entity);
      end if;
   end Trace;

   ------------
   -- Assert --
   ------------

   procedure Assert (Handle             : Debug_Handle;
                     Condition          : Boolean;
                     Error_Message      : String;
                     Message_If_Success : String := "";
                     Location : String := GNAT.Source_Info.Source_Location;
                     Entity   : String := GNAT.Source_Info.Enclosing_Entity) is
   begin
      if Debug_Mode and then Handle.Active then
         if not Condition then
            Log (Handle, Error_Message, Location, Entity, Red_Bg & Default_Fg);
            Raise_Assert_Failure (Error_Message);
         elsif Message_If_Success'Length /= 0 then
            Log (Handle, Message_If_Success,
                 Location, Entity);
         end if;
      end if;
   end Assert;

   ------------
   -- Active --
   ------------

   function Active (Handle : Debug_Handle) return Boolean is
   begin
      return Handle.Active;
   end Active;

   -----------------------
   -- Put_Absolute_Time --
   -----------------------

   procedure Put_Absolute_Time (Handle : Debug_Handle) is
      T : Ada.Calendar.Time := Ada.Calendar.Clock;
      Ms : constant String := Integer'Image (Integer (Sub_Second (T) * 1000));
   begin
      Put (Handle.Stream.all, "(" & Image (T, ISO_Date & " %T.")
           & Ms (Ms'First + 1 .. Ms'Last) & ')');
   end Put_Absolute_Time;

   ----------------------
   -- Put_Elapsed_Time --
   ----------------------

   procedure Put_Elapsed_Time (Handle : Debug_Handle) is
      T : Ada.Calendar.Time := Ada.Calendar.Clock;
      Dur : Integer;
   begin
      Dur := Integer ((T - Handle.Timer) * 1000);
      Put (Handle.Stream.all,
           "(elapsed:"
           & Integer'Image (Dur)
           & "ms)");
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
         Put (Handle.Stream.all,
              System.Address_Image (Tracebacks (J)) & ' ');
      end loop;
      Put (Handle.Stream.all, ')');
   end Put_Stack_Trace;

   ------------------------------
   -- Put_Symbolic_Stack_Trace --
   ------------------------------

   procedure Put_Symbolic_Stack_Trace
     (Handle : Debug_Handle; Line_Prefix : String)
   is
      Tracebacks : Tracebacks_Array (1 .. 50);
      Len        : Natural;
   begin
      Call_Chain (Tracebacks, Len);
      Put (Handle.Stream.all, ASCII.LF);
      declare
         Symb : constant String :=
           Symbolic_Traceback (Tracebacks (Tracebacks'First .. Len));
         First, Last : Natural;
      begin
         First := Symb'First;
         while First <= Symb'Last loop
            Last := First + 1;
            while Last <= Symb'Last
              and then Symb (Last) /= ASCII.LF
            loop
               Last := Last + 1;
            end loop;

            Put (Handle.Stream.all, Line_Prefix
                 & Symb (First .. Last - 1) & ASCII.LF);
            First := Last + 1;
         end loop;
      end;
   end Put_Symbolic_Stack_Trace;

   ---------
   -- Log --
   ---------

   procedure Log (Handle   : Debug_Handle;
                  Message  : String;
                  Location : String := GNAT.Source_Info.Source_Location;
                  Entity   : String := GNAT.Source_Info.Enclosing_Entity;
                  Message_Color : String := Default_Fg)
   is
      Start, Last : Positive;
      Continuation : constant String := '_' & Handle.Name.all & "_ ";
   begin
      if Message'Length = 0 then
         return;
      end if;

      Lock.all;

      if Colors.Active then
         Put (Handle.Stream.all, Cyan_Fg);
      end if;
      Put (Handle.Stream.all, '[' & Handle.Name.all & "] ");
      if Colors.Active then
         Put (Handle.Stream.all, Message_Color);
      end if;

      Start := Message'First;
      loop
         Last := Start + 1;
         while Last <= Message'Last
           and then Message (Last) /= ASCII.LF
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

      if Debug.Location.Active then
         Put (Handle.Stream.all, "(loc: " & Location & ')');
      end if;

      if Enclosing_Entity.Active then
         Put (Handle.Stream.all, "(entity:" & Entity & ')');
      end if;

      if Stack_Trace.Active and then not Stack_Trace_Symbolic.Active then
         Put_Stack_Trace (Handle);
      end if;

      if Stack_Trace_Symbolic.Active then
         Put_Symbolic_Stack_Trace (Handle, Continuation);
      end if;

      if Colors.Active then
         Put (Handle.Stream.all, Default_Fg);
      end if;

      New_Line (Handle.Stream.all);

      Flush (Handle.Stream.all);

      Unlock.all;

   exception
      when others =>
         Unlock.all;
         raise;
   end Log;

   -----------------
   -- Config_File --
   -----------------

   function Config_File return String is
      Env  : String_Access := Getenv (Config_File_Environment);
      Home : String_Access;
   begin
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

      if File_Exists (Default_Config_File) then
         return Default_Config_File;

      else
         Home := Getenv ("HOME");

         if Home /= null and then Home.all /= "" then
            declare
               N : constant String :=
                 Home.all & Directory_Separator & Default_Config_File;
            begin
               Free (Home);
               if File_Exists (N) then
                  return N;
               end if;
            end;
         end if;

         Free (Home);
      end if;

      return "";
   end Config_File;

   -----------------------
   -- Parse_Config_File --
   -----------------------

   procedure Parse_Config_File is
      File_Name  : aliased constant String := Config_File;
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
           and then (Stop_At_First_Blank
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
         File : File_Type_Access;
         First : Natural;
         Last : Natural;
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
            Create (File.all, Out_File, Buffer (First .. Last));
            return File;
         end if;
      end Parse_Stream;

   begin
      if File_Name /= "" then
         declare
            C_Name : aliased constant String := File_Name & ASCII.NUL;
         begin
            F := Open_Read (C_Name'Address, Text);
            if F = Invalid_FD then
               return;
            end if;
         end;

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

                  when '+' =>
                     Default_Activation := True;
                     Skip_To_Newline;

                  when others =>
                     First := Index;
                     while Index <= Buffer'Last
                       and then Buffer (Index) /= '='
                       and then Buffer (Index) /= '>'
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

begin
   Parse_Config_File;
end Debug;

