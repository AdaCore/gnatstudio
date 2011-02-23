------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2002-2009, AdaCore                     --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

--  This is the standard version to be used with the standalone version of
--  Templates_Parser.

with Ada.Exceptions;
with Ada.Streams.Stream_IO;
with Ada.Unchecked_Deallocation;

package body Templates_Parser.Input is

   use Ada.Exceptions;
   use Ada.Streams;

   Buffer_Size : constant := 8_192;

   type File_Record is record
      File    : Stream_IO.File_Type;
      LFT     : Boolean; -- LF terminated state
      Buffer  : Streams.Stream_Element_Array (1 .. Buffer_Size);
      Current : Streams.Stream_Element_Offset;
      Last    : Streams.Stream_Element_Count;
   end record;

   procedure Free is new Ada.Unchecked_Deallocation (File_Record, File_Type);

   -----------
   -- Close --
   -----------

   procedure Close (File : in out File_Type) is
   begin
      if File = null then
         raise Stream_IO.Status_Error;

      else
         begin
            Stream_IO.Close (File.File);
            Free (File);
         exception
            when others =>
               Free (File);
               raise;
         end;
      end if;
   end Close;

   -----------------
   -- End_Of_File --
   -----------------

   function End_Of_File (File : File_Type) return Boolean is
   begin
      if File = null then
         raise Stream_IO.Status_Error;
      else
         return Stream_IO.End_Of_File (File.File)
           and then File.Current > File.Last;
      end if;
   end End_Of_File;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (File   : File_Type;
      Buffer :    out String;
      Last   :    out Natural)
   is
      C : Character;
      --  Current character

      procedure Next_Char;
      --  Set C with next character in the file, update Resource.Last

      ---------------
      -- Next_Char --
      ---------------

      procedure Next_Char is
      begin
         if File.Current > File.Last then
            Stream_IO.Read (File.File, File.Buffer, File.Last);
            File.Current := File.Buffer'First;
         end if;

         C := Character'Val (File.Buffer (File.Current));
         File.Current := File.Current + 1;
      end Next_Char;

   begin
      if File = null then
         raise Stream_IO.Status_Error;

      else
         Last     := 0;
         File.LFT := False;

         loop
            Next_Char;

            if File.Last < File.Buffer'First then
               exit;

            else
               if C = ASCII.LF then         -- UNIX style line terminator
                  File.LFT := True;
                  exit;

               elsif C = ASCII.CR then      -- DOS style line terminator
                  Next_Char;

                  if File.Last < File.Buffer'First then -- No more char
                     exit;

                  elsif  C = ASCII.LF then  --  Ok, found CR+LF
                     File.LFT := True;
                     exit;

                  else                      --  CR, but no LF, continue reading
                     Last := Last + 1;
                     Buffer (Last) := C;
                  end if;

               else
                  Last := Last + 1;
                  Buffer (Last) := C;
               end if;
            end if;
         end loop;
      end if;
   end Get_Line;

   -------------------
   -- LF_Terminated --
   -------------------

   function LF_Terminated (File : File_Type) return Boolean is
   begin
      if File = null then
         raise Stream_IO.Status_Error;
      else
         return File.LFT;
      end if;
   end LF_Terminated;

   ----------
   -- Open --
   ----------

   procedure Open
     (File : in out File_Type;
      Name : String;
      Form : String    := "") is
   begin
      if File /= null then
         Close (File);
      end if;

      File := new File_Record;

      File.Current := 1;
      File.Last    := 0;

      Stream_IO.Open (File.File, Stream_IO.In_File, Name, Form);

   exception
      when Stream_IO.Name_Error =>
         Free (File);
         Raise_Exception
           (IO_Exceptions.Name_Error'Identity,
            "File '" & Name & "' not found.");
   end Open;

end Templates_Parser.Input;
