-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2004-2005                      --
--                               AdaCore                             --
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

with Remote_Connections;      use Remote_Connections;
with Glib;                    use Glib;
with GNAT.OS_Lib;             use GNAT.OS_Lib;
with Ada.Calendar;            use Ada.Calendar;
with VFS;                     use VFS;
with GNAT.Sockets;            use GNAT.Sockets;
with Traces;                  use Traces;
with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Exceptions;          use Ada.Exceptions;
with Ada.Streams;             use Ada.Streams;

package body HTTP_Protocol is

   Me : constant Debug_Handle := Create ("HTTP", Off);

   type Http_Connection_Record is new Remote_Connection_Record with record
      Host : String_Access;
   end record;
   type Http_Connection is access all Http_Connection_Record'Class;

   function Get_Protocol (Http : access Http_Connection_Record) return String;
   function Get_Description
     (Http : access Http_Connection_Record) return String;
   function Is_Regular_File
     (Http : access Http_Connection_Record; Local_Name : Glib.UTF8_String)
     return Boolean;
   function Read_File
     (Http : access Http_Connection_Record; Local_Name : Glib.UTF8_String)
      return GNAT.OS_Lib.String_Access;
   function Delete
     (Http : access Http_Connection_Record; Local_Name : Glib.UTF8_String)
     return Boolean;
   function Is_Writable
     (Http : access Http_Connection_Record; Local_Name : Glib.UTF8_String)
     return Boolean;
   function Is_Directory
     (Http : access Http_Connection_Record; Local_Name : Glib.UTF8_String)
     return Boolean;
   function File_Time_Stamp
     (Http : access Http_Connection_Record; Local_Name : Glib.UTF8_String)
      return Ada.Calendar.Time;
   procedure Write
     (Http            : access Http_Connection_Record;
      Local_Full_Name : Glib.UTF8_String;
      Temporary_File  : String);
   procedure Set_Writable
     (Http            : access Http_Connection_Record;
      Local_Full_Name : Glib.UTF8_String;
      Writable        : Boolean);
   procedure Set_Readable
     (Http            : access Http_Connection_Record;
      Local_Full_Name : String;
      Readable        : Boolean);
   function Make_Dir
     (Http           : access Http_Connection_Record;
      Local_Dir_Name : Glib.UTF8_String)
      return Boolean;
   function Remove_Dir
     (Http           : access Http_Connection_Record;
      Local_Dir_Name : Glib.UTF8_String;
      Recursive      : Boolean)
      return Boolean;
   function Read_Dir
     (Http           : access Http_Connection_Record;
      Local_Dir_Name : Glib.UTF8_String) return GNAT.OS_Lib.String_List;

   function Factory
     (Http       : access Http_Connection_Record;
      User, Host : String;
      Passwd     : String := "";
      Reuse      : Boolean := False) return Remote_Connection;
   --  See from inherited subprograms

   -------------
   -- Factory --
   -------------

   function Factory
     (Http       : access Http_Connection_Record;
      User, Host : String;
      Passwd     : String := "";
      Reuse      : Boolean := False) return Remote_Connection
   is
      pragma Unreferenced (Http, User, Passwd, Reuse);
   begin
      return new Http_Connection_Record'
        (Remote_Connection_Record
         with Host => new String'(Host));
   end Factory;

   --------------
   -- Make_Dir --
   --------------

   function Make_Dir
     (Http           : access Http_Connection_Record;
      Local_Dir_Name : Glib.UTF8_String) return Boolean
   is
      pragma Unreferenced (Http, Local_Dir_Name);
   begin
      return False;
   end Make_Dir;

   ----------------
   -- Remove_Dir --
   ----------------

   function Remove_Dir
     (Http           : access Http_Connection_Record;
      Local_Dir_Name : Glib.UTF8_String;
      Recursive      : Boolean) return Boolean
   is
      pragma Unreferenced (Http, Local_Dir_Name, Recursive);
   begin
      return False;
   end Remove_Dir;

   function Read_Dir
     (Http           : access Http_Connection_Record;
      Local_Dir_Name : Glib.UTF8_String) return GNAT.OS_Lib.String_List
   is
      pragma Unreferenced (Http, Local_Dir_Name);
   begin
      return (1 .. 0 => null);
   end Read_Dir;

   ------------------
   -- Set_Readable --
   ------------------

   procedure Set_Readable
     (Http            : access Http_Connection_Record;
      Local_Full_Name : Glib.UTF8_String;
      Readable        : Boolean)
   is
      pragma Unreferenced (Http, Local_Full_Name, Readable);
   begin
      null;
   end Set_Readable;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable
     (Http            : access Http_Connection_Record;
      Local_Full_Name : Glib.UTF8_String;
      Writable        : Boolean)
   is
      pragma Unreferenced (Http, Local_Full_Name, Writable);
   begin
      null;
   end Set_Writable;

   -----------
   -- Write --
   -----------

   procedure Write
     (Http            : access Http_Connection_Record;
      Local_Full_Name : Glib.UTF8_String;
      Temporary_File  : String)
   is
      pragma Unreferenced (Http, Local_Full_Name, Temporary_File);
   begin
      null;
   end Write;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp
     (Http : access Http_Connection_Record; Local_Name : Glib.UTF8_String)
      return Ada.Calendar.Time
   is
      pragma Unreferenced (Http, Local_Name);
   begin
      return VFS.No_Time;
   end File_Time_Stamp;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory
     (Http : access Http_Connection_Record; Local_Name : Glib.UTF8_String)
     return Boolean
   is
      pragma Unreferenced (Http, Local_Name);
   begin
      return False;
   end Is_Directory;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable
     (Http : access Http_Connection_Record; Local_Name : Glib.UTF8_String)
      return Boolean
   is
      pragma Unreferenced (Http, Local_Name);
   begin
      return False;
   end Is_Writable;

   ------------
   -- Delete --
   ------------

   function Delete
     (Http : access Http_Connection_Record; Local_Name : Glib.UTF8_String)
     return Boolean
   is
      pragma Unreferenced (Http, Local_Name);
   begin
      return False;
   end Delete;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (Http       : access Http_Connection_Record;
      Local_Name : Glib.UTF8_String)
      return GNAT.OS_Lib.String_Access
   is
      HTTP_Token_OK        : constant String := "HTTP/1.1 200 OK";
      HTTP_Token2_OK       : constant String := "HTTP/1.0 200 OK";
      Content_Length_Token : constant String := "CONTENT-LENGTH: ";
      --  These must be upper-cased.

      Buffer      : Stream_Element_Array (1 .. 20000);
      Buffer_Last : Stream_Element_Count := 0;
      Index       : Stream_Element_Count := Buffer'First;
      Result      : String_Access;
      Length  : Integer;
      Socket  : Socket_Type;
      Addr    : Sock_Addr_Type;
      Channel : Stream_Access;

      function Parse_Header return Integer;
      --  Parse the headers of the http message, and return the length of the
      --  message. Return -1 if we should read till the end of the message

      function Get_Char return Character;
      --  Return the next character from the buffer

      procedure Write (Str : String);
      --  Write a request to the socket

      --------------
      -- Get_Char --
      --------------

      function Get_Char return Character is
      begin
         if Index >= Buffer_Last then
            GNAT.Sockets.Receive_Socket (Socket, Buffer, Buffer_Last);
            if Buffer_Last < Buffer'First then
               Index := Buffer'Last + 1;
            else
               Index := Buffer'First;
            end if;
         end if;

         if Index >= Buffer_Last then
            return ASCII.NUL;
         else
            Index := Index + 1;
            return Character'Val (Buffer (Index - 1));
         end if;
      end Get_Char;

      ------------------
      -- Parse_Header --
      ------------------

      function Parse_Header return Integer is
         Line       : String (1 .. 20000);
         Line_Index : Natural;
         Length     : Integer := -1;
         C          : Character;
         Ok         : Boolean := False;
      begin
         loop
            Line_Index := Line'First;
            loop
               C := Get_Char;
               exit when C = ASCII.LF
                 or else C = ASCII.NUL;

               Line (Line_Index) := To_Upper (C);
               Line_Index := Line_Index + 1;
               exit when Line_Index > Line'Last;
            end loop;

            Trace (Me, "<- " & Line (Line'First .. Line_Index - 1));

            if Line_Index > Line'First
              and then Line (Line_Index - 1) = ASCII.CR
            then
               Line_Index := Line_Index - 1;
            end if;

            exit when Line_Index = Line'First;

            if Line_Index > HTTP_Token_OK'Length
              and then Line (1 .. HTTP_Token_OK'Length) = HTTP_Token_OK
            then
               Ok := True;

            elsif Line_Index > HTTP_Token2_OK'Length
              and then Line (1 .. HTTP_Token2_OK'Length) = HTTP_Token2_OK
            then
               Ok := True;

            elsif Line_Index > Content_Length_Token'Length
              and then Line (1 .. Content_Length_Token'Length) =
                Content_Length_Token
            then
               begin
                  Length := Natural'Value
                    (Line (Content_Length_Token'Length + 1 .. Line_Index - 1));
               exception
                  when others =>
                     Length := 0;
               end;
            end if;
         end loop;

         if Ok then
            return Length;
         else
            return 0;
         end if;
      end Parse_Header;

      -----------
      -- Write --
      -----------

      procedure Write (Str : String) is
      begin
         Trace (Me, "-> " & Str);
         String'Write (Channel, Str);
      end Write;

      Port : Integer := 80;
      Host_Last : Integer := Http.Host'Last;
   begin
      for H in Http.Host'Range loop
         if Http.Host (H) = ':' then
            Host_Last := H - 1;
            begin
               Port := Integer'Value (Http.Host (H + 1 .. Http.Host'Last));
            exception
               when others =>
                  Port := 80;
            end;
            exit;
         end if;
      end loop;

      Addr := (GNAT.Sockets.Family_Inet,
               Addresses (Get_Host_By_Name
                            (Http.Host (Http.Host'First .. Host_Last)), 1),
               Port_Type (Port));

      Create_Socket (Socket);
      Set_Socket_Option (Socket, Socket_Level, (Reuse_Address, True));
      Set_Socket_Option (Socket, Option => (Receive_Buffer, 3000));
      Connect_Socket (Socket, Addr);

      Channel := Stream (Socket);

      declare
         Port_Image : constant String := Integer'Image (Port);
      begin
         Write ("GET " & Local_Name & " HTTP/1.1" & ASCII.LF);
         Write ("Host: " & Http.Host (Http.Host'First .. Host_Last) & ":"
                & Port_Image (Port_Image'First + 1 .. Port_Image'Last)
                & ASCII.LF);
         Write ("" & ASCII.LF);
      end;

      Length := Parse_Header;

      Trace (Me, "Length=" & Length'Img);

      if Length = 0 then
         return null;

      elsif Length > 0 then
         Result := new String (1 .. Length - 1);

         for A in 1 .. Length - 1 loop
            Result (A) := Get_Char;
         end loop;

      elsif Length = -1 then
         --  Less efficient, but we do not know the message length
         declare
            Tmp   : String_Access;
            Index : Natural;
            C     : Character;
         begin
            Result := new String (1 .. 2000);
            Index := Result'First;
            loop
               C := Get_Char;
               exit when C = ASCII.NUL;
               if Index > Result'Last then
                  Tmp := Result;
                  Result := new String (1 .. Result'Length * 2);
                  Result (Tmp'Range) := Tmp.all;
                  Free (Tmp);
               end if;
               Result (Index) := C;
               Index := Index + 1;
            end loop;

            Tmp := Result;
            Result := new String (1 .. Index - 1);
            Result.all := Tmp (1 .. Index - 1);
            Free (Tmp);
         end;
      end if;

      Close_Socket (Socket);
      return Result;

   exception
      when Host_Error =>
         Trace (Me, "Invalid host "
                & Http.Host (Http.Host'First .. Host_Last));
         return null;
      when Socket_Error =>
         Trace (Me, "Socket error when connecting to " & Http.Host.all);
         return null;
      when E : others =>
         Trace (Exception_Handle, "Unexpected exception "
                & Exception_Information (E));
         return null;
   end Read_File;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File
     (Http : access Http_Connection_Record; Local_Name : Glib.UTF8_String)
      return Boolean
   is
      pragma Unreferenced (Http, Local_Name);
   begin
      --  Assume we'll be able to download it
      return True;
   end Is_Regular_File;

   ------------------
   -- Get_Protocol --
   ------------------

   function Get_Protocol
     (Http : access Http_Connection_Record) return String
   is
      pragma Unreferenced (Http);
   begin
      return "http";
   end Get_Protocol;

   ---------------------
   -- Get_Description --
   ---------------------

   function Get_Description
     (Http : access Http_Connection_Record) return String
   is
      pragma Unreferenced (Http);
   begin
      return "Interface to the standard HTTP protocol";
   end Get_Description;

   -----------------------
   -- Register_Protocol --
   -----------------------

   procedure Register_Protocol is
      Http : constant Http_Connection := new Http_Connection_Record;
   begin
      Remote_Connections.Register_Protocol (Http);
      Initialize;
   end Register_Protocol;

end HTTP_Protocol;
