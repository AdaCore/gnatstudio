-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2003                         --
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

--  This module implements a virtual file system.
--  It gives access to local and remote files, abstract file system specific
--  features (case sensitivity),...

with Glib;                       use Glib;
with Glib.Convert;               use Glib.Convert;
with Glib.Values;                use Glib.Values;
with GNAT.Directory_Operations;  use GNAT.Directory_Operations;
with Ada.Unchecked_Deallocation;
with Ada.Calendar;               use Ada.Calendar;
with GNAT.Calendar;              use GNAT.Calendar;
with GNAT.OS_Lib;                use GNAT.OS_Lib;
with File_Utils;                 use File_Utils;
with OS_Utils;                   use OS_Utils;
with GNAT.Case_Util;             use GNAT.Case_Util;
with GNAT.Heap_Sort;             use GNAT.Heap_Sort;
with Interfaces.C.Strings;       use Interfaces.C.Strings;
with Remote_Connections;         use Remote_Connections;
with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;

package body VFS is

   Temporary_Dir : constant String := Get_Tmp_Dir;
   --  Cache the name of the temporary directory

   Empty_String : constant Cst_UTF8_String_Access := new String'("");

   Virtual_File_Type : Glib.GType := Glib.GType_None;
   --  Initialized only the first time this is needed, since we need glib
   --  initialized for this.
   --  ??? Could this be made a local variable

   procedure Ensure_Normalized (File : Virtual_File);
   --  Ensure that the information for the normalized file have been correctly
   --  computed.

   procedure Finalize (Value : in out Contents_Access);
   --  Internal version of Finalize

   function Virtual_File_Boxed_Copy
     (Boxed : System.Address) return System.Address;
   pragma Convention (C, Virtual_File_Boxed_Copy);
   procedure Virtual_File_Boxed_Free (Boxed : System.Address);
   pragma Convention (C, Virtual_File_Boxed_Free);
   --  Subprograms required for the support of GValue

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Contents_Record, Contents_Access);
   function To_Contents_Access is new Ada.Unchecked_Conversion
     (System.Address, Contents_Access);

   ---------
   -- "=" --
   ---------

   function "=" (File1, File2 : Virtual_File) return Boolean is
   begin
      if File1.Value = null then
         return File2.Value = null;
      elsif File2.Value = null then
         return False;
      elsif File1.Value = File2.Value then
         return True;
      else
         Ensure_Normalized (File1);
         Ensure_Normalized (File2);

         return File2.Value.Normalized_Full.all =
           File1.Value.Normalized_Full.all;
      end if;
   end "=";

   ------------
   -- Create --
   ------------

   function Create (Full_Filename : UTF8_String) return Virtual_File is
      Protocol, User, Host, Path : String_Access;
      Connection : Remote_Connection;
   begin
      Parse_URL (Full_Filename, Protocol, User, Host, Path);

      if Protocol /= null then
         Connection := Get_Connection (Protocol.all, User.all, Host.all);
         Free (Protocol);
         Free (User);
         Free (Host);

         if Connection /= null then
            if Path = null then
               raise Program_Error;
            end if;

            return (Ada.Finalization.Controlled with
                    Value => new Contents_Record'
                      (Connection      => Connection,
                       Ref_Count       => 1,
                       Full_Name       => Path,
                       Normalized_Full => null,
                       Dir_Name        => null));
         else
            Free (Path);

            --  Behave as if we have a local file, although nobody will be
            --  able to open it

            return (Ada.Finalization.Controlled with
                    Value => new Contents_Record'
                      (Connection      => null,
                       Ref_Count       => 1,
                       Full_Name       => new String'(Full_Filename),
                       Normalized_Full => null,
                       Dir_Name        => null));
         end if;

      else
         return (Ada.Finalization.Controlled with
                 Value => new Contents_Record'
                   (Connection      => null,
                    Ref_Count       => 1,
                    Full_Name       => new String'(Full_Filename),
                    Normalized_Full => null,
                    Dir_Name        => null));
      end if;
   end Create;

   ----------------------
   -- Create_From_Base --
   ----------------------

   function Create_From_Base (Base_Name : UTF8_String) return Virtual_File is
   begin
      return (Ada.Finalization.Controlled with
                Value => new Contents_Record'
                (Connection      => null,
                 Ref_Count       => 1,
                 Full_Name       => new String'(Base_Name),
                 Normalized_Full => null,
                 Dir_Name        => null));
   end Create_From_Base;

   -----------------------
   -- Ensure_Normalized --
   -----------------------

   procedure Ensure_Normalized (File : Virtual_File) is
   begin
      if File.Value.Normalized_Full = null then
         --  If the user didn't create a file with a full name, no need to
         --  spend time now trying to find the file.
         if not Is_Absolute_Path (File.Value.Full_Name.all) then
            File.Value.Normalized_Full :=
              new UTF8_String'(File.Value.Full_Name.all);

         elsif File.Value.Connection /= null then
            --  Can't normalize files on remote hosts
            File.Value.Normalized_Full :=
              new UTF8_String'(File.Value.Full_Name.all);

         else
            File.Value.Normalized_Full := new UTF8_String'
              (Locale_To_UTF8
                 (To_Host_Pathname
                    (Normalize_Pathname
                       (Locale_From_UTF8 (File.Value.Full_Name.all),
                        Resolve_Links => True))));
            if not File_Utils.Filenames_Are_Case_Sensitive then
               To_Lower (File.Value.Normalized_Full.all);
            end if;
         end if;
      end if;
   end Ensure_Normalized;

   ---------------
   -- Base_Name --
   ---------------

   function Base_Name
     (File : Virtual_File; Suffix : String := "") return Glib.UTF8_String is
   begin
      if File.Value = null then
         return "";
      else
         --  Since we can't ensure that Prefix will be the same in two
         --  successive calls, we have to reallocate the string every time.

         return Base_Name (File.Value.Full_Name.all, Suffix);
      end if;
   end Base_Name;

   ---------------
   -- Full_Name --
   ---------------

   function Full_Name
     (File : Virtual_File; Normalize : Boolean := False)
      return Cst_UTF8_String_Access is
   begin
      if File.Value = null then
         return Empty_String;

      elsif Normalize then
         Ensure_Normalized (File);
         return Cst_UTF8_String_Access (File.Value.Normalized_Full);

      else
         return Cst_UTF8_String_Access (File.Value.Full_Name);
      end if;
   end Full_Name;

   -------------------
   -- URL_File_Name --
   -------------------

   function URL_File_Name (File : Virtual_File) return String is
   begin
      if File.Value = null or else File.Value.Connection = null then
         return Full_Name (File).all;
      else
         return Get_Protocol (File.Value.Connection)
         & "://"
         & Get_User (File.Value.Connection)
         & '@'
         & Get_Host (File.Value.Connection)
         & Full_Name (File).all;
      end if;
   end URL_File_Name;

   --------------
   -- Dir_Name --
   --------------

   function Dir_Name (File : Virtual_File) return Cst_UTF8_String_Access is
   begin
      if File.Value = null then
         return Empty_String;
      else
         if File.Value.Dir_Name = null then
            File.Value.Dir_Name :=
              new UTF8_String'(Dir_Name (File.Value.Full_Name.all));
         end if;
         return Cst_UTF8_String_Access (File.Value.Dir_Name);
      end if;
   end Dir_Name;

   ---------------
   -- Read_File --
   ---------------

   function Read_File (File : Virtual_File) return String_Access is
   begin
      if File.Value = null then
         return null;
      elsif File.Value.Connection = null then
         return Read_File (Full_Name (File).all);
      else
         return Read_File (File.Value.Connection, File.Value.Full_Name.all);
      end if;
   end Read_File;

   ------------
   -- Delete --
   ------------

   procedure Delete (File : Virtual_File) is
      Success : Boolean;
   begin
      if File.Value.Connection = null then
         Delete_File (Locale_Full_Name (File), Success);
      else
         Delete (File.Value.Connection, File.Value.Full_Name.all);
      end if;
   end Delete;

   ----------------------
   -- Locale_Full_Name --
   ----------------------

   function Locale_Full_Name (File : Virtual_File) return String is
   begin
      if File.Value = null then
         return "";
      else
         --  ??? This is not cached, should it ?
         return Locale_From_UTF8 (File.Value.Full_Name.all);
      end if;
   end Locale_Full_Name;

   ----------------------
   -- Locale_Base_Name --
   ----------------------

   function Locale_Base_Name (File : Virtual_File) return String is
      Base : constant String := Base_Name (File);
   begin
      return Locale_From_UTF8 (Base);
   end Locale_Base_Name;

   ---------------------
   -- Is_Regular_File --
   ---------------------

   function Is_Regular_File (File : Virtual_File) return Boolean is
   begin
      if File.Value = null then
         return False;
      elsif File.Value.Connection = null then
         return Is_Regular_File (Locale_Full_Name (File));
      else
         return Is_Regular_File
           (File.Value.Connection, File.Value.Full_Name.all);
      end if;
   end Is_Regular_File;

   --------------------
   -- Unchecked_Free --
   --------------------

   procedure Unchecked_Free (Arr : in out File_Array_Access) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (File_Array, File_Array_Access);
   begin
      Internal (Arr);
   end Unchecked_Free;

   -----------------
   -- Is_Writable --
   -----------------

   function Is_Writable (File : Virtual_File) return Boolean is
   begin
      if File.Value = null then
         return False;
      elsif File.Value.Connection /= null then
         return Is_Writable (File.Value.Connection, File.Value.Full_Name.all);
      else
         return Is_Writable_File (Locale_Full_Name (File));
      end if;
   end Is_Writable;

   ------------------
   -- Is_Directory --
   ------------------

   function Is_Directory (File : Virtual_File) return Boolean is
   begin
      if File.Value = null then
         return False;
      elsif File.Value.Connection /= null then
         return Is_Directory (File.Value.Connection, File.Value.Full_Name.all);
      else
         return Is_Directory (Locale_Full_Name (File));
      end if;
   end Is_Directory;

   ----------------------
   -- Is_Absolute_Path --
   ----------------------

   function Is_Absolute_Path (File : Virtual_File) return Boolean is
   begin
      return Is_Absolute_Path (Full_Name (File).all);
   end Is_Absolute_Path;

   --------------------
   -- File_Extension --
   --------------------

   function File_Extension (File : Virtual_File) return UTF8_String is
   begin
      return File_Extension (Full_Name (File).all);
   end File_Extension;

   ----------------
   -- Write_File --
   ----------------

   function Write_File (File : Virtual_File) return Writable_File is
      Tmp : GNAT.OS_Lib.String_Access;
      Fd  : File_Descriptor;
   begin
      if File.Value.Connection /= null then
         declare
            Current_Dir : constant String := Get_Current_Dir;
            Base        : String_Access;
         begin
            Change_Dir (Temporary_Dir);
            Create_Temp_File (Fd, Base);
            Tmp := new String'(Name_As_Directory (Temporary_Dir) & Base.all);
            Free (Base);
            Change_Dir (Current_Dir);
         end;
      else
         Fd := Create_File (Locale_Full_Name (File), Binary);
      end if;

      if Fd = Invalid_FD then
         Free (Tmp);
         return Invalid_File;
      else
         return (File => File, FD => Fd, Filename => Tmp);
      end if;
   end Write_File;

   -----------
   -- Write --
   -----------

   procedure Write
     (File    : in out Writable_File;
      Str     : UTF8_String;
      As_UTF8 : Boolean := True)
   is
      function To_Address is new Ada.Unchecked_Conversion
        (chars_ptr, System.Address);

      procedure C_Free (S : Interfaces.C.Strings.chars_ptr);
      pragma Import (C, C_Free, "free");

      Written : aliased Natural;
      Read    : aliased Natural;
      S       : chars_ptr;
   begin
      if As_UTF8 then
         S := Locale_From_UTF8
           (Str,
            Read'Access,
            Written'Access);

         if S = Null_Ptr then
            --  Couldn't convert ? Just save the string as is, this is better
            --  than nothing
            Written := Write (File.FD, Str'Address, Str'Length);
         else
            Written := Write (File.FD, To_Address (S), Written);
            C_Free (S);
         end if;
      else
         Written := Write (File.FD, Str'Address, Str'Length);
      end if;

      --  ??? Should raise an exception if we couldn't write all the bytes.
   end Write;

   -----------
   -- Close --
   -----------

   procedure Close (File : in out Writable_File) is
      Success : Boolean;
   begin
      if File.File.Value.Connection /= null then
         Write (File.File.Value.Connection,
                File.File.Value.Full_Name.all, File.Filename.all);
         Close (File.FD);
         Delete_File (File.Filename.all, Success);
      else
         Close (File.FD);
      end if;

      Free (File.Filename);
   end Close;

   ------------------
   -- Set_Writable --
   ------------------

   procedure Set_Writable (File : VFS.Virtual_File; Writable : Boolean) is
      procedure Internal (File : String; Set : Integer);
      pragma Import (C, Internal, "__gps_set_writable");

   begin
      if File.Value.Connection = null then
         Internal
           (Locale_Full_Name (File) & ASCII.NUL, Boolean'Pos (Writable));
      else
         Set_Writable
           (File.Value.Connection, File.Value.Full_Name.all, Writable);
      end if;
   end Set_Writable;

   ------------------
   -- Set_Readable --
   ------------------

   procedure Set_Readable (File : VFS.Virtual_File; Readable : Boolean) is
      procedure Internal (File : String; Set : Integer);
      pragma Import (C, Internal, "__gps_set_readable");

   begin
      if File.Value.Connection = null then
         Internal
           (Locale_Full_Name (File) & ASCII.NUL, Boolean'Pos (Readable));
      else
         Set_Readable
           (File.Value.Connection, File.Value.Full_Name.all, Readable);
      end if;
   end Set_Readable;

   ---------------------
   -- File_Time_Stamp --
   ---------------------

   function File_Time_Stamp
     (File : Virtual_File) return Ada.Calendar.Time is
   begin
      if File.Value.Connection = null then
         if Is_Regular_File (File) then
            declare
               T      : constant OS_Time :=
                 File_Time_Stamp (Locale_Full_Name (File));
               Year   : Year_Type;
               Month  : Month_Type;
               Day    : Day_Type;
               Hour   : Hour_Type;
               Minute : Minute_Type;
               Second : Second_Type;
            begin
               GM_Split (T, Year, Month, Day, Hour, Minute, Second);
               return GNAT.Calendar.Time_Of
                 (Year   => Year,
                  Month  => Month,
                  Day    => Day,
                  Hour   => Hour,
                  Minute => Minute,
                  Second => Second);
            end;
         else
            return No_Time;
         end if;
      else
         return File_Time_Stamp
           (File.Value.Connection, File.Value.Full_Name.all);
      end if;
   end File_Time_Stamp;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (Value : in out Contents_Access) is
   begin
      if Value /= null then
         Value.Ref_Count := Value.Ref_Count - 1;

         if Value.Ref_Count = 0 then
            if Value.Connection /= null then
               Close (Value.Connection, GPS_Termination => False);
            end if;

            Free (Value.Full_Name);
            Free (Value.Dir_Name);
            Free (Value.Normalized_Full);
            Unchecked_Free (Value);
         end if;
      end if;
   end Finalize;

   --------------
   -- Finalize --
   --------------

   procedure Finalize (File : in out Virtual_File) is
   begin
      Finalize (File.Value);
   end Finalize;

   ------------
   -- Adjust --
   ------------

   procedure Adjust (File : in out Virtual_File) is
   begin
      if File.Value /= null then
         File.Value.Ref_Count := File.Value.Ref_Count + 1;
      end if;
   end Adjust;

   ---------
   -- "<" --
   ---------

   function "<" (File1, File2 : Virtual_File) return Boolean is
      C1, C2 : Character;
   begin
      if File1.Value = null then
         return True;
      elsif File2.Value = null then
         return False;
      elsif Filenames_Are_Case_Sensitive then
         return File1.Value.Full_Name.all < File2.Value.Full_Name.all;
      else
         for C in File1.Value.Full_Name'Range loop
            if C > File2.Value.Full_Name'Last then
               return False;
            end if;

            C1 := To_Lower (File1.Value.Full_Name (C));
            C2 := To_Lower (File2.Value.Full_Name (C));

            if C1 < C2 then
               return True;
            elsif C1 > C2 then
               return False;
            end if;

         end loop;
         return True;
      end if;
   end "<";

   ----------
   -- Sort --
   ----------

   procedure Sort (Files : in out File_Array) is
      --  ??? Right now, this sorts only on the full name. Do we want to
      --  provide other choices for sorting ?

      procedure Xchg (Op1, Op2 : Natural);
      --  Exchanges two items in the array.

      function Lt (Op1, Op2 : Natural) return Boolean;
      --  Return True if the first item is to be sorted before the second.

      procedure Xchg (Op1, Op2 : Natural) is
         Buffer : Virtual_File;
      begin
         Buffer := Files (Files'First - 1 + Op1);
         Files (Files'First - 1 + Op1) := Files (Files'First - 1 + Op2);
         Files (Files'First - 1 + Op2) := Buffer;
      end Xchg;

      function Lt (Op1, Op2 : Natural) return Boolean is
      begin
         return Files (Files'First - 1 + Op1) <
           Files (Files'First - 1 + Op2);
      end Lt;
   begin
      Sort (Files'Length, Xchg'Unrestricted_Access, Lt'Unrestricted_Access);
   end Sort;

   -----------------------------
   -- Virtual_File_Boxed_Copy --
   -----------------------------

   function Virtual_File_Boxed_Copy
     (Boxed : System.Address) return System.Address
   is
      Value : constant Contents_Access := To_Contents_Access (Boxed);
   begin
      if Value /= null then
         Value.Ref_Count := Value.Ref_Count + 1;
      end if;
      return Boxed;
   end Virtual_File_Boxed_Copy;

   -----------------------------
   -- Virtual_File_Boxed_Free --
   -----------------------------

   procedure Virtual_File_Boxed_Free (Boxed : System.Address) is
      Value : Contents_Access := To_Contents_Access (Boxed);
   begin
      Finalize (Value);
   end Virtual_File_Boxed_Free;

   --------------
   -- Set_File --
   --------------

   procedure Set_File
     (Value : in out Glib.Values.GValue; File : Virtual_File) is
   begin
      Init (Value, Get_Virtual_File_Type);

      if File.Value = null then
         Set_Boxed (Value, System.Null_Address);
      else
         Set_Boxed (Value, File.Value.all'Address);
      end if;
   end Set_File;

   --------------
   -- Get_File --
   --------------

   function Get_File (Value : Glib.Values.GValue) return Virtual_File is
      File : Virtual_File;
   begin
      File.Value := To_Contents_Access (Get_Boxed (Value));

      if File.Value /= null then
         File.Value.Ref_Count := File.Value.Ref_Count + 1;
      end if;

      return File;
   end Get_File;

   ---------------------------
   -- Get_Virtual_File_Type --
   ---------------------------

   function Get_Virtual_File_Type return Glib.GType is
   begin
      if Virtual_File_Type = GType_None then
         Virtual_File_Type := Boxed_Type_Register_Static
           ("Virtual_File", Virtual_File_Boxed_Copy'Access,
            Virtual_File_Boxed_Free'Access);
      end if;
      return Virtual_File_Type;
   end Get_Virtual_File_Type;

end VFS;
