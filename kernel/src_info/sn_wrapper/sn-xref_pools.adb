-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
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

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with String_Utils; use String_Utils;

package body SN.Xref_Pools is

   Max_Filename_Length : constant := 1024;
   --  1024 is the value of FILENAME_MAX in stdio.h (see
   --  GNAT.Directory_Operations)

   function Generate_Filename
     (Source_Filename : String; N : Integer) return String;
   --  Generate xref file name based on specified source file name and counter

   --------------
   -- Str_Hash --
   --------------

   function Str_Hash is new HTables.Hash (Header_Num => Hash_Range);
   pragma Inline (Str_Hash);
   --  Standard hash function for strings

   ----------
   -- Free --
   ----------

   procedure Free is new Ada.Unchecked_Deallocation
     (String, String_Access);

   procedure Free is new Ada.Unchecked_Deallocation
     (Xref_Elmt_Record, Xref_Elmt_Ptr);

   --------------
   -- Set_Next --
   --------------

   procedure Set_Next (Xref : Xref_Elmt_Ptr; Next : Xref_Elmt_Ptr) is
   begin
      Xref.Next := Next;
   end Set_Next;

   ----------
   -- Next --
   ----------

   function Next (Xref : Xref_Elmt_Ptr) return Xref_Elmt_Ptr is
   begin
      return Xref.Next;
   end Next;

   -------------
   -- Get_Key --
   -------------

   function Get_Key (Xref : Xref_Elmt_Ptr) return String_Access is
   begin
      return Xref.Source_Filename;
   end Get_Key;

   ----------
   -- Hash --
   ----------

   function Hash (Key : String_Access) return Hash_Range is
   begin
      return Str_Hash (Key.all);
   end Hash;

   -----------
   -- Equal --
   -----------

   function Equal (K1, K2 : String_Access) return Boolean is
   begin
      return K1.all = K2.all;
   end Equal;

   ----------
   -- Init --
   ----------

   procedure Init (Pool : out Xref_Pool) is
   begin
      Pool := new Xref_Pool_Record;
   end Init;

   ----------
   -- Load --
   ----------

   procedure Load (Pool : in out Xref_Pool; Filename : String) is
   begin
      Init (Pool);

      if not Is_Regular_File (Filename) then
         return;
      end if;

      --  Open file and read its content to hashtable
      --  file format: <src_file_name>\n<valid_FLAG><xref_files_name>\n...
      --  where <valid_flag> is either '0' or '1'

      declare
         FD           : File_Type;
         Src_Buf      : String (1 .. Max_Filename_Length);
         Ref_Buf      : String (1 .. Max_Filename_Length);
         Src_Buf_Last : Natural;
         Ref_Buf_Last : Natural;

      begin
         --  ??? Should use GNAT.OS_Lib Open/Read instead, would be more
         --  efficient

         Open (FD, In_File, Filename);

         if Is_Open (FD) then
            loop
               exit when End_Of_File (FD);

               Get_Line (FD, Src_Buf, Src_Buf_Last);
               Get_Line (FD, Ref_Buf, Ref_Buf_Last);

               exit when
                 Src_Buf_Last < Src_Buf'First or
                 Ref_Buf_Last < Ref_Buf'First;

               declare
                  Xref_Elmt : Xref_Elmt_Ptr := new Xref_Elmt_Record;
               begin
                  Xref_Elmt.Source_Filename :=
                    new String'(Src_Buf (Src_Buf'First .. Src_Buf_Last));
                  Xref_Elmt.Xref_Filename :=
                    new String'(Ref_Buf (Ref_Buf'First + 1 .. Ref_Buf_Last));

                  if Ref_Buf (Ref_Buf'First) = '1' then
                     Xref_Elmt.Valid := True;
                  end if;

                  STable.Set (Pool.HTable, Xref_Elmt);
               end;

            end loop;
         end if;

         Close (FD);

      exception
         when others => -- ignore errors
            begin
               Close (FD);
            exception
               when others => null;
            end;
      end;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Pool : Xref_Pool; Filename : String) is
      FD : File_Type;
      E  : Xref_Elmt_Ptr;
   begin
      if not Pool.Changed then
         --  pool was not changed, saving is not necessary
         return;
      end if;

      Create (FD, Out_File, Filename);
      STable.Get_First (Pool.HTable, E);

      while E /= Null_Xref_Elmt loop
         Put_Line (FD, E.Source_Filename.all);
         if E.Valid then
            Put (FD, '1');
         else
            Put (FD, '0');
         end if;
         Put_Line (FD, E.Xref_Filename.all);
         STable.Get_Next (Pool.HTable, E);
      end loop;

      Close (FD);
      Pool.Changed := False;

   exception
      when E : others =>
         begin
            Close (FD);
         exception
            when others => null;
         end;

         Raise_Exception (Xref_File_Error'Identity,
           Exception_Name (E) & ": " & Exception_Message (E));
   end Save;

   ----------
   -- Free --
   ----------

   procedure Free (Pool : in out Xref_Pool) is
      procedure Internal_Free is new Ada.Unchecked_Deallocation
        (Xref_Pool_Record, Xref_Pool);

      E    : Xref_Elmt_Ptr;
      Next : Xref_Elmt_Ptr;

   begin
      if Pool = null then
         return;
      end if;

      STable.Get_First (Pool.HTable, E);

      while E /= Null_Xref_Elmt loop
         STable.Get_Next (Pool.HTable, Next);
         STable.Remove (Pool.HTable, E.Source_Filename);
         Free (E.Xref_Filename);
         Free (E.Source_Filename);
         Free (E);
         E := Next;
      end loop;

      Internal_Free (Pool);
   end Free;

   -----------------------
   -- Generate_Filename --
   -----------------------

   function Generate_Filename
     (Source_Filename : String; N : Integer) return String
   is
      Name  : constant String := Base_Name (Source_Filename);
   begin
      if N = 0 then
         return Name & Xref_Suffix;
      else
         return Name & Image (N) & Xref_Suffix;
      end if;
   end Generate_Filename;

   -----------------------
   -- Xref_Filename_For --
   -----------------------

   function Xref_Filename_For
     (Source_Filename : String;
      Directory       : String;
      Pool            : Xref_Pool) return String_Access
   is
      Data  : Xref_Elmt_Ptr;
      N     : Integer := 0;
   begin
      --  Get hashed value

      Data := STable.Get (Pool.HTable, Source_Filename'Unrestricted_Access);

      if Data /= null then
         return Data.Xref_Filename;
      end if;

      --  Generate new xref file name

      Data := new Xref_Elmt_Record; -- new hashtable value
      Data.Source_Filename := new String'(Source_Filename);
      loop
         declare
            Name : constant String := Generate_Filename (Source_Filename, N);
            Full_Name   : constant String := Directory & Name;
            FD          : File_Descriptor;
         begin
            if not Is_Directory (Directory) then
               Make_Dir (Directory);
            end if;

            if not Is_Regular_File (Full_Name) then
               Data.Xref_Filename := new String'(Full_Name);

               --  touch this file
               FD := Create_New_File (Full_Name, Binary);

               if FD = Invalid_FD then -- unable to create a new file
                  --  raise an exception if unable to create new xref file
                  Free (Data.Source_Filename);
                  Free (Data.Xref_Filename);
                  Raise_Exception (Xref_File_Error'Identity,
                    "unable to create a new file: " & Full_Name);
               end if;

               Close (FD);
               exit;
            end if;
         end;

         N := N + 1;
      end loop;

      --  add generated file to hashtable
      STable.Set (Pool.HTable, Data);
      Pool.Changed := True;

      return Data.Xref_Filename;
   end Xref_Filename_For;

   -----------------------
   -- Free_Filename_For --
   -----------------------

   procedure Free_Filename_For
     (Source_Filename : String;
      Directory       : String;
      Pool            : Xref_Pool)
   is
      Key       : String_Access := new String'(Source_Filename);
      Xref_Elmt : Xref_Elmt_Ptr :=
        STable.Get (Pool.HTable, Key);
   begin
      if Xref_Elmt = null then -- nothing to do
         return;
      end if;

      declare
         Result     : Boolean;
         Full_Name  : constant String := Name_As_Directory (Directory) &
            Xref_Elmt.Xref_Filename.all;
      begin
         --  remove file (ignoring errors)
         Delete_File (Full_Name, Result);
         --  remove from hashtable
         STable.Remove (Pool.HTable, Key);
         Free (Key);
         Free (Xref_Elmt.Source_Filename);
         Free (Xref_Elmt.Xref_Filename);
         Free (Xref_Elmt);
         Pool.Changed := True;
      end;
   end Free_Filename_For;

   -------------------
   -- Is_Xref_Valid --
   -------------------

   function Is_Xref_Valid
     (Source_Filename : String;
      Pool            : Xref_Pool) return Boolean
   is
      Xref_Elmt : constant Xref_Elmt_Ptr :=
        STable.Get (Pool.HTable, Source_Filename'Unrestricted_Access);
   begin
      return Xref_Elmt /= null and then Xref_Elmt.Valid;
   end Is_Xref_Valid;

   ---------------
   -- Set_Valid --
   ---------------

   procedure Set_Valid
     (Source_Filename : String;
      Valid           : Boolean;
      Pool            : Xref_Pool)
   is
      Xref_Elmt : Xref_Elmt_Ptr :=
        STable.Get (Pool.HTable, Source_Filename'Unrestricted_Access);
   begin
      if Xref_Elmt /= null then
         if Xref_Elmt.Valid /= Valid then
            Xref_Elmt.Valid := Valid;
            Pool.Changed := True;
         end if;
      end if;
   end Set_Valid;

end SN.Xref_Pools;
