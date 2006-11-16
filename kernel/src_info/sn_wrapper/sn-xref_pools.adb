-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2006                      --
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

with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with Ada.Exceptions; use Ada.Exceptions;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with String_Utils; use String_Utils;
with VFS; use VFS;

package body SN.Xref_Pools is

   Max_Filename_Length : constant := 1024;
   --  1024 is the value of FILENAME_MAX in stdio.h (see
   --  GNAT.Directory_Operations)

   function Generate_Filename
     (Source_Filename : VFS.Virtual_File;
      Directory       : String;
      N               : Integer) return Virtual_File;
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

   procedure Free (Xref : in out Xref_Elmt_Ptr) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Xref_Elmt_Record, Xref_Elmt_Ptr);
   begin
      Free (Xref.Source_Filename);
      Unchecked_Free (Xref);
   end Free;

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

   function Get_Key (Xref : Xref_Elmt_Ptr) return GNAT.Strings.String_Access is
   begin
      return Xref.Source_Filename;
   end Get_Key;

   ----------
   -- Hash --
   ----------

   function Hash (Key : GNAT.Strings.String_Access) return Hash_Range is
   begin
      return Str_Hash (Key.all);
   end Hash;

   -----------
   -- Equal --
   -----------

   function Equal (K1, K2 : GNAT.Strings.String_Access) return Boolean is
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

   procedure Load (Pool : in out Xref_Pool; Filename : VFS.Virtual_File) is
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

         Open (FD, In_File, Locale_Full_Name (Filename));

         if Is_Open (FD) then
            loop
               exit when End_Of_File (FD);

               Get_Line (FD, Src_Buf, Src_Buf_Last);
               Get_Line (FD, Ref_Buf, Ref_Buf_Last);

               exit when
                 Src_Buf_Last < Src_Buf'First or
                 Ref_Buf_Last < Ref_Buf'First;

               declare
                  Xref_Elmt : constant Xref_Elmt_Ptr := new Xref_Elmt_Record;
               begin
                  Xref_Elmt.Source_Filename :=
                    new String'(Src_Buf (Src_Buf'First .. Src_Buf_Last));
                  Xref_Elmt.Xref_Filename := Create
                    (Full_Filename =>
                       Ref_Buf (Ref_Buf'First + 1 .. Ref_Buf_Last));

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

   procedure Save (Pool : Xref_Pool; Filename : VFS.Virtual_File) is
      FD : File_Type;
      Iter : STable.Iterator;
      E  : Xref_Elmt_Ptr;
   begin
      if not Pool.Changed then
         --  pool was not changed, saving is not necessary
         return;
      end if;

      Create (FD, Out_File, Locale_Full_Name (Filename));
      STable.Get_First (Pool.HTable, Iter);

      loop
         E := STable.Get_Element (Iter);
         exit when E = Null_Xref_Elmt;

         Put_Line (FD, E.Source_Filename.all);
         if E.Valid then
            Put (FD, '1');
         else
            Put (FD, '0');
         end if;
         Put_Line (FD, Full_Name (E.Xref_Filename).all);
         STable.Get_Next (Pool.HTable, Iter);
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
   begin
      if Pool = null then
         return;
      end if;

      STable.Reset (Pool.HTable);
      Internal_Free (Pool);
   end Free;

   -----------------------
   -- Generate_Filename --
   -----------------------

   function Generate_Filename
     (Source_Filename : VFS.Virtual_File;
      Directory       : String;
      N               : Integer) return Virtual_File
   is
      Name  : constant String := Base_Name (Source_Filename);
   begin
      if N = 0 then
         return Create (Full_Filename => Directory & Name & Xref_Suffix);
      else
         return Create
           (Full_Filename => Directory & Name & Image (N) & Xref_Suffix);
      end if;
   end Generate_Filename;

   -----------------------
   -- Xref_Filename_For --
   -----------------------

   function Xref_Filename_For
     (Source_Filename : VFS.Virtual_File;
      Directory       : String;
      Pool            : Xref_Pool) return VFS.Virtual_File
   is
      Data   : Xref_Elmt_Ptr;
      N      : Integer := 0;
      Source : GNAT.Strings.String_Access :=
        new String'(Full_Name (Source_Filename).all);

   begin
      --  Get hashed value

      Data := STable.Get (Pool.HTable, Source);

      if Data /= null then
         Free (Source);
         return Data.Xref_Filename;
      end if;

      --  Generate new xref file name

      Data := new Xref_Elmt_Record; -- new hashtable value
      Data.Source_Filename := Source;
      loop
         declare
            Full_Name : constant Virtual_File :=
              Generate_Filename (Source_Filename, Directory, N);
            FD        : File_Descriptor;
         begin
            if not Is_Directory (Directory) then
               Make_Dir (Directory);
            end if;

            if not Is_Regular_File (Full_Name) then
               Data.Xref_Filename := Full_Name;

               --  touch this file
               FD := Create_New_File (Locale_Full_Name (Full_Name), Binary);

               if FD = Invalid_FD then -- unable to create a new file
                  --  raise an exception if unable to create new xref file
                  Free (Data.Source_Filename);
                  Raise_Exception
                    (Xref_File_Error'Identity,
                     "unable to create a new file: "
                     & VFS.Full_Name (Full_Name).all);
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

   -------------------
   -- Is_Xref_Valid --
   -------------------

   function Is_Xref_Valid
     (Source_Filename : VFS.Virtual_File;
      Pool            : Xref_Pool) return Boolean
   is
      Full      : aliased String := Full_Name (Source_Filename).all;
      Xref_Elmt : constant Xref_Elmt_Ptr :=
        STable.Get (Pool.HTable, Full'Unchecked_Access);
   begin
      return Xref_Elmt /= null and then Xref_Elmt.Valid;
   end Is_Xref_Valid;

   ---------------
   -- Set_Valid --
   ---------------

   procedure Set_Valid
     (Source_Filename : VFS.Virtual_File;
      Valid           : Boolean;
      Pool            : Xref_Pool)
   is
      Full      : aliased String := Full_Name (Source_Filename).all;
      Xref_Elmt : constant Xref_Elmt_Ptr :=
        STable.Get (Pool.HTable, Full'Unchecked_Access);
   begin
      if Xref_Elmt /= null then
         if Xref_Elmt.Valid /= Valid then
            Xref_Elmt.Valid := Valid;
            Pool.Changed := True;
         end if;
      end if;
   end Set_Valid;

end SN.Xref_Pools;
