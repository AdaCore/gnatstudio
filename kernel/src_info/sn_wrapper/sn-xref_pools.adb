with Ada.Unchecked_Deallocation;
with Ada.Text_IO; use Ada.Text_IO;
with GNAT.OS_Lib; use GNAT.OS_Lib;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;

package body SN.Xref_Pools is

   Max_Filename_Length : constant := 1024;
   --  1024 is the value of FILENAME_MAX in stdio.h (see
   --  GNAT.Directory_Operations)

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

   ----------
   -- Free --
   ----------

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
      Pool := new STable.HTable;
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

      --  open file and read its content to hashtable
      --  file format: <src_file_name>\n<xref_files_name>\n...
      declare
         FD : File_Type;
         Src_Buf : String (1 .. Max_Filename_Length);
         Ref_Buf : String (1 .. Max_Filename_Length);
         Src_Buf_Last : Natural;
         Ref_Buf_Last : Natural;
      begin
         Open (FD, In_File, Filename);
         if Is_Open (FD) then
            loop
               Get_Line (FD, Src_Buf, Src_Buf_Last);
               Get_Line (FD, Ref_Buf, Ref_Buf_Last);

               exit when
                 Src_Buf_Last < Src_Buf'First or
                 Ref_Buf_Last < Ref_Buf'First;

               declare
                  Xref_Elmt : Xref_Elmt_Ptr := new Xref_Elmt_Record;
               begin
                  Xref_Elmt.Source_Filename :=
                    new String' (Src_Buf (Src_Buf'First .. Src_Buf_Last));
                  Xref_Elmt.Xref_Filename :=
                    new String' (Ref_Buf (Ref_Buf'First .. Ref_Buf_Last));
                  STable.Set (Pool.all, Xref_Elmt);
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
      Create (FD, Out_File, Filename);
      STable.Get_First (Pool.all, E);
      while E /= Null_Xref_Elmt loop
         Put_Line (FD, E.Source_Filename.all);
         Put_Line (FD, E.Xref_Filename.all);
         STable.Get_Next (Pool.all, E);
      end loop;
      Close (FD);
   exception
      when others =>
         begin
            Close (FD);
         exception
            when others => null;
         end;
         raise Xref_File_Error;
   end Save;

   ----------
   -- Free --
   ----------

   procedure Free (Pool : in out Xref_Pool) is
      procedure Internal_Free is new Ada.Unchecked_Deallocation
        (STable.HTable, Xref_Pool);
      E : Xref_Elmt_Ptr;
   begin
      STable.Get_First (Pool.all, E);
      while E /= Null_Xref_Elmt loop
         Free (E.Source_Filename);
         Free (E.Xref_Filename);
         Free (E);
         STable.Get_Next (Pool.all, E);
      end loop;
      Internal_Free (Pool);
   end Free;

   function Generate_Filename
     (Source_Filename : String;
      N               : Integer) return String;

   -----------------------
   -- Generate_Filename --
   -----------------------

   function Generate_Filename
     (Source_Filename : String;
      N               : Integer) return String
   is
      Name  : String := Base_Name (Source_Filename);
   begin
      --  generates xref file name based on specified source file name and
      --  counter
      if N = 0 then
         return Name & Xref_Suffix;
      else
         declare
            S : String := Integer'Image (N);
         begin
            return Name & S (S'First + 1 .. S'Last) & Xref_Suffix;
         end;
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
      Key   : String_Access := new String' (Source_Filename);
   begin
      --  get hashed value
      Data := STable.Get (Pool.all, Key);
      if Data /= null then
         Free (Key);
         return Data.Xref_Filename;
      end if;

      --  generate new xref file name
      Data := new Xref_Elmt_Record; -- new hashtable value
      Data.Source_Filename := Key;
      loop
         declare
            Name        : String := Generate_Filename (Source_Filename, N);
            Full_Name   : String := Directory & Directory_Separator & Name;
            FD          : File_Descriptor;
         begin
            if not Is_Regular_File (Full_Name) then
               Data.Xref_Filename := new String' (Name);
               --  touch this file
               FD := Create_New_File (Full_Name, Binary);
               if FD = Invalid_FD then -- unable to create a new file
                  --  raise an exception if unable to create new xref file
                  Free (Data.Source_Filename);
                  Free (Data.Xref_Filename);
                  raise Xref_File_Error;
               end if;
               Close (FD);
               exit;
            end if;
         end;
         N := N + 1;
      end loop;

      --  add generated file to hashtable
      STable.Set (Pool.all, Data);

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
      Key       : String_Access := new String' (Source_Filename);
      Xref_Elmt : Xref_Elmt_Ptr :=
        STable.Get (Pool.all, Key);
   begin
      if Xref_Elmt = null then -- nothing to do
         return;
      end if;
      declare
         Result : Boolean;
         Full_Name  : String := Directory & Directory_Separator &
            Xref_Elmt.Xref_Filename.all;
      begin
         --  remove file (ignoring errors)
         Delete_File (Full_Name, Result);
         --  remove from hashtable
         STable.Remove (Pool.all, Key);
         Free (Key);
         Free (Xref_Elmt.Source_Filename);
         Free (Xref_Elmt.Xref_Filename);
         Free (Xref_Elmt);
      end;
   end Free_Filename_For;

end SN.Xref_Pools;
