------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 1999-2019, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

separate (Templates_Parser)

package body Cached_Files is

   Initial_Size : constant := 20; -- cache initial size
   Growing_Size : constant := 50; -- cache growing size

   type File_Array is array (Positive range <>) of Tree;
   type File_Array_Access is access File_Array;

   Files : File_Array_Access;
   Index : Natural := 0;

   procedure Growth;
   --  Growth the size (by Growing_Size places) of Files array

   function Get (Filename : String) return Natural;
   --  Look for Filename into the set and return its index. Returns 0 if
   --  filename was not found.

   function Up_To_Date (T : Tree) return Boolean;
   --  Returns True if the file tree is up to date (the templates files
   --  have not been modified on disk) or False otherwise.

   type Mark_Mode is (Used, Released);

   procedure Update_Used_Counter (T : in out Static_Tree; Mode : Mark_Mode);
   --  Update C_Info used counter according to Mode

   ---------
   -- Add --
   ---------

   procedure Add
     (Filename : String;
      T        : Tree;
      Old      :    out Tree)
   is
      L_Filename : constant Unbounded_String := To_Unbounded_String (Filename);

      S : Natural := 1;
      E : Natural;
      N : Natural;
      I : Tree;

   begin
      E := Index;

      --  Does the table initialized and do we have enough place on it ?

      if Files = null or else Index = Files'Last then
         Growth;
      end if;

      loop
         exit when S > E;

         N := (S + E) / 2;

         if Files (N).Filename = L_Filename then
            --  This is a file that was already loaded. If loaded again
            --  it is because the file timestamp has changed. We want to
            --  just update the tree and not the info node (first node).

            Old := Files (N).Next;
            --  This is a pointer to the C_Info tree node, skipping the
            --  info node (first node).

            I := Files (N).I_File;
            --  Old include files dependencies

            Files (N).Next      := T.Next;
            Files (N).Timestamp := T.Timestamp;
            Files (N).I_File    := T.I_File;

            --  Now free old I_File

            declare
               O : Tree;
            begin
               while I /= null loop
                  O := I;
                  I := I.Next;
                  Unchecked_Free (O);
               end loop;
            end;

            --  This part is tricky, the tree could be currently used
            --  (parsed). So we need to be careful to not release the tree
            --  too early.

            if Old.Used = 0 then
               --  File is not currently used, we can release it safely
               Release (Old, Include => False);

               Old := T.Next;

            else
               --  Tree is used, mark it as obsoleted, it will be removed
               --  when no more used by the Release call.
               Old.Obsolete := True;
               Old.Used     := Old.Used + 1;

               --  But current tree is not used, it has been posted here
               --  for futur use. But if replaced right away it should be
               --  freed.
               Files (N).Next.Used := 0;
            end if;

            --  Nothing more to do in this case

            return;

         elsif Files (N).Filename < L_Filename then
            S := N + 1;

         else
            E := N - 1;
         end if;
      end loop;

      --  Filename was not found, insert it in the array at position S

      Files (S + 1 .. Index + 1) := Files (S .. Index);

      Index := Index + 1;

      Files (S) := T;

      Old := T.Next;
      --  Old point to the current C_Info tree
   end Add;

   ---------
   -- Get --
   ---------

   procedure Get
     (Filename : String;
      Result   :    out Static_Tree)
   is
      N : constant Natural := Get (Filename);
   begin
      if N = 0 then
         Result := Null_Static_Tree;

      else
         Result := (Files (N), Files (N).Next);

         Update_Used_Counter (Result, Mode => Used);
      end if;
   end Get;

   function Get (Filename : String) return Natural is

      L_Filename : constant Unbounded_String := To_Unbounded_String (Filename);

      S : Natural := 1;
      E : Natural := Index;
      N : Natural;

   begin
      loop
         exit when S > E;

         N := (S + E) / 2;

         if Files (N).Filename = L_Filename then

            if Up_To_Date (Files (N)) then
               return N;
            else
               --  File has changed on disk, we need to read it again. Just
               --  pretend that the file was not found.
               return 0;
            end if;

         elsif Files (N).Filename < L_Filename then
            S := N + 1;

         else
            E := N - 1;
         end if;
      end loop;

      return 0;
   end Get;

   ------------
   -- Growth --
   ------------

   procedure Growth is

      procedure Unchecked_Free is
         new Ada.Unchecked_Deallocation (File_Array, File_Array_Access);

   begin
      if Files = null then
         Files := new File_Array (1 .. Initial_Size);
      else

         declare
            New_Array : File_Array_Access;
         begin
            New_Array := new File_Array (1 .. Files'Length + Growing_Size);
            New_Array (1 .. Files'Length) := Files.all;
            Unchecked_Free (Files);
            Files := New_Array;
         end;
      end if;
   end Growth;

   -------------
   -- Release --
   -------------

   procedure Release (T : in out Static_Tree) is
   begin
      Templates_Parser_Tasking.Lock;
      pragma Assert (T.C_Info /= null);

      Update_Used_Counter (T, Mode => Released);

      if T.C_Info.Obsolete and then T.C_Info.Used = 0 then
         pragma Assert (T.Info.Next /= T.C_Info);
         Release (T.C_Info, Include => False);
      end if;
      Templates_Parser_Tasking.Unlock;
   exception
      when others =>
         Templates_Parser_Tasking.Unlock;
         raise;
   end Release;

   -------------
   -- Release --
   -------------

   procedure Release is
   begin
      Templates_Parser_Tasking.Lock;

      for K in 1 .. Index loop
         --  We do not want to release the include files, each include file as
         --  its own entry into the cache and is released as part of this loop.
         Release (Files (K), Include => False);
      end loop;

      Index := 0;

      Templates_Parser_Tasking.Unlock;
   exception
      when others =>
         Templates_Parser_Tasking.Unlock;
         raise;
   end Release;

   ----------------
   -- Up_To_Date --
   ----------------

   function Up_To_Date (T : Tree) return Boolean is
      use Configuration;

      P      : Tree;
      Result : Boolean;
   begin
      --  Check main file

      if Configuration.File_Time_Stamp
        (To_String (T.Filename)) /= T.Timestamp
      then
         return False;
      end if;

      --  Check all include files

      P := T.I_File;

      while P /= null loop
         Result := Up_To_Date (P.File.Info);

         if not Result then
            return False;
         end if;
         P := P.Next;
      end loop;

      return True;
   end Up_To_Date;

   -------------------------
   -- Update_Used_Counter --
   -------------------------

   procedure Update_Used_Counter
     (T : in out Static_Tree; Mode : Mark_Mode)
   is
      P : Tree;
   begin
      --  Mark current tree
      if T.Info = null then
         return;
      end if;

      case Mode is
         when Used =>
            T.Info.Next.Used := T.Info.Next.Used + 1;
         when Released =>
            T.Info.Next.Used := T.Info.Next.Used - 1;
      end case;

      --  And mark all include files

      P := T.Info.I_File;

      while P /= null loop
         Update_Used_Counter (P.File, Mode);
         P := P.Next;
      end loop;
   end Update_Used_Counter;

end Cached_Files;
