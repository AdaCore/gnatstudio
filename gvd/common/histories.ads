-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                     Copyright (C) 2002                            --
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

--  This package provides high-level histories management. It should be used
--  for all combo boxes and the reopen menus, so that when GPS is started again
--  we an restore the histories.

with GNAT.OS_Lib;
with Gtk.Combo;
with String_Hash;

package Histories is

   type History_Record is private;
   type History is access History_Record;

   type History_Key is new String;

   procedure Set_Max_Length (Hist : in out History_Record; Num : Positive);
   --  Set the maximal number of entries stored in each key of Hist.

   procedure Load (Hist : out History_Record; File_Name : String);
   --  Load Hist from file File_Name

   procedure Save (Hist : in out History_Record; File_Name : String);
   --  Save Hist to a file

   procedure Free (Hist : in out History_Record);
   --  Free the memory used by Hist. Get_History will return empty results
   --  afterwards.

   function Get_History
     (Hist : History_Record; Key : History_Key)
      return GNAT.OS_Lib.String_List_Access;
   --  Return the list of strings stored as Key.
   --  The returned array mustn't be freed by the user, it references internal
   --  data.
   --  The most recent entry is returned first.

   procedure Get_History
     (Hist        : History_Record;
      Key         : History_Key;
      Combo       : access Gtk.Combo.Gtk_Combo_Record'Class;
      Clear_Combo : Boolean := True);
   --  Set the contents of the combo to the list of strings associated with
   --  Key.
   --  If Clear_Combo is False, then the previous contents of the combo is kept

   procedure Add_To_History
     (Hist      : in out History_Record;
      Key       : History_Key;
      New_Entry : String);
   --  Store a new history string.
   --  If too many strings are stored, the oldest one is removed.
   --  If New_Entry is already in the history, it is not added a second time,
   --  but moved into first position.

private

   procedure No_Free (A : in out GNAT.OS_Lib.String_List_Access);
   --  Does nothing

   package History_Hash is new String_Hash
     (Data_Type    => GNAT.OS_Lib.String_List_Access,
      Free_Data    => No_Free,
      Null_Ptr     => null);

   type History_Record is record
      Max_Length : Positive := Positive'Last;
      Table      : History_Hash.String_Hash_Table.HTable;
   end record;

end Histories;

