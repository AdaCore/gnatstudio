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
--  we can restore the histories.
--  It also provides a way to save the current value for check buttons from one
--  session of GPS to the other.

with GNAT.OS_Lib;
with Gtk.Combo;
with Gtk.Toggle_Button;
with String_Hash;

package Histories is

   type History_Record is private;
   type History is access History_Record;

   type History_Key is new String;

   procedure Load (Hist : out History_Record; File_Name : String);
   --  Load Hist from file File_Name

   procedure Save (Hist : in out History_Record; File_Name : String);
   --  Save Hist to a file

   procedure Free (Hist : in out History_Record);
   --  Free the memory used by Hist. Get_History will return empty results
   --  afterwards.

   ---------------------
   -- List of strings --
   ---------------------

   procedure Set_Max_Length
     (Hist : in out History_Record; Num : Positive; Key : History_Key := "");
   --  Set the maximal number of entries stored in each key of Hist.
   --  If Key is the empty string, then this becomes the default maximum
   --  length, used unless a key-specific one is specified later on.

   procedure Allow_Duplicates
     (Hist        : in out History_Record;
      Key         : History_Key;
      Allow       : Boolean;
      Merge_First : Boolean := False);
   --  Indicates whether Key allows duplicates. If it doesn, adding an entry
   --  that already exists to the history will simply move it to the first
   --  position. If Allow is True, a copy is inserted at the first position,
   --  but other similar entries are not removed.
   --  If Merge_First is True, however, duplicates will not be inserted if the
   --  new entry is already the first one in the history. Merge_First is
   --  irrelevant if Allow is False.
   --  Note: changing this setting dynamically will not remove existing
   --  duplicates in Key.
   --  The default is not to allow duplicates (more convenient for combo
   --  boxes).

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

   --------------
   -- Booleans --
   --------------

   procedure Set_History
     (Hist      : in out History_Record;
      Key       : History_Key;
      Value     : Boolean);
   --  Set the value of the history key

   function Get_History
     (Hist      : History_Record;
      Key       : History_Key) return Boolean;
   --  Return the current value of Key.

   procedure Associate
     (Hist      : in out History_Record;
      Key       : History_Key;
      Button    : access Gtk.Toggle_Button.Gtk_Toggle_Button_Record'Class);
   --  Associate Button with Key.
   --  The status of the button is set to the value of Key.
   --  Every time the button is toggled, the key's value is changed. Thus, its
   --  current value will be saved when Hist is saved.

private

   type History_Key_Type is (Strings, Booleans);

   type History_Key_Record (Typ : History_Key_Type := Strings) is record
      case Typ is
         when Strings =>
            List             : GNAT.OS_Lib.String_List_Access;
            Max_Length       : Integer := -1;
            --  -1 means unspecified, use the default one for the whole
            --  History_Record

            Allow_Duplicates : Boolean := False;
            Merge_First      : Boolean := True;

         when Booleans =>
            Value : Boolean;
      end case;
   end record;

   type History_Key_Access is access History_Key_Record;

   procedure No_Free (A : in out History_Key_Access);
   --  Does nothing

   Null_History : constant History_Key_Access := null;

   package History_Hash is new String_Hash
     (Data_Type    => History_Key_Access,
      Free_Data    => No_Free,
      Null_Ptr     => Null_History);

   type History_Record is record
      Max_Length : Positive := Positive'Last;
      Table      : History_Hash.String_Hash_Table.HTable;
   end record;

end Histories;

