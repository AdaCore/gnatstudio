------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2012, AdaCore                     --
--                                                                          --
-- This is free software;  you can redistribute it  and/or modify it  under --
-- terms of the  GNU General Public License as published  by the Free Soft- --
-- ware  Foundation;  either version 3,  or (at your option) any later ver- --
-- sion.  This software is distributed in the hope  that it will be useful, --
-- but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN- --
-- TABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the GNU General Public --
-- License for  more details.  You should have  received  a copy of the GNU --
-- General  Public  License  distributed  with  this  software;   see  file --
-- COPYING3.  If not, go to http://www.gnu.org/licenses for a complete copy --
-- of the license.                                                          --
------------------------------------------------------------------------------

--  This package provides high-level histories management. It should be used
--  for all combo boxes and the reopen menus, so that when GPS is started again
--  we can restore the histories.
--  It also provides a way to save the current value for check buttons from one
--  session of GPS to the other.

with GNAT.Strings;
with GNATCOLL.VFS;

with Glib;
with Gtk.Check_Menu_Item;
with Gtk.Combo_Box;
with Gtk.GEntry;
with Gtk.Toggle_Button;
with Gtk.Menu_Item;
with String_Hash;

package Histories is

   type History_Record is private;
   type History is access History_Record;

   type History_Key is new String;
   type History_Key_Type is (Strings, Booleans);

   No_Key : constant History_Key := "";

   procedure Load
     (Hist      : in out History_Record;
      File_Name : GNATCOLL.VFS.Virtual_File);
   --  Load Hist from file File_Name

   procedure Save
     (Hist      : in out History_Record;
      File_Name : GNATCOLL.VFS.Virtual_File;
      Success   : out Boolean);
   --  Save Hist to a file

   procedure Free (Hist : in out History_Record);
   --  Free the memory used by Hist. Get_History will return empty results
   --  afterwards.

   procedure Create_New_Key_If_Necessary
     (Hist     : in out History_Record;
      Key      : History_Key;
      Key_Type : History_Key_Type);
   --  Make sure that a key with the appropriate type exists.
   --  If a key with the same name but a wrong type exists, a Invalid_Key_Type
   --  is raised.
   --  Calling this procedure is not mandatory. By default, keys are created
   --  with the appropriate type the first time some data is stored.

   Invalid_Key_Type : exception;

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
   --  Indicates whether Key allows duplicates. If it doesn't, adding an entry
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
      return GNAT.Strings.String_List_Access;
   --  Return the list of strings stored as Key.
   --  The returned array mustn't be freed by the user, it references internal
   --  data.
   --  The most recent entry is returned first.

   procedure Get_History
     (Hist        : History_Record;
      Key         : History_Key;
      Combo       : access Gtk.Combo_Box.Gtk_Combo_Box_Record'Class;
      Clear_Combo : Boolean := True;
      Prepend     : Boolean := False;
      Col         : Glib.Gint := 0);
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

   type Menu_Callback_Record is abstract tagged null record;
   type Menu_Callback is access all Menu_Callback_Record'Class;
   procedure Activate
     (Callback : access Menu_Callback_Record; Item : String) is abstract;
   --  Called by the menu entries created by Associate below. User-data should
   --  be stored in Callback directly.

   procedure Free (Callback : in out Menu_Callback_Record) is null;
   --  Called when the menu associated with callback is destroyed

   procedure Associate
     (Hist     : in out History_Record;
      Key      : History_Key;
      Menu     : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      Callback : Menu_Callback);
   --  Associate the menu with Key.
   --  Every time some entry is added to Key, a corresponding entry is added to
   --  the submenu of Menu. Callback is set for all these new entries.
   --  Entries in this submenu are shorten if required (see
   --  GUI_Utils.Full_Path_Menu_Item).
   --  Callback is automatically freed when the menu is destroyed.

   function Most_Recent
     (Hist : access History_Record;
      Key  : History_Key;
      Default : String := "") return String;
   --  Return the most recent history for this key, or Default if the key did
   --  not exist yet.

   procedure Save_Text
     (Self : access Gtk.GEntry.Gtk_Entry_Record'Class;
      Hist : access History_Record;
      Key  : History_Key);
   --  Saves the text of the widget as the most recent entry in the
   --  corresponding history. A history key is created as needed, which
   --  stores only one entry.

   --------------
   -- Booleans --
   --------------

   procedure Create_New_Boolean_Key_If_Necessary
     (Hist          : in out History_Record;
      Key           : History_Key;
      Default_Value : Boolean);
   --  Create a new key, if no such one already exists in the internal tables.
   --  In that case, the default_value is assigned to it

   procedure Set_History
     (Hist  : in out History_Record;
      Key   : History_Key;
      Value : Boolean);
   --  Set the value of the history key

   function Get_History
     (Hist : History_Record;
      Key  : History_Key) return Boolean;
   --  Return the current value of Key

   procedure Associate
     (Hist   : in out History_Record;
      Key    : History_Key;
      Button : access Gtk.Toggle_Button.Gtk_Toggle_Button_Record'Class);
   --  Associate Button with Key.
   --  The status of the button is set to the value of Key.
   --  Every time the button is toggled, the key's value is changed. Thus, its
   --  current value will be saved when Hist is saved.

   procedure Associate
     (Hist : in out History_Record;
      Key  : History_Key;
      Item : access Gtk.Check_Menu_Item.Gtk_Check_Menu_Item_Record'Class);
   --  Same as above

private

   type Changed_Notifier_Record is abstract tagged limited null record;
   type Changed_Notifier is access all Changed_Notifier_Record'Class;
   procedure On_Changed
     (Notifier : access Changed_Notifier_Record;
      Hist     : in out History_Record;
      Key      : History_Key) is abstract;
   procedure Free (Notifier : in out Changed_Notifier_Record) is null;

   type History_Key_Record (Typ : History_Key_Type := Strings) is record
      Notifier : Changed_Notifier;

      case Typ is
         when Strings =>
            List             : GNAT.Strings.String_List_Access;
            Max_Length       : Integer := -1;
            --  -1 means unspecified, use the default one for the whole
            --  History_Record

            Allow_Duplicates : Boolean := False;
            Merge_First      : Boolean := True;

         when Booleans =>
            Value : Boolean := False;
      end case;
   end record;

   type History_Key_Access is access History_Key_Record;

   procedure No_Free (A : in out History_Key_Access) is null;

   Null_History : constant History_Key_Access := null;

   package History_Hash is new String_Hash
     (Data_Type    => History_Key_Access,
      Free_Data    => No_Free,
      Null_Ptr     => Null_History);
   type HTable_Access is access History_Hash.String_Hash_Table.Instance;

   type History_Record is record
      Max_Length : Positive := Positive'Last;
      Table     : HTable_Access := new History_Hash.String_Hash_Table.Instance;
   end record;

   No_History : constant History_Record :=
     (Positive'Last, null);

end Histories;
