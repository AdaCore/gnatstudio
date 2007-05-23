-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2002-2007                      --
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

with Ada.Exceptions;      use Ada.Exceptions;
with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;         use GNAT.OS_Lib;

with Glib.Xml_Int;        use Glib.Xml_Int;
with Glib;                use Glib;

with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Combo;           use Gtk.Combo;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.List;            use Gtk.List;
with Gtk.List_Item;       use Gtk.List_Item;
with Gtk.Menu;            use Gtk.Menu;
with Gtk.Menu_Item;       use Gtk.Menu_Item;
with Gtk.Object;          use Gtk.Object;
with Gtk.Toggle_Button;   use Gtk.Toggle_Button;
with Gtk.Widget;          use Gtk.Widget;

with GUI_Utils;           use GUI_Utils;
with Traces;              use Traces;
with XML_Parsers;

package body Histories is

   Me : constant Debug_Handle := Create ("Histories");

   use History_Hash.String_Hash_Table;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (History_Key_Record, History_Key_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Changed_Notifier_Record'Class, Changed_Notifier);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Menu_Callback_Record'Class, Menu_Callback);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (History_Hash.String_Hash_Table.HTable, HTable_Access);

   package Value_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, History_Key_Access);
   package Notifier_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Changed_Notifier);

   procedure Update_History
     (Button : access Gtk_Widget_Record'Class;
      Value  : History_Key_Access);
   procedure Update_History_Item
     (Item : access Gtk_Widget_Record'Class;
      Value  : History_Key_Access);
   --  Called when the button is toggled.

   type Menu_Changed_Notifier_Record is new Changed_Notifier_Record with record
      Menu     : Gtk_Menu_Item;
      Callback : Menu_Callback;
      Data     : History_Key_Access;
   end record;
   type Menu_Changed_Notifier is access all Menu_Changed_Notifier_Record'Class;
   procedure On_Changed
     (Notifier : access Menu_Changed_Notifier_Record;
      Hist     : in out History_Record;
      Key      : History_Key);

   procedure On_Menu_Destroy
     (Menu     : access Gtk_Widget_Record'Class;
      Notifier : Changed_Notifier);
   --  Called when the menu associated with a history key is destroyed

   procedure On_Menu_Selected
     (Menu_Item : access Gtk_Widget_Record'Class;
      Notifier  : Changed_Notifier);
   --  Called when one of the entries in a menu associated with a history key
   --  is destroyed.

   function Create_New_Key_If_Necessary
     (Hist     : History_Record;
      Key      : History_Key;
      Key_Type : History_Key_Type) return History_Key_Access;
   --  Internal version of the public procedure

   ---------------------------------
   -- Create_New_Key_If_Necessary --
   ---------------------------------

   procedure Create_New_Key_If_Necessary
     (Hist     : in out History_Record;
      Key      : History_Key;
      Key_Type : History_Key_Type)
   is
      Val : History_Key_Access;
      pragma Unreferenced (Val);
   begin
      Val := Create_New_Key_If_Necessary (Hist, Key, Key_Type);
   end Create_New_Key_If_Necessary;

   ---------------------------------
   -- Create_New_Key_If_Necessary --
   ---------------------------------

   function Create_New_Key_If_Necessary
     (Hist     : History_Record;
      Key      : History_Key;
      Key_Type : History_Key_Type) return History_Key_Access
   is
      Value : History_Key_Access := Get (Hist.Table.all, String (Key));
   begin
      if Value = Null_History then
         Value := new History_Key_Record (Key_Type);
         Set (Hist.Table.all, String (Key), Value);

      elsif Value.Typ /= Key_Type then
         raise Invalid_Key_Type;
      end if;

      return Value;
   end Create_New_Key_If_Necessary;

   -----------------------------------------
   -- Create_New_Boolean_Key_If_Necessary --
   -----------------------------------------

   procedure Create_New_Boolean_Key_If_Necessary
     (Hist          : in out History_Record;
      Key           : History_Key;
      Default_Value : Boolean)
   is
      Value : History_Key_Access := Get (Hist.Table.all, String (Key));
   begin
      if Value = Null_History then
         Value := new History_Key_Record (Booleans);
         Set (Hist.Table.all, String (Key), Value);
         Value.Value := Default_Value;
      end if;
   end Create_New_Boolean_Key_If_Necessary;

   --------------------
   -- Set_Max_Length --
   --------------------

   procedure Set_Max_Length
     (Hist : in out History_Record; Num : Positive; Key : History_Key := "")
   is
      Current : History_Key_Access;
   begin
      if Key = "" then
         Hist.Max_Length := Num;
      else
         Current := Create_New_Key_If_Necessary (Hist, Key, Strings);
         Current.Max_Length := Num;
      end if;
   end Set_Max_Length;

   ----------------------
   -- Allow_Duplicates --
   ----------------------

   procedure Allow_Duplicates
     (Hist        : in out History_Record;
      Key         : History_Key;
      Allow       : Boolean;
      Merge_First : Boolean := False)
   is
      Current : constant History_Key_Access := Create_New_Key_If_Necessary
        (Hist, Key, Strings);
   begin
      Current.Allow_Duplicates := Allow;
      Current.Merge_First := Merge_First;
   end Allow_Duplicates;

   ----------
   -- Load --
   ----------

   procedure Load (Hist : in out History_Record; File_Name : String) is
      N     : Node_Ptr;
      File  : Node_Ptr;
      Key   : Node_Ptr;
      Num   : Natural;
      Value : History_Key_Access;
      Err   : String_Access;

   begin
      if not Is_Regular_File (File_Name) then
         return;
      end if;

      XML_Parsers.Parse (File_Name, File, Err);

      if File = null then
         Trace (Me, "Couldn't load history file: " & Err.all);
         Free (Err);
      else
         Key := File.Child;

         while Key /= null loop
            begin
               if Key.Attributes = null
                 or else Get_Attribute (Key, "type") = "strings"
               then
                  Value := Create_New_Key_If_Necessary
                    (Hist,
                     History_Key (Get_Attribute (Key, "name")),
                     Strings);

               elsif Get_Attribute (Key, "type") = "booleans" then
                  Value := Create_New_Key_If_Necessary
                    (Hist, History_Key (Get_Attribute (Key, "name")),
                     Booleans);

               else
                  Value := null;
                  Trace (Me, "Invalid data type in " & File_Name
                         & " : " & Get_Attribute (Key, "type"));
               end if;

            exception
               when E : others =>
                  Value := null;
                  Trace (Exception_Handle, E);
            end;

            if Value /= null then
               N := Key.Child;

               case Value.Typ is
               when Strings =>
                  Num := 0;

                  while N /= null loop
                     if N.Tag.all /= "Length" then
                        Num := Num + 1;
                     end if;
                     N := N.Next;
                  end loop;

                  Value.Max_Length := -1;
                  if Num /= 0 then
                     Value.List := new String_List (1 .. Num);
                  else
                     Value.List := null;
                  end if;
                  N := Key.Child;
                  Num := 1;

                  while N /= null loop
                     if N.Tag.all = "Length" then
                        begin
                           Value.Max_Length := Integer'Value (N.Value.all);
                        exception
                           when Constraint_Error => null;
                        end;
                     elsif N.Value /= null then
                        Value.List (Num) := new String'(N.Value.all);
                        Num := Num + 1;
                     else
                        Value.List (Num) := new String'("");
                        Num := Num + 1;
                     end if;
                     N := N.Next;
                  end loop;

               when Booleans =>
                  if N /= null
                    and then N.Tag.all = "value"
                  then
                     Value.Value := Boolean'Value (N.Value.all);
                  else
                     Value.Value := False;
                  end if;
               end case;
            end if;

            Key := Key.Next;
         end loop;

         Free (File);
      end if;

   exception
      when Status_Error | Name_Error =>
         null;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Hist : in out History_Record; File_Name : String) is
      File, Key, N : Node_Ptr;
      Iter         : Iterator;
      Value        : History_Key_Access;
   begin
      File := new Node;
      File.Tag := new String'("History");

      Get_First (Hist.Table.all, Iter);
      loop
         Value := Get_Element (Iter);
         exit when Value = Null_History;

         Key := new Node;
         Key.Tag := new String'("key");
         Set_Attribute (Key, "name", Get_Key (Iter));

         case Value.Typ is
            when Strings =>
               Set_Attribute (Key, "type", "strings");

               if Value.Max_Length /= -1 then
                  N := new Node;
                  N.Tag := new String'("Length");
                  N.Value := new String'(Integer'Image (Value.Max_Length));
                  Add_Child (Key, N);
               end if;

               if Value.List /= null then
                  for V in reverse Value.List'Range loop
                     N := new Node;
                     N.Tag := new String'("value");
                     N.Value := new String'(Value.List (V).all);
                     Add_Child (Key, N);
                  end loop;
               end if;

            when Booleans =>
               Set_Attribute (Key, "type", "booleans");
               N := new Node;
               N.Tag := new String'("value");
               N.Value := new String'(Boolean'Image (Value.Value));
               Add_Child (Key, N);
         end case;

         Add_Child (File, Key);

         Get_Next (Hist.Table.all, Iter);
      end loop;

      Print (File, File_Name);
      Free (File);
   end Save;

   ----------
   -- Free --
   ----------

   procedure Free (Hist : in out History_Record) is
      Iter  : Iterator;
      Value : History_Key_Access;
   begin
      Get_First (Hist.Table.all, Iter);
      loop
         Value := Get_Element (Iter);
         exit when Value = Null_History;

         case Value.Typ is
            when Strings  => Free (Value.List);
            when Booleans => null;
         end case;

         if Value.Notifier /= null then
            Free (Value.Notifier.all);
            Unchecked_Free (Value.Notifier);
         end if;

         Unchecked_Free (Value);

         Get_Next (Hist.Table.all, Iter);
      end loop;
      Reset (Hist.Table.all);
      Unchecked_Free (Hist.Table);
   end Free;

   -----------------
   -- Get_History --
   -----------------

   function Get_History
     (Hist : History_Record; Key : History_Key)
      return GNAT.Strings.String_List_Access
   is
      Val : constant History_Key_Access :=
              Create_New_Key_If_Necessary (Hist, Key, Strings);
   begin
      return Val.List;
   end Get_History;

   -----------------
   -- Get_History --
   -----------------

   procedure Get_History
     (Hist        : History_Record;
      Key         : History_Key;
      Combo       : access Gtk.Combo.Gtk_Combo_Record'Class;
      Clear_Combo : Boolean := True;
      Prepend     : Boolean := False)
   is
      List  : constant Gtk_List := Get_List (Combo);
      Value : constant String_List_Access := Get_History (Hist, Key);
      Item  : Gtk_List_Item;
   begin
      if Clear_Combo then
         Clear_Items (List, 0, -1);
      end if;

      if Value /= null then
         for V in Value'Range loop
            --  Do not add the empty item. It is stored internally to properly
            --  restore the contents of the entry, but shouldn't appear in the
            --  list.
            if Value (V).all /= "" then
               --  Do not add the item directly, in case there was already a
               --  similar entry in the list if it wasn't cleared
               if Clear_Combo then
                  Gtk_New (Item, Value (V).all);
                  Show (Item);
                  Add (List, Item);
               else
                  Add_Unique_List_Entry (List, Value (V).all, Prepend);
               end if;
            end if;
         end loop;

         Set_Text (Get_Entry (Combo), Value (Value'First).all);
         Select_Region (Get_Entry (Combo), 0, -1);

      else
         Set_Text (Get_Entry (Combo), "");
      end if;
   end Get_History;

   --------------------
   -- Add_To_History --
   --------------------

   procedure Add_To_History
     (Hist      : in out History_Record;
      Key       : History_Key;
      New_Entry : String)
   is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (String_List, String_List_Access);
      Value : constant History_Key_Access :=
                Create_New_Key_If_Necessary (Hist, Key, Strings);
      Tmp   : String_Access;
      Tmp2  : String_List_Access;
   begin
      --  Is this item already in the table ? We do not need to check, if
      --  duplicates are allowed.

      if Value.List /= null then
         if not Value.Allow_Duplicates then
            for V in Value.List'Range loop
               if Value.List (V).all = New_Entry then
                  Tmp := Value.List (V);
                  Value.List (Value.List'First + 1 .. V) :=
                    Value.List (Value.List'First .. V - 1);
                  Value.List (Value.List'First) := Tmp;
                  return;
               end if;
            end loop;

         elsif Value.Merge_First
           and then Value.List (Value.List'First).all = New_Entry
         then
            return;
         end if;

         --  Do we already have enough elements in the table

         if (Value.Max_Length /= -1
             and then Value.List'Length >= Value.Max_Length)
           or else
           (Value.Max_Length = -1
            and then Value.List'Length >= Hist.Max_Length)
         then
            Free (Value.List (Value.List'Last));
            Value.List (Value.List'First + 1 .. Value.List'Last) :=
              Value.List (Value.List'First .. Value.List'Last - 1);
            Value.List (Value.List'First) := new String'(New_Entry);
         else
            --  Insert the element in the table
            Tmp2 := new String_List (1 .. Value.List'Length + 1);
            Tmp2 (2 .. Tmp2'Last) := Value.List.all;
            Unchecked_Free (Value.List);
            Tmp2 (Tmp2'First) := new String'(New_Entry);
            Value.List := Tmp2;
         end if;

         if Value.Notifier /= null then
            On_Changed (Value.Notifier, Hist, Key);
         end if;

      else
         Value.List := new String_List'(1 => new String'(New_Entry));
      end if;
   end Add_To_History;

   -----------------
   -- Set_History --
   -----------------

   procedure Set_History
     (Hist      : in out History_Record;
      Key       : History_Key;
      Value     : Boolean)
   is
      Val : constant History_Key_Access := Create_New_Key_If_Necessary
        (Hist, Key, Booleans);
   begin
      Val.Value := Value;

      if Val.Notifier /= null then
         On_Changed (Val.Notifier, Hist, Key);
      end if;
   end Set_History;

   -----------------
   -- Get_History --
   -----------------

   function Get_History
     (Hist      : History_Record;
      Key       : History_Key) return Boolean
   is
      Val : constant History_Key_Access := Create_New_Key_If_Necessary
        (Hist, Key, Booleans);
   begin
      return Val.Value;
   end Get_History;

   --------------------
   -- Update_History --
   --------------------

   procedure Update_History
     (Button : access Gtk_Widget_Record'Class;
      Value  : History_Key_Access) is
   begin
      Value.Value := Get_Active (Gtk_Toggle_Button (Button));
   end Update_History;

   -------------------------
   -- Update_History_Item --
   -------------------------

   procedure Update_History_Item
     (Item  : access Gtk_Widget_Record'Class;
      Value : History_Key_Access) is
   begin
      Value.Value := Get_Active (Gtk_Check_Menu_Item (Item));
   end Update_History_Item;

   ---------------
   -- Associate --
   ---------------

   procedure Associate
     (Hist   : in out History_Record;
      Key    : History_Key;
      Button : access Gtk.Toggle_Button.Gtk_Toggle_Button_Record'Class)
   is
      Val : constant History_Key_Access :=
              Create_New_Key_If_Necessary (Hist, Key, Booleans);
   begin
      Set_Active (Button, Val.Value);
      Value_Callback.Connect
        (Button, Gtk.Toggle_Button.Signal_Toggled,
         Update_History'Access, User_Data => Val);
   end Associate;

   ---------------
   -- Associate --
   ---------------

   procedure Associate
     (Hist : in out History_Record;
      Key  : History_Key;
      Item : access Gtk.Check_Menu_Item.Gtk_Check_Menu_Item_Record'Class)
   is
      Val : constant History_Key_Access :=
              Create_New_Key_If_Necessary (Hist, Key, Booleans);
   begin
      Set_Active (Item, Val.Value);
      Value_Callback.Connect
        (Item, Gtk.Toggle_Button.Signal_Toggled,
         Update_History_Item'Access, User_Data => Val);
   end Associate;

   ---------------
   -- Associate --
   ---------------

   procedure Associate
     (Hist      : in out History_Record;
      Key       : History_Key;
      Menu      : access Gtk.Menu_Item.Gtk_Menu_Item_Record'Class;
      Callback  : Menu_Callback)
   is
      Val      : constant History_Key_Access :=
                   Create_New_Key_If_Necessary (Hist, Key, Strings);
      Notifier : constant Menu_Changed_Notifier :=
                   new Menu_Changed_Notifier_Record;
   begin
      Notifier.Menu     := Gtk_Menu_Item (Menu);
      Notifier.Callback := Callback;
      Notifier.Data     := Val;
      Val.Notifier      := Changed_Notifier (Notifier);

      Notifier_Callback.Connect
        (Menu, Signal_Destroy,
         On_Menu_Destroy'Access, Changed_Notifier (Notifier));
      On_Changed (Notifier, Hist, Key);
   end Associate;

   ---------------------
   -- On_Menu_Destroy --
   ---------------------

   procedure On_Menu_Destroy
     (Menu     : access Gtk_Widget_Record'Class;
      Notifier : Changed_Notifier)
   is
      pragma Unreferenced (Menu);
      Notif         : constant Menu_Changed_Notifier :=
                        Menu_Changed_Notifier (Notifier);
      Changed_Notif : Changed_Notifier := Changed_Notifier (Notif);
   begin
      Free (Notif.Callback.all);
      Unchecked_Free (Notif.Callback);
      Notif.Data.Notifier := null;
      Free (Notif.all);
      Unchecked_Free (Changed_Notif);
   end On_Menu_Destroy;

   ----------------------
   -- On_Menu_Selected --
   ----------------------

   procedure On_Menu_Selected
     (Menu_Item : access Gtk_Widget_Record'Class;
      Notifier  : Changed_Notifier)
   is
      Notif : constant Menu_Changed_Notifier :=
                Menu_Changed_Notifier (Notifier);
   begin
      Activate (Notif.Callback, Get_Path (Full_Path_Menu_Item (Menu_Item)));
   end On_Menu_Selected;

   ----------------
   -- On_Changed --
   ----------------

   procedure On_Changed
     (Notifier : access Menu_Changed_Notifier_Record;
      Hist     : in out History_Record;
      Key      : History_Key)
   is
      Value : constant String_List_Access :=
                Create_New_Key_If_Necessary (Hist, Key, Strings).List;
      Menu  : Gtk_Menu;
   begin
      if Get_Submenu (Notifier.Menu) /= null then
         Remove_Submenu (Notifier.Menu);
      end if;

      Gtk_New (Menu);
      Set_Submenu (Notifier.Menu, Gtk_Widget (Menu));

      if Value /= null then
         for V in Value'Range loop
            declare
               Path  : constant String := Value (V).all;
               Mitem : Full_Path_Menu_Item;
            begin
               Gtk_New (Mitem, Path, Path);
               Append (Menu, Mitem);

               if Notifier.Callback /= null then
                  Notifier_Callback.Connect
                    (Mitem,
                     Gtk.Menu_Item.Signal_Activate, On_Menu_Selected'Access,
                     Changed_Notifier (Notifier));
               end if;
            end;
         end loop;

         Show_All (Menu);
      end if;
   end On_Changed;

end Histories;
