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

with Ada.Text_IO;   use Ada.Text_IO;
with Glib;          use Glib;
with GNAT.OS_Lib;   use GNAT.OS_Lib;
with Gtk.Combo;     use Gtk.Combo;
with Gtk.GEntry;    use Gtk.GEntry;
with Glib.Xml_Int;  use Glib.Xml_Int;
with Gtk.List;      use Gtk.List;
with Gtk.List_Item; use Gtk.List_Item;
with Ada.Unchecked_Deallocation;
with GUI_Utils;     use GUI_Utils;
with Gtk.Handlers;  use Gtk.Handlers;
with Gtk.Toggle_Button; use Gtk.Toggle_Button;

package body Histories is

   use History_Hash.String_Hash_Table;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (History_Key_Record, History_Key_Access);

   package Value_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Toggle_Button_Record, History_Key_Access);

   procedure Update_History
     (Button : access Gtk_Toggle_Button_Record'Class;
      Value  : History_Key_Access);
   --  Called when the button is toggled.

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
         Current := Get (Hist.Table, String (Key));
         if Current /= null then
            Current.Max_Length := Num;
         end if;
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
      Current : History_Key_Access := Get (Hist.Table, String (Key));
   begin
      if Current /= null then
         Current.Allow_Duplicates := Allow;
         Current.Merge_First := Merge_First;
      end if;
   end Allow_Duplicates;

   ----------
   -- Load --
   ----------

   procedure Load (Hist : out History_Record; File_Name : String) is
      N : Node_Ptr;
      File : Node_Ptr;
      Key  : Node_Ptr;
      Num  : Natural;
      Value : History_Key_Access;
   begin
      File := Parse (File_Name);

      Key := File.Child;

      while Key /= null loop

         if Key.Attributes = null
           or else Get_Attribute (Key, "type") = "strings"
         then
            Value := new History_Key_Record (Strings);
         else
            Value := new History_Key_Record (Booleans);
         end if;

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
                  else
                     Value.List (Num) := new String'(N.Value.all);
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

         Set (Hist.Table, Key.Tag.all, Value);

         Key := Key.Next;
      end loop;

      Free (File);

   exception
      when Status_Error | Name_Error =>
         null;
   end Load;

   ----------
   -- Save --
   ----------

   procedure Save (Hist : in out History_Record; File_Name : String) is
      File, Key, N : Node_Ptr;
      Iter : Iterator;
      Value : History_Key_Access;
   begin
      File := new Node;
      File.Tag := new String'("History");

      Get_First (Hist.Table, Iter);
      loop
         Value := Get_Element (Iter);
         exit when Value = Null_History;

         Key := new Node;
         Key.Tag := new String'(Get_Key (Iter));

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

         Get_Next (Hist.Table, Iter);
      end loop;

      Print (File, File_Name);
      Free (File);
   end Save;

   ----------
   -- Free --
   ----------

   procedure Free (Hist : in out History_Record) is
      Iter : Iterator;
      Value : History_Key_Access;
   begin
      Get_First (Hist.Table, Iter);
      loop
         Value := Get_Element (Iter);
         exit when Value = Null_History;

         case Value.Typ is
            when Strings  => Free (Value.List);
            when Booleans => null;
         end case;

         Unchecked_Free (Value);

         Get_Next (Hist.Table, Iter);
      end loop;
      Reset (Hist.Table);
   end Free;

   -----------------
   -- Get_History --
   -----------------

   function Get_History
     (Hist : History_Record; Key : History_Key)
      return GNAT.OS_Lib.String_List_Access
   is
      Val : constant History_Key_Access := Get (Hist.Table, String (Key));
   begin
      if Val /= null then
         return Get (Hist.Table, String (Key)).List;
      else
         return null;
      end if;
   end Get_History;

   -----------------
   -- Get_History --
   -----------------

   procedure Get_History
     (Hist  : History_Record;
      Key   : History_Key;
      Combo : access Gtk.Combo.Gtk_Combo_Record'Class;
      Clear_Combo : Boolean := True)
   is
      Item : Gtk_List_Item;
      List : constant Gtk_List := Get_List (Combo);
      Value : constant String_List_Access := Get_History (Hist, Key);
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
                  Add_Unique_List_Entry (List, Value (V).all);
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
      Value : History_Key_Access := Get (Hist.Table, String (Key));
      Tmp   : String_Access;
      Tmp2  : String_List_Access;
   begin
      if Value /= Null_History then
         --  Is this item already in the table ? We do not need to check, if
         --  duplicates are allowed.

         if not Value.Allow_Duplicates then
            for V in Value.List'Range loop
               if Value.List (V).all = New_Entry then
                  Tmp := Value.List (V);
                  Value.List (Value.List'First + 1 .. V) :=
                    Value.List (Value.List'First .. V - 1);
                  Value.List (Value.List'First) := Tmp;

                  Set (Hist.Table, String (Key), Value);
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

            Set (Hist.Table, String (Key), Value);
            return;
         end if;

         --  Insert the element in the table
         Tmp2 := new String_List (1 .. Value.List'Length + 1);
         Tmp2 (2 .. Tmp2'Last) := Value.List.all;
         Unchecked_Free (Value.List);
         Tmp2 (Tmp2'First) := new String'(New_Entry);
         Value.List := Tmp2;

      else
         Value := new History_Key_Record'
           (Typ              => Strings,
            List             => new String_List'(1 => new String'(New_Entry)),
            Max_Length       => -1,
            Allow_Duplicates => False,
            Merge_First      => True);
         Set (Hist.Table, String (Key), Value);
      end if;
   end Add_To_History;

   -------------
   -- No_Free --
   -------------

   procedure No_Free (A : in out History_Key_Access) is
      pragma Unreferenced (A);
   begin
      null;
   end No_Free;

   -----------------
   -- Set_History --
   -----------------

   procedure Set_History
     (Hist      : in out History_Record;
      Key       : History_Key;
      Value     : Boolean)
   is
      Val : History_Key_Access := Get (Hist.Table, String (Key));
   begin
      if Val = Null_History then
         Val := new History_Key_Record'
           (Typ   => Booleans,
            Value => Value);
         Set (Hist.Table, String (Key), Val);
      else
         Val.Value := Value;
      end if;
   end Set_History;

   -----------------
   -- Get_History --
   -----------------

   function Get_History
     (Hist      : History_Record;
      Key       : History_Key) return Boolean
   is
      Val : constant History_Key_Access := Get (Hist.Table, String (Key));
   begin
      if Val = null then
         return False;
      else
         return Val.Value;
      end if;
   end Get_History;

   --------------------
   -- Update_History --
   --------------------

   procedure Update_History
     (Button : access Gtk_Toggle_Button_Record'Class;
      Value  : History_Key_Access) is
   begin
      Value.Value := Get_Active (Button);
   end Update_History;

   ---------------
   -- Associate --
   ---------------

   procedure Associate
     (Hist      : in out History_Record;
      Key       : History_Key;
      Button    : access Gtk.Toggle_Button.Gtk_Toggle_Button_Record'Class)
   is
      Val : History_Key_Access := Get (Hist.Table, String (Key));
   begin
      --  This call is needed to ensure that Val is created and not null
      if Val = null then
         Set_History (Hist, Key, False);
         Val := Get (Hist.Table, String (Key));
      end if;

      Set_Active (Button, Val.Value);
      Value_Callback.Connect
        (Button, "toggled",
         Value_Callback.To_Marshaller (Update_History'Access),
         User_Data => Val);
   end Associate;

end Histories;

