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

package body Histories is

   use History_Hash.String_Hash_Table;

   --------------------
   -- Set_Max_Length --
   --------------------

   procedure Set_Max_Length (Hist : in out History_Record; Num : Positive) is
   begin
      Hist.Max_Length := Num;
   end Set_Max_Length;

   ----------
   -- Load --
   ----------

   procedure Load (Hist : out History_Record; File_Name : String) is
      N : Node_Ptr;
      File : Node_Ptr;
      Key  : Node_Ptr;
      Num  : Natural;
      Value : String_List_Access;
   begin
      File := Parse (File_Name);

      Key := File.Child;

      while Key /= null loop
         N := Key.Child;
         Num := 0;

         while N /= null loop
            Num := Num + 1;
            N := N.Next;
         end loop;

         Value := new String_List (1 .. Num);
         N := Key.Child;
         Num := 1;

         while N /= null loop
            Value (Num) := new String'(N.Value.all);
            Num := Num + 1;
            N := N.Next;
         end loop;

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
      Value : String_List_Access;
   begin
      File := new Node;
      File.Tag := new String'("History");

      Get_First (Hist.Table, Iter);
      while Get_Element (Iter) /= null loop
         Key := new Node;
         Key.Tag := new String'(Get_Key (Iter));

         Value := Get_Element (Iter);
         for V in reverse Value'Range loop
            N := new Node;
            N.Tag := new String'("value");
            N.Value := new String'(Value (V).all);
            Add_Child (Key, N);
         end loop;

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
      Value : String_List_Access;
   begin
      Get_First (Hist.Table, Iter);
      while Get_Element (Iter) /= null loop
         Value := Get_Element (Iter);
         Free (Value);
         Get_Next (Hist.Table, Iter);
      end loop;
      Reset (Hist.Table);
   end Free;

   -----------------
   -- Get_History --
   -----------------

   function Get_History
     (Hist : History_Record; Key : History_Key)
      return GNAT.OS_Lib.String_List_Access is
   begin
      return Get (Hist.Table, String (Key));
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
            --  Do not add the item directly, in case there was already a
            --  similar entry in the list if it wasn't cleared
            if Clear_Combo then
               Gtk_New (Item, Value (V).all);
               Show (Item);
               Add (List, Item);
            else
               Add_Unique_List_Entry (List, Value (V).all);
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
      Value : String_List_Access := Get_History (Hist, Key);
      Tmp   : String_Access;
      Tmp2  : String_List_Access;
   begin
      if Value /= null then
         --  Is this item already in the table ?
         for V in Value'Range loop
            if Value (V).all = New_Entry then
               Tmp := Value (V);
               Value (Value'First + 1 .. V) :=
                 Value (Value'First .. V - 1);
               Value (Value'First) := Tmp;

               Set (Hist.Table, String (Key), Value);
               return;
            end if;
         end loop;

         --  Do we already have enough elements in the table

         if Value'Length = Hist.Max_Length then
            Free (Value (Value'Last));
            Value (Value'First + 1 .. Value'Last) :=
              Value (Value'First .. Value'Last - 1);
            Value (Value'First) := new String'(New_Entry);

            Set (Hist.Table, String (Key), Value);
            return;
         end if;

         --  Insert the element in the table
         Tmp2 := new String_List (1 .. Value'Length + 1);
         Tmp2 (2 .. Tmp2'Last) := Value.all;
         Unchecked_Free (Value);

      else
         Tmp2 := new String_List (1 .. 1);
      end if;

      Tmp2 (Tmp2'First) := new String'(New_Entry);
      Set (Hist.Table, String (Key), Tmp2);
   end Add_To_History;

   -------------
   -- No_Free --
   -------------

   procedure No_Free (A : in out GNAT.OS_Lib.String_List_Access) is
      pragma Unreferenced (A);
   begin
      null;
   end No_Free;

end Histories;

