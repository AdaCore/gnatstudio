------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2002-2018, AdaCore                     --
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

with Ada.Text_IO;         use Ada.Text_IO;
with Ada.Unchecked_Deallocation;
with GNAT.OS_Lib;         use GNAT.OS_Lib;

with GNATCOLL.VFS;        use GNATCOLL.VFS;

with Glib;                use Glib;

with Gtk.Check_Menu_Item; use Gtk.Check_Menu_Item;
with Gtk.Combo_Box;       use Gtk.Combo_Box;
with Gtk.GEntry;          use Gtk.GEntry;
with Gtk.Handlers;        use Gtk.Handlers;
with Gtk.List_Store;      use Gtk.List_Store;
with Gtk.Toggle_Button;   use Gtk.Toggle_Button;
with Gtk.Toggle_Tool_Button; use Gtk.Toggle_Tool_Button;
with Gtk.Tree_Model;      use Gtk.Tree_Model;
with Gtk.Widget;          use Gtk.Widget;

with GUI_Utils;             use GUI_Utils;
with XML_Utils;             use XML_Utils;
with GNATCOLL.Traces;       use GNATCOLL.Traces;
with XML_Parsers;

package body Histories is

   Me : constant Trace_Handle := Create ("GPS.COMMON.HISTORIES");

   use History_Hash.String_Hash_Table;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (History_Key_Record, History_Key_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Changed_Notifier_Record'Class, Changed_Notifier);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (History_Hash.String_Hash_Table.Instance, HTable_Access);

   package Value_Callback is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, History_Key_Access);

   procedure Update_History
     (Button : access Gtk_Widget_Record'Class;
      Value  : History_Key_Access);
   procedure Update_History_Tool_Button
     (Button : access Gtk_Widget_Record'Class;
      Value  : History_Key_Access);
   procedure Update_History_Item
     (Item : access Gtk_Widget_Record'Class;
      Value  : History_Key_Access);
   --  Called when the button is toggled

   function Create_New_Key_If_Necessary
     (Hist     : History_Record;
      Key      : History_Key;
      Key_Type : History_Key_Type) return History_Key_Access;
   function Create_New_Boolean_Key_If_Necessary
     (Hist          : in out History_Record;
      Key           : History_Key;
      Default_Value : Boolean) return History_Key_Access;
   --  Internal versions of the public procedures

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

   function Create_New_Boolean_Key_If_Necessary
     (Hist          : in out History_Record;
      Key           : History_Key;
      Default_Value : Boolean) return History_Key_Access
   is
      Value : History_Key_Access := Get (Hist.Table.all, String (Key));
   begin
      if Value = Null_History then
         Value := new History_Key_Record (Booleans);
         Set (Hist.Table.all, String (Key), Value);
         Value.Value := Default_Value;
      end if;
      return Value;
   end Create_New_Boolean_Key_If_Necessary;

   -----------------------------------------
   -- Create_New_Boolean_Key_If_Necessary --
   -----------------------------------------

   procedure Create_New_Boolean_Key_If_Necessary
     (Hist          : in out History_Record;
      Key           : History_Key;
      Default_Value : Boolean)
   is
      Tmp : History_Key_Access;
      pragma Unreferenced (Tmp);
   begin
      Tmp := Create_New_Boolean_Key_If_Necessary (Hist, Key, Default_Value);
   end Create_New_Boolean_Key_If_Necessary;

   --------------
   -- Get_Type --
   --------------

   function Get_Type
     (Hist : access History_Record;
      Key  : History_Key) return History_Key_Type
   is
      Val : constant History_Key_Access := Get (Hist.Table.all, String (Key));
   begin
      if Val = Null_History then
         raise Invalid_Key_Type with "Key " & String (Key) & " is not defined";
      else
         return Val.Typ;
      end if;
   end Get_Type;

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
      Current : constant History_Key_Access :=
                  Create_New_Key_If_Necessary (Hist, Key, Strings);
   begin
      Current.Allow_Duplicates := Allow;
      Current.Merge_First := Merge_First;
   end Allow_Duplicates;

   --------------------
   -- Set_Persistent --
   --------------------

   procedure Set_Persistent
     (Hist  : in out History_Record;
      Key   : History_Key;
      Value : Boolean)
   is
      Current : constant History_Key_Access :=
                  Create_New_Key_If_Necessary (Hist, Key, Strings);
   begin
      Current.Persistent := Value;
   end Set_Persistent;

   ----------
   -- Load --
   ----------

   procedure Load
     (Hist      : in out History_Record;
      File_Name : Virtual_File)
   is
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
                  Trace (Me, "Invalid data type in "
                         & File_Name.Display_Full_Name
                         & " : " & Get_Attribute (Key, "type"));
               end if;

            exception
               when E : others =>
                  Value := null;
                  Trace (Me, E);
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

   procedure Save
     (Hist      : in out History_Record;
      File_Name : Virtual_File;
      Success   : out Boolean)
   is
      File, Key, N : Node_Ptr;
      Iter         : Cursor;
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

         if Value.Persistent then
            Add_Child (File, Key);
         end if;

         Get_Next (Hist.Table.all, Iter);
      end loop;

      Print (File, File_Name, Success);
      Free (File);
   end Save;

   ----------
   -- Free --
   ----------

   procedure Free (Hist : in out History_Record) is
      Iter  : Cursor;
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
      Combo       : access Gtk.Combo_Box.Gtk_Combo_Box_Record'Class;
      Clear_Combo : Boolean := True;
      Prepend     : Boolean := False;
      Col         : Gint := 0;
      Filter      : access function (Item : String) return Boolean := null)
   is
      List  : constant Gtk_List_Store := -Get_Model (Combo);
      Value : constant String_List_Access := Get_History (Hist, Key);
      Iter  : Gtk_Tree_Iter;

   begin
      if Clear_Combo then
         List.Clear;
      end if;

      if Value /= null then
         for V in Value'Range loop
            --  Do not add the empty item. It is stored internally to properly
            --  restore the contents of the entry, but shouldn't appear in the
            --  list.
            if Value (V).all /= ""
              and then (Filter = null or else Filter (Value (V).all))
            then
               --  Do not add the item directly, in case there was already a
               --  similar entry in the list if it wasn't cleared
               if Clear_Combo then
                  List.Append (Iter);
                  List.Set (Iter, Col, Value (V).all);
               else
                  Iter := Add_Unique_List_Entry (List, Value (V).all, Prepend);
               end if;
            end if;
         end loop;

         if Value (Value'First).all /= "" then
            --  select ony when combo had value last time
            Set_Active (Combo, 0);
            Select_Region (Gtk_Entry (Get_Child (Combo)), 0, -1);
         end if;

      else
         Set_Text (Gtk_Entry (Get_Child (Combo)), "");
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
     (Hist  : in out History_Record;
      Key   : History_Key;
      Value : Boolean)
   is
      Val : constant History_Key_Access :=
              Create_New_Key_If_Necessary (Hist, Key, Booleans);
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
     (Hist : History_Record;
      Key  : History_Key) return Boolean
   is
      Val : constant History_Key_Access :=
              Create_New_Key_If_Necessary (Hist, Key, Booleans);
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

   --------------------------------
   -- Update_History_Tool_Button --
   --------------------------------

   procedure Update_History_Tool_Button
     (Button : access Gtk_Widget_Record'Class;
      Value  : History_Key_Access) is
   begin
      Value.Value := Get_Active (Gtk_Toggle_Tool_Button (Button));
   end Update_History_Tool_Button;

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
     (Hist    : in out History_Record;
      Key     : History_Key;
      Button  : access Gtk.Toggle_Button.Gtk_Toggle_Button_Record'Class;
      Default : Boolean := True)
   is
      Val : History_Key_Access;
   begin
      Val := Create_New_Boolean_Key_If_Necessary (Hist, Key, Default);
      Button.Set_Active (Val.Value);
      Value_Callback.Connect
        (Button, Gtk.Toggle_Button.Signal_Toggled,
         Update_History'Access, User_Data => Val);
   end Associate;

   ---------------
   -- Associate --
   ---------------

   procedure Associate
     (Hist   : in out History_Record;
      Key    : History_Key;
      Button : not null access
        Gtk.Toggle_Tool_Button.Gtk_Toggle_Tool_Button_Record'Class;
      Default : Boolean := True)
   is
      Val : History_Key_Access;
   begin
      Val := Create_New_Boolean_Key_If_Necessary (Hist, Key, Default);
      Button.Set_Active (Val.Value);
      Value_Callback.Connect
        (Button, Gtk.Toggle_Tool_Button.Signal_Toggled,
         Update_History_Tool_Button'Access, User_Data => Val);
   end Associate;

   ---------------
   -- Associate --
   ---------------

   procedure Associate
     (Hist : in out History_Record;
      Key  : History_Key;
      Item : access Gtk.Check_Menu_Item.Gtk_Check_Menu_Item_Record'Class;
      Default : Boolean := True)
   is
      Val : History_Key_Access;
   begin
      Val := Create_New_Boolean_Key_If_Necessary (Hist, Key, Default);
      Set_Active (Item, Val.Value);
      Value_Callback.Connect
        (Item, Gtk.Toggle_Button.Signal_Toggled,
         Update_History_Item'Access, User_Data => Val);
   end Associate;

   -----------------
   -- Most_Recent --
   -----------------

   function Most_Recent
     (Hist : access History_Record;
      Key  : History_Key;
      Default : String := "") return String
   is
      Past : String_List_Access;
   begin
      Create_New_Key_If_Necessary (Hist.all, Key, Strings);

      Past := Get_History (Hist.all, Key);
      if Past /= null then
         return Past (Past'First).all;
      end if;

      return Default;
   end Most_Recent;

   ---------------
   -- Save_Text --
   ---------------

   procedure Save_Text
     (Self : access Gtk.GEntry.Gtk_Entry_Record'Class;
      Hist : access History_Record;
      Key  : History_Key)
   is
   begin
      Add_To_History (Hist.all, Key, Self.Get_Text);
   end Save_Text;

end Histories;
