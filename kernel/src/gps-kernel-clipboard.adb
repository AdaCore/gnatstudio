------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2005-2018, AdaCore                     --
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

with Ada.Unchecked_Conversion;
with Ada.Unchecked_Deallocation;
with System;                     use System;

with GNAT.OS_Lib;                use GNAT.OS_Lib;
with GNATCOLL.Scripts;           use GNATCOLL.Scripts;
with GNATCOLL.Traces;            use GNATCOLL.Traces;
with Glib.Types;                 use Glib.Types;
with Glib.Object;                use Glib.Object;
with Gtk.Clipboard;              use Gtk.Clipboard;
with Gtk.Editable;               use Gtk.Editable;
with Gtk.Text_View;              use Gtk.Text_View;
with Gtk.Text_Iter;              use Gtk.Text_Iter;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Widget;                 use Gtk.Widget;

with Default_Preferences;        use Default_Preferences;
with GPS.Intl;                   use GPS.Intl;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GUI_Utils;                  use GUI_Utils;
with XML_Utils;                  use XML_Utils;
with XML_Parsers;

package body GPS.Kernel.Clipboard is

   Me : constant Trace_Handle := Create ("GPS.KERNEL.CLIPBOARD");

   Clipboard_Size_Pref : Integer_Preference;

   Text_Cst            : aliased constant String := "text";
   Append_Cst          : aliased constant String := "append";
   Index1_Cst          : aliased constant String := "index1";
   Index2_Cst          : aliased constant String := "index2";

   package Implements_Editable is new Glib.Types.Implements
     (Gtk.Editable.Gtk_Editable, GObject_Record, GObject);
   function "+"
     (Widget : access GObject_Record'Class)
      return Gtk.Editable.Gtk_Editable
      renames Implements_Editable.To_Interface;
   --  Conversion from objects to their Gtk.Editable interface

   function Convert is new Ada.Unchecked_Conversion
     (Clipboard_Access, System.Address);
   function Convert is new Ada.Unchecked_Conversion
     (System.Address, Clipboard_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Selection_List, Selection_List_Access);
   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Clipboard_Record, Clipboard_Access);

   procedure Append_To_Clipboard (Clipboard : access Clipboard_Record);
   --  Add the contents of the Gtk.Clipboard to Clipboard

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed

   procedure Clipboard_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handles shell commands associated with the clipboard

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self, Pref);
      Size      : constant Integer := Clipboard_Size_Pref.Get_Pref;
      Clipboard : constant Clipboard_Access := Get_Clipboard (Kernel);
      List      : Selection_List_Access;
   begin
      if Size /= Clipboard.List'Length then
         List := new Selection_List (1 .. Size);
         List (1 .. Integer'Min (Size, Clipboard.List'Length)) :=
           Clipboard.List (1 .. Integer'Min (Size, Clipboard.List'Length));
         Unchecked_Free (Clipboard.List);
         Clipboard.List := List;
         Clipboard_Changed_Hook.Run (Kernel);
      end if;
   end Execute;

   -------------------
   -- On_Paste_Done --
   -------------------

   procedure On_Paste_Done
     (Clipboard : not null access Clipboard_Record;
      Buffer    : not null access Gtk_Text_Buffer_Record'Class)
   is
      Iter : Gtk_Text_Iter;
   begin
      Get_Iter_At_Mark (Buffer, Iter, Get_Insert (Buffer));
      Clipboard.Last_Position := Get_Offset (Iter);
   end On_Paste_Done;

   ----------------------
   -- Create_Clipboard --
   ----------------------

   procedure Create_Clipboard
     (Kernel : access Kernel_Handle_Record'Class)
   is
      Clipboard   : constant Clipboard_Access := new Clipboard_Record;
      Size        : Integer;
      Filename    : constant Virtual_File :=
                      Create_From_Dir
                        (Get_Home_Dir (Kernel), "clipboards.xml");
      File, Child : Node_Ptr;
      Err         : GNAT.Strings.String_Access;
   begin
      if Clipboard_Size_Pref = null then
         Clipboard_Size_Pref := Create
           (Get_Preferences (Kernel),
            Path    => -"General:Clipboard",
            Name    => "Clipboard-Size",
            Label   => "Clipboard Size",
            Default => 10,
            Doc     =>
               -("Number of entries in the clipboard that can be accessed"
                 & " via Paste Previous."),
            Minimum => 1,
            Maximum => 1_000);
      end if;

      Clipboard.Kernel := Kernel_Handle (Kernel);
      Size := Clipboard_Size_Pref.Get_Pref;
      Clipboard.List := new Selection_List (1 .. Size);
      Clipboard.Last_Paste := Clipboard.List'First;

      if Is_Regular_File (Filename) then
         Trace (Me, "Loading " & Filename.Display_Full_Name);
         XML_Parsers.Parse (Filename, File, Err);
         if File = null then
            Insert (Kernel, Err.all, Mode => Error);
         else
            Child := File.Child;
            Size  := 1;
            while Size <= Clipboard.List'Last
              and then Child /= null
            loop
               Clipboard.List (Size) := new String'(Child.Value.all);
               if Get_Attribute (Child, "last", "false") = "true" then
                  Clipboard.Last_Paste := Size;
               end if;

               Size  := Size + 1;
               Child := Child.Next;
            end loop;
            Free (File);

            Clipboard_Changed_Hook.Run (Kernel);
         end if;
      end if;

      Destroy_Clipboard (Kernel);
      Kernel.Clipboard := Convert (Clipboard);

      Preferences_Changed_Hook.Add (new On_Pref_Changed);
   end Create_Clipboard;

   -----------------------
   -- Destroy_Clipboard --
   -----------------------

   procedure Destroy_Clipboard (Kernel : access Kernel_Handle_Record'Class) is
      Filename  : constant Virtual_File :=
                    Create_From_Dir (Get_Home_Dir (Kernel), "clipboards.xml");
      File      : Node_Ptr;
      Child     : Node_Ptr;
      Clipboard : Clipboard_Access;
      Success   : Boolean;

   begin
      if Kernel.Clipboard /= System.Null_Address then
         Clipboard := Convert (Kernel.Clipboard);

         Trace (Me, "Saving " & Filename.Display_Full_Name);
         File := new Node;
         File.Tag := new String'("Clipboard");
         for L in Clipboard.List'Range loop
            if Clipboard.List (L) /= null then
               Child := new Node;
               Child.Tag := new String'("clipboard");

               if L = Clipboard.Last_Paste then
                  Set_Attribute (Child, "last", "true");
               end if;

               if Clipboard.List (L)'Length <= 100_000 then
                  Child.Value := new String'(Clipboard.List (L).all);
               else
                  Child.Value := new String'("[Big entry has been removed]");
               end if;

               Add_Child (File, Child, Append => True);
            end if;
         end loop;

         Print (File, Filename, Success);
         Free (File);

         if not Success then
            Report_Preference_File_Error (Kernel, Filename);
         end if;

         for L in Clipboard.List'Range loop
            Free (Clipboard.List (L));
         end loop;
         Unchecked_Free (Clipboard.List);
         Unchecked_Free (Clipboard);
         Kernel.Clipboard := System.Null_Address;
      end if;
   end Destroy_Clipboard;

   -------------------
   -- Get_Clipboard --
   -------------------

   function Get_Clipboard
     (Kernel : access Kernel_Handle_Record'Class) return Clipboard_Access is
   begin
      return Convert (Kernel.Clipboard);
   end Get_Clipboard;

   -------------------------
   -- Append_To_Clipboard --
   -------------------------

   procedure Append_To_Clipboard (Clipboard : access Clipboard_Record) is
      Text : constant String := Wait_For_Text (Gtk.Clipboard.Get);
   begin
      if Clipboard.List (Clipboard.List'First) = null
        or else Text /= Clipboard.List (Clipboard.List'First).all
      then
         Free (Clipboard.List (Clipboard.List'Last));
         Clipboard.List (Clipboard.List'First + 1 .. Clipboard.List'Last) :=
           Clipboard.List (Clipboard.List'First .. Clipboard.List'Last - 1);
         Clipboard.List (Clipboard.List'First) := new String'
           (Wait_For_Text (Gtk.Clipboard.Get));

         Clipboard.Last_Paste  := Clipboard.List'First;
         Clipboard.Last_Widget := null;

         Clipboard_Changed_Hook.Run (Clipboard.Kernel);
      end if;
   end Append_To_Clipboard;

   ----------------------------
   -- Remove_Clipboard_Entry --
   ----------------------------

   procedure Remove_Clipboard_Entry
     (Clipboard : access Clipboard_Record; Index : Natural) is
   begin
      if Index in Clipboard.List'Range then
         Clipboard.List (Index .. Clipboard.List'Last - 1) :=
           Clipboard.List (Index + 1 .. Clipboard.List'Last);
         Clipboard.List (Clipboard.List'Last) := null;
         Clipboard_Changed_Hook.Run (Clipboard.Kernel);
      end if;
   end Remove_Clipboard_Entry;

   -------------------
   -- Cut_Clipboard --
   -------------------

   procedure Cut_Clipboard
     (Clipboard : access Clipboard_Record;
      Widget    : access Glib.Object.GObject_Record'Class)
   is
      Buffer : Gtk_Text_Buffer;
   begin
      if Is_A (Widget.Get_Type, Gtk.Editable.Get_Type) then
         Cut_Clipboard (+Widget);
         Append_To_Clipboard (Clipboard);

      elsif Widget.all in Gtk_Text_View_Record'Class then
         Buffer := Get_Buffer (Gtk_Text_View (Widget));
         Cut_Clipboard
           (Buffer,
            Gtk.Clipboard.Get,
            Default_Editable => Get_Editable (Gtk_Text_View (Widget)));
         Append_To_Clipboard (Clipboard);

      elsif Widget.all in Gtk_Text_Buffer_Record'Class then
         Cut_Clipboard
           (Gtk_Text_Buffer (Widget),
            Gtk.Clipboard.Get,
            Default_Editable => True);
         Append_To_Clipboard (Clipboard);
      end if;
   end Cut_Clipboard;

   --------------------
   -- Copy_Clipboard --
   --------------------

   procedure Copy_Clipboard
     (Clipboard : access Clipboard_Record;
      Widget    : access Glib.Object.GObject_Record'Class)
   is
      Buffer : Gtk_Text_Buffer;
   begin
      --  The calls to Clear are required so that if the user does a paste,
      --  the GPS clipboard is used (and therefore one can access the previous
      --  entry immediately). If we don't do that, the user has to press
      --  "previous" twice.
      if Is_A (Widget.Get_Type, Gtk.Editable.Get_Type) then
         Copy_Clipboard (+Widget);
         Append_To_Clipboard (Clipboard);

      elsif Widget.all in Gtk_Text_View_Record'Class then
         Buffer := Get_Buffer (Gtk_Text_View (Widget));
         Copy_Clipboard (Buffer, Gtk.Clipboard.Get);
         Append_To_Clipboard (Clipboard);

      elsif Widget.all in Gtk_Text_Buffer_Record'Class then
         Buffer := Gtk_Text_Buffer (Widget);
         Copy_Clipboard (Buffer, Gtk.Clipboard.Get);
         Append_To_Clipboard (Clipboard);

      elsif Widget.all in Gtk_Tree_View_Record'Class then
         Set_Text (Gtk.Clipboard.Get, Get_Selection (Gtk_Tree_View (Widget)));
         Append_To_Clipboard (Clipboard);
      end if;
   end Copy_Clipboard;

   ----------------------------
   -- Copy_Text_In_Clipboard --
   ----------------------------

   procedure Copy_Text_In_Clipboard
     (Clipboard : access Clipboard_Record;
      Text      : String) is
   begin
      Set_Text (Gtk.Clipboard.Get, Text);
      Append_To_Clipboard (Clipboard);
   end Copy_Text_In_Clipboard;

   ---------------------
   -- Paste_Clipboard --
   ---------------------

   procedure Paste_Clipboard
     (Clipboard     : access Clipboard_Record;
      Widget        : access Glib.Object.GObject_Record'Class;
      Index_In_List : Natural := 0)
   is
      Buffer           : Gtk_Text_Buffer;
      Result           : Boolean;
      pragma Unreferenced (Result);
      Iter             : Gtk_Text_Iter;
      Default_Editable : Boolean;
      Pasted           : Boolean := False;
   begin
      Clipboard.Last_Is_From_System := False;

      if Index_In_List /= 0
        and then Index_In_List in Clipboard.List'Range
      then
         Clipboard.Last_Paste := Index_In_List;
         Clipboard_Changed_Hook.Run (Clipboard.Kernel);

         if Clipboard.Last_Paste not in Clipboard.List'Range
           or else Clipboard.List (Clipboard.Last_Paste) = null
         then
            Clipboard.Last_Paste := Clipboard.List'First;
            Clipboard_Changed_Hook.Run (Clipboard.Kernel);
         end if;
      end if;

      --  Should we paste the system clipboard instead of ours ?
      --  We only paste it if we are not the owner, ie if the owner is
      --  unknown (that seems more of a workaround, but works fine since gtk+
      --  always sets the owner of the selection)
      if Index_In_List = 0
        and then Wait_Is_Text_Available (Gtk.Clipboard.Get)
      then
         declare
            Str : constant String := Wait_For_Text (Gtk.Clipboard.Get);
         begin
            --  Only paste it if different from our own first entry, otherwise
            --  it is likely we copied it ourselves anyway, and in this case
            --  we want to paste our own entry, so that if the user does
            --  Paste Previous afterward we immediately paste the second entry.

            if Clipboard.List (Clipboard.Last_Paste) = null
              or else Str /= Clipboard.List (Clipboard.Last_Paste).all
            then
               Trace (Me, "Pasting system clipboard");
               Clipboard.Last_Widget := GObject (Widget);
               Clipboard.Last_Is_From_System := True;
               Pasted := True;
            end if;
         end;
      end if;

      if Pasted then
         null;

      elsif Clipboard.List (Clipboard.Last_Paste) /= null then
         Trace (Me, "Pasting GPS clipboard");
         Set_Text (Gtk.Clipboard.Get,
                   Clipboard.List (Clipboard.Last_Paste).all);
         Clipboard.Last_Widget := GObject (Widget);

      else
         Clipboard.Last_Widget := null;
      end if;

      if Clipboard.Last_Widget /= null then
         if Is_A (Widget.Get_Type, Gtk.Editable.Get_Type) then
            Clipboard.First_Position := Get_Position (+Widget);
            Paste_Clipboard (+Widget);
            Clipboard.Last_Position := Get_Position (+Widget);

         else
            if Widget.all in Gtk_Text_View_Record'Class then
               Buffer := Get_Buffer (Gtk_Text_View (Widget));
               Default_Editable := Get_Editable (Gtk_Text_View (Widget));
            elsif Widget.all in Gtk_Text_Buffer_Record'Class then
               Buffer := Gtk_Text_Buffer (Widget);
               Default_Editable := True;
            else
               Clipboard.Last_Widget := null;
               return;
            end if;

            if not Default_Editable then
               --  Cannot paste in read-only buffers
               return;
            end if;

            --  Delete the selected region if it exists.
            --  ??? This works around a bug which it seems is in gtk+,
            --  to be investigated.
            --  Scenario to reproduce the gtk bug : do a "select_region"
            --  and then a "paste_clipboard", twice. (See C703-005)

            if Selection_Exists (Buffer) then
               Result := Delete_Selection (Buffer, False, False);
            end if;

            Get_Iter_At_Mark (Buffer, Iter, Get_Insert (Buffer));
            Clipboard.First_Position := Get_Offset (Iter);

            Paste_Clipboard
               (Buffer, Gtk.Clipboard.Get,
                Default_Editable => Default_Editable);
         end if;
      end if;
   end Paste_Clipboard;

   ------------------------------
   -- Paste_Previous_Clipboard --
   ------------------------------

   procedure Paste_Previous_Clipboard
     (Clipboard : access Clipboard_Record;
      Widget    : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      Buffer      : Gtk_Text_Buffer;
      Iter, Iter2 : Gtk_Text_Iter;
   begin
      if Clipboard.Last_Widget = null then
         return;
      end if;

      --  If the position is not the same as at the end of the previous paste,
      --  do nothing.

      if Is_A (Widget.Get_Type, Gtk.Editable.Get_Type) then
         if Clipboard.Last_Position /= Get_Position (+Widget) then
            Clipboard.Last_Widget := null;
            Trace (Me, "Paste Previous not at the same position in Editable "
                   & Clipboard.Last_Position'Img
                   & Get_Position (+Widget)'Img);
            return;
         end if;

      elsif Widget.all in Gtk_Text_View_Record'Class then
         Buffer := Get_Buffer (Gtk_Text_View (Widget));
         Get_Iter_At_Mark (Buffer, Iter, Get_Insert (Buffer));
         if Clipboard.Last_Position /= Get_Offset (Iter) then
            Trace (Me, "Paste Previous not at the same position "
                   & Clipboard.Last_Position'Img
                   & Get_Offset (Iter)'Img);
            Clipboard.Last_Widget := null;
            return;
         end if;
      else
         return;
      end if;

      --  Remove the previous insert

      if Is_A (Widget.Get_Type, Gtk.Editable.Get_Type) then
         Delete_Text
           (+Widget,
            Start_Pos => Clipboard.First_Position,
            End_Pos   => Clipboard.Last_Position);
      else
         Buffer.Get_Iter_At_Offset (Iter2, Clipboard.First_Position);
         Delete (Buffer, Iter2, Iter);
      end if;

      --  Prepare the next paste.
      --  If we have just pasted the system's clipboard, do not move the
      --  current position.

      if not Clipboard.Last_Is_From_System then
         Clipboard.Last_Paste := Clipboard.Last_Paste + 1;
      end if;

      if Clipboard.Last_Paste > Clipboard.List'Last
        or else Clipboard.Last_Paste < Clipboard.List'First
        or else Clipboard.List (Clipboard.Last_Paste) = null
      then
         Clipboard.Last_Paste := Clipboard.List'First;
      end if;

      Clipboard.Last_Is_From_System := False;

      if Clipboard.List (Clipboard.Last_Paste) /= null then
         Set_Text (Gtk.Clipboard.Get,
                   Clipboard.List (Clipboard.Last_Paste).all);

         --  Paste the new contents
         if Is_A (Widget.Get_Type, Gtk.Editable.Get_Type) then
            Clipboard.First_Position := Get_Position (+Widget);
            Paste_Clipboard (+Widget);
            Clipboard.Last_Position := Get_Position (+Widget);
         else
            Get_Iter_At_Mark (Buffer, Iter, Get_Insert (Buffer));
            Clipboard.First_Position := Get_Offset (Iter);
            Paste_Clipboard
              (Buffer, Gtk.Clipboard.Get,
               Default_Editable => Get_Editable (Gtk_Text_View (Widget)));
            Get_Iter_At_Mark (Buffer, Iter, Get_Insert (Buffer));
            Clipboard.Last_Position := Get_Offset (Iter);
         end if;
      end if;

      Clipboard_Changed_Hook.Run (Clipboard.Kernel);
   end Paste_Previous_Clipboard;

   -----------------
   -- Get_Content --
   -----------------

   function Get_Content
     (Clipboard : access Clipboard_Record) return Selection_List is
   begin
      return Clipboard.List.all;
   end Get_Content;

   --------------------
   -- Get_Last_Paste --
   --------------------

   function Get_Last_Paste
     (Clipboard : access Clipboard_Record) return Integer is
   begin
      return Clipboard.Last_Paste;
   end Get_Last_Paste;

   ---------------------
   -- Merge_Clipboard --
   ---------------------

   procedure Merge_Clipboard
     (Clipboard      : access Clipboard_Record;
      Index1, Index2 : Natural)
   is
      Str : GNAT.Strings.String_Access;
   begin
      if Index1 in Clipboard.List'Range
        and then Index2 in Clipboard.List'Range
        and then Clipboard.List (Index1) /= null
        and then Clipboard.List (Index2) /= null
      then
         Str := new String'(Clipboard.List (Index2).all
                            & Clipboard.List (Index1).all);
         Free (Clipboard.List (Index1));
         Clipboard.List (Index1) := Str;

         Free (Clipboard.List (Index2));
         if Index2 /= Clipboard.List'Last then
            Clipboard.List (Index2 .. Clipboard.List'Last - 1) :=
              (Clipboard.List (Index2 + 1 .. Clipboard.List'Last));
            Clipboard.List (Clipboard.List'Last) := null;
         end if;

         Clipboard.Last_Paste := Index1;
         Set_Text (Gtk.Clipboard.Get, Clipboard.List (Index1).all);
         Clipboard_Changed_Hook.Run (Clipboard.Kernel);
      end if;
   end Merge_Clipboard;

   -----------------------
   -- Register_Commands --
   -----------------------

   procedure Register_Commands (Kernel : access Kernel_Handle_Record'Class) is
      Class : constant Class_Type := New_Class (Kernel, "Clipboard");
   begin
      Register_Command
        (Kernel, "copy", 1, 2, Class => Class, Static_Method => True,
         Handler => Clipboard_Handler'Access);
      Register_Command
        (Kernel, "merge", 2, 2, Class => Class, Static_Method => True,
         Handler => Clipboard_Handler'Access);
      Register_Command
        (Kernel, "current", 0, 0, Class => Class, Static_Method => True,
         Handler => Clipboard_Handler'Access);
      Register_Command
        (Kernel, "contents", 0, 0, Class => Class, Static_Method => True,
         Handler => Clipboard_Handler'Access);
   end Register_Commands;

   -----------------------
   -- Clipboard_Handler --
   -----------------------

   procedure Clipboard_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      List   : Selection_List_Access;
   begin
      if Command = "copy" then
         Name_Parameters (Data, (1 => Text_Cst'Access,
                                 2 => Append_Cst'Access));
         declare
            Append : constant Boolean := Nth_Arg (Data, 2, False);
         begin
            Copy_Text_In_Clipboard (Get_Clipboard (Kernel), Nth_Arg (Data, 1));
            if Append then
               Merge_Clipboard (Get_Clipboard (Kernel), 1, 2);
            end if;
         end;

      elsif Command = "merge" then
         Name_Parameters (Data, (1 => Index1_Cst'Access,
                                 2 => Index2_Cst'Access));
         Merge_Clipboard (Get_Clipboard (Kernel), Nth_Arg (Data, 1) + 1,
                          Nth_Arg (Data, 2) + 1);

      elsif Command = "current" then
         Set_Return_Value
           (Data, Get_Last_Paste (Get_Clipboard (Kernel)) - 1);

      elsif Command = "contents" then
         Set_Return_Value_As_List (Data);
         List := Get_Clipboard (Kernel).List;
         if List /= null then
            for L in List'Range loop
               if List (L) /= null then
                  Set_Return_Value (Data, List (L).all);
               end if;
            end loop;
         end if;
      end if;
   end Clipboard_Handler;

end GPS.Kernel.Clipboard;
