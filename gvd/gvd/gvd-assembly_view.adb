-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2000-2007                      --
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

with Ada.Exceptions;          use Ada.Exceptions;
with GNAT.Strings;            use GNAT.Strings;
with GNAT.Regpat;             use GNAT.Regpat;

with Gdk.Color;               use Gdk.Color;
with Gdk.Event;               use Gdk.Event;
with Gdk.Types.Keysyms;       use Gdk.Types.Keysyms;
with Glib;                    use Glib;

with Gtk.Container;           use Gtk.Container;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Handlers;            use Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Scrolled_Window;     use Gtk.Scrolled_Window;
with Gtk.Text_Buffer;         use Gtk.Text_Buffer;
with Gtk.Text_Iter;           use Gtk.Text_Iter;
with Gtk.Text_Mark;           use Gtk.Text_Mark;
with Gtk.Text_Tag;            use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;      use Gtk.Text_Tag_Table;
with Gtk.Text_View;           use Gtk.Text_View;

with Pango.Font;              use Pango.Font;

with Debugger;                use Debugger;
with GPS.Intl;                use GPS.Intl;
with GPS.Kernel.Preferences;  use GPS.Kernel.Preferences;
with GVD.Preferences;         use GVD.Preferences;
with GVD.Process;             use GVD.Process;
with GVD.Types;               use GVD.Types;
with String_Utils;            use String_Utils;
with Traces;                  use Traces;

package body GVD.Assembly_View is
   package Assembly_View_Cb is new Callback (GVD_Assembly_View_Record);
   package Assembly_View_Event_Cb is
     new Return_Callback (GVD_Assembly_View_Record, Boolean);

   procedure Destroy_Cb
     (Assembly_View : access GVD_Assembly_View_Record'Class);
   --  Free the memory occupied by the editor and the buttons layout, as well
   --  as all the associated pixmaps.

   function Key_Press_Cb
     (Assembly_View : access GVD_Assembly_View_Record'Class;
      Event         : Gdk_Event) return Boolean;
   --  Called when a key is pressed in the child (handling of meta-scrolling)

   procedure Show_Current_Line_Menu
     (Assembly_View : access GVD_Assembly_View_Record'Class);
   --  Display the current line in the editor.

   procedure Iter_From_Address
     (Assembly_View : GVD_Assembly_View;
      Address       : Address_Type;
      Iter          : out Gtk_Text_Iter;
      Found         : out Boolean);
   --  Return an iterator pointing at the beginning of the Address in the
   --  text buffer. Addresses are searched at the begginning of lines in the
   --  buffer.
   --  Found indicates whether the address was found.

   function Address_From_Line
     (Assembly_View : GVD_Assembly_View;
      Line          : Natural) return Address_Type;
   --  Return the address associated with a given line in the text widget.
   --  "" is returned if no address was found.

   procedure Is_Breakpoint_Address
     (Assembly_View : GVD_Assembly_View;
      Addr          : Address_Type;
      Result        : out Boolean;
      Num           : out Breakpoint_Identifier);
   pragma Unreferenced (Is_Breakpoint_Address);
   --  Result is set to True if a breakpoint is set at address Addr

   procedure Reset_Highlighting (Assembly_View : GVD_Assembly_View);
   --  Reset the buffer highlighting.

   procedure Highlight (Assembly_View : GVD_Assembly_View);
   --  Redo the buffer highlighting.

   procedure Highlight_Address_Range (Assembly_View : GVD_Assembly_View);
   --  Highlight the range of addresses corresponding to the current source
   --  line.

   procedure Highlight_Pc_Line (Assembly_View : GVD_Assembly_View);
   --  Highlight the Pc line.

   procedure Highlight_Breakpoint_Lines (Assembly_View : GVD_Assembly_View);
   --  Highlight lines on which a breakpoint is set.

   procedure On_Frame_Changed
     (Assembly_View : GVD_Assembly_View;
      Pc            : Address_Type;
      End_Pc        : Address_Type);
   --  Called when the assembly code for the address PC needs to be loaded.
   --  This gets the assembly source code for a range starting at PC, and
   --  going up to End_Pc.
   --  A minimal range of Assembly_Range_Size is displayed, unless End_Pc is
   --  "-1", in which case the assembly code for the whole current function is
   --  displayed (??? To be updated).

   function In_Range
     (Address : Address_Type;
      R       : Cache_Data_Access) return Boolean;
   --  Return True if Address is in the range of addresses described by R.

   function Find_In_Cache
     (Assembly_View : GVD_Assembly_View;
      Pc            : Address_Type) return Cache_Data_Access;
   --  Return the cached data that contains PC.
   --  null is returned if none is found.

   function Add_Address (Addr : String; Offset : Integer) return String;
   --  Add the value of Offset to the hexadecimal number Addr.
   --  Addr is coded in C (0x....), and so is the returned string

   procedure Meta_Scroll
     (Assembly_View : GVD_Assembly_View;
      Down          : Boolean);
   --  The user has asked to see the assembly range outside what is currently
   --  displayed in the assembly editor.

   procedure Meta_Scroll_Down
     (Assembly_View : access GVD_Assembly_View_Record'Class);
   procedure Meta_Scroll_Up
     (Assembly_View : access GVD_Assembly_View_Record'Class);
   --  The user has asked for the previous or next undisplayed assembly page

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Assembly_View : out GVD_Assembly_View;
      Process       : access Glib.Object.GObject_Record'Class) is
   begin
      Assembly_View := new GVD_Assembly_View_Record;
      Initialize (Assembly_View, Process);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Assembly_View : GVD_Assembly_View;
      Process       : access Glib.Object.GObject_Record'Class)
   is
      Scrolling_Area : Gtk_Scrolled_Window;
   begin
      Gtk.Box.Initialize_Hbox (Assembly_View, Homogeneous => False);

      Gtk_New (Assembly_View.View);
      Set_Editable (Assembly_View.View, False);
      Set_Wrap_Mode (Assembly_View.View, Wrap_None);

      Gtk_New (Scrolling_Area);
      Set_Policy
        (Scrolling_Area,
         H_Scrollbar_Policy => Gtk.Enums.Policy_Automatic,
         V_Scrollbar_Policy => Gtk.Enums.Policy_Automatic);
      Add (Container => Scrolling_Area, Widget => Assembly_View.View);

      Assembly_View_Cb.Connect
        (Assembly_View, "destroy",
         Assembly_View_Cb.To_Marshaller (Destroy_Cb'Access));
      Assembly_View_Event_Cb.Object_Connect
        (Assembly_View.View, "key_press_event",
         Assembly_View_Event_Cb.To_Marshaller (Key_Press_Cb'Access),
         Assembly_View);

      Pack_Start (Assembly_View, Scrolling_Area, Expand => True, Fill => True);
      Assembly_View.Process := Glib.Object.GObject (Process);
      Show_All (Assembly_View);
   end Initialize;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Assembly_View     : GVD_Assembly_View;
      Font              : Pango.Font.Pango_Font_Description)
   is
      Tag_Table : constant Gtk_Text_Tag_Table :=
                    Get_Tag_Table (Get_Buffer (Assembly_View.View));
      Color     : Gdk_Color;
      Success   : Boolean;
   begin
      --  Font

      Set_Font (Assembly_View, Font);

      --  Current address range highlighting

      Gtk_New (Assembly_View.Highlight_Tag);
      Set_Property
        (Assembly_View.Highlight_Tag, Foreground_Gdk_Property,
         Get_Pref (Asm_Highlight_Color));
      Add (Tag_Table, Assembly_View.Highlight_Tag);

      --  Breakpoints highlighting

      Gtk_New (Assembly_View.Breakpoint_Tag);
      Set_Property
        (Assembly_View.Breakpoint_Tag, Background_Gdk_Property,
         Get_Pref (Asm_Breakpoint_Color));
      Add (Tag_Table, Assembly_View.Breakpoint_Tag);

      --  Pc Hightlighting

      --  ??? This a temporrary solution used to materialized the program
      --  counter at the Gtk_Text_Buffer level.

      Gtk_New (Assembly_View.Pc_Tag);
      Set_Rgb (Color, 0, Guint16'Last, 0);
      Alloc_Color (Get_Colormap (Assembly_View), Color, Success => Success);
      Set_Property
        (Assembly_View.Pc_Tag, Background_Gdk_Property, Color);
      Add (Tag_Table, Assembly_View.Pc_Tag);

   end Configure;

   ----------------
   -- Destroy_Cb --
   ----------------

   procedure Destroy_Cb
     (Assembly_View : access GVD_Assembly_View_Record'Class) is
   begin
      Assembly_View.View := null;
      Unref (Assembly_View.Font);
   end Destroy_Cb;

   ---------------------
   -- Set_Source_Line --
   ---------------------

   procedure Set_Source_Line
     (Assembly_View : GVD_Assembly_View;
      Source_Line   : Natural)
   is
   begin
      Get_Line_Address
        (Visual_Debugger (Assembly_View.Process).Debugger,
         Source_Line,
         Assembly_View.Source_Line_Start,
         Assembly_View.Source_Line_End);
   end Set_Source_Line;

   ---------------------
   -- Index_From_Line --
   ---------------------

   function Index_From_Line
     (Assembly_View : GVD_Assembly_View;
      Line          : Natural)
      return Natural
   is
      Buffer : constant Gtk_Text_Buffer := Get_Buffer (Assembly_View.View);
      Iter   : Gtk_Text_Iter;
   begin
      Get_Iter_At_Line (Buffer, Iter, Gint (Line));
      return Natural (Get_Offset (Iter));
   end Index_From_Line;

   ----------------------
   -- Insert_At_Cursor --
   ----------------------

   procedure Insert_At_Cursor
     (Assembly_View : GVD_Assembly_View;
      Chars         : String := "")
   is
      Buffer : constant Gtk_Text_Buffer := Get_Buffer (Assembly_View.View);
   begin
      Insert_At_Cursor (Buffer, Chars);
   end Insert_At_Cursor;

   ----------------------
   -- Insert_At_Cursor --
   ----------------------

   procedure Insert_At_Cursor
     (Assembly_View : GVD_Assembly_View;
      Tag           : Gtk.Text_Tag.Gtk_Text_Tag;
      Chars         : String := "")
   is
      Buffer     : constant Gtk_Text_Buffer := Get_Buffer (Assembly_View.View);
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Start_Mark : Gtk_Text_Mark;
   begin
      Get_Iter_At_Mark (Buffer, Start_Iter, Get_Insert (Buffer));
      Start_Mark := Create_Mark (Buffer, Where => Start_Iter);
      Insert_At_Cursor (Buffer, Chars);
      Get_Iter_At_Mark (Buffer, Start_Iter, Start_Mark);
      Get_Iter_At_Mark (Buffer, End_Iter, Get_Insert (Buffer));
      Apply_Tag (Buffer, Tag, Start_Iter, End_Iter);
      Delete_Mark (Buffer, Start_Mark);
   end Insert_At_Cursor;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (Assembly_View : GVD_Assembly_View;
      Text          : String)
   is
      Buffer : constant Gtk_Text_Buffer := Get_Buffer (Assembly_View.View);
   begin
      Begin_User_Action (Buffer);
      Set_Text (Buffer, Text);
      End_User_Action (Buffer);
   end Set_Text;

   ---------------------------
   -- Child_Contextual_Menu --
   ---------------------------

   function Child_Contextual_Menu
     (Assembly_View : GVD_Assembly_View;
      Line   : Natural;
      Entity : String) return Gtk.Menu.Gtk_Menu
   is
      pragma Unreferenced (Line, Entity);

      Menu  : Gtk_Menu;
      Menu_Item : Gtk_Menu_Item;
   begin
      Gtk_New (Menu);

      Gtk_New (Menu_Item, Label => -"Show Current Location");
      Append (Menu, Menu_Item);
      Assembly_View_Cb.Object_Connect
        (Menu_Item, "activate",
         Assembly_View_Cb.To_Marshaller (Show_Current_Line_Menu'Access),
         Assembly_View);

      Gtk_New (Menu_Item, Label => -"Show Previous Page");
      Append (Menu, Menu_Item);
      Assembly_View_Cb.Object_Connect
        (Menu_Item, "activate",
         Assembly_View_Cb.To_Marshaller (Meta_Scroll_Up'Access),
         Assembly_View);

      Gtk_New (Menu_Item, Label => -"Show Next Page");
      Append (Menu, Menu_Item);
      Assembly_View_Cb.Object_Connect
        (Menu_Item, "activate",
         Assembly_View_Cb.To_Marshaller (Meta_Scroll_Down'Access),
         Assembly_View);

      Show_All (Menu);
      return Menu;
   end Child_Contextual_Menu;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font
     (Assembly_View : GVD_Assembly_View;
      Font          : Pango_Font_Description) is
   begin
      Modify_Font (Assembly_View.View, Font);
   end Set_Font;

   -----------------
   -- Set_Address --
   -----------------

   procedure Set_Address
     (Assembly_View : GVD_Assembly_View;
      Pc            : GVD.Types.Address_Type) is
   begin
      Assembly_View.Pc := Pc;
   end Set_Address;

   ------------------------
   -- Reset_Highlighting --
   ------------------------

   procedure Reset_Highlighting (Assembly_View : GVD_Assembly_View) is
      Buffer     : constant Gtk_Text_Buffer := Get_Buffer (Assembly_View.View);
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      Get_Bounds (Buffer, Start_Iter, End_Iter);
      Remove_Tag (Buffer, Assembly_View.Highlight_Tag, Start_Iter, End_Iter);
      Remove_Tag (Buffer, Assembly_View.Pc_Tag, Start_Iter, End_Iter);
      Remove_Tag (Buffer, Assembly_View.Breakpoint_Tag, Start_Iter, End_Iter);
   end Reset_Highlighting;

   -----------------------------
   -- Highlight_Address_Range --
   -----------------------------

   procedure Highlight_Address_Range (Assembly_View : GVD_Assembly_View) is
      Buffer      : constant Gtk_Text_Buffer :=
                      Get_Buffer (Assembly_View.View);
      Start_Iter  : Gtk_Text_Iter;
      End_Iter    : Gtk_Text_Iter;
      Found       : Boolean := False;

   begin
      if Assembly_View.Source_Line_Start /= Invalid_Address
        and then Assembly_View.Source_Line_End /= Invalid_Address
      then
         Iter_From_Address
           (Assembly_View, Assembly_View.Source_Line_Start, Start_Iter,
            Found);

         if Found then
            Iter_From_Address
              (Assembly_View, Assembly_View.Source_Line_End, End_Iter, Found);
         end if;

         --  Highlight the new range

         if Found then
            Begin_User_Action (Buffer);
            Apply_Tag
              (Buffer, Assembly_View.Highlight_Tag, Start_Iter, End_Iter);
            End_User_Action (Buffer);
         end if;
      end if;
   end Highlight_Address_Range;

   -----------------------
   -- Highlight_Pc_Line --
   -----------------------

   procedure Highlight_Pc_Line (Assembly_View : GVD_Assembly_View) is
      Buffer        : constant Gtk_Text_Buffer :=
                        Get_Buffer (Assembly_View.View);
      Start_Iter,
      End_Iter      : Gtk_Text_Iter;
      Dummy_Boolean : Boolean;
   begin
      Iter_From_Address
        (Assembly_View, Assembly_View.Pc, Start_Iter, Dummy_Boolean);
      Copy (Start_Iter, Dest => End_Iter);
      Forward_To_Line_End (End_Iter, Dummy_Boolean);
      Apply_Tag (Buffer, Assembly_View.Pc_Tag, Start_Iter, End_Iter);
   end Highlight_Pc_Line;

   --------------------------------
   -- Highlight_Breakpoint_Lines --
   --------------------------------

   procedure Highlight_Breakpoint_Lines
     (Assembly_View : GVD_Assembly_View)
   is
      Buffer    : constant Gtk_Text_Buffer :=
                    Get_Buffer (Assembly_View.View);

      Start_Iter,
      End_Iter  : Gtk_Text_Iter;
      Found     : Boolean;
      Dummy     : Boolean;
   begin
      if Assembly_View.Breakpoints = null then
         return;
      end if;

      --  Add the new ones
      for B in Assembly_View.Breakpoints'Range loop
         if Assembly_View.Breakpoints (B).Address /= Invalid_Address then
            Iter_From_Address
              (Assembly_View,
               Assembly_View.Breakpoints (B).Address,
               Start_Iter,
               Found);

            if Found then
               Copy (Start_Iter, Dest => End_Iter);
               Forward_To_Line_End (End_Iter, Dummy);
               Apply_Tag
                 (Buffer, Assembly_View.Breakpoint_Tag, Start_Iter, End_Iter);
            end if;
         end if;
      end loop;
   end Highlight_Breakpoint_Lines;

   ---------------
   -- Highlight --
   ---------------

   procedure Highlight (Assembly_View : GVD_Assembly_View) is
   begin
      if Assembly_View.View /= null then
         Reset_Highlighting (Assembly_View);
         Highlight_Address_Range (Assembly_View);
         Highlight_Breakpoint_Lines (Assembly_View);
         Highlight_Pc_Line (Assembly_View);
      end if;
   end Highlight;

   -----------------------
   -- Iter_From_Address --
   -----------------------

   procedure Iter_From_Address
     (Assembly_View : GVD_Assembly_View;
      Address       : Address_Type;
      Iter          : out Gtk_Text_Iter;
      Found         : out Boolean)
   is
      Buffer     : constant Gtk_Text_Buffer := Get_Buffer (Assembly_View.View);
      End_Iter   : Gtk_Text_Iter;
      Result     : Boolean := True;
   begin
      Get_Start_Iter (Buffer, Iter);

      while Result loop
         Copy (Iter, End_Iter);
         Forward_To_Line_End (End_Iter, Result);

         declare
            Line  : constant String := Get_Text (Buffer, Iter, End_Iter);
            Index : Natural := Line'First;
         begin
            Skip_Hexa_Digit (Line, Index);

            if
              String_To_Address (Line (Line'First .. Index - 1)) = Address
            then
               Found := True;
               return;
            end if;
         end;

         Forward_Line (Iter, Result);
      end loop;

      Found := False;
   end Iter_From_Address;

   -----------------------
   -- Address_From_Line --
   -----------------------

   function Address_From_Line
     (Assembly_View : GVD_Assembly_View;
      Line          : Natural) return Address_Type
   is
      Buffer     : constant Gtk_Text_Buffer := Get_Buffer (Assembly_View.View);
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Result     : Boolean;
      Matched    : Match_Array (0 .. 1);

   begin
      Get_Iter_At_Line (Buffer, Start_Iter, Gint (Line));
      Copy (Start_Iter, End_Iter);
      Forward_To_Line_End (End_Iter, Result);

      declare
         Line_Text : constant String :=
                       Get_Text (Buffer, Start_Iter, End_Iter);
      begin
         --  ??? Regexp should depend on the debugger.
         --  It does not include any 0 right after "0x"
         Match ("^0x0*([0-9a-f]+)", Line_Text, Matched);

         if Matched (1) /= No_Match then
            return String_To_Address
              ("0x" & Line_Text (Matched (1).First .. Matched (1).Last));
         end if;
      end;

      return Invalid_Address;
   end Address_From_Line;

   ---------------------------
   -- Is_Breakpoint_Address --
   ---------------------------

   procedure Is_Breakpoint_Address
     (Assembly_View : GVD_Assembly_View;
      Addr          : Address_Type;
      Result        : out Boolean;
      Num           : out Breakpoint_Identifier)
   is
      Breakpoints_Array : constant GVD.Types.Breakpoint_Array_Ptr :=
                            Visual_Debugger
                              (Assembly_View.Process).Breakpoints;
   begin
      for Index in Breakpoints_Array'Range loop
         if Breakpoints_Array (Index).Address = Addr then
            Num := Breakpoints_Array (Index).Num;
            Result := True;
            return;
         end if;
      end loop;

      Result := False;
   end Is_Breakpoint_Address;

   ------------------------
   -- Update_Breakpoints --
   ------------------------

   procedure Update_Breakpoints
     (Assembly_View : GVD_Assembly_View;
      Br            : GVD.Types.Breakpoint_Array)
   is
   begin
      Assembly_View.Breakpoints := new Breakpoint_Array'(Br);
      Highlight (Assembly_View);
   end Update_Breakpoints;

   --------------
   -- In_Range --
   --------------

   function In_Range
     (Address : Address_Type;
      R       : Cache_Data_Access) return Boolean is
   begin
      return R /= null
        and then R.Low /= Invalid_Address
        and then R.High /= Invalid_Address
        and then Address >= R.Low
        and then Address <= R.High;
   end In_Range;

   -------------------
   -- Find_In_Cache --
   -------------------

   function Find_In_Cache
     (Assembly_View : GVD_Assembly_View;
      Pc            : Address_Type) return Cache_Data_Access
   is
      Tmp : Cache_Data_Access := Assembly_View.Cache;
   begin
      while Tmp /= null loop
         if In_Range (Pc, Tmp) then
            return Tmp;
         end if;

         Tmp := Tmp.Next;
      end loop;

      return null;
   end Find_In_Cache;

   ----------------------------
   -- Show_Current_Line_Menu --
   ----------------------------

   procedure Show_Current_Line_Menu
     (Assembly_View : access GVD_Assembly_View_Record'Class)
   is
      pragma Unreferenced (Assembly_View);
   begin
      null;
   end Show_Current_Line_Menu;

   -------------------------
   -- Preferences_Changed --
   -------------------------

   procedure Preferences_Changed (Assembly_View : GVD_Assembly_View) is
   begin
      Set_Property
        (Assembly_View.Highlight_Tag,
         Foreground_Gdk_Property,
         Get_Pref (Asm_Highlight_Color));
      Set_Font (Assembly_View, Get_Pref_Font (Default_Style));

      Update_Display (Assembly_View);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
   end Preferences_Changed;

   -----------------
   -- Meta_Scroll --
   -----------------

   procedure Meta_Scroll
     (Assembly_View : GVD_Assembly_View;
      Down          : Boolean) is
   begin
      if Assembly_View.Current_Range /= null
        and then Get_Pref (Assembly_Range_Size) /= 0
      then
         Set_Busy
           (Visual_Debugger (Assembly_View.Process), True,
            Force_Refresh => True);

         if Down then
            declare
               Addr : constant Address_Type :=
                        Address_From_Line
                          (Assembly_View, Assembly_View.Current_Line);
               Iter  : Gtk_Text_Iter;
               Found : Boolean;
            begin
               On_Frame_Changed
                 (Assembly_View, Invalid_Address, Invalid_Address);

               Iter_From_Address (Assembly_View, Addr, Iter, Found);
            end;
         elsif Assembly_View.Current_Range.High /= Invalid_Address then
            On_Frame_Changed
              (Assembly_View,
               Assembly_View.Current_Range.High,
               String_To_Address
                 (Add_Address
                    (Address_To_String (Assembly_View.Current_Range.High),
                     Integer (Get_Pref (Assembly_Range_Size)))));
         end if;

         Highlight (Assembly_View);

         Set_Busy (Visual_Debugger (Assembly_View.Process), False);
      end if;
   end Meta_Scroll;

   -----------------
   -- Add_Address --
   -----------------

   function Add_Address
     (Addr   : String;
      Offset : Integer) return String
   is
      Convert : constant String := "0123456789abcdef";
      Buffer  : String (1 .. 32);
      Pos     : Natural := Buffer'Last;

   begin
      if Addr'Length < 2
        or else Addr (Addr'First .. Addr'First + 1) /= "0x"
      then
         return "0x0";
      end if;

      declare
         Str     : constant String :=
           "16#" & Addr (Addr'First + 2 .. Addr'Last) & '#';
         Value   : Long_Long_Integer := Long_Long_Integer'Value (Str) +
           Long_Long_Integer (Offset);
      begin
         while Value > 0 loop
            Buffer (Pos) := Convert (Integer (Value mod 16) + Convert'First);
            Pos := Pos - 1;
            Value := Value / 16;
         end loop;

         return "0x" & Buffer (Pos + 1 .. Buffer'Last);
      end;
   end Add_Address;

   ----------------------
   -- Meta_Scroll_Down --
   ----------------------

   procedure Meta_Scroll_Down
     (Assembly_View : access GVD_Assembly_View_Record'Class) is
   begin
      Meta_Scroll (GVD_Assembly_View (Assembly_View), Down => True);
   end Meta_Scroll_Down;

   --------------------
   -- Meta_Scroll_Up --
   --------------------

   procedure Meta_Scroll_Up
     (Assembly_View : access GVD_Assembly_View_Record'Class) is
   begin
      Meta_Scroll (GVD_Assembly_View (Assembly_View), Down => False);
   end Meta_Scroll_Up;

   ------------------
   -- Key_Press_Cb --
   ------------------

   function Key_Press_Cb
     (Assembly_View : access GVD_Assembly_View_Record'Class;
      Event         : Gdk_Event) return Boolean
   is
   begin
      case Get_Key_Val (Event) is
         when GDK_Page_Down =>
            Meta_Scroll_Down (Assembly_View);
            return True;

         when GDK_Page_Up =>
            Meta_Scroll_Up (Assembly_View);
            return True;

         when others => null;
      end case;

      return False;

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         return False;
   end Key_Press_Cb;

   ----------------------
   -- On_Frame_Changed --
   ----------------------

   procedure On_Frame_Changed
     (Assembly_View : GVD_Assembly_View;
      Pc            : Address_Type;
      End_Pc        : Address_Type)
   is
      Process               : constant Visual_Debugger :=
                                Visual_Debugger (Assembly_View.Process);
      S                     : String_Access;
      S2                    : String_Access;
      S3                    : String_Access;
      Start                 : Address_Type;
      Last                  : Address_Type;
      Low_Range, High_Range : Address_Type;
      Pc_In_Range           : constant Boolean :=
                                In_Range (Pc, Assembly_View.Current_Range);
      Pc_End_In_Range       : constant Boolean :=
                                In_Range (End_Pc, Assembly_View.Current_Range);
      S_First               : Natural;

   begin
      --  Is the range already visible ?

      if Pc_In_Range and then Pc_End_In_Range then
         return;
      end if;

      Set_Busy (Process, True);

      --  Should we prepend to the current buffer ?
      if not Pc_In_Range and then Pc_End_In_Range then
         Get_Machine_Code
           (Process.Debugger,
            Range_Start     => Start,
            Range_End       => Last,
            Code            => S,
            Start_Address   => Pc,
            End_Address     => Assembly_View.Current_Range.Low);

         Assembly_View.Current_Range.Low := Pc;

         S2 := Assembly_View.Current_Range.Data;
         Assembly_View.Current_Range.Data := new String'
           (Do_Tab_Expansion (S.all, 8) & ASCII.LF & S2.all);
         Free (S2);

      --  Should we append to the current buffer ?
      elsif Pc_In_Range and then not Pc_End_In_Range then
         Get_Machine_Code
           (Process.Debugger,
            Range_Start     => Start,
            Range_End       => Last,
            Code            => S,
            Start_Address   => Assembly_View.Current_Range.High,
            End_Address     => Set_Offset (End_Pc, 1));

         Assembly_View.Current_Range.High := End_Pc;

         --  Avoid duplicating the first assembly line since it was already
         --  displayed.
         S_First := S'First;
         Skip_To_Char (S.all, S_First, ASCII.LF);
         S_First := S_First + 1;

         S2 := Assembly_View.Current_Range.Data;
         Assembly_View.Current_Range.Data := new String'
           (S2.all & ASCII.LF &
            Do_Tab_Expansion (S (S_First .. S'Last), 8));
         Free (S2);

      --  Else get a whole new range (minimum size Assembly_Range_Size)
      else
         Assembly_View.Current_Range := Find_In_Cache (Assembly_View, Pc);
         if Assembly_View.Current_Range = null then
            if Get_Pref (Assembly_Range_Size) = 0
              or else End_Pc = Invalid_Address
            then
               Get_Machine_Code
                 (Process.Debugger,
                  Range_Start     => Start,
                  Range_End       => Last,
                  Code            => S);
            else
               Get_Machine_Code
                 (Process.Debugger,
                  Range_Start     => Start,
                  Range_End       => Last,
                  Code            => S,
                  Start_Address   => Pc,
                  End_Address     => Set_Offset
                    (Pc, Integer (Get_Pref (Assembly_Range_Size))));
            end if;

            if Start /= Invalid_Address then
               Low_Range := Start;
            end if;

            if Last /= Invalid_Address then
               High_Range := Last;
            end if;

            --  If both are null, this means that gdb couldn't get the assembly
            --  at all, and there's no point in trying again afterwards.
            --  We just pretend things worked....

            if Start = Invalid_Address and then Last = Invalid_Address then
               Assembly_View.Cache := new Cache_Data'
                 (Low  => Pc,
                  High => Pc,
                  Data => new String'(-"Couldn't get assembly code"),
                  Next => Assembly_View.Cache);
            else

               --  If the end address is not visible, disassemble a little
               --  bit more...

               if High_Range /= Invalid_Address
                 and then End_Pc > High_Range
               then
                  Get_Machine_Code
                    (Process.Debugger,
                     Range_Start     => Start,
                     Range_End       => Last,
                     Code            => S2,
                     Start_Address   => High_Range,
                     End_Address     => Set_Offset (End_Pc, 1));
                  S3 := new String'(S.all & S2.all);
                  Free (S);
                  Free (S2);
                  S := S3;

                  if Last /= Invalid_Address then
                     High_Range := Last;
                  end if;
               end if;

               Assembly_View.Cache := new Cache_Data'
                 (Low  => Low_Range,
                  High => High_Range,
                  Data => new String'(Do_Tab_Expansion (S.all, 8)),
                  Next => Assembly_View.Cache);
            end if;
            Free (S);
            Assembly_View.Current_Range := Assembly_View.Cache;
         end if;
      end if;

      Set_Text (Assembly_View, Assembly_View.Current_Range.Data.all);
      Set_Busy (Process, False);

   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception: " & Exception_Information (E));
         Set_Busy (Process, False);
   end On_Frame_Changed;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display (Assembly_View : GVD_Assembly_View) is
      Buffer       : constant Gtk_Text_Buffer :=
                       Get_Buffer (Assembly_View.View);
      Start_Iter   : Gtk_Text_Iter;
      Dummy        : Boolean;

      Address_Low  : Address_Type := Assembly_View.Source_Line_Start;
      Address_High : Address_Type := Assembly_View.Source_Line_End;
   begin
      if Assembly_View.Pc /= Invalid_Address
        and then (Assembly_View.Source_Line_End = Invalid_Address
                  or else Assembly_View.Pc > Assembly_View.Source_Line_End)
      then
         Address_High := Assembly_View.Pc;
      elsif Assembly_View.Pc /= Invalid_Address
        and then (Assembly_View.Source_Line_Start /= Invalid_Address
                  or else Assembly_View.Pc < Assembly_View.Source_Line_Start)
      then
         Address_Low := Assembly_View.Pc;
      end if;

      if not In_Range (Address_Low, Assembly_View.Current_Range)
        or else not In_Range (Address_High, Assembly_View.Current_Range)
      then
         On_Frame_Changed
           (Assembly_View,
            Address_Low,
            Address_High);
      end if;

      --  Redo the highlighting

      Highlight (Assembly_View);

      --  Make sure that the Pc line is visible

      Iter_From_Address
        (Assembly_View, Assembly_View.Pc, Start_Iter, Dummy);
      Place_Cursor (Buffer, Start_Iter);
      Scroll_Mark_Onscreen (Assembly_View.View, Get_Insert (Buffer));

   end Update_Display;

end GVD.Assembly_View;
