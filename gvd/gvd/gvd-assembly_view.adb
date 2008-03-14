-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                 Copyright (C) 2000-2008, AdaCore                  --
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

with GNAT.Strings;            use GNAT.Strings;
with GNAT.Regpat;             use GNAT.Regpat;

with Gdk.Color;               use Gdk.Color;
with Gdk.Event;               use Gdk.Event;
with Gdk.Types.Keysyms;       use Gdk.Types.Keysyms;
with Glib;                    use Glib;
with Glib.Object;             use Glib.Object;

with Gtk.Enums;               use Gtk.Enums;
with Gtk.Handlers;            use Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Object;              use Gtk.Object;
with Gtk.Scrolled_Window;     use Gtk.Scrolled_Window;
with Gtk.Text_Buffer;         use Gtk.Text_Buffer;
with Gtk.Text_Iter;           use Gtk.Text_Iter;
with Gtk.Text_Tag;            use Gtk.Text_Tag;
with Gtk.Text_Tag_Table;      use Gtk.Text_Tag_Table;
with Gtk.Text_View;           use Gtk.Text_View;
with Gtk.Widget;              use Gtk.Widget;

with Pango.Font;              use Pango.Font;

with Gtkada.MDI;              use Gtkada.MDI;

with Debugger;                use Debugger;
with GPS.Intl;                use GPS.Intl;
with GPS.Kernel;              use GPS.Kernel;
with GPS.Kernel.MDI;          use GPS.Kernel.MDI;
with GPS.Kernel.Hooks;        use GPS.Kernel.Hooks;
with GPS.Kernel.Modules;      use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;  use GPS.Kernel.Preferences;
with GVD.Code_Editors;        use GVD.Code_Editors;
with GVD.Preferences;         use GVD.Preferences;
with GVD.Process;             use GVD.Process;
with GVD.Types;               use GVD.Types;
with GVD.Views;               use GVD.Views;
with GVD_Module;              use GVD_Module;
with String_Utils;            use String_Utils;
with Traces;                  use Traces;

package body GVD.Assembly_View is

   procedure Initialize
     (Widget : access Assembly_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class);
   --  Internal initialization function

   function Get_View
     (Process : access Visual_Debugger_Record'Class)
      return Gtk_Scrolled_Window;
   procedure Set_View
     (Process : access Visual_Debugger_Record'Class;
      View    : Gtk_Scrolled_Window);
   --  Store or retrieve the view from the process

   package Simple_Views is new Scrolled_Views.Simple_Views
     (Module_Name        => "Assembly_View",
      View_Name          => -"Assembly View",
      Formal_View_Record => Assembly_View_Record,
      Get_View           => Get_View,
      Set_View           => Set_View,
      Group              => Group_Debugger_Stack,
      Position           => Position_Right,
      Initialize         => Initialize);

   package Assembly_View_Cb is new Callback (Assembly_View_Record);
   package Assembly_View_Event_Cb is
     new Return_Callback (Assembly_View_Record, Boolean);

   procedure On_Assembly
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Data->Assembly

   function Key_Press_Cb
     (View  : access Assembly_View_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when a key is pressed in the child (handling of meta-scrolling)

   procedure Show_Current_Line_Menu
     (View : access Assembly_View_Record'Class);
   --  Display the current line in the editor.

   procedure Iter_From_Address
     (View     : Assembly_View;
      Address  : Address_Type;
      Iter     : out Gtk_Text_Iter;
      Found    : out Boolean);
   --  Return an iterator pointing at the beginning of the Address in the
   --  text buffer. Addresses are searched at the begginning of lines in the
   --  buffer.
   --  Found indicates whether the address was found.

   function Address_From_Line
     (View : Assembly_View;
      Line : Natural) return Address_Type;
   --  Return the address associated with a given line in the text widget.
   --  "" is returned if no address was found.

   procedure Is_Breakpoint_Address
     (View   : Assembly_View;
      Addr   : Address_Type;
      Result : out Boolean;
      Num    : out Breakpoint_Identifier);
   pragma Unreferenced (Is_Breakpoint_Address);
   --  Result is set to True if a breakpoint is set at address Addr

   procedure Reset_Highlighting (View : Assembly_View);
   --  Reset the buffer highlighting

   procedure Highlight (View : Assembly_View);
   --  Redo the buffer highlighting

   procedure Highlight_Address_Range (View : Assembly_View);
   --  Highlight the range of addresses corresponding to the current source
   --  line.

   procedure Highlight_Pc_Line (View : Assembly_View);
   --  Highlight the Pc line

   procedure Highlight_Breakpoint_Lines (View : Assembly_View);
   --  Highlight lines on which a breakpoint is set

   procedure On_Frame_Changed
     (View          : Assembly_View;
      Start_Address : Address_Type;
      End_Address   : Address_Type);
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
     (View    : Assembly_View;
      Address : Address_Type) return Cache_Data_Access;
   --  Return the cached data that contains Address.
   --  null is returned if none is found.

   function Add_Address (Addr : String; Offset : Integer) return String;
   --  Add the value of Offset to the hexadecimal number Addr.
   --  Addr is coded in C (0x....), and so is the returned string

   procedure Meta_Scroll
     (View : Assembly_View;
      Down : Boolean);
   --  The user has asked to see the assembly range outside what is currently
   --  displayed in the assembly editor.

   procedure Meta_Scroll_Down
     (View : access Assembly_View_Record'Class);
   procedure Meta_Scroll_Up
     (View : access Assembly_View_Record'Class);
   --  The user has asked for the previous or next undisplayed assembly page

   type Preferences_Hook_Record is new Function_No_Args with record
      View : Assembly_View;
   end record;
   type Preferences_Hook is access all Preferences_Hook_Record'Class;
   procedure Execute
     (Hook   : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class);
   --  Called when the preferences have changed, to refresh the editor
   --  appropriately.

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (View : Assembly_View;
      Font : Pango.Font.Pango_Font_Description)
   is
      Tag_Table : constant Gtk_Text_Tag_Table :=
                    Get_Tag_Table (Get_Buffer (View.View));
      Color     : Gdk_Color;
      Success   : Boolean;
   begin
      --  Font

      Set_Font (View, Font);

      --  Current address range highlighting

      Gtk_New (View.Highlight_Tag);
      Set_Property
        (View.Highlight_Tag, Foreground_Gdk_Property,
         Get_Pref (Asm_Highlight_Color));
      Add (Tag_Table, View.Highlight_Tag);

      --  Breakpoints highlighting

      Gtk_New (View.Breakpoint_Tag);
      Set_Property
        (View.Breakpoint_Tag, Background_Gdk_Property,
         Get_Pref (Asm_Breakpoint_Color));
      Add (Tag_Table, View.Breakpoint_Tag);

      --  Pc Hightlighting

      --  ??? This a temporary solution used to materialized the program
      --  counter at the Gtk_Text_Buffer level.

      Gtk_New (View.Pc_Tag);
      Set_Rgb (Color, 0, Guint16'Last, 0);
      Alloc_Color (Get_Colormap (View), Color, Success => Success);
      Set_Property
        (View.Pc_Tag, Background_Gdk_Property, Color);
      Add (Tag_Table, View.Pc_Tag);

   end Configure;

   ---------------------
   -- Set_Source_Line --
   ---------------------

   procedure Set_Source_Line
     (View        : Assembly_View;
      Source_Line : Natural) is
   begin
      if View = null then
         return;
      end if;

      Get_Line_Address
        (Get_Process (View).Debugger,
         Source_Line,
         View.Source_Line_Start,
         View.Source_Line_End);
   end Set_Source_Line;

   ---------------------
   -- Index_From_Line --
   ---------------------

   function Index_From_Line
     (View : Assembly_View;
      Line : Natural)
      return Natural
   is
      Buffer : Gtk_Text_Buffer;
      Iter   : Gtk_Text_Iter;
   begin
      if View = null then
         return Natural'Last;
      end if;

      Buffer := Get_Buffer (View.View);
      Get_Iter_At_Line (Buffer, Iter, Gint (Line));
      return Natural (Get_Offset (Iter));
   end Index_From_Line;

   --------------
   -- Set_Text --
   --------------

   procedure Set_Text
     (View : Assembly_View;
      Text : String)
   is
      Buffer : Gtk_Text_Buffer;
   begin
      if View = null then
         return;
      end if;

      Buffer := Get_Buffer (View.View);
      Begin_User_Action (Buffer);
      Set_Text (Buffer, Text);
      End_User_Action (Buffer);
   end Set_Text;

   ---------------------------
   -- Child_Contextual_Menu --
   ---------------------------

   function Child_Contextual_Menu
     (View   : Assembly_View;
      Line   : Natural;
      Entity : String) return Gtk.Menu.Gtk_Menu
   is
      pragma Unreferenced (Line, Entity);

      Menu      : Gtk_Menu;
      Menu_Item : Gtk_Menu_Item;
   begin
      Gtk_New (Menu);

      Gtk_New (Menu_Item, Label => -"Show Current Location");
      Append (Menu, Menu_Item);
      Assembly_View_Cb.Object_Connect
        (Menu_Item, Signal_Activate,
         Assembly_View_Cb.To_Marshaller (Show_Current_Line_Menu'Access),
         View);

      Gtk_New (Menu_Item, Label => -"Show Previous Page");
      Append (Menu, Menu_Item);
      Assembly_View_Cb.Object_Connect
        (Menu_Item, Signal_Activate,
         Assembly_View_Cb.To_Marshaller (Meta_Scroll_Up'Access), View);

      Gtk_New (Menu_Item, Label => -"Show Next Page");
      Append (Menu, Menu_Item);
      Assembly_View_Cb.Object_Connect
        (Menu_Item, Signal_Activate,
         Assembly_View_Cb.To_Marshaller (Meta_Scroll_Down'Access), View);

      Show_All (Menu);
      return Menu;
   end Child_Contextual_Menu;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font
     (View : Assembly_View;
      Font : Pango_Font_Description) is
   begin
      if View = null then
         return;
      end if;

      Modify_Font (View.View, Font);
   end Set_Font;

   ------------------------
   -- Reset_Highlighting --
   ------------------------

   procedure Reset_Highlighting (View : Assembly_View) is
      Buffer     : Gtk_Text_Buffer;
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      if View = null then
         return;
      end if;

      Buffer := Get_Buffer (View.View);
      Get_Bounds (Buffer, Start_Iter, End_Iter);
      Remove_Tag (Buffer, View.Highlight_Tag, Start_Iter, End_Iter);
      Remove_Tag (Buffer, View.Pc_Tag, Start_Iter, End_Iter);
      Remove_Tag (Buffer, View.Breakpoint_Tag, Start_Iter, End_Iter);
   end Reset_Highlighting;

   -----------------------------
   -- Highlight_Address_Range --
   -----------------------------

   procedure Highlight_Address_Range (View : Assembly_View) is
      Buffer     : Gtk_Text_Buffer;
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Found      : Boolean := False;

   begin
      if View = null then
         return;
      end if;

      Buffer := Get_Buffer (View.View);

      if View.Source_Line_Start /= Invalid_Address
        and then View.Source_Line_End /= Invalid_Address
      then
         Iter_From_Address
           (View, View.Source_Line_Start, Start_Iter,
            Found);

         if Found then
            Iter_From_Address
              (View, View.Source_Line_End, End_Iter, Found);
         end if;

         --  Highlight the new range

         if Found then
            Begin_User_Action (Buffer);
            Apply_Tag
              (Buffer, View.Highlight_Tag, Start_Iter, End_Iter);
            End_User_Action (Buffer);
         end if;
      end if;
   end Highlight_Address_Range;

   -----------------------
   -- Highlight_Pc_Line --
   -----------------------

   procedure Highlight_Pc_Line (View : Assembly_View) is
      Buffer        : Gtk_Text_Buffer;
      Start_Iter,
      End_Iter      : Gtk_Text_Iter;
      Dummy_Boolean : Boolean;
      Process       : Visual_Debugger;
   begin
      if View = null then
         return;
      end if;

      Process := Get_Process (View);
      Buffer := Get_Buffer (View.View);
      Iter_From_Address
        (View, Process.Pc, Start_Iter, Dummy_Boolean);
      Copy (Start_Iter, Dest => End_Iter);
      Forward_To_Line_End (End_Iter, Dummy_Boolean);
      Apply_Tag (Buffer, View.Pc_Tag, Start_Iter, End_Iter);
   end Highlight_Pc_Line;

   --------------------------------
   -- Highlight_Breakpoint_Lines --
   --------------------------------

   procedure Highlight_Breakpoint_Lines (View : Assembly_View) is
      Process   : Visual_Debugger;
      Buffer    : Gtk_Text_Buffer;
      Start_Iter,
      End_Iter  : Gtk_Text_Iter;
      Found     : Boolean;
      Dummy     : Boolean;
   begin
      if View = null then
         return;
      end if;

      Process := Get_Process (View);

      if Process.Breakpoints = null then
         return;
      end if;

      Buffer := Get_Buffer (View.View);

      --  Add the new ones

      for B in Process.Breakpoints'Range loop
         if Process.Breakpoints (B).Address /= Invalid_Address then
            Iter_From_Address
              (View,
               Process.Breakpoints (B).Address,
               Start_Iter,
               Found);

            if Found then
               Copy (Start_Iter, Dest => End_Iter);
               Forward_To_Line_End (End_Iter, Dummy);
               Apply_Tag
                 (Buffer, View.Breakpoint_Tag, Start_Iter, End_Iter);
            end if;
         end if;
      end loop;
   end Highlight_Breakpoint_Lines;

   ---------------
   -- Highlight --
   ---------------

   procedure Highlight (View : Assembly_View) is
   begin
      if View = null then
         return;
      end if;

      Reset_Highlighting (View);
      Highlight_Address_Range (View);
      Highlight_Breakpoint_Lines (View);
      Highlight_Pc_Line (View);
   end Highlight;

   -----------------------
   -- Iter_From_Address --
   -----------------------

   procedure Iter_From_Address
     (View    : Assembly_View;
      Address : Address_Type;
      Iter    : out Gtk_Text_Iter;
      Found   : out Boolean)
   is
      Buffer   : Gtk_Text_Buffer;
      End_Iter : Gtk_Text_Iter;
      Result   : Boolean := True;
   begin
      if View = null then
         null;
      end if;

      Buffer := Get_Buffer (View.View);

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
     (View : Assembly_View;
      Line : Natural) return Address_Type
   is
      Buffer     : Gtk_Text_Buffer;
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Result     : Boolean;
      Matched    : Match_Array (0 .. 1);

   begin
      if View = null then
         return Invalid_Address;
      end if;

      Buffer := Get_Buffer (View.View);

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
     (View   : Assembly_View;
      Addr   : Address_Type;
      Result : out Boolean;
      Num    : out Breakpoint_Identifier)
   is
      Breakpoints_Array : GVD.Types.Breakpoint_Array_Ptr;
   begin
      if View = null then
         Num := Breakpoint_Identifier'Last;
         Result := False;
         return;
      end if;

      Breakpoints_Array := Get_Process (View).Breakpoints;

      for Index in Breakpoints_Array'Range loop
         if Breakpoints_Array (Index).Address = Addr then
            Num := Breakpoints_Array (Index).Num;
            Result := True;
            return;
         end if;
      end loop;

      Result := False;
   end Is_Breakpoint_Address;

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
     (View    : Assembly_View;
      Address : Address_Type) return Cache_Data_Access
   is
      Tmp : Cache_Data_Access;
   begin
      if View = null then
         return null;
      end if;

      Tmp := View.Cache;

      while Tmp /= null loop
         if In_Range (Address, Tmp) then
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
     (View : access Assembly_View_Record'Class)
   is
      pragma Unreferenced (View);
   begin
      null;
   end Show_Current_Line_Menu;

   -----------------
   -- Meta_Scroll --
   -----------------

   procedure Meta_Scroll
     (View : Assembly_View;
      Down : Boolean) is
   begin
      if View = null then
         return;
      end if;

      if View.Current_Range /= null
        and then Get_Pref (Assembly_Range_Size) /= 0
      then
         Set_Busy
           (Get_Process (View), True,
            Force_Refresh => True);

         if Down then
            declare
               Addr : constant Address_Type :=
                        Address_From_Line
                          (View, View.Current_Line);
               Iter  : Gtk_Text_Iter;
               Found : Boolean;
            begin
               On_Frame_Changed
                 (View, Invalid_Address, Invalid_Address);

               Iter_From_Address (View, Addr, Iter, Found);
            end;
         elsif View.Current_Range.High /= Invalid_Address then
            On_Frame_Changed
              (View,
               View.Current_Range.High,
               String_To_Address
                 (Add_Address
                    (Address_To_String (View.Current_Range.High),
                     Integer (Get_Pref (Assembly_Range_Size)))));
         end if;

         Highlight (View);

         Set_Busy (Get_Process (View), False);
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

   procedure Meta_Scroll_Down (View : access Assembly_View_Record'Class) is
   begin
      Meta_Scroll (Assembly_View (View), Down => True);
   end Meta_Scroll_Down;

   --------------------
   -- Meta_Scroll_Up --
   --------------------

   procedure Meta_Scroll_Up (View : access Assembly_View_Record'Class) is
   begin
      Meta_Scroll (Assembly_View (View), Down => False);
   end Meta_Scroll_Up;

   ------------------
   -- Key_Press_Cb --
   ------------------

   function Key_Press_Cb
     (View  : access Assembly_View_Record'Class;
      Event : Gdk_Event) return Boolean is
   begin
      case Get_Key_Val (Event) is
         when GDK_Page_Down =>
            Meta_Scroll_Down (Assembly_View (View));
            return True;

         when GDK_Page_Up =>
            Meta_Scroll_Up (Assembly_View (View));
            return True;

         when others => null;
      end case;

      return False;

   exception
      when E : others => Trace (Exception_Handle, E);
         return False;
   end Key_Press_Cb;

   ----------------------
   -- On_Frame_Changed --
   ----------------------

   procedure On_Frame_Changed
     (View          : Assembly_View;
      Start_Address : Address_Type;
      End_Address   : Address_Type)
   is
      Process               : Visual_Debugger;
      S                     : String_Access;
      S2                    : String_Access;
      S3                    : String_Access;
      Start                 : Address_Type;
      Last                  : Address_Type;
      Low_Range, High_Range : Address_Type;
      Start_In_Range        : Boolean;
      End_In_Range          : Boolean;
      S_First               : Natural;

   begin
      if View = null then
         return;
      end if;

      Process := Get_Process (View);

      --  Is the range already visible ?

      Start_In_Range := In_Range (Start_Address, View.Current_Range);
      End_In_Range := In_Range (End_Address, View.Current_Range);

      if Start_In_Range and then End_In_Range then
         return;
      end if;

      Set_Busy (Process, True);

      --  Should we prepend to the current buffer ?
      if not Start_In_Range and then End_In_Range then
         Get_Machine_Code
           (Process.Debugger,
            Range_Start     => Start,
            Range_End       => Last,
            Code            => S,
            Start_Address   => Start_Address,
            End_Address     => View.Current_Range.Low);

         View.Current_Range.Low := Start_Address;

         S2 := View.Current_Range.Data;
         View.Current_Range.Data := new String'
           (Do_Tab_Expansion (S.all, 8) & ASCII.LF & S2.all);
         Free (S2);

      --  Should we append to the current buffer ?
      elsif Start_In_Range and then not End_In_Range then
         Get_Machine_Code
           (Process.Debugger,
            Range_Start     => Start,
            Range_End       => Last,
            Code            => S,
            Start_Address   => View.Current_Range.High,
            End_Address     => Set_Offset (End_Address, 1));

         View.Current_Range.High := End_Address;

         --  Avoid duplicating the first assembly line since it was already
         --  displayed.
         S_First := S'First;
         Skip_To_Char (S.all, S_First, ASCII.LF);
         S_First := S_First + 1;

         S2 := View.Current_Range.Data;
         View.Current_Range.Data := new String'
           (S2.all & ASCII.LF &
            Do_Tab_Expansion (S (S_First .. S'Last), 8));
         Free (S2);

      --  Else get a whole new range (minimum size Assembly_Range_Size)
      else
         View.Current_Range := Find_In_Cache (View, Start_Address);
         if View.Current_Range = null then
            if Get_Pref (Assembly_Range_Size) = 0
              or else End_Address = Invalid_Address
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
                  Start_Address   => Start_Address,
                  End_Address     => Set_Offset
                    (Start_Address, Integer (Get_Pref (Assembly_Range_Size))));
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
               View.Cache := new Cache_Data'
                 (Low  => Start_Address,
                  High => Start_Address,
                  Data => new String'(-"Couldn't get assembly code"),
                  Next => View.Cache);
            else

               --  If the end address is not visible, disassemble a little
               --  bit more...

               if High_Range /= Invalid_Address
                 and then End_Address > High_Range
               then
                  Get_Machine_Code
                    (Process.Debugger,
                     Range_Start     => Start,
                     Range_End       => Last,
                     Code            => S2,
                     Start_Address   => High_Range,
                     End_Address     => Set_Offset (End_Address, 1));
                  S3 := new String'(S.all & S2.all);
                  Free (S);
                  Free (S2);
                  S := S3;

                  if Last /= Invalid_Address then
                     High_Range := Last;
                  end if;
               end if;

               View.Cache := new Cache_Data'
                 (Low  => Low_Range,
                  High => High_Range,
                  Data => new String'(Do_Tab_Expansion (S.all, 8)),
                  Next => View.Cache);
            end if;

            Free (S);
            View.Current_Range := View.Cache;
         end if;
      end if;

      Set_Text (View, View.Current_Range.Data.all);
      Set_Busy (Process, False);

   exception
      when E : others => Trace (Exception_Handle, E);
         Set_Busy (Process, False);
   end On_Frame_Changed;

   ------------
   -- Update --
   ------------

   procedure Update (View : access Assembly_View_Record) is
      Process      : constant Visual_Debugger := Get_Process (View);
      Buffer       : constant Gtk_Text_Buffer := Get_Buffer (View.View);

      Start_Iter   : Gtk_Text_Iter;
      Dummy        : Boolean;
      Address_Low  : Address_Type;
      Address_High : Address_Type;

   begin
      Set_Source_Line (Assembly_View (View), Get_Line (Process.Editor_Text));

      Address_Low := View.Source_Line_Start;
      Address_High := View.Source_Line_End;

      if Process.Pc /= Invalid_Address
        and then (View.Source_Line_End = Invalid_Address
                  or else Process.Pc > View.Source_Line_End)
      then
         Address_High := Process.Pc;

      elsif Process.Pc /= Invalid_Address
        and then (View.Source_Line_Start /= Invalid_Address
                  or else Process.Pc < View.Source_Line_Start)
      then
         Address_Low := Process.Pc;
      end if;

      if not In_Range (Address_Low, View.Current_Range)
        or else not In_Range (Address_High, View.Current_Range)
      then
         On_Frame_Changed
           (Assembly_View (View),
            Address_Low,
            Address_High);
      end if;

      --  Redo the highlighting

      Highlight (Assembly_View (View));

      --  Make sure that the Pc line is visible

      Iter_From_Address
        (Assembly_View (View), Process.Pc, Start_Iter, Dummy);
      Place_Cursor (Buffer, Start_Iter);
      Scroll_Mark_Onscreen (View.View, Get_Insert (Buffer));
   end Update;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Process : access Visual_Debugger_Record'Class)
      return Gtk_Scrolled_Window is
   begin
      return Gtk_Scrolled_Window (Process.Assembly);
   end Get_View;

   --------------
   -- Set_View --
   --------------

   procedure Set_View
     (Process : access Visual_Debugger_Record'Class;
      View    : Gtk_Scrolled_Window) is
   begin
      Process.Assembly := Gtk_Widget (View);
   end Set_View;

   -----------------
   -- On_Assembly --
   -----------------

   procedure On_Assembly
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);
      Process : Visual_Debugger;
      List    : Debugger_List_Link := Get_Debugger_List (Kernel);
   begin
      while List /= null loop
         Process := Visual_Debugger (List.Debugger);

         if Process.Debugger /= null then
            Attach_To_Assembly_View (Process, Create_If_Necessary => True);
         end if;

         List := List.Next;
      end loop;
   exception
      when E : others => Trace (Exception_Handle, E);
   end On_Assembly;

   -----------------------------
   -- Attach_To_Assembly_View --
   -----------------------------

   procedure Attach_To_Assembly_View
     (Debugger            : access GVD.Process.Visual_Debugger_Record'Class;
      Create_If_Necessary : Boolean) renames Simple_Views.Attach_To_View;

   --------------------------
   -- Update_Assembly_View --
   --------------------------

   procedure Update_Assembly_View
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class) is
   begin
      if Debugger.Assembly = null then
         return;
      end if;

      Update (Assembly_View (Debugger.Assembly));
   end Update_Assembly_View;

   ------------------------
   -- Update_Breakpoints --
   ------------------------

   procedure Update_Breakpoints
     (Debugger : access GVD.Process.Visual_Debugger_Record'Class) is
   begin
      if Debugger.Assembly = null then
         return;
      end if;

      Highlight (Assembly_View (Debugger.Assembly));
   end Update_Breakpoints;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Debug    : constant String := '/' & (-"_Debug") & '/';
      Data_Sub : constant String := Debug & (-"D_ata") & '/';
   begin
      Simple_Views.Register_Desktop_Functions (Kernel);
      Register_Menu
        (Kernel, Data_Sub, -"A_ssembly", "", On_Assembly'Access,
         Ref_Item   => -"Protection Domains",
         Add_Before => False);
   end Register_Module;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Widget : access Assembly_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
   is
      Hook : Preferences_Hook;
   begin
      Gtk.Scrolled_Window.Initialize (Widget);
      Set_Policy (Widget, Policy_Automatic, Policy_Automatic);

      Gtk_New (Widget.View);
      Set_Editable (Widget.View, False);
      Set_Wrap_Mode (Widget.View, Wrap_None);
      Add (Widget, Widget.View);

      Assembly_View_Event_Cb.Object_Connect
        (Widget.View, Signal_Key_Press_Event,
         Assembly_View_Event_Cb.To_Marshaller (Key_Press_Cb'Access),
         Widget);

      Configure (Assembly_View (Widget), Get_Pref_Font (Default_Style));

      Hook := new Preferences_Hook_Record'
        (Function_No_Args with View => Assembly_View (Widget));
      Add_Hook
        (Kernel, Preferences_Changed_Hook, Hook,
         Name => "gvd.assembly_view.preferences_changed",
         Watch => GObject (Widget));
   end Initialize;

   -------------
   -- Execute --
   -------------

   procedure Execute
     (Hook   : Preferences_Hook_Record;
      Kernel : access Kernel_Handle_Record'Class)
   is
      pragma Unreferenced (Kernel);
   begin
      Set_Property
        (Hook.View.Highlight_Tag,
         Foreground_Gdk_Property,
         Get_Pref (Asm_Highlight_Color));
      Set_Property
        (Hook.View.Breakpoint_Tag,
         Background_Gdk_Property,
         Get_Pref (Asm_Breakpoint_Color));
      Set_Font (Hook.View, Get_Pref_Font (Default_Style));

      Update (Hook.View);

   exception
      when E : others => Trace (Exception_Handle, E);
   end Execute;

end GVD.Assembly_View;
