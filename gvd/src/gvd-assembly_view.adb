------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2017, AdaCore                     --
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

with Ada.Unchecked_Deallocation;

with GNAT.Strings;            use GNAT.Strings;
with GNAT.Regpat;             use GNAT.Regpat;

with Gdk.RGBA;                use Gdk.RGBA;
with Gdk.Event;               use Gdk.Event;
with Gdk.Types.Keysyms;       use Gdk.Types.Keysyms;
with Glib;                    use Glib;
with Glib.Object;             use Glib.Object;

with Gtk.Box;                 use Gtk.Box;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Handlers;            use Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
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
with Default_Preferences;     use Default_Preferences;
with Generic_Views;           use Generic_Views;
with GPS.Debuggers;           use GPS.Debuggers;
with GPS.Intl;                use GPS.Intl;
with GPS.Kernel;              use GPS.Kernel;
with GPS.Kernel.MDI;          use GPS.Kernel.MDI;
with GPS.Kernel.Hooks;        use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;  use GPS.Kernel.Preferences;
with GVD.Breakpoints_List;    use GVD.Breakpoints_List;
with GVD.Generic_View;        use GVD.Generic_View;
with GVD.Preferences;         use GVD.Preferences;
with GVD.Process;             use GVD.Process;
with GVD.Types;               use GVD.Types;
with GVD_Module;              use GVD_Module;
with String_Utils;            use String_Utils;

package body GVD.Assembly_View is
   type Cache_Data;
   type Cache_Data_Access is access Cache_Data;
   type Cache_Data is record
      Low, High : GVD.Types.Address_Type;
      --  The low and high ranges for this item

      Data      : GNAT.Strings.String_Access;
      --  The assembly code for that range

      Next      : Cache_Data_Access;
   end record;
   --  This implements a cache for the assembly code, for specific ranges.
   --  Some debuggers (gdb) might take a long time to output the assembly code
   --  for a specific region, so it is better to keep it once we have it.

   type Assembly_View_Record is new Process_View_Record with
      record
         View                : Gtk.Text_View.Gtk_Text_View;

         Cache               : Cache_Data_Access;
         Current_Range       : Cache_Data_Access;
         --  The range of assembly code being displayed.

         Current_Line        : Natural := 0;
         --  ??? Not sure we need to keep that.

         Highlight_Tag       : Gtk.Text_Tag.Gtk_Text_Tag;
         --  The way the assembly range corresponding to the current source
         --  line should be displayed.

         Pc_Tag              : Gtk.Text_Tag.Gtk_Text_Tag;
         --  Tag used to materialized the PC.

         Breakpoint_Tag      : Gtk.Text_Tag.Gtk_Text_Tag;
         --  Tag used to materialized breakpoints.

         Source_Line_Start   : GVD.Types.Address_Type :=
                                 GVD.Types.Invalid_Address;
         Source_Line_End     : GVD.Types.Address_Type :=
                                 GVD.Types.Invalid_Address;
      end record;
   type Assembly_View is access all Assembly_View_Record'Class;

   procedure Configure
     (View : Assembly_View;
      Font : Pango.Font.Pango_Font_Description);
   --  Set the various settings of the assembly view.
   --  Ps_Font_Name is the name of the postscript font that will be used to
   --  display the text. It should be a fixed-width font, which is nice for
   --  source code.

   function Initialize
     (Widget : access Assembly_View_Record'Class) return Gtk_Widget;
   --  Internal initialization function

   procedure Set_Source_Line
     (View        : Assembly_View;
      Source_Line : Natural);
   --  Store in the assembly view the range of address that corresponds to the
   --  current source line.

   overriding procedure Update (View : not null access Assembly_View_Record);

   procedure Set_Font
     (View : Assembly_View;
      Font : Pango.Font.Pango_Font_Description);
   --  Set the font used for the box.
   --  This is called by Configure internally.

   procedure Set_Text
     (View : Assembly_View;
      Text : String);
   --  Set the text associated with the box. The Hightlighting is reset.

   function Get_View
     (Process : not null access Base_Visual_Debugger'Class)
      return access Assembly_View_Record'Class;
   procedure Set_View
     (Process : not null access Base_Visual_Debugger'Class;
      View    : access Assembly_View_Record'Class := null);
   --  Store or retrieve the view from the process

   package Assembly_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Assembly_View",
      View_Name          => -"Assembly",
      Formal_View_Record => Assembly_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Reuse_If_Exist     => False,
      Commands_Category  => "",
      Group              => Group_Debugger_Stack,
      Position           => Position_Right,
      Areas              => Gtkada.MDI.Both,
      Initialize         => Initialize);
   package Simple_Views is new GVD.Generic_View.Simple_Views
     (Views              => Assembly_MDI_Views,
      Formal_View_Record => Assembly_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record,
      Get_View           => Get_View,
      Set_View           => Set_View);

   package Assembly_View_Event_Cb is
     new Return_Callback (Assembly_View_Record, Boolean);

   type On_Location_Changed is new Debugger_Hooks_Function with null record;
   overriding procedure Execute
     (Self     : On_Location_Changed;
      Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class);
   --  Hook for "debugger_location_changed"
   --  Highlight frame number Frame based on the current debugger output
   --  stored in Process.

   function Key_Press_Cb
     (View  : access Assembly_View_Record'Class;
      Event : Gdk_Event) return Boolean;
   --  Called when a key is pressed in the child (handling of meta-scrolling)

   procedure Iter_From_Address
     (View     : not null access Assembly_View_Record'Class;
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

   procedure Highlight (View : access Assembly_View_Record'Class);
   --  Redo the buffer highlighting

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

   type On_Breakpoints_Changed is new Debugger_Hooks_Function
      with null record;
   overriding procedure Execute
      (Self     : On_Breakpoints_Changed;
       Kernel   : not null access Kernel_Handle_Record'Class;
       Debugger : access Base_Visual_Debugger'Class);
   --  Called when the breakpoints might have changed

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      View : Assembly_View;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed, to refresh the editor
   --  appropriately.

   procedure Free (Data : in out Cache_Data_Access);

   procedure Unchecked_Free is
     new Ada.Unchecked_Deallocation (Cache_Data, Cache_Data_Access);

   Invalid_Cache_Data : constant Cache_Data_Access := new Cache_Data'
     (Low  => Invalid_Address,
      High => Invalid_Address,
      Data => new String'(-"Couldn't get assembly code"),
      Next => null);

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (View : Assembly_View;
      Font : Pango.Font.Pango_Font_Description)
   is
      Tag_Table : constant Gtk_Text_Tag_Table :=
                    Get_Tag_Table (Get_Buffer (View.View));
      Color     : Gdk_RGBA;
   begin
      --  Font

      Set_Font (View, Font);

      --  Current address range highlighting

      Gtk_New (View.Highlight_Tag);
      Set_Property
        (View.Highlight_Tag, Foreground_Rgba_Property,
         Asm_Highlight_Color.Get_Pref);
      Add (Tag_Table, View.Highlight_Tag);

      --  Breakpoints highlighting

      Gtk_New (View.Breakpoint_Tag);
      Set_Property
        (View.Breakpoint_Tag, Background_Rgba_Property,
         Asm_Breakpoint_Color.Get_Pref);
      Add (Tag_Table, View.Breakpoint_Tag);

      --  Pc Hightlighting

      --  ??? This a temporary solution used to materialized the program
      --  counter at the Gtk_Text_Buffer level.

      Gtk_New (View.Pc_Tag);
      Color := (Red => 0.0, Green => 1.0, Blue => 0.0, Alpha => 1.0);
      Set_Property
        (View.Pc_Tag, Background_Rgba_Property, Color);
      Add (Tag_Table, View.Pc_Tag);

   end Configure;

   ---------------------
   -- Set_Source_Line --
   ---------------------

   procedure Set_Source_Line
     (View        : Assembly_View;
      Source_Line : Natural) is
   begin
      if View /= null then
         Get_Line_Address
           (Visual_Debugger (Get_Process (View)).Debugger,
            Source_Line,
            View.Source_Line_Start,
            View.Source_Line_End);
      end if;
   end Set_Source_Line;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self     : On_Location_Changed;
      Kernel   : not null access GPS.Kernel.Kernel_Handle_Record'Class;
      Debugger : access Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Self, Kernel);
      Process : constant Visual_Debugger := Visual_Debugger (Debugger);
   begin
      Set_Source_Line (Get_View (Process), Process.Current_Line);
   end Execute;

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

   ---------------
   -- Highlight --
   ---------------

   procedure Highlight (View : access Assembly_View_Record'Class) is
      Process    : Visual_Debugger;
      Buffer     : Gtk_Text_Buffer;
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Found      : Boolean := False;
      Dummy      : Boolean;
   begin
      if View /= null then
         Buffer  := Get_Buffer (View.View);
         Process := Get_Process (View);

         --  Reset the current highlighting

         Get_Bounds (Buffer, Start_Iter, End_Iter);
         Remove_Tag (Buffer, View.Highlight_Tag, Start_Iter, End_Iter);
         Remove_Tag (Buffer, View.Pc_Tag, Start_Iter, End_Iter);
         Remove_Tag (Buffer, View.Breakpoint_Tag, Start_Iter, End_Iter);

         --  Highlight address range

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

         --  Highlight breakpoint lines

         for B of Get_Stored_List_Of_Breakpoints (Process).List loop
            if B.Address /= Invalid_Address then
               Iter_From_Address (View, B.Address, Start_Iter, Found);
               if Found then
                  Copy (Start_Iter, Dest => End_Iter);
                  Forward_To_Line_End (End_Iter, Dummy);
                  Apply_Tag
                    (Buffer, View.Breakpoint_Tag, Start_Iter, End_Iter);
               end if;
            end if;
         end loop;

         --  Highlight PC line

         Iter_From_Address (View, Process.Pc, Start_Iter, Dummy);
         Copy (Start_Iter, Dest => End_Iter);
         Forward_To_Line_End (End_Iter, Dummy);
         Apply_Tag (Buffer, View.Pc_Tag, Start_Iter, End_Iter);
      end if;
   end Highlight;

   -----------------------
   -- Iter_From_Address --
   -----------------------

   procedure Iter_From_Address
     (View     : not null access Assembly_View_Record'Class;
      Address : Address_Type;
      Iter    : out Gtk_Text_Iter;
      Found   : out Boolean)
   is
      Buffer   : Gtk_Text_Buffer;
      End_Iter : Gtk_Text_Iter;
      Result   : Boolean := True;
   begin
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
      Process : Visual_Debugger;
   begin
      if View = null then
         Num    := Breakpoint_Identifier'Last;
         Result := False;
         return;
      end if;

      Process := Visual_Debugger (Get_Process (View));
      for B of Get_Stored_List_Of_Breakpoints (Process).List loop
         if B.Address = Addr then
            Num    := B.Num;
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

   procedure Free (Data : in out Cache_Data_Access) is
   begin
      Free (Data.Data);
      Unchecked_Free (Data);
   end Free;

   -----------------
   -- Meta_Scroll --
   -----------------

   procedure Meta_Scroll
     (View : Assembly_View;
      Down : Boolean) is
   begin
      if View /= null
        and then View.Current_Range /= null
        and then Assembly_Range_Size.Get_Pref /= 0
      then
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
                     Assembly_Range_Size.Get_Pref)));
         end if;

         Highlight (View);
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
            Meta_Scroll_Up (Assembly_View (View));
            return True;

         when GDK_Page_Up =>
            Meta_Scroll_Down (Assembly_View (View));
            return True;

         when others => null;
      end case;

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
      Process        : Visual_Debugger;
      S              : String_Access;
      S2             : String_Access;
      S3             : String_Access;
      Start, Last    : Address_Type;
      Low, High      : Address_Type;
      Start_In_Range : Boolean := False;
      End_In_Range   : Boolean := False;
      S_First        : Natural;

      procedure Free_Current;
      procedure Free_Current is
         Prev, Tmp : Cache_Data_Access;
      begin
         if View.Current_Range /= Invalid_Cache_Data then
            Tmp := View.Cache;
            while Tmp /= null
              and then Tmp /= View.Current_Range
            loop
               Prev := Tmp;
               Tmp  := Tmp.Next;
            end loop;

            if Tmp /= null then
               if Prev /= null then
                  Prev.Next := Tmp.Next;
               else
                  View.Cache := Tmp.Next;
               end if;
               Free (Tmp);
            end if;
         end if;

         View.Current_Range := null;
      end Free_Current;

   begin
      if View = null then
         return;
      end if;

      Process := Get_Process (View);

      --  Is the range already visible ?

      if View.Current_Range /= null then
         if View.Current_Range.Low = Invalid_Address
           or else View.Current_Range.High = Invalid_Address
         then
            Free_Current;

         else
            Start_In_Range := In_Range (Start_Address, View.Current_Range);
            End_In_Range   := In_Range (End_Address, View.Current_Range);
         end if;
      end if;

      if Start_In_Range
        and then End_In_Range
      then
         return;
      end if;

      --  Should we prepend to the current buffer ?
      if End_In_Range then
         Get_Machine_Code
           (Process.Debugger,
            Range_Start     => Start,
            Range_End       => Last,
            Code            => S,
            Start_Address   => Start_Address,
            End_Address     => View.Current_Range.Low);

         if Start /= Invalid_Address
           and then Last /= Invalid_Address
           and then Last = View.Current_Range.Low
         then
            View.Current_Range.Low := Start;

            S2 := View.Current_Range.Data;
            View.Current_Range.Data := new String'
              (Do_Tab_Expansion (S.all, 8) & ASCII.LF & S2.all);
            Free (S2);

         else
            Free_Current;
         end if;

      --  Should we append to the current buffer ?
      elsif Start_In_Range then
         Get_Machine_Code
           (Process.Debugger,
            Range_Start     => Start,
            Range_End       => Last,
            Code            => S,
            Start_Address   => View.Current_Range.High,
            End_Address     => Set_Offset (End_Address, 1));

         if Start /= Invalid_Address
           and then Start = View.Current_Range.High
           and then Last /= Invalid_Address
         then
            View.Current_Range.High := Last;

            --  Avoid duplicating the first assembly line since it
            --  was already displayed.
            S_First := S'First;
            Skip_To_Char (S.all, S_First, ASCII.LF);
            S_First := S_First + 1;

            S2 := View.Current_Range.Data;
            View.Current_Range.Data := new String'
              (S2.all & ASCII.LF &
                 Do_Tab_Expansion (S (S_First .. S'Last), 8));
            Free (S2);

         else
            Free_Current;
         end if;

      else
         View.Current_Range := null;
      end if;

      --  Else get a whole new range (minimum size Assembly_Range_Size)
      if View.Current_Range = null then
         View.Current_Range := Find_In_Cache (View, Start_Address);
      end if;

      if View.Current_Range = null then
         if Assembly_Range_Size.Get_Pref = 0
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
                 (Start_Address, Assembly_Range_Size.Get_Pref));
         end if;

         --  If both are null, this means that gdb couldn't get the assembly
         --  at all, and there's no point in trying again afterwards.
         --  We just pretend things worked....

         if Start = Invalid_Address
           and then Last = Invalid_Address
         then
            View.Current_Range := Invalid_Cache_Data;
         else
            Low  := Start;
            High := Last;

            --  If the end address is not visible, disassemble a little
            --  bit more...

            if Assembly_Range_Size.Get_Pref = 0
              and then End_Address /= Invalid_Address
              and then High /= Invalid_Address
              and then End_Address > High
            then
               Get_Machine_Code
                 (Process.Debugger,
                  Range_Start     => Start,
                  Range_End       => Last,
                  Code            => S2,
                  Start_Address   => High,
                  End_Address     => Set_Offset (End_Address, 1));

               if Start /= Invalid_Address
                 and then Last /= Invalid_Address
               then
                  High := Last;

                  S_First := S2'First;
                  Skip_To_Char (S2.all, S_First, ASCII.LF);
                  S_First := S_First + 1;
                  S3 := new String'(S.all & S2 (S_First .. S2'Last));
                  Free (S);
                  Free (S2);
                  S := S3;
               end if;
            end if;

            View.Cache := new Cache_Data'
              (Low  => Low,
               High => High,
               Data => new String'(Do_Tab_Expansion (S.all, 8)),
               Next => View.Cache);

            View.Current_Range := View.Cache;
         end if;

         Free (S);
      end if;

      Set_Text (View, View.Current_Range.Data.all);
   end On_Frame_Changed;

   ------------
   -- Update --
   ------------

   overriding procedure Update (View : not null access Assembly_View_Record) is
      Process      : constant Visual_Debugger := Get_Process (View);
      Buffer       : constant Gtk_Text_Buffer := Get_Buffer (View.View);

      Start_Iter   : Gtk_Text_Iter;
      Dummy        : Boolean;
      Address_Low  : Address_Type;
      Address_High : Address_Type;

   begin
      if Process = null then
         return;
      end if;

      Set_Source_Line (Assembly_View (View), Process.Current_Line);

      Address_Low  := View.Source_Line_Start;
      Address_High := View.Source_Line_End;

      if Process.Pc /= Invalid_Address then
         if Address_Low = Invalid_Address
           and then Address_High = Invalid_Address
         then
            --  don't have adresses for the current line, use $pc
            Address_Low  := Process.Pc;
            Address_High := Process.Pc;

         elsif Address_Low /= Invalid_Address
           and then Address_High /= Invalid_Address
         then
            if Address_Low > Process.Pc
              or else Address_High < Process.Pc
            then
               --  line addresses are incorrect, use $pc
               Address_Low  := Process.Pc;
               Address_High := Process.Pc;
            end if;

         else
            --  have only one address for current line,
            --  try use $pc for opposite address
            if Address_Low = Invalid_Address then
               Address_Low := Process.Pc;

            elsif Address_High = Invalid_Address then
               Address_High := Process.Pc;
            end if;

            if Set_Offset (Address_Low, 200) < Address_High then
               --  frame is too big which can hang gdb/gps
               Address_Low  := Process.Pc;
               Address_High := Process.Pc;
            end if;
         end if;
      end if;

      if not In_Range (Address_Low, View.Current_Range)
        or else not In_Range (Address_High, View.Current_Range)
      then
         On_Frame_Changed
           (Assembly_View (View),
            Address_Low,
            Address_High);
      end if;

      if Process.Pc < View.Source_Line_Start
        or else Process.Pc > View.Source_Line_End
      then
         --  sometimes "info line" returns addresses of previous line
         --  instead current line where $pc is, so remove Highlight
         --  to don't confuse user
         View.Source_Line_Start := Invalid_Address;
         View.Source_Line_End   := Invalid_Address;
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
     (Process : not null access Base_Visual_Debugger'Class)
      return access Assembly_View_Record'Class is
   begin
      return Assembly_View (Visual_Debugger (Process).Assembly);
   end Get_View;

   --------------
   -- Set_View --
   --------------

   procedure Set_View
     (Process : not null access Base_Visual_Debugger'Class;
      View    : access Assembly_View_Record'Class := null)
   is
      V : constant Visual_Debugger := Visual_Debugger (Process);
      Old : constant Assembly_View := Get_View (Process);
   begin
      --  If we are detaching, clear the old view
      if Old /= null then
         Set_Text (Old, "");
      end if;

      V.Assembly := Abstract_View_Access (View);
   end Set_View;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self     : On_Breakpoints_Changed;
       Kernel   : not null access Kernel_Handle_Record'Class;
       Debugger : access Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Self, Kernel);
      V : access Assembly_View_Record'Class;
   begin
      if Debugger /= null then
         V := Get_View (Debugger);
         if V /= null then
            Highlight (V);
         end if;
      end if;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Simple_Views.Register_Module (Kernel);
      Simple_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open assembly view",
         Description => -"Open the Assembly view for the debugger");

      Debugger_Breakpoints_Changed_Hook.Add (new On_Breakpoints_Changed);
   end Register_Module;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Widget : access Assembly_View_Record'Class) return Gtk_Widget
   is
      Hook : access On_Pref_Changed;
      Scrolled : Gtk_Scrolled_Window;
   begin
      Initialize_Vbox (Widget, Homogeneous => False);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Widget.Pack_Start (Scrolled, Expand => True, Fill => True);

      Gtk_New (Widget.View);
      Widget.View.Get_Buffer.Insert_At_Cursor ("");
      Scrolled.Add (Widget.View);
      Set_Editable (Widget.View, False);
      Set_Wrap_Mode (Widget.View, Wrap_None);

      Assembly_View_Event_Cb.Object_Connect
        (Widget.View, Signal_Key_Press_Event,
         Assembly_View_Event_Cb.To_Marshaller (Key_Press_Cb'Access),
         Widget);

      Configure (Assembly_View (Widget), Default_Style.Get_Pref_Font);

      Hook := new On_Pref_Changed;
      Hook.View := Assembly_View (Widget);
      Preferences_Changed_Hook.Add (Hook, Watch => Widget);

      Debugger_Location_Changed_Hook.Add
        (new On_Location_Changed, Watch => Widget);

      return Gtk_Widget (Widget.View);
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Kernel);
   begin
      if Pref = null
        or else Pref = Preference (Asm_Highlight_Color)
      then
         Set_Property
           (Self.View.Highlight_Tag,
            Foreground_Rgba_Property,
            Asm_Highlight_Color.Get_Pref);
      end if;

      if Pref = null
        or else Pref = Preference (Asm_Breakpoint_Color)
      then
         Set_Property
           (Self.View.Breakpoint_Tag,
            Background_Rgba_Property,
            Asm_Breakpoint_Color.Get_Pref);
      end if;

      if Pref = null
        or else Pref = Preference (Default_Style)
      then
         Set_Font (Self.View, Default_Style.Get_Pref_Font);
      end if;

      Update (Self.View);
   end Execute;

end GVD.Assembly_View;
