-----------------------------------------------------------------------
--                 Odd - The Other Display Debugger                  --
--                                                                   --
--                         Copyright (C) 2000                        --
--                 Emmanuel Briot and Arnaud Charlet                 --
--                                                                   --
-- Odd is free  software;  you can redistribute it and/or modify  it --
-- under the terms of the GNU General Public License as published by --
-- the Free Software Foundation; either version 2 of the License, or --
-- (at your option) any later version.                               --
--                                                                   --
-- This program is  distributed in the hope that it will be  useful, --
-- but  WITHOUT ANY WARRANTY;  without even the  implied warranty of --
-- MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU --
-- General Public License for more details. You should have received --
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Debugger;         use Debugger;
with Gdk.Bitmap;       use Gdk.Bitmap;
with Gdk.Color;        use Gdk.Color;
with Gdk.Pixmap;       use Gdk.Pixmap;
with Gdk.Window;       use Gdk.Window;
with Glib;             use Glib;
with Gtk.Handlers;     use Gtk.Handlers;
with Gtk.Layout;       use Gtk.Layout;
with Gtk.Menu;         use Gtk.Menu;
with Gtk.Menu_Item;    use Gtk.Menu_Item;
with Gtk.Pixmap;       use Gtk.Pixmap;
with Gtk.Text;         use Gtk.Text;
with Gtk.Widget;       use Gtk.Widget;
with Gtkada.Types;     use Gtkada.Types;
with Odd.Process;      use Odd.Process;
with Odd.Code_Editors; use Odd.Code_Editors;
with Odd.Strings;      use Odd.Strings;
with Odd.Text_Boxes;   use Odd.Text_Boxes;
with Odd.Types;        use Odd.Types;
with GNAT.Regpat;      use GNAT.Regpat;
with Odd_Intl;         use Odd_Intl;

package body Odd.Asm_Editors is

   Editor_Highlight_Color : constant String := "#FF0000";
   --  Color to use to highlight the assembly code for the current line (red)

   package Editor_Cb is new Callback (Asm_Editor_Record);

   function Line_From_Address
     (Editor  : access Asm_Editor_Record'Class;
      Address : String)
     return Natural;
   --  Return the line, in the text widget, that matches a given Address.
   --  0 is returned if the address was not found.

   function Address_From_Line
     (Editor  : access Asm_Editor_Record'Class;
      Line    : Natural)
     return String;
   --  Return the address associated with a given line in the text widget.
   --  "" is returned if no address was found.

   function Pos_From_Address
     (Editor  : access Asm_Editor_Record'Class;
      Address : String)
     return Natural;
   --  Return the offset in the text widget, that matches a given Address.
   --  0 is returned if the address was not found.

   procedure Is_Breakpoint_Address
     (Editor : access Asm_Editor_Record'Class;
      Addr   : String;
      Result : out Boolean;
      Num    : out Integer);
   --  Result is set to True if a breakpoint is set at address Addr

   procedure On_Frame_Changed
     (Editor : access Asm_Editor_Record'Class;
      Pc     : String);
   --  Called when the assembly code for the address PC needs to be loaded.
   --  This gets the assembly source code for that frame, and display it in
   --  the editor.

   function In_Range
     (Pc     : String;
      R      : Cache_Data_Access)
     return Boolean;
   --  Return True if PC is in the range of address described by R.

   function Find_In_Cache
     (Editor : access Asm_Editor_Record'Class;
      Pc     : String)
     return Cache_Data_Access;
   --  Return the cached data that contains PC.
   --  null is returned if none is found.

   procedure Show_Current_Line_Menu
     (Editor : access Asm_Editor_Record'Class);
   --  Display the current line in the editor.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor  : out Asm_Editor;
      Process : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      Editor := new Asm_Editor_Record;
      Initialize (Editor, Process);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor  : access Asm_Editor_Record'Class;
      Process : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
   begin
      Odd.Text_Boxes.Initialize (Editor);
      Editor.Process := Gtk_Widget (Process);
      Show_All (Editor);
   end Initialize;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Editor            : access Asm_Editor_Record;
      Ps_Font_Name      : String;
      Font_Size         : Glib.Gint;
      Current_Line_Icon : Gtkada.Types.Chars_Ptr_Array;
      Stop_Icon         : Gtkada.Types.Chars_Ptr_Array;
      Strings_Color     : String;
      Keyword_Color     : String)
   is
   begin
      Configure (Editor, Ps_Font_Name, Font_Size, Current_Line_Icon);

      Create_From_Xpm_D
        (Editor.Stop_Pixmap,
         Null_Window,
         Get_System,
         Editor.Stop_Mask,
         White (Get_System),
         Stop_Icon);

      Editor.Strings_Color  := Parse (Strings_Color);
      Alloc (Get_System, Editor.Strings_Color);
      Editor.Keywords_Color := Parse (Keywords_Color);
      Alloc (Get_System, Editor.Keywords_Color);

      Editor.Highlight_Color := Parse (Editor_Highlight_Color);
      Alloc (Get_System, Editor.Highlight_Color);
   end Configure;

   ----------------------
   -- On_Frame_Changed --
   ----------------------

   procedure On_Frame_Changed
     (Editor : access Asm_Editor_Record'Class;
      Pc     : String)
   is
      Process : Debugger_Process_Tab := Debugger_Process_Tab (Editor.Process);
      S       : String_Access;
      Start,
      Last    : Address_Type;
      Start_End, Last_End : Natural;
      Low_Range, High_Range : String_Access;
   begin
      Editor.Current_Range := Find_In_Cache (Editor, Pc);

      if Editor.Current_Range = null then
         Get_Machine_Code
           (Process.Debugger,
            Range_Start     => Start,
            Range_End       => Last,
            Range_Start_Len => Start_End,
            Range_End_Len   => Last_End,
            Code            => S);

         if Start_End /= 0 then
            Low_Range := new String'(Start (1 .. Start_End));
         end if;
         if Last_End /= 0 then
            High_Range := new String'(Last (1 .. Last_End));
         end if;

         Editor.Cache := new Cache_Data'
           (Low  => Low_Range,
            High => High_Range,
            Data => S,
            Next => Editor.Cache);
         Editor.Current_Range := Editor.Cache;
      end if;

      Set_Buffer (Editor, Editor.Current_Range.Data, Clear_Previous => False);
      Editor.Highlight_Start := 0;
      Update_Child (Editor);
   end On_Frame_Changed;

   -----------------------
   -- On_Pixmap_Clicked --
   -----------------------

   function On_Pixmap_Clicked
     (Editor : access Asm_Editor_Record;
      Button : Natural;
      Line   : Natural) return Boolean
   is
      Result : Boolean;
      Num    : Integer;
   begin
      if Button = 1 then
         declare
            Addr : constant String := Address_From_Line (Editor, Line);
            Process : Debugger_Process_Tab :=
              Debugger_Process_Tab (Editor.Process);
         begin
            Is_Breakpoint_Address (Editor, Addr, Result, Num);

            if Result then
               Remove_Breakpoint (Process.Debugger, Num, Display => True);
            else
               if Addr /= "" then
                  Break_Address (Process.Debugger, Addr, Display => True);
               end if;
            end if;
         end;
      end if;
      return True;
   end On_Pixmap_Clicked;

   ---------------------------
   -- Child_Contextual_Menu --
   ---------------------------

   function Child_Contextual_Menu
     (Editor : access Asm_Editor_Record;
      Line   : Natural;
      Entity : String) return Gtk.Menu.Gtk_Menu
   is
      Menu  : Gtk_Menu;
      Mitem : Gtk_Menu_Item;
   begin
      Gtk_New (Menu);

      Gtk_New (Mitem, Label => -"Show Current Location");
      Append (Menu, Mitem);
      Editor_Cb.Object_Connect
        (Mitem, "activate",
         Editor_Cb.To_Marshaller (Show_Current_Line_Menu'Access),
         Editor);

      Append_To_Contextual_Menu
        (Debugger_Process_Tab (Editor.Process).Editor_Text, Menu);

      Show_All (Menu);
      return Menu;
   end Child_Contextual_Menu;

   -----------------
   -- Set_Address --
   -----------------

   procedure Set_Address
     (Editor : access Asm_Editor_Record;
      Pc     : String)
   is
      Line : Natural;
   begin
      --  Do we need to reload some assembly code ?

      if not In_Range (Pc, Editor.Current_Range) then
         On_Frame_Changed (Editor, Pc);
      end if;

      --  Find the right line ?

      Line := Line_From_Address (Editor, Pc);
      if Line /= 0 then
         Set_Line (Editor, Line, Set_Current => True);
      end if;
   end Set_Address;

   -----------------------------
   -- Highlight_Address_Range --
   -----------------------------

   procedure Highlight_Address_Range
     (Editor   : access Asm_Editor_Record;
      Source_Line : Natural)
   is
      Range_Start     : Address_Type;
      Range_End       : Address_Type;
      Range_Start_Len : Natural;
      Range_End_Len   : Natural;
      Pos_Start,
      Pos_End         : Natural;
   begin
      if Editor.Highlight_Start /= 0 then
         Delete_Text
           (Get_Child (Editor),
            Gint (Editor.Highlight_Start) - 1,
            Gint (Editor.Highlight_End) - 1);
         Set_Position
           (Get_Child (Editor), Gint (Editor.Highlight_Start));
         Insert
           (Editor,
            Chars => Get_Buffer (Editor)
            (Editor.Highlight_Start .. Editor.Highlight_End - 1));
         Editor.Highlight_Start := 0;
      end if;

      Get_Line_Address
        (Debugger_Process_Tab (Editor.Process).Debugger,
         Source_Line, Range_Start, Range_End, Range_Start_Len, Range_End_Len);

      Pos_Start := Pos_From_Address
        (Editor, Range_Start (1 .. Range_Start_Len));
      if Pos_Start /= 0 then
         Pos_End := Pos_From_Address
           (Editor, Range_End (1 .. Range_End_Len));

         if Pos_End /= 0 then
            Editor.Highlight_Start := Pos_Start;
            Editor.Highlight_End   := Pos_End;
            Delete_Text
              (Get_Child (Editor),
               Gint (Editor.Highlight_Start) - 1,
               Gint (Editor.Highlight_End) - 1);
            Set_Position
              (Get_Child (Editor), Gint (Editor.Highlight_Start));
            Insert
              (Editor,
               Fore  => Editor.Highlight_Color,
               Chars => Get_Buffer (Editor)
               (Editor.Highlight_Start .. Editor.Highlight_End - 1));
         end if;
      end if;
   end Highlight_Address_Range;

   -----------------------
   -- Line_From_Address --
   -----------------------

   function Line_From_Address
     (Editor  : access Asm_Editor_Record'Class;
      Address : String)
     return Natural
   is
      Max_Lines : Natural;
      Buffer    : String_Access;
      Index     : Natural;
   begin
      Buffer := Get_Buffer (Editor);
      if Buffer = null then
         return 0;
      end if;

      Max_Lines := Lines_Count (Editor);
      Index     := Buffer'First;
      for Line in 1 .. Max_Lines loop

         if Index + Address'Length - 1 <= Buffer'Last
           and then Buffer (Index .. Index + Address'Length - 1) = Address
         then
            return Line;
         end if;

         Skip_To_Char (Buffer.all, Index, ASCII.LF);
         Index := Index + 1;
      end loop;
      return 0;
   end Line_From_Address;

   ----------------------
   -- Pos_From_Address --
   ----------------------

   function Pos_From_Address
     (Editor  : access Asm_Editor_Record'Class;
      Address : String)
     return Natural
   is
      Max_Lines : Natural;
      Buffer    : String_Access;
      Index     : Natural;
   begin
      Buffer := Get_Buffer (Editor);
      if Buffer = null then
         return 0;
      end if;

      Max_Lines := Lines_Count (Editor);
      Index     := Buffer'First;
      for Line in 1 .. Max_Lines loop
         if Index + Address'Length - 1 <= Buffer'Last
           and then Buffer (Index .. Index + Address'Length - 1) = Address
         then
            return Index;
         end if;

         while Index <= Buffer'Last
           and then Buffer (Index) /= ASCII.LF
         loop
            Index := Index + 1;
         end loop;
         Index := Index + 1;
      end loop;
      return 0;
   end Pos_From_Address;

   -----------------------
   -- Address_From_Line --
   -----------------------

   function Address_From_Line
     (Editor  : access Asm_Editor_Record'Class;
      Line    : Natural)
     return String
   is
      Buffer  : String_Access := Get_Buffer (Editor);
      Current : Natural := 1;
      Index   : Natural;
      Matched : Match_Array (0 .. 1);
   begin
      if Buffer = null then
         return "";
      end if;

      Index := Buffer'First;
      while Current < Line loop
         Skip_To_Char (Buffer.all, Index, ASCII.LF);
         Index := Index + 1;
         Current := Current + 1;
      end loop;

      if Current = Line then
         --  ??? Regexp should depend on the debugger.
         --  It does not include any 0 right after "0x"
         Match ("^0x0*([0-9a-f]+)", Buffer (Index .. Buffer'Last), Matched);
         if Matched (1) /= No_Match then
            return "0x" & Buffer.all (Matched (1).First .. Matched (1).Last);
         end if;
      end if;
      return "";
   end Address_From_Line;

   ---------------------------
   -- Is_Breakpoint_Address --
   ---------------------------

   procedure Is_Breakpoint_Address
     (Editor : access Asm_Editor_Record'Class;
      Addr   : String;
      Result : out Boolean;
      Num    : out Integer)
   is
      Process : Debugger_Process_Tab := Debugger_Process_Tab (Editor.Process);
      Breakpoints_Array : Odd.Types.Breakpoint_Array_Ptr :=
        Process.Breakpoints;
   begin
      for Index in Breakpoints_Array'Range loop
         if Breakpoints_Array (Index).Address.all = Addr then
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
     (Editor    : access Asm_Editor_Record;
      Br        : Odd.Types.Breakpoint_Array)
   is
      Line  : Natural;
      Pix   : Gtk_Pixmap;
   begin
      if Get_Buffer (Editor) = null then
         return;
      end if;

      Freeze (Get_Buttons (Editor));
      Hide_Current_Line_Button (Editor);

      --  Remove all existing breakpoints
      Forall (Get_Buttons (Editor), Gtk.Widget.Destroy_Cb'Access);

      --  Add the new ones
      for B in Br'Range loop
         if Br (B).Address /= null then
            Line := Line_From_Address (Editor, Br (B).Address.all);
            if Line /= 0 then
               Gtk_New (Pix, Editor.Stop_Pixmap, Editor.Stop_Mask);
               Put (Get_Buttons (Editor), Pix,
                    0, Pixels_From_Line (Editor, Line));
            end if;
         end if;
      end loop;

      Set_Line (Editor, Get_Line (Editor), Set_Current => True);
      Show_All (Get_Buttons (Editor));
      Thaw (Get_Buttons (Editor));
   end Update_Breakpoints;

   --------------
   -- In_Range --
   --------------

   function In_Range
     (Pc     : String;
      R      : Cache_Data_Access)
     return Boolean is
   begin
      return R /= null
        and then R.Low /= null
        and then R.High /= null
        and then Pc >= R.Low.all
        and then Pc <= R.High.all;
   end In_Range;

   -------------------
   -- Find_In_Cache --
   -------------------

   function Find_In_Cache
     (Editor : access Asm_Editor_Record'Class;
      Pc     : String)
     return Cache_Data_Access
   is
      Tmp : Cache_Data_Access := Editor.Cache;
   begin
      while Tmp /= null loop
         if In_Range (Pc, Tmp) then
            return Tmp;
         end if;
         Tmp := Tmp.Next;
      end loop;
      return null;
   end Find_In_Cache;

   ---------------------------
   -- On_Executable_Changed --
   ---------------------------

   procedure On_Executable_Changed
     (Editor : access Asm_Editor_Record)
   is
      Tmp : Cache_Data_Access := Editor.Cache;
   begin
      --  Clear the cache, since it is no longer valid.
      while Editor.Cache /= null loop
         Tmp := Editor.Cache.Next;
         Free (Tmp.Low);
         Free (Tmp.High);
         Free (Tmp.Data);
         Editor.Cache := Tmp;
      end loop;
   end On_Executable_Changed;

   ----------------------------
   -- Show_Current_Line_Menu --
   ----------------------------

   procedure Show_Current_Line_Menu
     (Editor : access Asm_Editor_Record'Class)
   is
   begin
      Set_Line (Editor, Get_Line (Editor), Set_Current => True);
   end Show_Current_Line_Menu;

end Odd.Asm_Editors;
