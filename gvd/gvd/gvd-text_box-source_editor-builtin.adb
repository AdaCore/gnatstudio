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

with Ada.Text_IO;           use Ada.Text_IO;
with Debugger;              use Debugger;
with GNAT.OS_Lib;           use GNAT.OS_Lib;
with Gdk.Bitmap;            use Gdk.Bitmap;
with Gdk.Color;             use Gdk.Color;
with Gdk.Pixmap;            use Gdk.Pixmap;
with Gdk.Window;            use Gdk.Window;
with Glib;                  use Glib;
with Gtk.Adjustment;        use Gtk.Adjustment;
with Gtk.Check_Menu_Item;   use Gtk.Check_Menu_Item;
with Gtk.Enums;             use Gtk.Enums;
with Gtk.Handlers;          use Gtk.Handlers;
with Gtk.Layout;            use Gtk.Layout;
with Gtk.Main;              use Gtk.Main;
with Gtk.Menu;              use Gtk.Menu;
with Gtk.Menu_Item;         use Gtk.Menu_Item;
with Gtk.Pixmap;            use Gtk.Pixmap;
with Gtk.Text;              use Gtk.Text;
with Gtk.Widget;            use Gtk.Widget;
with Gtkada.Types;          use Gtkada.Types;
with Language;              use Language;
with Main_Debug_Window_Pkg; use Main_Debug_Window_Pkg;
with Odd.Code_Editors;      use Odd.Code_Editors;
with Odd.Explorer;          use Odd.Explorer;
with Odd.Menus;             use Odd.Menus;
with Odd.Process;           use Odd.Process;
with Odd.Strings;           use Odd.Strings;
with Odd.Text_Boxes;        use Odd.Text_Boxes;
with Odd.Types;             use Odd.Types;
with Odd_Intl;              use Odd_Intl;
with Process_Proxies;       use Process_Proxies;

package body Odd.Source_Editors is

   Line_Numbers_Width : constant Positive := 6;
   --  Number of characters reserved on the left for line numbers (including
   --  the space character)

   Do_Color_Highlighting : constant Boolean := True;
   --  Indicate whether the editor should provide color highlighting.

   No_Breakpoint : Breakpoint_Array (1 .. 0);
   --  Array used to reset the breakpoint list

   subtype Line_Number is String (1 .. Line_Numbers_Width);
   --  Type of strings used to display line numbers.

   Editor_Contextual_Menu_Name : constant String := "odd_editor_context";
   --  String used to store the editor contextual menu as a user data

   --------------------
   -- Local packages --
   --------------------

   type Breakpoint_Record (File_Length : Natural) is record
      Process : Debugger_Process_Tab;
      File    : String (1 .. File_Length);
      Line    : Integer;
   end record;

   type Variable_Record (Name_Length : Natural) is record
      Process      : Debugger_Process_Tab;
      Name         : String (1 .. Name_Length);
      Auto_Refresh : Boolean;
   end record;

   package Check_Editor_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Check_Menu_Item_Record, Source_Editor);
   package Widget_Breakpoint_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Breakpoint_Record);
   package Widget_Variable_Handler is new Gtk.Handlers.User_Callback
     (Gtk_Widget_Record, Variable_Record);
   package Editor_Idle is new Gtk.Main.Idle (Source_Editor);
   package Editor_Cb is new Callback (Source_Editor_Record);

   procedure Update_Buttons
     (Editor : access Source_Editor_Record'Class;
      Reset_Line : Boolean := True);
   --  Update the display of line-breaks buttons.
   --  If this feature is disabled for the editor, then they are all removed.
   --  If Reset_Line is True, then the editor is scrolled so as to show the
   --  the current line.

   procedure Set_Breakpoint
     (Widget : access Gtk_Widget_Record'Class;
      Br     : Breakpoint_Record);
   --  Set a breakpoint on a specific line.

   procedure Till_Breakpoint
     (Widget : access Gtk_Widget_Record'Class;
      Br     : Breakpoint_Record);
   --  Set a temporary breakpoint on a line, and continue execution.

   procedure Change_Line_Nums
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Editor : Source_Editor);
   --  Callback for the "show line numbers" contextual menu item.

   procedure Print_Variable
     (Widget : access Gtk_Widget_Record'Class;
      Var    : Variable_Record);
   --  Callback for the "print variable" or "display variable" contextual menu
   --  items.

   procedure Change_Lines_With_Code
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Editor : Source_Editor);
   --  Callback for the "show lines with code" contextual menu item.

   procedure Show_Current_Line_Menu
     (Editor : access Source_Editor_Record'Class);
   --  Display the current file and current line in the editor.

   procedure Is_Breakpoint
     (Editor : access Source_Editor_Record'Class;
      Line   : Integer;
      Result : out Boolean;
      Num    : out Integer);
   --  Tell if a breakpoint is set at a specific line.
   --  If it is the case, return the number of the breakpoint.

   function Idle_Compute_Lines (Editor : Source_Editor) return Boolean;
   --  Idle function called to compute the lines with code in the editor

   function Check_Single_Line
     (Editor     : access Source_Editor_Record'Class;
      Line       : Natural) return Boolean;
   --  Check whether Line contains executable code, and put an icon for it
   --  in the button toolbar if needed.
   --  Returns False if no line after Line contains code.

   procedure Destroy_Cb (Editor : access Source_Editor_Record'Class);
   --  Free the memory occupied by the editor and the buttons layout, as well
   --  as all the associated pixmaps.

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Editor  : out Source_Editor;
      Process : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Editor := new Source_Editor_Record;
      Initialize (Editor, Process);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Editor  : access Source_Editor_Record'Class;
      Process : access Gtk.Widget.Gtk_Widget_Record'Class) is
   begin
      Odd.Text_Boxes.Initialize (Editor);
      Editor.Process := Gtk_Widget (Process);
      Editor_Cb.Connect
        (Editor, "destroy", Editor_Cb.To_Marshaller (Destroy_Cb'Access));
      Show_All (Editor);
   end Initialize;

   -------------------
   -- Is_Breakpoint --
   -------------------

   procedure Is_Breakpoint
     (Editor : access Source_Editor_Record'Class;
      Line   : Integer;
      Result : out Boolean;
      Num    : out Integer)
   is
      Process : Debugger_Process_Tab := Debugger_Process_Tab (Editor.Process);
      Breakpoints_Array : Odd.Types.Breakpoint_Array_Ptr :=
        Process.Breakpoints;
   begin
      if Breakpoints_Array /= null then
         for Index in Breakpoints_Array'Range loop
            if Breakpoints_Array (Index).Line = Line
              and then Breakpoints_Array (Index).File.all =
              Base_File_Name (Editor.Current_File.all)
            then
               Num := Breakpoints_Array (Index).Num;
               Result := True;
               return;
            end if;
         end loop;
      end if;

      Result := False;
   end Is_Breakpoint;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Editor            : access Source_Editor_Record;
      Ps_Font_Name      : String;
      Font_Size         : Glib.Gint;
      Default_Icon      : Gtkada.Types.Chars_Ptr_Array;
      Current_Line_Icon : Gtkada.Types.Chars_Ptr_Array;
      Stop_Icon         : Gtkada.Types.Chars_Ptr_Array;
      Comments_Color    : String;
      Strings_Color     : String;
      Keywords_Color    : String) is
   begin
      Configure (Editor, Ps_Font_Name, Font_Size, Current_Line_Icon);

      Create_From_Xpm_D
        (Editor.Default_Pixmap,
         Null_Window,
         Get_System,
         Editor.Default_Mask,
         White (Get_System),
         Default_Icon);
      Create_From_Xpm_D
        (Editor.Stop_Pixmap,
         Null_Window,
         Get_System,
         Editor.Stop_Mask,
         White (Get_System),
         Stop_Icon);

      Editor.Colors (Comment_Text) := Parse (Comments_Color);
      Alloc (Get_System, Editor.Colors (Comment_Text));
      Editor.Colors (String_Text)  := Parse (Strings_Color);
      Alloc (Get_System, Editor.Colors (String_Text));
      Editor.Colors (Keyword_Text) := Parse (Keywords_Color);
      Alloc (Get_System, Editor.Colors (Keyword_Text));
   end Configure;

   -----------------------
   -- On_Pixmap_Clicked --
   -----------------------

   function On_Pixmap_Clicked
     (Editor : access Source_Editor_Record;
      Button : Natural;
      Line   : Natural) return Boolean
   is
      Process : Debugger_Process_Tab := Debugger_Process_Tab (Editor.Process);
      Result  : Boolean;
      Num     : Natural;
   begin
      if Button = 1 then
         Is_Breakpoint (Editor, Line, Result, Num);

         if Result then
            Remove_Breakpoint (Process.Debugger, Num, Display => True);
         else
            Break_Source (Process.Debugger, Editor.Current_File.all,
                          Line, Display => True);
         end if;
      end if;
      return True;
   end On_Pixmap_Clicked;

   ----------------------------
   -- Invisible_Column_Width --
   ----------------------------

   function Invisible_Column_Width
     (Editor : access Source_Editor_Record) return Glib.Gint is
   begin
      if Editor.Show_Line_Nums then
         return Gint (Line_Numbers_Width);
      else
         return 0;
      end if;
   end Invisible_Column_Width;

   ---------------------------
   -- Child_Contextual_Menu --
   ---------------------------

   function Child_Contextual_Menu
     (Source : access Source_Editor_Record;
      Line   : Natural;
      Entity : String) return Gtk.Menu.Gtk_Menu
   is
      Menu  : Gtk_Menu;
      Mitem : Gtk_Menu_Item;
      Check : Gtk_Check_Menu_Item;

   begin
      --  Destroy the previous menu (which we couldn't do earlier because
      --  of the call to popup. It will change every item anyway.

      begin
         Menu := Menu_User_Data.Get (Source, Editor_Contextual_Menu_Name);
         Destroy (Menu);
      exception
         when Gtkada.Types.Data_Error => null;
      end;

      --  Create a new menu

      Gtk_New (Menu);

      Gtk_New (Mitem, Label => -"Print " & Entity);
      Append (Menu, Mitem);
      Widget_Variable_Handler.Connect
        (Mitem, "activate",
         Widget_Variable_Handler.To_Marshaller (Print_Variable'Access),
         Variable_Record'
         (Name_Length  => Entity'Length,
          Name         => Entity,
          Auto_Refresh => False,
          Process      => Debugger_Process_Tab (Source.Process)));
      if Entity'Length = 0 then
         Set_State (Mitem, State_Insensitive);
      end if;

      Gtk_New (Mitem, Label => -"Display " & Entity);
      Append (Menu, Mitem);
      Widget_Variable_Handler.Connect
        (Mitem, "activate",
         Widget_Variable_Handler.To_Marshaller (Print_Variable'Access),
         Variable_Record'
         (Name_Length  => Entity'Length,
          Name         => Entity,
          Auto_Refresh => True,
          Process      => Debugger_Process_Tab (Source.Process)));
      if Entity'Length = 0 then
         Set_State (Mitem, State_Insensitive);
      end if;

      --  Display a separator

      Gtk_New (Mitem);
      Append (Menu, Mitem);

      --  Line specific items

      Gtk_New
        (Mitem, Label => -"Set Breakpoint on Line" & Integer'Image (Line));
      Append (Menu, Mitem);
      Widget_Breakpoint_Handler.Connect
        (Mitem, "activate",
         Widget_Breakpoint_Handler.To_Marshaller (Set_Breakpoint'Access),
         Breakpoint_Record'
         (File_Length  => Get_Current_File (Source)'Length,
          Process      => Debugger_Process_Tab (Source.Process),
          File         => Get_Current_File (Source),
          Line         => Line));

      Gtk_New
        (Mitem, Label => -"Continue Until Line" & Integer'Image (Line));
      Append (Menu, Mitem);
      Widget_Breakpoint_Handler.Connect
        (Mitem, "activate",
         Widget_Breakpoint_Handler.To_Marshaller (Till_Breakpoint'Access),
         Breakpoint_Record'
         (File_Length  => Get_Current_File (Source)'Length,
          Process      => Debugger_Process_Tab (Source.Process),
          File         => Get_Current_File (Source),
          Line         => Line));

      Gtk_New (Mitem);
      Append (Menu, Mitem);

      Gtk_New (Mitem, Label => -"Show Current Location");
      Append (Menu, Mitem);
      Editor_Cb.Object_Connect
        (Mitem, "activate",
         Editor_Cb.To_Marshaller (Show_Current_Line_Menu'Access),
         Source);
      Set_Sensitive
        (Mitem, Source.Debugger_Current_File /= null
         and then Source.Debugger_Current_File.all /= "");

      Gtk_New (Mitem);
      Append (Menu, Mitem);

      --  Editor specific items

      Gtk_New (Check, Label => -"Display Line Numbers");
      Set_Always_Show_Toggle (Check, True);
      Set_Active (Check, Get_Show_Line_Nums (Source));
      Append (Menu, Check);
      Check_Editor_Handler.Connect
        (Check, "activate",
         Check_Editor_Handler.To_Marshaller (Change_Line_Nums'Access),
         Source_Editor (Source));

      Gtk_New (Check, Label => -"Show lines with code");
      Set_Always_Show_Toggle (Check, True);
      Set_Active (Check, Get_Show_Lines_With_Code (Source));
      Append (Menu, Check);
      Check_Editor_Handler.Connect
        (Check, "activate",
         Check_Editor_Handler.To_Marshaller (Change_Lines_With_Code'Access),
         Source_Editor (Source));

      Append_To_Contextual_Menu
        (Debugger_Process_Tab (Source.Process).Editor_Text, Menu);

      Show_All (Menu);
      Menu_User_Data.Set (Source, Menu, Editor_Contextual_Menu_Name);
      return Menu;
   end Child_Contextual_Menu;

   -------------------
   -- Insert_Buffer --
   -------------------

   procedure Insert_Buffer
     (Editor : access Source_Editor_Record;
      Buffer : String)
   is
      function Line_Number_String (Line : Positive) return String;
      --  Return a string that contains the line number.
      --  The number is aligned to the right, and the string as a length of
      --  Line_Numbers_Width.

      function Line_Number_String (Line : Positive) return String is
         S : String        := Positive'Image (Line);
         N : Line_Number   := (others => ' ');
         Length : Positive := Positive'Min (S'Length, Line_Numbers_Width);
      begin
         N (N'Last - Length + 1  .. N'Last - 1) := S (2 .. Length);
         return N;
      end Line_Number_String;

      Index       : Positive := 1;
      Line        : Positive := 1;
      Line_Start  : Positive := 1;
      Entity      : Language_Entity;
      Next_Char   : Positive;

   begin
      if Editor.Show_Line_Nums then
         Insert (Editor, Chars => Line_Number_String (1));
      end if;

      while Index <= Buffer'Last loop
         case Buffer (Index) is
            when ASCII.CR =>  --  ignore, this is processed as ASCII.LF
               Index := Index + 1;

            when ASCII.LF =>
               if Do_Color_Highlighting then
                  Insert (Editor, Chars => Buffer (Index .. Index));
               else
                  Insert (Editor, Chars => Buffer (Line_Start .. Index));
               end if;

               Line := Line + 1;
               Index := Index + 1;
               Line_Start := Index;

               if Editor.Show_Line_Nums then
                  Insert (Editor, Chars => Line_Number_String (Line));
               end if;

            when others =>
               if Do_Color_Highlighting then
                  if Editor.Lang /= null then
                     Looking_At (Editor.Lang,
                                 Buffer (Index .. Buffer'Last),
                                 Entity, Next_Char);
                  else
                     Next_Char := Index + 1;
                     Entity := Normal_Text;
                  end if;

                  --  Print every line separately, so that we can add line
                  --  numbers as well.

                  declare
                     J          : Positive := Index;
                     Line_Start : Positive := Index;

                  begin
                     while J < Next_Char loop
                        if Buffer (J) = ASCII.LF then
                           Insert (Editor,
                                   Editor.Colors (Entity),
                                   Chars => Buffer (Line_Start .. J));
                           Line := Line + 1;

                           if Editor.Show_Line_Nums then
                              Insert (Editor,
                                      Chars => Line_Number_String (Line));
                           end if;

                           Line_Start := J + 1;
                        end if;

                        J := J + 1;
                     end loop;

                     Insert (Editor,
                             Editor.Colors (Entity),
                             Null_Color,
                             Buffer (Line_Start .. Next_Char - 1));
                  end;

                  Index := Next_Char;

               else
                  Index := Index + 1;
               end if;
         end case;
      end loop;
   end Insert_Buffer;

   --------------------------
   -- Set_Current_Language --
   --------------------------

   procedure Set_Current_Language
     (Editor : access Source_Editor_Record;
      Lang   : Language.Language_Access) is
   begin
      Free (Editor.Lang);

      if Lang /= null then
         Editor.Lang := new Language_Root'Class' (Lang.all);
      end if;
   end Set_Current_Language;

   --------------------
   -- Highlight_Word --
   --------------------

   procedure Highlight_Word
     (Editor   : access Source_Editor_Record;
      Position : Odd.Explorer.Position_Type)
   is
      Last   : Positive;
      Pos    : constant Positive := Position.Index +
        (Position.Line + 1) * Natural (Invisible_Column_Width (Editor)) - 1;
      Text   : constant Gtk_Text := Get_Child (Editor);
      Buffer : constant String := Get_Chars (Text, Gint (Pos));

   begin
      Last := Buffer'First;

      while Last < Buffer'Last
        and then Buffer (Last) /= ' '
        and then Buffer (Last) /= '('
        and then Buffer (Last) /= ';'
      loop
         Last := Last + 1;
      end loop;

      Freeze (Text);

      --  Set the adjustment directly, so that the text is not scrolled
      --  on the screen (which is too slow for big files)
      Set_Value
        (Get_Vadj (Text),
         Gfloat (Pixels_From_Line (Editor, Position.Line)));
      Changed (Get_Vadj (Text));

      --  Change the cursor position, and highlight the entity.
      --  We claim the selection so that the selected entity always has the
      --  same color (if we don't, the first selection has a different color
      --  than the following ones).
      Claim_Selection (Text, True, 0);
      Set_Position (Text, Gint (Pos));
      Select_Region (Text, Gint (Pos), Gint (Last + Pos - Buffer'First));
      Thaw (Text);
   end Highlight_Word;

   ------------------------
   -- Update_Breakpoints --
   ------------------------

   procedure Update_Breakpoints
     (Editor    : access Source_Editor_Record;
      Br        : Odd.Types.Breakpoint_Array)
   is
      use Gtk.Widget.Widget_List;
      Tmp : Glist := Editor.Breakpoint_Buttons;
      Pix : Gtk_Pixmap;

   begin
      if Editor.Current_File = null then
         return;
      end if;

      declare
         Base_File : String := Base_File_Name (Editor.Current_File.all);
      begin
         Freeze (Get_Buttons (Editor));
         Hide_All (Get_Buttons (Editor));

         --  Remove all existing breakpoints

         while Tmp /= Null_List loop
            Destroy (Get_Data (Tmp));
            Tmp := Next (Tmp);
         end loop;

         Free (Editor.Breakpoint_Buttons);
         Editor.Breakpoint_Buttons := Null_List;

         --  Add the new ones
         for B in Br'Range loop
            if Br (B).File /= null
              and then Br (B).File.all = Base_File
            then
               Gtk_New (Pix, Editor.Stop_Pixmap, Editor.Stop_Mask);
               Put (Get_Buttons (Editor), Pix,
                    0, Pixels_From_Line (Editor, Br (B).Line));
               Prepend (Editor.Breakpoint_Buttons, Gtk_Widget (Pix));
            end if;
         end loop;

         Show_All (Get_Buttons (Editor));
         Thaw (Get_Buttons (Editor));
      end;
   end Update_Breakpoints;

   ------------------------
   -- Set_Show_Line_Nums --
   ------------------------

   procedure Set_Show_Line_Nums
     (Editor : access Source_Editor_Record;
      Show   : Boolean := False)
   is
      --  Save the currently displayed line

      Value : constant Gfloat :=
        Get_Value (Get_Vadj (Get_Child (Editor)));

   begin
      if Show /= Editor.Show_Line_Nums then
         Editor.Show_Line_Nums := Show;
         Update_Child (Editor);
         Set_Value (Get_Vadj (Get_Child (Editor)), Value);
      end if;
   end Set_Show_Line_Nums;

   ------------------------
   -- Get_Show_Line_Nums --
   ------------------------

   function Get_Show_Line_Nums
     (Editor : access Source_Editor_Record) return Boolean is
   begin
      return Editor.Show_Line_Nums;
   end Get_Show_Line_Nums;

   ------------------------------
   -- Set_Show_Lines_With_Code --
   ------------------------------

   procedure Set_Show_Lines_With_Code
     (Editor : access Source_Editor_Record;
      Show   : Boolean) is
   begin
      if Show /= Editor.Show_Lines_With_Code then
         Editor.Show_Lines_With_Code := Show;
         Update_Buttons (Editor, False);

         if Editor.Debugger_Current_File /= null
           and then Editor.Current_File /= null
           and then Editor.Debugger_Current_File.all =
             Editor.Current_File.all
         then
            Set_Line (Editor, Get_Line (Editor), Set_Current => True);
         end if;
      end if;
   end Set_Show_Lines_With_Code;

   ------------------------------
   -- Get_Show_Lines_With_Code --
   ------------------------------

   function Get_Show_Lines_With_Code
     (Editor : access Source_Editor_Record) return Boolean is
   begin
      return Editor.Show_Lines_With_Code;
   end Get_Show_Lines_With_Code;

   ----------------------
   -- Get_Current_File --
   ----------------------

   function Get_Current_File
     (Editor : access Source_Editor_Record) return String is
   begin
      if Editor.Current_File = null then
         return "";
      else
         return Editor.Current_File.all;
      end if;
   end Get_Current_File;

   ---------------
   -- Load_File --
   ---------------

   procedure Load_File
     (Editor      : access Source_Editor_Record;
      File_Name   : String;
      Set_Current : Boolean := True)
   is
      F      : File_Descriptor;
      Length : Positive;
      Name   : aliased constant String := File_Name & ASCII.NUL;
   begin
      --  Avoid reloading a file twice.
      --  This also solve the problem of recursive loops ("info line" in gdb,
      --  with annotation level set to 1 will print a file reference as well).

      if Editor.Current_File /= null
        and then Editor.Current_File.all = File_Name
      then
         return;
      else
         Free (Editor.Current_File);
         Set_Buffer (Editor, null);
         Editor.Current_File := new String'(File_Name);
      end if;

      --  Read the size of the file
      F := Open_Read (Name'Address, Text);

      if F = Invalid_FD then
         File_Not_Found (Editor, File_Name);
         return;
      else
         Length := Positive (File_Length (F));

         --  Allocate the buffer
         --  and strip the ^Ms from the string
         declare
            S : String (1 .. Length);
         begin
            Length := Read (F, S'Address, Length);
            Set_Buffer (Editor, new String' (Strip_Control_M (S)));
         end;

         Close (F);
      end if;

      Update_Child (Editor);
      Update_Buttons (Editor, True);

      if Debugger_Process_Tab (Editor.Process).Breakpoints /= null then
         Update_Breakpoints
           (Editor, Debugger_Process_Tab (Editor.Process).Breakpoints.all);
      else
         Update_Breakpoints (Editor, No_Breakpoint);
      end if;

      if Set_Current then
         Free (Editor.Debugger_Current_File);
         Editor.Debugger_Current_File := new String'(File_Name);
      end if;

   exception

      --  File not found
      when Name_Error =>
         null;
   end Load_File;

   --------------------
   -- File_Not_Found --
   --------------------

   procedure File_Not_Found
     (Editor    : access Source_Editor_Record;
      File_Name : String) is
   begin
      --  Clear the old file
      Delete_Text (Get_Child (Editor));
      Hide_Current_Line_Button (Editor);
      Forall (Get_Buttons (Editor), Gtk.Widget.Destroy_Cb'Access);

      --  Print a warning message
      Insert (Editor, Chars => File_Name & (-": File not found"));
   end File_Not_Found;

   --------------------
   -- Update_Buttons --
   --------------------

   procedure Update_Buttons
     (Editor     : access Source_Editor_Record'Class;
      Reset_Line : Boolean := True)
   is
      Pix       : Gtk_Pixmap;
      Num_Lines : Natural := 0;
      Value     : Gfloat;
      Process   : constant Debugger_Process_Tab :=
        Debugger_Process_Tab (Editor.Process);
   begin
      if Is_Empty (Editor) then
         return;
      end if;

      --  Clear the existing buttons.

      Freeze (Get_Buttons (Editor));
      Hide_Current_Line_Button (Editor);

      --  Remove all existing buttons
      Gtk.Widget.Widget_List.Free (Editor.Breakpoint_Buttons);
      Editor.Breakpoint_Buttons := Gtk.Widget.Widget_List.Null_List;
      Forall (Get_Buttons (Editor), Gtk.Widget.Destroy_Cb'Access);

      --  Display the breakpoint icons

      Editor.Current_File_Cache := Find_In_Cache
        (Process.Window, Editor.Current_File.all);

      if Editor.Idle_Id /= 0 then
         Idle_Remove (Editor.Idle_Id);
      end if;

      if Editor.Show_Lines_With_Code then
         Editor.Idle_Id := Editor_Idle.Add
           (Idle_Compute_Lines'Access, Source_Editor (Editor));

         --  Show the breakpoints we already know about
         if Editor.Current_File_Cache.Line_Has_Code /= null then
            for Line in Editor.Current_File_Cache.Line_Has_Code'Range loop
               if Editor.Current_File_Cache.Line_Has_Code (Line) then
                  Gtk_New (Pix, Editor.Default_Pixmap, Editor.Default_Mask);
                  Put (Get_Buttons (Editor), Pix,
                       X => 0,
                       Y => Pixels_From_Line (Editor, Line));
               end if;
            end loop;
         end if;

         --  Allocate the arrays if required
         if Editor.Current_File_Cache.Current_Line = 0
           and then Editor.Current_File_Cache.Line_Has_Code = null
         then
            Num_Lines := Lines_Count (Editor);

            Editor.Current_File_Cache.Line_Has_Code :=
              new Packed_Boolean_Array (1 .. Num_Lines);
            Editor.Current_File_Cache.Line_Has_Code.all := (others => False);
            Editor.Current_File_Cache.Line_Parsed :=
              new Packed_Boolean_Array (1 .. Num_Lines);
            Editor.Current_File_Cache.Line_Parsed.all := (others => False);
         end if;
      end if;

      Value := Get_Value (Get_Vadj (Get_Child (Editor)));
      Set_Line (Editor, Get_Line (Editor), Set_Current => False);

      if not Reset_Line then
         Set_Value (Get_Vadj (Get_Child (Editor)), Value);
         Value_Changed (Get_Vadj (Get_Child (Editor)));
      end if;

      Show_All (Get_Buttons (Editor));
      Thaw (Get_Buttons (Editor));
   end Update_Buttons;

   ----------------------------
   -- Change_Lines_With_Code --
   ----------------------------

   procedure Change_Lines_With_Code
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Editor : Source_Editor) is
   begin
      Set_Show_Lines_With_Code (Editor, Get_Active (Item));
   end Change_Lines_With_Code;

   --------------------
   -- Set_Breakpoint --
   --------------------

   procedure Set_Breakpoint
     (Widget : access Gtk_Widget_Record'Class;
      Br     : Breakpoint_Record) is
   begin
      Break_Source (Br.Process.Debugger, Br.File, Br.Line, Display => True);
   end Set_Breakpoint;

   ---------------------
   -- Till_Breakpoint --
   ---------------------

   procedure Till_Breakpoint
     (Widget : access Gtk_Widget_Record'Class;
      Br     : Breakpoint_Record) is
   begin
      Break_Source (Br.Process.Debugger, Br.File, Br.Line, Temporary => True);
      Continue (Br.Process.Debugger, Display => True);
   end Till_Breakpoint;

   --------------------
   -- Print_Variable --
   --------------------

   procedure Print_Variable
     (Widget : access Gtk_Widget_Record'Class;
      Var    : Variable_Record)
   is
      pragma Warnings (Off, Widget);
   begin
      if Var.Auto_Refresh then
         Process_User_Command
           (Var.Process, "graph display " & Var.Name,
            Output_Command => True);
      else
         Process_User_Command
           (Var.Process, "graph print " & Var.Name,
            Output_Command => True);
      end if;
   end Print_Variable;

   ----------------------
   -- Change_Line_Nums --
   ----------------------

   procedure Change_Line_Nums
     (Item   : access Gtk_Check_Menu_Item_Record'Class;
      Editor : Source_Editor) is
   begin
      Set_Show_Line_Nums (Editor, Get_Active (Item));
   end Change_Line_Nums;

   ----------------------------
   -- Show_Current_Line_Menu --
   ----------------------------

   procedure Show_Current_Line_Menu
     (Editor : access Source_Editor_Record'Class)
   is
      Name : constant String := Editor.Debugger_Current_File.all;
      Lang : Language_Access;

   begin
      if Name /= "" then
         Lang := Get_Language_From_File (Name);
         Set_Current_Language (Editor, Lang);
         Load_File (Editor,
                    Find_File
                    (Debugger_Process_Tab (Editor.Process).Debugger, Name),
                    Set_Current => False);
         Set_Line (Editor, Get_Line (Editor), Set_Current => True);
      end if;
   end Show_Current_Line_Menu;

   ------------------------
   -- Idle_Compute_Lines --
   ------------------------

   function Idle_Compute_Lines (Editor : Source_Editor) return Boolean
   is
      Process  : Debugger_Process_Tab := Debugger_Process_Tab (Editor.Process);
      Debug    : Debugger_Access := Process.Debugger;
      Line     : Integer;
      Line_Max : Integer;
      Found    : Boolean := False;

   begin
      --  If we already reached the end, cancel the Idle loop

      if Editor.Current_File_Cache.Line_Parsed = null then
         Editor.Idle_Id := 0;
         return False;
      end if;

      if Command_In_Process (Get_Process (Debug)) then
         return True;
      end if;

      --  Priority is given to computing the visible lines on the screen.

      Line := Line_From_Pixels
        (Editor,
         Gint (Get_Value (Get_Vadj (Get_Child (Editor)))));
      if Line <= Editor.Current_File_Cache.Line_Parsed'First then
         Line := Editor.Current_File_Cache.Line_Parsed'First;
      end if;

      Line_Max := Line + Line_From_Pixels
        (Editor, Gint (Get_Allocation_Height (Editor)));
      while Line <= Line_Max loop
         if Line > Editor.Current_File_Cache.Line_Parsed'Last then
            exit;
         end if;

         if not Editor.Current_File_Cache.Line_Parsed (Line) then
            Found := True;
            exit;
         end if;

         Line := Line + 1;
      end loop;

      --  Else find the first line we did not parse
      if not Found then
         loop
            Editor.Current_File_Cache.Current_Line :=
              Editor.Current_File_Cache.Current_Line + 1;
            if Editor.Current_File_Cache.Current_Line >
              Editor.Current_File_Cache.Line_Has_Code'Last
            then
               Free (Editor.Current_File_Cache.Line_Parsed);
               Editor.Idle_Id := 0;
               return False;
            end if;

            exit when not Editor.Current_File_Cache.Line_Parsed
              (Editor.Current_File_Cache.Current_Line);
         end loop;

         Line := Editor.Current_File_Cache.Current_Line;
      end if;

      --  Check whether the line contains some code

      if not Check_Single_Line (Editor, Line)
        and then Line = Editor.Current_File_Cache.Current_Line
      then
         Free (Editor.Current_File_Cache.Line_Parsed);
         Editor.Idle_Id := 0;
         return False;
      end if;

      return True;
   end Idle_Compute_Lines;

   -----------------------
   -- Check_Single_Line --
   -----------------------

   function Check_Single_Line
     (Editor     : access Source_Editor_Record'Class;
      Line       : Natural) return Boolean
   is
      Kind    : Line_Kind;
      Pix     : Gtk_Pixmap;
      Process : constant Debugger_Process_Tab :=
        Debugger_Process_Tab (Editor.Process);
      Debug   : constant Debugger_Access := Process.Debugger;

   begin
      Push_Internal_Command_Status (Get_Process (Debug), True);
      Set_Parse_File_Name (Get_Process (Debug), False);

      --  Check whether the line contains code

      Kind := Line_Contains_Code (Debug, Editor.Current_File.all, Line);
      Editor.Current_File_Cache.Line_Parsed (Line) := True;

      Set_Parse_File_Name (Get_Process (Debug), True);
      Pop_Internal_Command_Status (Get_Process (Debug));

      --  Deactivate the idle callback if we have finished
      if Kind = No_More_Code then
         return False;
      end if;

      if Kind = Have_Code then
         Freeze (Get_Buttons (Editor));
         Hide_All (Get_Buttons (Editor));

         Editor.Current_File_Cache.Line_Has_Code (Line) := True;
         Gtk_New (Pix, Editor.Default_Pixmap, Editor.Default_Mask);
         Put (Get_Buttons (Editor), Pix,
              X => 0,
              Y => Pixels_From_Line (Editor, Line));

         Show_All (Get_Buttons (Editor));
         Thaw (Get_Buttons (Editor));
      end if;

      return True;
   end Check_Single_Line;

   ----------------
   -- Destroy_Cb --
   ----------------

   procedure Destroy_Cb (Editor : access Source_Editor_Record'Class) is
   begin
      Gdk.Pixmap.Unref (Editor.Default_Pixmap);
      Gdk.Bitmap.Unref (Editor.Default_Mask);
   end Destroy_Cb;

end Odd.Source_Editors;
