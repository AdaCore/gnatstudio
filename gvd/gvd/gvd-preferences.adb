-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                      Copyright (C) 2000-2001                      --
--                              ACT-Europe                           --
--                                                                   --
-- GVD is free  software;  you can redistribute it and/or modify  it --
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

with Glib;                    use Glib;
with General_Preferences_Pkg; use General_Preferences_Pkg;
with Gint_Xml;                use Gint_Xml;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.GEntry;              use Gtk.GEntry;
with Gtk.Extra.Font_Combo;    use Gtk.Extra.Font_Combo;
with Gdk.Color;               use Gdk.Color;
with Gtk.Combo;               use Gtk.Combo;
with Gtk.Label;               use Gtk.Label;
with Gtk.Check_Button;        use Gtk.Check_Button;
with Gtk.Spin_Button;         use Gtk.Spin_Button;
with Gtk.Toggle_Button;       use Gtk.Toggle_Button;
with GVD.Color_Combo;         use GVD.Color_Combo;
with Ada.Integer_Text_IO;     use Ada.Integer_Text_IO;

package body GVD.Preferences is

   Current_Preferences : Node_Ptr;
   --  The XML tree that contains the current preferences.

   Saved_Preferences : Node_Ptr := null;
   --  The XML tree that saves the state of the preferences at the time the
   --  dialog was initially opened.
   --  This should be null most of the time, except when the preferences
   --  dialog is open.

   Tab_Size_Cached : Gint;
   --  Cached value for tab size, for fast access

   procedure Set
     (Var : String_Gint; Value : Gint; Override : Boolean := False);
   procedure Set
     (Var : String_Guint; Value : Guint; Override : Boolean := False);
   procedure Set
     (Var : String_Boolean; Value : Boolean; Override : Boolean := False);
   procedure Set
     (Var : String_Color; Value : Gdk_Color; Override : Boolean := False);
   procedure Set
     (Var      : String_Font;
      Size     : String_Gint;
      Dialog   : access Gtk_Font_Combo_Record'Class;
      Override : Boolean := False);
   procedure Set
     (Var : String_Tooltips_In_Source;
      Value : Tooltips_In_Source_Type;
      Override : Boolean := False);
   pragma Inline (Set);

   procedure Set (Var : String; Value : String; Override : Boolean := False);
   --  Create a new entry in the current preferences, or modify the value
   --  of the existing one (only if Override is True)

   procedure Set_From_Dialog
     (Dialog : General_Preferences_Pkg.General_Preferences_Access);
   --  Set the preferences from the contents of Dialog.
   --  This modifies Current_PReferences, not Saved_Preferences.

   ----------------------
   -- Load_Preferences --
   ----------------------

   procedure Load_Preferences (File_Name : String) is
   begin
      if Current_Preferences /= null then
         Free (Current_Preferences);
      end if;

      Current_Preferences := Parse (File_Name);
      Set_Default_Preferences;
   end Load_Preferences;

   ----------------------
   -- Save_Preferences --
   ----------------------

   procedure Save_Preferences (File_Name : String) is
   begin
      Print (Current_Preferences, File_Name => File_Name);
   end Save_Preferences;

   ------------------
   -- Get_Tab_Size --
   ------------------

   function Get_Tab_Size return Gint is
   begin
      return Tab_Size_Cached;
   end Get_Tab_Size;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref (Name : String_Guint) return Guint is
      Node : Node_Ptr := Find_Tag (Current_Preferences.Child, String (Name));
   begin
      pragma Assert (Node /= null);
      pragma Assert (Node.Value /= null);
      return Guint'Value (Node.Value.all);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref (Name : String_String) return String is
      Node : Node_Ptr := Find_Tag (Current_Preferences.Child, String (Name));
   begin
      pragma Assert (Node /= null);
      pragma Assert (Node.Value /= null);
      return Node.Value.all;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref (Name : String_Boolean) return Boolean is
      Node : Node_Ptr := Find_Tag (Current_Preferences.Child, String (Name));
   begin
      pragma Assert (Node /= null);
      pragma Assert (Node.Value /= null);
      return Boolean'Value (Node.Value.all);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref (Name : String_Gint) return Gint is
      Node : Node_Ptr := Find_Tag (Current_Preferences.Child, String (Name));
   begin
      pragma Assert (Node /= null);
      pragma Assert (Node.Value /= null);
      return Gint'Value (Node.Value.all);
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref (Name : String_Color) return Gdk.Color.Gdk_Color is
      Node : Node_Ptr := Find_Tag (Current_Preferences.Child, String (Name));
      Color : Gdk_Color;
   begin
      pragma Assert (Node /= null);
      pragma Assert (Node.Value /= null);
      Color := Parse (Node.Value.all);
      Alloc (Gtk.Widget.Get_Default_Colormap, Color);
      --  Alloc_Color (Get_System, Color, True, True, Success);
      return Color;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref (Name : String_Font) return String is
      Node : Node_Ptr := Find_Tag (Current_Preferences.Child, String (Name));
   begin
      pragma Assert (Node /= null);
      pragma Assert (Node.Value /= null);
      return Node.Value.all;
   end Get_Pref;

   --------------
   -- Get_Pref --
   --------------

   function Get_Pref (Name : String_Tooltips_In_Source)
      return Tooltips_In_Source_Type
   is
      Node : Node_Ptr := Find_Tag (Current_Preferences.Child, String (Name));
   begin
      pragma Assert (Node /= null);
      pragma Assert (Node.Value /= null);
      return Tooltips_In_Source_Type'Value (Node.Value.all);
   end Get_Pref;

   ---------
   -- Set --
   ---------

   procedure Set
     (Var : String; Value : String; Override : Boolean := False)
   is
      N : Node_Ptr := Find_Tag (Current_Preferences.Child, Var);
   begin
      if N = null then
         N := new Node;
         N.Tag := new String' (Var);
         N.Value := new String' (Value);
         Add_Child (Current_Preferences, N);
      elsif Override then
         Gint_Xml.Free (N.Value);
         N.Value := new String' (Value);
      end if;
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Var : String_Gint; Value : Gint; Override : Boolean := False)
   is
   begin
      Set (String (Var), Gint'Image (Value), Override);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Var : String_Guint; Value : Guint; Override : Boolean := False) is
   begin
      Set (String (Var), Guint'Image (Value), Override);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Var : String_Boolean; Value : Boolean; Override : Boolean := False) is
   begin
      Set (String (Var), Boolean'Image (Value), Override);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Var : String_Tooltips_In_Source;
      Value : Tooltips_In_Source_Type;
      Override : Boolean := False) is
   begin
      Set (String (Var), Tooltips_In_Source_Type'Image (Value), Override);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Var : String_Color; Value : Gdk_Color; Override : Boolean := False)
   is
      function Normalize (V : Gushort) return String;
      function Normalize (V : Gushort) return String is
         S : String (1 .. 8);  --  "16#....#" or "16#.#", ....
         O : String (1 .. 4) := "0000";
         Index : Natural := S'Last;
         O_Index : Natural := O'Last;
      begin
         Put (S, Integer (V), 16);
         while S (Index) /= '#' loop
            Index := Index - 1;
         end loop;
         Index := Index - 1;

         while S (Index) /= '#' loop
            O (O_Index) := S (Index);
            Index := Index - 1;
            O_Index := O_Index - 1;
         end loop;
         return O;
      end Normalize;

   begin
      Set (String (Var), '#'
           & Normalize (Red (Value))
           & Normalize (Green (Value))
           & Normalize (Blue (Value)),
           Override);
   end Set;

   ---------
   -- Set --
   ---------

   procedure Set
     (Var      : String_Font;
      Size     : String_Gint;
      Dialog   : access Gtk_Font_Combo_Record'Class;
      Override : Boolean := False) is
   begin
      Set (String (Var), Get_Chars (Get_Entry (Get_Name_Combo (Dialog))),
           Override);
      Set (Size, Gint'Value (Get_Chars (Get_Entry (Get_Size_Combo (Dialog)))),
           Override);
   end Set;

   -----------------------------
   -- Set_Default_Preferences --
   -----------------------------

   procedure Set_Default_Preferences is
   begin
      if Current_Preferences = null then
         Current_Preferences := new Node;
         Current_Preferences.Tag := new String' ("GVD_Preferences");
      end if;

      Set (Hide_Delay, Guint' (5000));
      Set (Display_Explorer, True);
      Set (String (File_Name_Bg_Color), "#BEBEBE");
      Set (String (Editor_Font), "Courier");
      Set (Editor_Font_Size, Gint' (12));
      Set (Editor_Show_Line_Nums, True);
      Set (Editor_Show_Line_With_Code, True);
      Set (Do_Color_Highlighting, True);
      Set (String (Comments_Color), "#FF0000");
      Set (String (Strings_Color), "#A52A2A");
      Set (String (Keywords_Color), "#0000FF");
      Set (Editor_Highlight_Current_Line, True);
      Set (String (Editor_Highlight_Color), "#00CC00");
      Set (Tab_Size, Gint' (8));
      Set (Tooltips_In_Source, Simple);
      Set (Should_Strip_CR, Need_To_Strip_CR);

      Set (String (Asm_Highlight_Color), "#FF0000");
      Set (String (Assembly_Range_Size), "100");

      Set (Separate_Data, False);
      Set (String (Xref_Color), "#0000FF");
      Set (String (Title_Color), "#BEBEBE");
      Set (String (Change_Color), "#FF0000");
      Set (String (Thaw_Bg_Color), "#FFFFFF");
      Set (String (Freeze_Bg_Color), "#AAAAAA");
      Set (Look_3d, True);
      Set (String (Title_Font), "Helvetica-Bold");
      Set (Title_Font_Size, Default_Font_Size);
      Set (String (Value_Font), "Helvetica");
      Set (Value_Font_Size, Default_Font_Size);
      Set (String (Command_Font), "Courier");
      Set (String (Type_Font), "Helvetica-Oblique");
      Set (Type_Font_Size, Default_Font_Size);
      Set (Annotation_Font_Size, Default_Link_Font_Size);
      Set (Hide_Big_Items, True);
      Set (Big_Item_Height, Gint' (150));
      Set (Default_Detect_Aliases, True);
      Set (Display_Grid, True);
      Set (Align_Items_On_Grid, True);
      Set (String (Debugger_Highlight_Color), "#0000FF");
      Set (String (Debugger_Font), "Courier");
      Set (Debugger_Font_Size, Gint' (12));

      Set (String (Memory_View_Font), "Courier");
      Set (Memory_View_Font_Size, Gint (12));
      Set (String (Memory_View_Color), "#333399");
      Set (String (Memory_Highlighted_Color), "#DDDDDD");
      Set (String (Memory_Selected_Color), "#00009c");
      Set (String (Memory_Modified_Color), "#FF0000");

      Set (String (List_Processes),
           "ps x 2> /dev/null || ps -ef 2> /dev/null || ps");
      Set (String (Default_External_Editor), "glide %f -emacs +%l");
      Set (String (Remote_Protocol), "rsh");
      Set (String (Remote_Copy), "rcp");

      Tab_Size_Cached := Get_Pref (Tab_Size);
   end Set_Default_Preferences;

   -----------------
   -- Fill_Dialog --
   -----------------

   procedure Fill_Dialog (Dialog : General_Preferences_Access) is
   begin
      --  Automatic display of button hints
      Set_Sensitive (Dialog.Label13, False);
      Set_Sensitive (Dialog.Button_Hint_Popup_Check, False);
      Set_Sensitive (Dialog.Button_Hint_Status_Check, False);

      --  Warn if multiple instances of GVD are running
      Set_Sensitive (Dialog.Warn_Multiple_Check, False);

      --  Break on exception
      Set_Sensitive (Dialog.Break_Exception_Check, False);

      --  Status bar timeout
      Set_Text (Dialog.Statusbar_Timeout_Entry,
                Guint'Image (Get_Pref (Hide_Delay)));

      --  Display Explorer Tree
      Set_Active (Dialog.Display_Explorer_Check, Get_Pref (Display_Explorer));

      --  File name background
      Set_Color
        (Dialog.File_Name_Bg_Combo, Get_Pref (File_Name_Bg_Color));

      --  Source font (??? Should handle bold and italic)
      Font_Combo_Select
        (Dialog.Editor_Font_Combo,
         Get_Pref (Editor_Font),
         Bold => False,
         Italic => False,
         Height => Get_Pref (Editor_Font_Size));
      Hide (Get_Bold_Button (Dialog.Editor_Font_Combo));
      Hide (Get_Italic_Button (Dialog.Editor_Font_Combo));

      --  Show line numbers
      Set_Active
        (Dialog.Show_Line_Numbers_Check, Get_Pref (Editor_Show_Line_Nums));

      --  Show lines with code
      Set_Active
        (Dialog.Show_Lines_Code_Check, Get_Pref (Editor_Show_Line_With_Code));

      --  Automatic display of variable values
      --  ??? Should give access to the other values
      Set_Active (Dialog.Tooltips_Check,
                  Get_Pref (Tooltips_In_Source) /= None);

      --  Syntax highlighting
      Set_Active
        (Dialog.Syntax_Highlight_Check, Get_Pref (Do_Color_Highlighting));

      --  Strip Carriage Return
      Set_Active (Dialog.Strip_Cr_Check, Get_Pref (Should_Strip_CR));

      --  Entities color
      Set_Color (Dialog.Comment_Color_Combo, Get_Pref (Comments_Color));
      Set_Color (Dialog.String_Color_Combo, Get_Pref (Strings_Color));
      Set_Color (Dialog.Keyword_Color_Combo, Get_Pref (Keywords_Color));

      --  Current assembly line
      Set_Color (Dialog.Asm_Highlight_Combo, Get_Pref (Asm_Highlight_Color));

      --  Data colors
      Set_Color (Dialog.Xref_Color_Combo, Get_Pref (Xref_Color));
      Set_Color (Dialog.Title_Color_Combo, Get_Pref (Title_Color));
      Set_Color (Dialog.Change_Color_Combo, Get_Pref (Change_Color));
      Set_Color (Dialog.Thaw_Bg_Color_Combo, Get_Pref (Thaw_Bg_Color));
      Set_Color (Dialog.Freeze_Bg_Color_Combo, Get_Pref (Freeze_Bg_Color));

      --  Separate Data
      Set_Active (Dialog.Separate_Data_Check, Get_Pref (Separate_Data));

      --  Look 3d
      Set_Active (Dialog.Look_3d_Check, Get_Pref (Look_3d));

      --  Title font (??? Should handle bold and italic)
      Font_Combo_Select
        (Dialog.Title_Font_Combo,
         Get_Pref (Title_Font),
         Bold => False,
         Italic => False,
         Height => Get_Pref (Title_Font_Size));
      Hide (Get_Bold_Button (Dialog.Title_Font_Combo));
      Hide (Get_Italic_Button (Dialog.Title_Font_Combo));

      --  Value font (??? Should handle bold and italic)
      Font_Combo_Select
        (Dialog.Value_Font_Combo,
         Get_Pref (Value_Font),
         Bold => False,
         Italic => False,
         Height => Get_Pref (Value_Font_Size));
      Hide (Get_Bold_Button (Dialog.Value_Font_Combo));
      Hide (Get_Italic_Button (Dialog.Value_Font_Combo));

      --  Type font (??? Should handle bold and italic)
      Font_Combo_Select
        (Dialog.Type_Font_Combo,
         Get_Pref (Type_Font),
         Bold => False,
         Italic => False,
         Height => Get_Pref (Type_Font_Size));
      Hide (Get_Bold_Button (Dialog.Type_Font_Combo));
      Hide (Get_Italic_Button (Dialog.Type_Font_Combo));

      --  Big items
      Set_Active (Dialog.Hide_Big_Items_Check, Get_Pref (Hide_Big_Items));
      Set_Value (Dialog.Big_Item_Spin, Gfloat (Get_Pref (Big_Item_Height)));
      Set_Sensitive (Dialog.Big_Item_Spin, Get_Pref (Hide_Big_Items));

      --  Detect aliases
      Set_Active
        (Dialog.Detect_Aliases_Check, Get_Pref (Default_Detect_Aliases));

      --  Display grid points
      Set_Active (Dialog.Display_Grid_Check, Get_Pref (Display_Grid));
      Set_Active (Dialog.Align_Grid_Check, Get_Pref (Align_Items_On_Grid));

      --  Command window
      Set_Color
        (Dialog.Debug_Highlight_Combo, Get_Pref (Debugger_Highlight_Color));
      Font_Combo_Select
        (Dialog.Debug_Font_Combo,
         Get_Pref (Debugger_Font),
         Bold => False,
         Italic => False,
         Height => Get_Pref (Debugger_Font_Size));
      Hide (Get_Bold_Button (Dialog.Debug_Font_Combo));
      Hide (Get_Italic_Button (Dialog.Debug_Font_Combo));

      --  Memory window
      Font_Combo_Select
        (Dialog.Memory_Font_Combo,
         Get_Pref (Memory_View_Font),
         Bold => False,
         Italic => False,
         Height => Get_Pref (Memory_View_Font_Size));
      Hide (Get_Bold_Button (Dialog.Memory_Font_Combo));
      Hide (Get_Italic_Button (Dialog.Memory_Font_Combo));
      Set_Color (Dialog.Memory_Default_Combo, Get_Pref (Memory_View_Color));
      Set_Color
        (Dialog.Memory_Highlight_Combo, Get_Pref (Memory_Highlighted_Color));
      Set_Color
        (Dialog.Memory_Selection_Combo, Get_Pref (Memory_Selected_Color));
      Set_Color
        (Dialog.Memory_Modified_Combo, Get_Pref (Memory_Modified_Color));

      --  Helpers
      Set_Text (Dialog.Edit_Source_Entry, Get_Pref (Default_External_Editor));
      Set_Text (Dialog.List_Processes_Entry, Get_Pref (List_Processes));
      Set_Text (Dialog.Remote_Shell_Entry, Get_Pref (Remote_Protocol));
      Set_Text (Dialog.Remote_Copy_Entry, Get_Pref (Remote_Copy));

      Saved_Preferences := Current_Preferences;

      --  ??? Not very efficient, we should fix the Get function to accept
      --  an empty tree.
      Current_Preferences := Deep_Copy (Saved_Preferences);
   end Fill_Dialog;

   ---------------------
   -- Set_From_Dialog --
   ---------------------

   procedure Set_From_Dialog
     (Dialog : General_Preferences_Pkg.General_Preferences_Access) is
   begin
      if Get_Active (Dialog.Tooltips_Check) then
         Set (Tooltips_In_Source, Simple, True);
      else
         Set (Tooltips_In_Source, None, True);
      end if;

      Set (Hide_Delay, Guint'Value (Get_Text (Dialog.Statusbar_Timeout_Entry)),
           True);
      Set (Display_Explorer, Get_Active (Dialog.Display_Explorer_Check), True);
      Set (File_Name_Bg_Color, Get_Color (Dialog.File_Name_Bg_Combo), True);
      Set (Editor_Font, Editor_Font_Size, Dialog.Editor_Font_Combo, True);
      Set (Editor_Show_Line_Nums, Get_Active (Dialog.Show_Line_Numbers_Check),
           True);
      Set (Editor_Show_Line_With_Code,
           Get_Active (Dialog.Show_Lines_Code_Check), True);
      Set (Do_Color_Highlighting, Get_Active (Dialog.Syntax_Highlight_Check),
           True);
      Set (Should_Strip_CR, Get_Active (Dialog.Strip_Cr_Check), True);
      Set (Comments_Color, Get_Color (Dialog.Comment_Color_Combo), True);
      Set (Strings_Color, Get_Color (Dialog.String_Color_Combo), True);
      Set (Keywords_Color, Get_Color (Dialog.Keyword_Color_Combo), True);
      Set (Asm_Highlight_Color, Get_Color (Dialog.Asm_Highlight_Combo), True);
      Set (Separate_Data, Get_Active (Dialog.Separate_Data_Check), True);
      Set (Xref_Color, Get_Color (Dialog.Xref_Color_Combo), True);
      Set (Title_Color, Get_Color (Dialog.Title_Color_Combo), True);
      Set (Change_Color, Get_Color (Dialog.Change_Color_Combo), True);
      Set (Thaw_Bg_Color, Get_Color (Dialog.Thaw_Bg_Color_Combo), True);
      Set (Freeze_Bg_Color, Get_Color (Dialog.Freeze_Bg_Color_Combo), True);
      Set (Look_3d, Get_Active (Dialog.Look_3d_Check), True);
      Set (Title_Font, Title_Font_Size, Dialog.Title_Font_Combo, True);
      Set (Value_Font, Value_Font_Size, Dialog.Value_Font_Combo, True);
      Set (Type_Font, Type_Font_Size, Dialog.Type_Font_Combo, True);
      Set (Hide_Big_Items, Get_Active (Dialog.Hide_Big_Items_Check), True);
      Set (Big_Item_Height, Get_Value_As_Int (Dialog.Big_Item_Spin), True);
      Set (Default_Detect_Aliases, Get_Active (Dialog.Detect_Aliases_Check),
           True);
      Set (Display_Grid, Get_Active (Dialog.Display_Grid_Check), True);
      Set (Align_Items_On_Grid, Get_Active (Dialog.Align_Grid_Check), True);

      Set (Debugger_Highlight_Color, Get_Color (Dialog.Debug_Highlight_Combo),
           True);
      Set (Debugger_Font, Debugger_Font_Size, Dialog.Debug_Font_Combo, True);

      Set (Memory_View_Font, Memory_View_Font_Size, Dialog.Memory_Font_Combo,
           True);
      Set (Memory_View_Color, Get_Color (Dialog.Memory_Default_Combo), True);
      Set (Memory_Highlighted_Color, Get_Color (Dialog.Memory_Highlight_Combo),
           True);
      Set (Memory_Selected_Color, Get_Color (Dialog.Memory_Selection_Combo),
           True);
      Set (Memory_Modified_Color, Get_Color (Dialog.Memory_Modified_Combo),
           True);

      Set (String (Default_External_Editor),
           Get_Chars (Dialog.Edit_Source_Entry), True);
      Set (String (List_Processes), Get_Chars (Dialog.List_Processes_Entry),
           True);
      Set (String (Remote_Protocol), Get_Chars (Dialog.Remote_Shell_Entry),
           True);
      Set (String (Remote_Copy), Get_Chars (Dialog.Remote_Copy_Entry), True);
   end Set_From_Dialog;

   -----------------------
   -- Apply_Preferences --
   -----------------------

   procedure Apply_Preferences
     (Dialog : General_Preferences_Pkg.General_Preferences_Access) is
   begin
      Set_From_Dialog (Dialog);
   end Apply_Preferences;

   ---------------------
   -- Set_Preferences --
   ---------------------

   procedure Set_Preferences
     (Dialog : General_Preferences_Pkg.General_Preferences_Access) is
   begin
      Set_From_Dialog (Dialog);
      Free (Saved_Preferences);
   end Set_Preferences;

   ------------------------
   -- Cancel_Preferences --
   ------------------------

   procedure Cancel_Preferences
     (Dialog : General_Preferences_Pkg.General_Preferences_Access) is
   begin
      Free (Current_Preferences);
      Current_Preferences := Saved_Preferences;
   end Cancel_Preferences;

begin
   --  Initialize the default values
   Set_Default_Preferences;
end GVD.Preferences;
