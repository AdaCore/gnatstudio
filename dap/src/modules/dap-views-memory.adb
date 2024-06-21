------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                     Copyright (C) 2023, AdaCore                          --
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

with Ada.Characters.Handling;  use Ada.Characters.Handling;
with Ada.Streams;
with Ada.Strings.Fixed;        use Ada.Strings.Fixed;
with Ada.Strings.Maps;         use Ada.Strings.Maps;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Ada.Text_IO;
with Ada.Unchecked_Deallocation;

with GNAT.Strings;             use GNAT.Strings;

with GNATCOLL.Coders.Base64;
with GNATCOLL.Utils;           use GNATCOLL.Utils;

with Glib;                     use Glib;
with Glib.Properties;          use Glib.Properties;
with Glib.Values;              use Glib.Values;

with Pango.Font;               use Pango.Font;

with Gdk.Event;                use Gdk.Event;
with Gdk.RGBA;                 use Gdk.RGBA;
with Gdk.Types.Keysyms;        use Gdk.Types.Keysyms;
with Gdk.Window;               use Gdk.Window;

with Gtk;                      use Gtk;
with Gtk.Adjustment;           use Gtk.Adjustment;
with Gtk.Arguments;            use Gtk.Arguments;
with Gtk.Arrow;                use Gtk.Arrow;
with Gtk.Button;               use Gtk.Button;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Check_Button;         use Gtk.Check_Button;
with Gtk.Combo_Box;
with Gtk.Combo_Box_Text;       use Gtk.Combo_Box_Text;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Flow_Box;             use Gtk.Flow_Box;
with Gtk.GEntry;               use Gtk.GEntry;
with Gtk.Handlers;
with Gtk.Hbutton_Box;          use Gtk.Hbutton_Box;
with Gtk.Label;                use Gtk.Label;
with Gtk.Separator;            use Gtk.Separator;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Spin_Button;          use Gtk.Spin_Button;
with Gtk.Toggle_Button;        use Gtk.Toggle_Button;
with Gtk.Text_Buffer;          use Gtk.Text_Buffer;
with Gtk.Text_Iter;            use Gtk.Text_Iter;
with Gtk.Text_Mark;            use Gtk.Text_Mark;
with Gtk.Text_Tag_Table;       use Gtk.Text_Tag_Table;
with Gtk.Text_Tag;             use Gtk.Text_Tag;
with Gtk.Text_View;            use Gtk.Text_View;
with Gtk.Viewport;             use Gtk.Viewport;
with Gtk.Widget;               use Gtk.Widget;

with Gtkada.Handlers;          use Gtkada.Handlers;
with Gtkada.MDI;               use Gtkada.MDI;
with Gtkada.Types;

with VSS.Strings.Conversions;

with GPS.Kernel.Actions;
with GPS.Kernel.Contexts;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;

with Commands.Interactive;     use Commands, Commands.Interactive;

with DAP.Clients;              use DAP.Clients;
with DAP.Clients.Evaluate;
with DAP.Module;
with DAP.Contexts;             use DAP.Contexts;
with DAP.Modules.Preferences;  use DAP.Modules.Preferences;
with DAP.Requests;             use DAP.Requests;
with DAP.Tools;
with DAP.Types;                use DAP.Types;
with DAP.Utils;                use DAP.Utils;

with DAP.Requests.ReadMemory;
with DAP.Requests.WriteMemory;

package body DAP.Views.Memory is

   type Display_Type is (Hex, Decimal, Octal, Text);
   --  The current display mode
   --  Note that any change in this type needs to be coordinated in
   --  Update_Display.

   type Data_Size is (Byte, Halfword, Word);
   --  The size of the data to display
   --  Note that any change in this type needs to be coordinated in
   --  Update_Display.

   Dump_Item_Size : constant := 16;
   --  Expected size of an memory dump line

   -- Memory_Dump_Item --

   type Memory_Dump_Item is record
      Label : Ada.Strings.Unbounded.Unbounded_String;
      Value : Ada.Strings.Unbounded.Unbounded_String;
   end record;
   --  Element of memody dump. Value should be a hexadecimal string with no
   --  separator. Label is optional symbol corresponding to first byte of
   --  Value.

   type Memory_Dump is array (Positive range <>) of Memory_Dump_Item;

   type Memory_Dump_Access is access all Memory_Dump;

   -- View_Record --

   type View_Record is new Gtk_Vbox_Record with record
      Pgup           : Gtk_Button;
      Pgdn           : Gtk_Button;
      Viewport       : Gtk_Viewport;
      View           : Gtk_Text_View;
      Reset          : Gtk_Button;
      Submit         : Gtk_Button;
      Close          : Gtk_Button;
      Address_View   : Gtk_Button;
      Format         : Gtk_Combo_Box_Text;
      Show_Ascii     : Gtk_Check_Button;
      Size           : Gtk_Combo_Box_Text;
      Address_Entry  : Gtk_Entry;
      Lines_Spin     : Gtk_Spin_Button;
   end record;
   type View_Access is access all View_Record'Class;

   procedure Gtk_New (View : out View_Access);
   procedure Initialize (View : access View_Record'Class);

   -- DAP_Memory_View_Record --

   type DAP_Memory_View_Record is new DAP.Views.View_Record with
      record
         Editor            : View_Access;

         Display           : Display_Type := Hex;
         --  The current display mode.

         Data              : Data_Size := Byte;
         --  The size of data to display;

         Starting_Address  : Long_Long_Integer := 0;
         --  The first address that is being explored.

         Dump              : Memory_Dump_Access;
         --  Dump of memory returned by Debugger

         Label_Length      : Natural;
         --  Length of labels printed after address

         Old_Values        : GNAT.Strings.String_Access;
         --  The data used to set markers on the values.
         --  This is a string of hexadecimal digits.

         New_Values        : GNAT.Strings.String_Access;
         --  The values that are to be shown in the window.
         --  A string of the same size as Old_Values.

         Edit_Mode         : Boolean := False;
         --  Edit_Mode is False till user starts to edit memory.
         --  If Edit_Mode then Old_Values represent actual values in memory,
         --  and New_Values includes user input not applied to memory yet.
         --  If Edit_Mode = False then Old_Values holds previous value
         --  and New_Values has actual values in memory.

         Number_Of_Bytes   : Integer := 256;
         --  The size of the pages that are currently stored.

         Number_Of_Columns : Integer := 16;
         --  The number of columns that are to be displayed.

         Unit_Size         : Integer := 2;
         --  The size, in number of elements from Values, of the current
         --  grouping unit (ie 2 for Bytes, 4 for Halfword, 8 for Word....)

         Trunc             : Integer;
         --  The size of a separate element in the view (ie 2 for a Byte
         --  displayed in Hex, 3 for a Byte displayed in Decimal ...)

         Default_Tag       : Gtk_Text_Tag;
         --  Tag used for the default text

         Modified_Tag      : Gtk_Text_Tag;
         --  Tag used to display modified chunks

         Address_Tag       : Gtk_Text_Tag;
         --  Tag used to display addresses

         Editable_Tag      : Gtk_Text_Tag;
         --  Tag used to display some text that could be modified by the user
      end record;
   type DAP_Memory_View is access all DAP_Memory_View_Record'Class;

   overriding procedure On_Process_Terminated
     (View : not null access DAP_Memory_View_Record);
   overriding procedure Update (View : not null access DAP_Memory_View_Record);

   function Initialize
     (Widget : access DAP_Memory_View_Record'Class) return Gtk_Widget;
   --  Internal initialization function
   --  Returns the focus child

   function Get_View
     (Client : not null access DAP.Clients.DAP_Client'Class)
      return access DAP_Memory_View_Record'Class;
   --  Store or retrieve the view from the client

   package Memory_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name                     => "Memory_View",
      View_Name                       => "Memory",
      Formal_View_Record              => DAP_Memory_View_Record,
      Formal_MDI_Child                => GPS_MDI_Child_Record,
      Reuse_If_Exist                  => False,
      Save_Duplicates_In_Perspectives => False,
      Commands_Category               => "",
      Areas                           => Gtkada.MDI.Sides_Only,
      Group                           => Group_Debugger_Stack,
      Position                        => Position_Bottom,
      Initialize                      => Initialize);
   package Simple_Views is new DAP.Views.Simple_Views
     (Formal_Views       => Memory_MDI_Views,
      Formal_View_Record => DAP_Memory_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record);

   procedure Display_Memory
     (View    : access DAP_Memory_View_Record'Class;
      Address : Long_Long_Integer);
   --  Display the contents of the memory into the text area.

   procedure Display_Memory
     (View    : access DAP_Memory_View_Record'Class;
      Address : String);
   --  Display the contents of the memory into the text area.
   --  Address is a string that represents an address in hexadecimal,
   --  it should be made of the "0x" prefix followed by hexadecimal.

   procedure Fill_Values
     (View    : access DAP_Memory_View_Record'Class;
      Address : Long_Long_Integer := 0);

   procedure Apply_Changes (View : access DAP_Memory_View_Record'Class);
   --  Write the changes into memory.

   procedure Page_Down (View : access DAP_Memory_View_Record'Class);
   procedure Page_Up (View : access DAP_Memory_View_Record'Class);
   --  Move up or down one page in the view.

   type View_Memory_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access View_Memory_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   procedure Init_Graphics
     (View : access DAP_Memory_View_Record'Class);
   --  Initialize fonts and graphics used for this widget.

   procedure Update_Display (View : access DAP_Memory_View_Record'Class);
   --  Refreshes the view.

   type Dir is (Up, Down, Left, Right);
   procedure Move_Cursor
     (View  : access DAP_Memory_View_Record'Class;
      Where : Dir);
   --  Moves the cursor.

   procedure Insert
     (View : access DAP_Memory_View_Record'Class;
      Char : String);
   --  Inserts string at the current location.

   procedure Start_Editing (View : access DAP_Memory_View_Record'Class);
   --  Move View to Edit_Mode

   procedure Stop_Editing (View : access DAP_Memory_View_Record'Class);
   --  Leave View from Edit_Mode

   procedure Watch_Cursor_Location
     (View : access DAP_Memory_View_Record'Class);
   --  Makes sure the cursor is within the editable area.

   procedure On_Address_Entry_Activate
     (Object : access Gtk_Widget_Record'Class);
   procedure On_Address_View_Clicked
     (Object : access Gtk_Widget_Record'Class);
   procedure On_Size_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);
   procedure On_Data_Entry_Changed
     (Object : access Gtk_Widget_Record'Class);
   procedure On_Show_Ascii_Toggled
     (Object : access Gtk_Widget_Record'Class);
   procedure On_Pgup_Clicked
     (Object : access Gtk_Widget_Record'Class);
   procedure On_Pgdn_Clicked
     (Object : access Gtk_Widget_Record'Class);
   function On_View_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : GValues) return Boolean;
   function On_View_Button_Release_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean;
   procedure On_Reset_Clicked
     (Object : access Gtk_Widget_Record'Class);
   procedure On_Submit_Clicked (Object : access Gtk_Widget_Record'Class);
   function On_Button_Release
     (Object : access Gtk_Widget_Record'Class) return Boolean;
   --  Callbacks for the various buttons

   procedure Free is new Ada.Unchecked_Deallocation
     (Memory_Dump, Memory_Dump_Access);

   --------------------
   -- Local packages --
   --------------------

   package Long_Int_IO is new Ada.Text_IO.Integer_IO (Long_Long_Integer);
   use Long_Int_IO;

   ---------------------
   -- Local constants --
   ---------------------

   Address_Length       : constant Integer := 16;
   Address_Separator    : constant String  := ": ";
   Data_Separator       : constant String  := " ";
   ASCII_Separator      : constant String  := " ";
   Data_ASCII_Separator : constant String  := " -  ";

   Non_Valid_Character  : constant String  := "-";

   End_Of_Line       : constant String  := (1 => ASCII.LF);
   Hex_Header        : constant String  := "16#";
   Hex_Footer        : constant String  := "#";

   Line_Base_Size    : constant Integer := 16;
   --  Number of bytes per line.

   procedure Clear_View (View : access DAP_Memory_View_Record'Class);
   --  Removes everything from the view.

   procedure Insert_ASCII (View : access DAP_Memory_View_Record'Class);
   --  Insert the ASCII representation of the memory shown on the current line.

   function Conversion
     (S        : String;
      Size     : Integer;
      Format   : Display_Type;
      Trunc_At : Integer;
      Is_ASCII : Boolean := False) return String;
   --  Converts a string of hexadecimal digits into a string representing
   --  the same number in Format, with a constant size.
   --  If Is_ASCII and the conversion failed returns Non_Valid_Character

   procedure Get_Coordinates
    (View     : access DAP_Memory_View_Record'Class;
     Position : Gint;
     Row      : out Integer;
     Column   : out Integer);
   --  Gives the bloc coordinates from a given position.

   function Position_To_Bloc
     (View     : access DAP_Memory_View_Record'Class;
      Position : Gint) return Integer;
   --  Gives the bloc number at the given position.

   function To_Standard_Base
     (Address  : Long_Long_Integer;
      Base     : Integer;
      Trunc_At : Integer := -1) return String;
   --  Conversion from a Long_Long_Integer to a based representation.
   --  Output is truncated to Trunc_At characters if Trunc_At /= -1.

   procedure Swap_Blocks
     (View : access DAP_Memory_View_Record'Class; Size : Data_Size);
   --  Swap blocks of size Size in the View's values to swap endianness.

   ------------------
   -- Read_Request --
   ------------------

   type Read_Request is
     new DAP.Requests.ReadMemory.Read_Memory_DAP_Request with record
      Address : Long_Long_Integer;
   end record;
   type Read_Request_Access is access all Read_Request;

   overriding procedure On_Result_Message
     (Self        : in out Read_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.ReadMemoryResponse;
      New_Request : in out DAP_Request_Access);

   overriding procedure On_Error_Message
     (Self    : in out Read_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String);

   -------------------
   -- Write_Request --
   -------------------

   type Write_Request is
     new DAP.Requests.WriteMemory.Write_Memory_DAP_Request with null record;
   type Write_Request_Access is access all Write_Request;

   overriding procedure On_Result_Message
     (Self        : in out Write_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.WriteMemoryResponse;
      New_Request : in out DAP_Request_Access) is null;

   ---------------------------
   -- On_Process_Terminated --
   ---------------------------

   overriding procedure On_Process_Terminated
     (View : not null access DAP_Memory_View_Record) is
   begin
      Clear_View (View);
   end On_Process_Terminated;

   -----------------
   -- Update_View --
   -----------------

   procedure Update_View
     (Client : not null access DAP.Clients.DAP_Client'Class)
   is
      View : constant DAP_Memory_View := Get_View (Client);
   begin
      if View /= null then
         View.Update;
      end if;
   end Update_View;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Client : not null access DAP.Clients.DAP_Client'Class)
      return access DAP_Memory_View_Record'Class is
   begin
      return Memory_MDI_Views.Retrieve_View (Client.Kernel);
   end Get_View;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Widget : access DAP_Memory_View_Record'Class) return Gtk_Widget is
   begin
      Gtk.Box.Initialize_Hbox (Widget);
      Gtk_New (Widget.Editor);
      Pack_Start (Widget, Widget.Editor, True, True);
      Init_Graphics (Widget);

      Widget_Callback.Object_Connect
        (Widget.Editor.Address_Entry, Gtk.GEntry.Signal_Activate,
         Widget_Callback.To_Marshaller (On_Address_Entry_Activate'Access),
         Widget);
      Widget_Callback.Object_Connect
        (Widget.Editor.Address_View, Signal_Clicked,
         Widget_Callback.To_Marshaller (On_Address_View_Clicked'Access),
         Widget);
      Widget_Callback.Object_Connect
        (Widget.Editor.Size, Gtk.Combo_Box.Signal_Changed,
         Widget_Callback.To_Marshaller (On_Size_Entry_Changed'Access),
         Widget);
      Widget_Callback.Object_Connect
        (Widget.Editor.Format, Gtk.Combo_Box.Signal_Changed,
         Widget_Callback.To_Marshaller (On_Data_Entry_Changed'Access),
         Widget);
      Widget_Callback.Object_Connect
        (Widget.Editor.Show_Ascii, Signal_Toggled,
         Widget_Callback.To_Marshaller (On_Show_Ascii_Toggled'Access),
         Widget);
      Widget_Callback.Object_Connect
        (Widget.Editor.Pgup, Signal_Clicked,
         Widget_Callback.To_Marshaller (On_Pgup_Clicked'Access), Widget);
      Widget_Callback.Object_Connect
        (Widget.Editor.Pgdn, Signal_Clicked,
         Widget_Callback.To_Marshaller (On_Pgdn_Clicked'Access), Widget);
      Return_Callback.Object_Connect
        (Widget.Editor.View, Signal_Key_Press_Event,
         On_View_Key_Press_Event'Access, Widget);
      Widget_Callback.Object_Connect
        (Widget.Editor.Submit, Signal_Clicked,
         Widget_Callback.To_Marshaller (On_Submit_Clicked'Access), Widget);
      Return_Callback.Object_Connect
        (Widget.Editor.View, Signal_Button_Release_Event,
         On_View_Button_Release_Event'Access, Widget);
      Widget_Callback.Object_Connect
        (Widget.Editor.Reset, Signal_Clicked,
         Widget_Callback.To_Marshaller (On_Reset_Clicked'Access), Widget);
      Return_Callback.Object_Connect
        (Gtk_Entry (Widget.Editor.Lines_Spin), Signal_Button_Release_Event,
         On_Button_Release'Access, Widget);

      Show_All (Widget);
      return Gtk_Widget (Widget.Editor);
   end Initialize;

   -------------------
   -- Apply_Changes --
   -------------------

   procedure Apply_Changes (View : access DAP_Memory_View_Record'Class) is
      use Ada.Streams;

      Client : constant DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;

      subtype Hex_String is String (1 .. 2);
      function To_Binary (Hex : Hex_String) return Stream_Element;
      function To_Binary (Hex : Hex_String) return Stream_Element is
         function Digit (C : Character) return Stream_Element;
         function Digit (C : Character) return Stream_Element is
         begin
            case C is
               when '0' .. '9' =>
                  return Character'Pos (C) - Character'Pos ('0');

               when 'A' .. 'F' =>
                  return Character'Pos (C) - Character'Pos ('A') + 10;

               when 'a' .. 'f' =>
                  return Character'Pos (C) - Character'Pos ('a') + 10;

               when others =>
                  raise Constraint_Error with
                    "Wrong character '" & C & "' in Apply_Changes";
            end case;
         end Digit;

      begin
         return Digit (Hex (Hex'First)) * 16 + Digit (Hex (Hex'First + 1));
      end To_Binary;

      Src   : Stream_Element_Array (1 .. 1);
      Index : Stream_Element_Offset := Src'First;
      Dest  : Stream_Element_Array (1 .. 4);
      Last  : Stream_Element_Offset;

      Base64_Encoder : GNATCOLL.Coders.Base64.Encoder_Type;
      Request        : Write_Request_Access;
   begin
      if Client.Get_Endian_Type = Little_Endian then
         Swap_Blocks (View, View.Data);
      end if;

      for J in 1 .. View.Number_Of_Bytes loop
         if View.New_Values (J * 2 - 1 .. J * 2) /=
           View.Old_Values (J * 2 - 1 .. J * 2)
         then
            Src (1) := To_Binary (View.New_Values (J * 2 - 1 .. J * 2));
            Base64_Encoder.Initialize;
            Base64_Encoder.Transcode
              (In_Data  => Src,
               In_Last  => Index,
               Out_Data => Dest,
               Out_Last => Last,
               Flush    => GNATCOLL.Coders.Finish);

            declare
               D   : constant Stream_Element_Array (1 .. Last) :=
                 Dest (1 .. Last);
               Str : constant String
                 (1 .. Integer (Last * Stream_Element'Size) / Character'Size)
                   with Import;
               for Str'Address use D'Address;
            begin
               Request := new Write_Request (View.Kernel);
               Request.Parameters.arguments.memoryReference :=
                 VSS.Strings.Conversions.To_Virtual_String
                   ("0x" & To_Standard_Base
                      (View.Starting_Address + Long_Long_Integer (J - 1),
                       16));
               Request.Parameters.arguments.allowPartial := True;
               Request.Parameters.arguments.data :=
                 VSS.Strings.Conversions.To_Virtual_String (Str);
               Client.Enqueue (DAP.Requests.DAP_Request_Access (Request));
            end;

            Base64_Encoder.Close;

            View.Old_Values (J * 2 - 1 .. J * 2) :=
              View.New_Values (J * 2 - 1 .. J * 2);
         end if;
      end loop;

      Stop_Editing (View);
      Display_Memory (View, View.Starting_Address);
   end Apply_Changes;

   ----------------
   -- Clear_View --
   ----------------

   procedure Clear_View (View : access DAP_Memory_View_Record'Class) is
      Buffer     : constant Gtk_Text_Buffer := Get_Buffer (View.Editor.View);
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
   begin
      Get_Bounds (Buffer, Start_Iter, End_Iter);
      Delete (Buffer, Start_Iter, End_Iter);
      Stop_Editing (View);
   end Clear_View;

   -------------------
   -- Init_Graphics --
   -------------------

   procedure Init_Graphics
     (View : access DAP_Memory_View_Record'Class)
   is
      Buffer    : constant Gtk_Text_Buffer := Get_Buffer (View.Editor.View);
      Tag_Table : constant Gtk_Text_Tag_Table := Get_Tag_Table (Buffer);
      Font      : constant Pango_Font_Description :=
        Default_Style.Get_Pref_Font;
   begin
      --  Tag used to display not modified memory
      Gtk_New (View.Default_Tag);
      Set_Property (View.Default_Tag, Background_Rgba_Property, Null_RGBA);
      Set_Property (View.Default_Tag, Foreground_Rgba_Property, Null_RGBA);
      Set_Property (View.Default_Tag, Font_Desc_Property, Font);
      Add (Tag_Table, View.Default_Tag);

      --  Tag used to display modified memory
      Gtk_New (View.Modified_Tag);
      Set_Property (View.Modified_Tag, Background_Rgba_Property, Null_RGBA);
      Set_Property (View.Modified_Tag, Foreground_Rgba_Property,
                    Blocks_Style.Get_Pref_Fg);
      Set_Property (View.Modified_Tag, Font_Desc_Property, Font);
      Add (Tag_Table, View.Modified_Tag);

      --  Tag used to display memory addresses
      Gtk_New (View.Address_Tag);
      Set_Property (View.Address_Tag, Background_Rgba_Property,
                    Memory_Highlighted_Color.Get_Pref);
      Set_Property (View.Address_Tag, Foreground_Rgba_Property,
                    Memory_View_Color.Get_Pref);
      Set_Property (View.Address_Tag, Font_Desc_Property, Font);
      Set_Property (View.Address_Tag, Text_Tag.Editable_Property, False);
      Add (Tag_Table, View.Address_Tag);

      --  Tag used to display editable text
      Gtk_New (View.Editable_Tag);
      Set_Property (View.Editable_Tag, Text_Tag.Editable_Property, True);
      Add (Tag_Table, View.Editable_Tag);
   end Init_Graphics;

   --------------------
   -- Display_Memory --
   --------------------

   procedure Display_Memory
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Address : String)
   is
      Client : constant DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
      View   : DAP_Memory_View;

   begin
      if Client /= null then
         Simple_Views.Attach_To_View
           (Client, Kernel, Create_If_Necessary => True);
         View := DAP_Memory_View (Get_View (Client));
         Display_Memory (View, Address);
      end if;
   end Display_Memory;

   -------------
   -- Page_Up --
   -------------

   procedure Page_Up (View : access DAP_Memory_View_Record'Class) is
   begin
      Display_Memory
        (View, View.Starting_Address -
           Long_Long_Integer
             (Integer (Get_Value_As_Int (View.Editor.Lines_Spin))
              * Line_Base_Size));
   end Page_Up;

   ---------------
   -- Page_Down --
   ---------------

   procedure Page_Down (View : access DAP_Memory_View_Record'Class) is
   begin
      Display_Memory
        (View, View.Starting_Address +
           Long_Long_Integer
             (Integer (Get_Value_As_Int (View.Editor.Lines_Spin))
              * Line_Base_Size));
   end Page_Down;

   --------------------
   -- Update_Display --
   --------------------

   procedure Update_Display (View : access DAP_Memory_View_Record'Class) is
      Buffer          : constant Gtk_Text_Buffer :=
        Get_Buffer (View.Editor.View);
      Number_Of_Lines : constant Integer :=
        Integer (Get_Value_As_Int (View.Editor.Lines_Spin));
      Endianness      : constant Endian_Type :=
        Get_Client (View).Get_Endian_Type;
      Old_Size        : constant Data_Size := View.Data;
      Index           : Integer;
      Tag             : Gtk_Text_Tag;
      Start_Mark      : Gtk_Text_Mark;
      Start_Iter      : Gtk_Text_Iter;
      End_Iter        : Gtk_Text_Iter;

   begin
      if View.Old_Values = null then
         return;
      end if;

      --  Use if/elsif statements instead of 'Value here to handle
      --  internationalization of strings properly.

      declare
         Size : constant String := Get_Active_Text (View.Editor.Size);
         Data : constant String := Get_Active_Text (View.Editor.Format);
      begin
         if Size = "Byte" then
            View.Data := Byte;
         elsif Size = "Halfword" then
            View.Data := Halfword;
         elsif Size = "Word" then
            View.Data := Word;
         else
            --  May happen if e.g. Size is null, while its value is being
            --  updated.

            return;
         end if;

         if Data = "Hex" then
            View.Display := Hex;
         elsif Data = "Decimal" then
            View.Display := Decimal;
         elsif Data = "Octal" then
            View.Display := Octal;
         elsif Data = "ASCII" then
            View.Display := Text;
         else
            return;
         end if;
      end;

      if Endianness = Little_Endian
        and then Old_Size /= View.Data
      then
         --  Swap back to original.
         Swap_Blocks (View, Old_Size);

         --  Swap again to new size.
         Swap_Blocks (View, View.Data);
      end if;

      case View.Data is
         when Byte =>
            View.Unit_Size := 2;
         when Halfword =>
            View.Unit_Size := 4;
         when Word =>
            View.Unit_Size := 8;
      end case;

      case View.Display is
         when Hex =>
            View.Trunc := View.Unit_Size;
         when Text =>
            View.Trunc := View.Unit_Size / 2;
         when Decimal =>
            View.Trunc := Integer (Float (View.Unit_Size) * 1.2 + 0.5);
         when Octal =>
            View.Trunc := View.Unit_Size * 2;
      end case;

      View.Number_Of_Columns := Line_Base_Size * 2 / View.Unit_Size;

      if Number_Of_Lines * View.Number_Of_Columns * View.Unit_Size >
        View.Old_Values'Length
      then
         Display_Memory (View, View.Starting_Address);
         return;
      end if;

      Begin_User_Action (Buffer);
      Clear_View (View);
      Index := 1;

      for Line_Index in 1 .. Number_Of_Lines loop
         Get_Iter_At_Mark (Buffer, End_Iter, Get_Insert (Buffer));
         Start_Mark := Create_Mark (Buffer, Where => End_Iter);

         --  Insert address and label
         Insert
           (Buffer,
            End_Iter,
            To_Standard_Base
              (View.Starting_Address +
                 Long_Long_Integer
                   ((Line_Index - 1) * View.Number_Of_Columns *
                      View.Unit_Size / 2),
               16, Address_Length)
            & ' ' & Ada.Strings.Unbounded.To_String
              (View.Dump (Line_Index).Label)
            & Address_Separator);

         Get_Iter_At_Mark (Buffer, Start_Iter, Start_Mark);

         Apply_Tag (Buffer, View.Address_Tag, Start_Iter, End_Iter);
         Delete_Mark (Buffer, Start_Mark);
         Place_Cursor (Buffer, End_Iter);

         for Column_Index in 1 .. View.Number_Of_Columns loop
            Index := (Line_Index - 1) *
              View.Number_Of_Columns * View.Unit_Size
              + (Column_Index - 1) * View.Unit_Size + 1;

            if View.Old_Values (Index .. Index + View.Unit_Size - 1) /=
              View.New_Values (Index .. Index + View.Unit_Size - 1)
            then
               Tag := View.Modified_Tag;
            else
               Tag := View.Default_Tag;
            end if;

            Get_Iter_At_Mark (Buffer, End_Iter, Get_Insert (Buffer));
            Start_Mark := Create_Mark (Buffer, Where => End_Iter);

            Insert
              (Buffer,
               End_Iter,
               Conversion
                 (View.New_Values (Index .. Index + View.Unit_Size - 1),
                  View.Unit_Size,
                  View.Display,
                  View.Trunc));

            Get_Iter_At_Mark (Buffer, Start_Iter, Start_Mark);
            Apply_Tag (Buffer, Tag, Start_Iter, End_Iter);
            Apply_Tag (Buffer, View.Editable_Tag, Start_Iter, End_Iter);
            Delete_Mark (Buffer, Start_Mark);
            Place_Cursor (Buffer, End_Iter);

            Get_Iter_At_Mark (Buffer, End_Iter, Get_Insert (Buffer));
            Start_Mark := Create_Mark (Buffer, Where => End_Iter);

            Insert (Buffer, End_Iter, Data_Separator);
            Get_Iter_At_Mark (Buffer, Start_Iter, Start_Mark);
            Delete_Mark (Buffer, Start_Mark);
            Place_Cursor (Buffer, End_Iter);
         end loop;

         if Get_Active (View.Editor.Show_Ascii) then
            Insert_ASCII (View);
         end if;

         if Line_Index /= Number_Of_Lines then
            Insert_At_Cursor (Buffer, End_Of_Line);
         end if;
      end loop;

      --  This manual refresh worksaround a problem seen on the win32 back-end
      --  of Gtk 2.14.7. If the format is changed through the combo pulldown
      --  menu, then only the pulldown area gets refreshed instead of the
      --  whole view.
      --  ??? Revisit with newest Gtk+ versions
      declare
         X_Box      : Gint;
         Y_Box      : Gint;
         Width_Box  : Gint;
         Height_Box : Gint;
      begin
         Gdk.Window.Get_Geometry
           (View.Get_Window, X_Box, Y_Box, Width_Box, Height_Box);

         Gdk.Window.Invalidate_Rect
           (View.Get_Window,
            (X      => 0,
             Y      => 0,
             Width  => Width_Box,
             Height => Height_Box),
            True);
      end;

      End_User_Action (Buffer);
   end Update_Display;

   --------------------
   -- Display_Memory --
   --------------------

   procedure Display_Memory
     (View    : access DAP_Memory_View_Record'Class;
      Address : Long_Long_Integer)
   is
      Client : constant DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
      Number_Of_Lines : constant Integer :=
        Integer (Get_Value_As_Int (View.Editor.Lines_Spin));
      Req    : Read_Request_Access;

   begin
      View.Number_Of_Columns := Line_Base_Size * 2 / View.Unit_Size;

      if View.Old_Values = null
        or else Number_Of_Lines * View.Number_Of_Columns * View.Unit_Size
          /= View.Old_Values'Length
      then
         View.Number_Of_Bytes := Number_Of_Lines * View.Number_Of_Columns
           * View.Unit_Size / 2;
      end if;

      Free (View.Dump);

      Req := new Read_Request (View.Kernel);
      Req.Address := Address;
      Req.Parameters.arguments.memoryReference :=
        VSS.Strings.Conversions.To_Virtual_String
          ("0x" & To_Standard_Base (Address, 16));
      Req.Parameters.arguments.count := View.Number_Of_Bytes;

      Client.Enqueue (DAP.Requests.DAP_Request_Access (Req));
   end Display_Memory;

   ----------------------
   -- On_Error_Message --
   ----------------------

   overriding procedure On_Error_Message
     (Self    : in out Read_Request;
      Client  : not null access DAP.Clients.DAP_Client'Class;
      Message : VSS.Strings.Virtual_String)
   is
      View : constant DAP_Memory_View := DAP_Memory_View
        (Memory_MDI_Views.Retrieve_View (Self.Kernel));
   begin
      if Self.Address = 0
        and then View /= null
      then
         View.Fill_Values;
      end if;

      DAP.Requests.ReadMemory.On_Error_Message
        (DAP.Requests.ReadMemory.Read_Memory_DAP_Request (Self),
         Client, Message);
   end On_Error_Message;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Read_Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : DAP.Tools.ReadMemoryResponse;
      New_Request : in out DAP_Request_Access)
   is
      use Ada.Streams;
      View : constant DAP_Memory_View := DAP_Memory_View
        (Memory_MDI_Views.Retrieve_View (Self.Kernel));

      Dump_Index : Integer := 1;
      Total      : Integer := 0;

      -- Swap --
      procedure Swap;
      procedure Swap
      is
         L : constant Natural := Length (View.Dump (Dump_Index).Value);
      begin
         if Client.Get_Endian_Type = Little_Endian then
            declare
               Src : constant String := Slice
                 (View.Dump (Dump_Index).Value, L - 15, L);
               Dst : String (1 .. 16);
            begin
               for J in 1 .. 8 loop
                  Dst (J * 2 - 1 .. J * 2) :=
                    Src (Src'Last - J * 2 + 1 .. Src'Last - J * 2 + 2);
               end loop;

               Ada.Strings.Unbounded.Replace_Slice
                 (View.Dump (Dump_Index).Value, L - 15, L, Dst);
            end;
         end if;
      end Swap;

   begin
      New_Request := null;
      if View = null then
         return;
      end if;

      View.Dump := new Memory_Dump
        (1 .. (View.Number_Of_Bytes + Dump_Item_Size - 1) /
             Dump_Item_Size);

      if Result.a_body.Is_Set then
         declare
            Base64_Decoder : GNATCOLL.Coders.Base64.Decoder_Type;
            Str     : constant String := To_UTF8 (Result.a_body.Value.data);
            Src     : Stream_Element_Array (1 .. Str'Length) with Import;
            for Src'Address use Str'Address;
            Index   : Stream_Element_Offset := Src'First;
            Dest    : Stream_Element_Array (1 .. 4096);
            Last    : Stream_Element_Offset;
            Count   : Integer := 0;

            function To_Hex_String (Num : Stream_Element) return String;
            function To_Hex_String (Num : Stream_Element) return String is
               Hex_Digit : constant array
                 (Stream_Element range 0 .. 15) of Character :=
                 "0123456789ABCDEF";
               Result : String (1 .. 2);
               Value  : Stream_Element := Num;
            begin
               for J in reverse Result'Range loop
                  Result (J) := Hex_Digit (Value mod 16);
                  Value := Value / 16;
               end loop;

               return Result;
            end To_Hex_String;

         begin
            Base64_Decoder.Initialize;
            Base64_Decoder.Transcode
              (In_Data  => Src,
               In_Last  => Index,
               Out_Data => Dest,
               Out_Last => Last,
               Flush    => GNATCOLL.Coders.Finish);
            Base64_Decoder.Close;

            Index := Dest'First;
            while Index <= Last loop
               Append
                 (View.Dump (Dump_Index).Value,
                  To_Hex_String (Dest (Index)));
               Index := Index + 1;
               Count := Count + 1;
               Total := Total + 1;

               if Count = 8 then
                  Swap;
               elsif Count = 16 then
                  Swap;
                  Dump_Index := Dump_Index + 1;
                  Count := 0;
               end if;
            end loop;
         end;
      end if;

      View.Fill_Values (Self.Address);
   end On_Result_Message;

   -----------------
   -- Fill_Values --
   -----------------

   procedure Fill_Values
     (View    : access DAP_Memory_View_Record'Class;
      Address : Long_Long_Integer := 0)
   is
      Values : String (1 .. 2 * View.Number_Of_Bytes);
      Index  : Positive := Values'First;
   begin
      if View.Dump = null then
         View.Dump := new Memory_Dump
           (1 .. (View.Number_Of_Bytes + Dump_Item_Size - 1) /
                Dump_Item_Size);
      end if;

      --  Fill the values that could not be accessed with "-"
      for J in View.Dump'Range loop
         while Length (View.Dump (J).Value) < Dump_Item_Size * 2 loop
            Append (View.Dump (J).Value, "--");
         end loop;
      end loop;

      View.Label_Length := 0;
      --  Copy all Dump.Value-s to Values
      for J in View.Dump'Range loop
         Values (Index .. Index + Length (View.Dump (J).Value) - 1)
           := To_String (View.Dump (J).Value);

         Index := Index + Length (View.Dump (J).Value);

         if View.Label_Length < Length (View.Dump (J).Label) then
            View.Label_Length := Length (View.Dump (J).Label);
         end if;
      end loop;

      --  Make length of all labels equal
      for J in View.Dump'Range loop
         Head (View.Dump (J).Label, View.Label_Length);
      end loop;

      Free (View.New_Values);
      View.New_Values := new String'(Values);

      if View.Starting_Address /= Address or else
        View.Old_Values = null or else
        View.Old_Values'Length /= View.Number_Of_Bytes * 2 or else
        View.Edit_Mode
      then
         --  Clear original data if Address or Number_Of_Bytes changed
         Free (View.Old_Values);
         View.Old_Values := new String'(Values);
         View.Edit_Mode := False;
      end if;

      View.Starting_Address := Address;
      View.Data   := Byte;
      Update_Display (View);
      Set_Text (View.Editor.Address_Entry,
                "0x" & To_Standard_Base (Address, 16, Address_Length));
   end Fill_Values;

   --------------------
   -- Display_Memory --
   --------------------

   procedure Display_Memory
     (View    : access DAP_Memory_View_Record'Class;
      Address : String)
   is
      Real_Address : Long_Long_Integer;
      Index        : Integer;
      Client       : constant DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
   begin
      if Address'Length > 2
        and then Address (Address'First .. Address'First + 1) = "0x"
      then
         Index := Address'First + 2;

         while Index <= Address'Last
           and then Is_Hexadecimal_Digit (Address (Index))
         loop
            Index := Index + 1;
         end loop;

         begin
            Real_Address := Long_Long_Integer'Value
              (Hex_Header &
               Address (Address'First + 2 .. Index - 1) &
               Hex_Footer);
            Display_Memory (View, Real_Address);
         exception
            when Constraint_Error =>
               Display_Memory (View, 0);
         end;

      elsif Address /= "" then
         DAP.Clients.Evaluate.Send_Get_Variable_Address_Request
           (Client.all, Address);

      else
         Display_Memory (View, 0);
      end if;
   end Display_Memory;

   ------------
   -- Insert --
   ------------

   procedure Insert
     (View : access DAP_Memory_View_Record'Class;
      Char : String)
   is
      Buffer      : constant Gtk_Text_Buffer := Get_Buffer (View.Editor.View);
      Position    : Gint;
      Prefix      : String (1 .. 3);
      Success     : Boolean;
      Value_Index : Integer;
      Start_Mark  : Gtk_Text_Mark;
      Start_Iter  : Gtk_Text_Iter;
      End_Iter    : Gtk_Text_Iter;
      Bloc_Begin  : Gint;
      Bloc_End    : Gint;
   begin
      pragma Assert (Char'Length = 1);

      --  Get the cursor position in the buffer
      Get_Iter_At_Mark (Buffer, Start_Iter, Get_Insert (Buffer));
      Position := Get_Offset (Start_Iter);

      if not Editable (Start_Iter, Default_Setting => False) then
         --  The cursor should not be located in a position where text cannot
         --  be inserted.
         Watch_Cursor_Location (View);
         return;
      end if;

      Start_Editing (View);

      Get_Start_Iter (Buffer, Start_Iter);
      Get_End_Iter (Buffer, End_Iter);

      declare
         Text : constant String :=
                  Get_Text (Buffer, Start_Iter, End_Iter, False);
      begin
         if View.Editor.View = null or else Text'Length <= 0 then
            return;
         end if;
      end;

      --  Check whether the character to insert is in an acceptable range

      case View.Display is
         when Hex =>
            if Is_Hexadecimal_Digit (Char (Char'First)) then
               Prefix := "16#";
            else
               return;
            end if;
         when Decimal =>
            if Is_Decimal_Digit (Char (Char'First)) then
               Prefix := "10#";
            else
               return;
            end if;
         when Octal =>
            if Char (Char'First) in '0' .. '7' then
               Prefix := "08#";
            else
               return;
            end if;
         when Text =>
            null;
      end case;

      Begin_User_Action (Buffer);

      Get_Iter_At_Mark (Buffer, End_Iter, Get_Insert (Buffer));
      Start_Mark := Create_Mark (Buffer, Where => End_Iter);

      --  Delete the character following the cursor

      Copy (End_Iter, Start_Iter);
      Forward_Cursor_Position (Start_Iter, Success);
      Delete (Buffer, Start_Iter, End_Iter);

      --  Insert the new character

      Insert (Buffer, End_Iter, Char);

      Get_Iter_At_Mark (Buffer, Start_Iter, Start_Mark);
      Bloc_Begin := Get_Offset (Start_Iter);
      Bloc_End := Bloc_Begin;
      Place_Cursor (Buffer, Start_Iter);

      --  Find the beginning of the bloc

      while Bloc_Begin > 0 loop
         Get_Iter_At_Offset (Buffer, Start_Iter, Bloc_Begin);
         Get_Iter_At_Offset (Buffer, End_Iter, Bloc_Begin - 1);

         exit when Get_Text (Buffer, Start_Iter, End_Iter) =
           Data_Separator (Data_Separator'Last .. Data_Separator'Last);

         Bloc_Begin := Bloc_Begin - 1;
      end loop;

      --  Find the end of the bloc

      loop
         Get_Iter_At_Offset (Buffer, Start_Iter, Bloc_End);
         Get_Iter_At_Offset (Buffer, End_Iter, Bloc_End + 1);

         declare
            S : constant String := Get_Text (Buffer, Start_Iter, End_Iter);
         begin
            exit when S = ""
              or else S = Data_Separator
                   (Data_Separator'First .. Data_Separator'First);
         end;

         Bloc_End := Bloc_End + 1;
      end loop;

      --  Mark the bloc as modified

      Get_Iter_At_Offset (Buffer, Start_Iter, Bloc_Begin);
      Get_Iter_At_Offset (Buffer, End_Iter, Bloc_End);

      Apply_Tag (Buffer, View.Modified_Tag, Start_Iter, End_Iter);
      Apply_Tag (Buffer, View.Editable_Tag, Start_Iter, End_Iter);

      --  Update the flags

      if View.Display = Text then
         declare
            Row        : Integer;
            Column     : Integer;
            ASCII_Size : Integer := 0;
         begin
            if Get_Active (View.Editor.Show_Ascii) then
               ASCII_Size :=
                 Data_ASCII_Separator'Length
                 + Line_Base_Size
                 + ASCII_Separator'Length * View.Number_Of_Columns;
            end if;

            Get_Coordinates (View, Position, Row, Column);
            Value_Index :=
              (Integer (Position)
               - Row *
                 (Address_Length + Address_Separator'Length
                  + 1 + View.Label_Length
                  + ASCII_Size
                  + End_Of_Line'Length
                  + View.Number_Of_Columns * Data_Separator'Length)
               - Address_Length - Address_Separator'Length
               - 1 - View.Label_Length
               - (Column - 1) * (Data_Separator'Length)) * 2 - 1;

            View.New_Values (Value_Index .. Value_Index + 1) :=
              To_Standard_Base
                (Long_Long_Integer (Character'Pos (Char (Char'First))),
                 16, 2);
         end;
      else
         Get_Iter_At_Mark (Buffer, Start_Iter, Get_Insert (Buffer));

         Value_Index :=
           Position_To_Bloc (View, Get_Offset (Start_Iter))
           * Line_Base_Size / View.Number_Of_Columns * 2 + 1;

         Get_Iter_At_Offset (Buffer, Start_Iter, Bloc_Begin);
         Get_Iter_At_Offset (Buffer, End_Iter, Bloc_End);

         declare
            S : constant String := Get_Text (Buffer, Start_Iter, End_Iter);
         begin
            if View.New_Values (Value_Index .. Value_Index) /=
              Non_Valid_Character
            then
               View.New_Values
                 (Value_Index .. Value_Index + View.Unit_Size - 1) :=
                   To_Standard_Base
                     (Long_Long_Integer'Value (Prefix & S & Hex_Footer),
                      16,
                      View.Unit_Size);
            end if;

         end;
      end if;

      if Get_Active (View.Editor.Show_Ascii) then
         --  Update the ASCII view

         Get_Iter_At_Offset (Buffer, End_Iter, Bloc_End);
         Copy (End_Iter, Start_Iter);
         Forward_Cursor_Positions
           (End_Iter, Gint (Data_ASCII_Separator'Length), Success);

         loop
            declare
               Text : constant String :=
                        Get_Text (Buffer, Start_Iter, End_Iter);
            begin
               exit when Text = Data_ASCII_Separator;
               Forward_Cursor_Position (Start_Iter, Success);
               Forward_Cursor_Position (End_Iter, Success);
            end;
         end loop;

         Forward_To_Line_End (End_Iter, Success);

         Delete (Buffer, Start_Iter, End_Iter);

         Place_Cursor (Buffer, Start_Iter);
         Insert_ASCII (View);
      end if;

      End_User_Action (Buffer);

      --  Update the position of the cursor

      Get_Iter_At_Offset (Buffer, Start_Iter, Position);
      Place_Cursor (Buffer, Start_Iter);
      Move_Cursor (View, Right);
      Get_Iter_At_Mark (Buffer, Start_Iter, Get_Insert (Buffer));
      Forward_Cursor_Position (Start_Iter, Success);
      Place_Cursor (Buffer, Start_Iter);
   end Insert;

   -----------------
   -- Move_Cursor --
   -----------------

   procedure Move_Cursor
     (View  : access DAP_Memory_View_Record'Class;
      Where : Dir)
   is
      Buffer     : constant Gtk_Text_Buffer := Get_Buffer (View.Editor.View);
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Position   : Gint;
      ASCII_Size : Integer := 0;

   begin
      Get_Iter_At_Mark (Buffer, Start_Iter, Get_Insert (Buffer));
      Position := Get_Offset (Start_Iter);

      if Get_Active (View.Editor.Show_Ascii) then
         ASCII_Size :=
           Data_ASCII_Separator'Length +
           Line_Base_Size +
           ASCII_Separator'Length * View.Number_Of_Columns;
      end if;

      case Where is
         when Right =>
            Get_Iter_At_Offset (Buffer, Start_Iter, Position + 1);
            Get_Iter_At_Offset
              (Buffer, End_Iter, Position + 1 + Data_Separator'Length);

            if Get_Text (Buffer, Start_Iter, End_Iter) = Data_Separator then
               --  Are we on the last bloc on the line ?

               if Position_To_Bloc (View, Position)
                 mod View.Number_Of_Columns = View.Number_Of_Columns - 1
               then
                  --  Is it the last bloc in the view ?

                  if Position_To_Bloc (View, Position) =
                    View.Number_Of_Columns
                    * Integer (Get_Value_As_Int (View.Editor.Lines_Spin)) - 1
                  then
                     Get_Iter_At_Offset (Buffer, Start_Iter, Position - 1);
                     Place_Cursor (Buffer, Start_Iter);
                  else
                     Get_Iter_At_Offset
                       (Buffer, Start_Iter,
                        Position
                        + Gint (Address_Length)
                        + Address_Separator'Length
                        + 1
                        + Gint (View.Label_Length)
                        + Data_Separator'Length
                        + Gint (ASCII_Size)
                        + End_Of_Line'Length);
                     Place_Cursor (Buffer, Start_Iter);
                  end if;
               else
                  Get_Iter_At_Offset
                    (Buffer, Start_Iter, Position + Data_Separator'Length);
                  Place_Cursor (Buffer, Start_Iter);
               end if;
            end if;

         when Left =>
            Get_Iter_At_Offset
              (Buffer, Start_Iter, Position - Data_Separator'Length);
            Get_Iter_At_Offset (Buffer, End_Iter, Position);

            if Get_Text (Buffer, Start_Iter, End_Iter) = Data_Separator then
               --  Are we on the first bloc on the line ?

               if Position_To_Bloc (View, Position)
                 mod View.Number_Of_Columns = 0
               then
                  --  Is it the first bloc in the view ?

                  if Position_To_Bloc (View, Position) = 0 then
                     Get_Iter_At_Offset (Buffer, Start_Iter, Position + 1);
                     Place_Cursor (Buffer, Start_Iter);
                  else
                     Get_Iter_At_Offset
                       (Buffer, Start_Iter,
                        Position
                        - Gint (Address_Length)
                        - Address_Separator'Length
                        - 1
                        - Gint (View.Label_Length)
                        - Data_Separator'Length
                        - Gint (ASCII_Size)
                        - End_Of_Line'Length);
                     Place_Cursor (Buffer, Start_Iter);
                  end if;
               else
                  Get_Iter_At_Offset
                    (Buffer, Start_Iter, Position - Data_Separator'Length);
                  Place_Cursor (Buffer, Start_Iter);
               end if;
            end if;
         when others =>
            null;
      end case;
   end Move_Cursor;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (View : not null access DAP_Memory_View_Record)
   is
      Client : constant DAP_Client_Access := View.Get_Client;
   begin
      if Client = null then
         return;
      end if;

      if Client.Get_Endian_Type = Unknown_Endian then
         --  We do not have endian, get it
         DAP.Clients.Evaluate.Send_Show_Endian_Request (Client);

      elsif Memory_Auto_Refresh.Get_Pref then
         if View.Edit_Mode then
            Stop_Editing (View);  --  no editing survive auto-refresh
         elsif View.Old_Values /= null and View.New_Values /= null then
            View.Old_Values.all := View.New_Values.all;
         end if;

         Display_Memory (View, Get_Text (View.Editor.Address_Entry));
      end if;
   end Update;

   ---------------------------
   -- Watch_Cursor_Location --
   ---------------------------

   procedure Watch_Cursor_Location (View : access DAP_Memory_View_Record'Class)
   is
      Buffer : constant Gtk_Text_Buffer := Get_Buffer (View.Editor.View);
      Row    : Integer;
      Column : Integer;
      Iter   : Gtk_Text_Iter;
      Result : Boolean;
   begin
      Get_Iter_At_Mark (Buffer, Iter, Get_Insert (Buffer));
      Get_Coordinates (View, Get_Offset (Iter), Row, Column);

      --  If the cursor is found at a place where text is not editable,
      --  reinitialize its position.

      if Row >= Integer (Get_Value_As_Int (View.Editor.Lines_Spin))
        or else Column >= View.Number_Of_Columns
        or else Column < 0
      then
         Set_Offset
           (Iter, Address_Separator'Length + Gint (Address_Length)
                     + 1 + Gint (View.Label_Length));
         Place_Cursor (Buffer, Iter);
      else
         while not Editable (Iter, False) loop
            Forward_Cursor_Position (Iter, Result);
         end loop;
         Place_Cursor (Buffer, Iter);
      end if;
   end Watch_Cursor_Location;

   -------------------
   -- Start_Editing --
   -------------------

   procedure Start_Editing (View : access DAP_Memory_View_Record'Class) is
      Update : Boolean := False;
   begin
      if not View.Edit_Mode then
         --  Check if view has bytes marked as changed
         Update := View.Old_Values /= null
           and then View.New_Values /= null
           and then View.Old_Values.all /= View.New_Values.all;

         Free (View.Old_Values);

         if View.New_Values /= null then
            View.Old_Values := new String'(View.New_Values.all);
         end if;

         if Update then
            declare
               Iter   : Gtk_Text_Iter;
               Cursor : Gint;
               Buffer : constant Gtk_Text_Buffer :=
                 Get_Buffer (View.Editor.View);
            begin
               --  Remember cursor position
               Get_Iter_At_Mark (Buffer, Iter, Get_Insert (Buffer));
               Cursor := Get_Offset (Iter);

               Update_Display (View);  --  Cleanup change markers before edit

               --  Restore cursor position
               Get_Iter_At_Offset (Buffer, Iter, Cursor);
               Place_Cursor (Buffer, Iter);
            end;
         end if;

         View.Edit_Mode := True;
         Set_Sensitive (View.Editor.Submit, True);
         Set_Sensitive (View.Editor.Reset, True);
      end if;
   end Start_Editing;

   ------------------
   -- Stop_Editing --
   ------------------

   procedure Stop_Editing (View : access DAP_Memory_View_Record'Class) is
   begin
      if View.Edit_Mode then
         Free (View.New_Values);

         if View.Old_Values /= null then
            View.New_Values := new String'(View.Old_Values.all);
         end if;

         View.Edit_Mode := False;
         Set_Sensitive (View.Editor.Submit, False);
         Set_Sensitive (View.Editor.Reset, False);
      end if;
   end Stop_Editing;

   -------------------------------
   -- On_Address_Entry_Activate --
   -------------------------------

   procedure On_Address_Entry_Activate
     (Object : access Gtk_Widget_Record'Class)
   is
      View : constant DAP_Memory_View := DAP_Memory_View (Object);
   begin
      Display_Memory (View, Get_Text (View.Editor.Address_Entry));
   end On_Address_Entry_Activate;

   -----------------------------
   -- On_Address_View_Clicked --
   -----------------------------

   procedure On_Address_View_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      View : constant DAP_Memory_View := DAP_Memory_View (Object);
   begin
      Display_Memory (View, Get_Text (View.Editor.Address_Entry));
   end On_Address_View_Clicked;

   ---------------------------
   -- On_Size_Entry_Changed --
   ---------------------------

   procedure On_Size_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      View : constant DAP_Memory_View := DAP_Memory_View (Object);
   begin
      Update_Display (View);
   end On_Size_Entry_Changed;

   ---------------------------
   -- On_Data_Entry_Changed --
   ---------------------------

   procedure On_Data_Entry_Changed
     (Object : access Gtk_Widget_Record'Class)
   is
      View : constant DAP_Memory_View := DAP_Memory_View (Object);
   begin
      Update_Display (View);
   end On_Data_Entry_Changed;

   ---------------------------
   -- On_Show_Ascii_Toggled --
   ---------------------------

   procedure On_Show_Ascii_Toggled
     (Object : access Gtk_Widget_Record'Class)
   is
      View : constant DAP_Memory_View := DAP_Memory_View (Object);
   begin
      Update_Display (View);
   end On_Show_Ascii_Toggled;

   ---------------------
   -- On_Pgup_Clicked --
   ---------------------

   procedure On_Pgup_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      View : constant DAP_Memory_View := DAP_Memory_View (Object);
   begin
      Page_Up (View);
   end On_Pgup_Clicked;

   ---------------------
   -- On_Pgdn_Clicked --
   ---------------------

   procedure On_Pgdn_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      View : constant DAP_Memory_View := DAP_Memory_View (Object);
   begin
      Page_Down (View);
   end On_Pgdn_Clicked;

   -----------------------------
   -- On_View_Key_Press_Event --
   -----------------------------

   function On_View_Key_Press_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : GValues) return Boolean
   is
      View  : constant DAP_Memory_View := DAP_Memory_View (Object);
      Arg1  : Gdk_Event;
      Proxy : constant C_Proxy := Get_Proxy (Nth (Params, 1));

   begin
      if Proxy = null then
         return False;
      else
         Arg1 := Get_Event (Nth (Params, 1));
      end if;

      if Arg1 = null
        or else Get_Event_Type (Arg1) /= Key_Press
      then
         return False;
      end if;

      case Get_Key_Val (Arg1) is
         when GDK_Right =>
            Move_Cursor (View, Right);
         when GDK_Left =>
            Move_Cursor (View, Left);
         when GDK_Up =>
            Move_Cursor (View, Up);
         when GDK_Down =>
            Move_Cursor (View, Down);
         when GDK_BackSpace | GDK_Clear | GDK_Delete =>
            Gtk.Handlers.Emit_Stop_By_Name
              (View.Editor.View, "key_press_event");
         when GDK_Page_Up | GDK_KP_Page_Up =>
            Page_Up (View);
         when GDK_Page_Down | GDK_KP_Page_Down =>
            Page_Down (View);
         when others =>
            Gtk.Handlers.Emit_Stop_By_Name
              (View.Editor.View, "key_press_event");

            begin
               declare
                  Str : constant String :=
                    Gtkada.Types.Value (Arg1.Key.String);
               begin
                  if Str'Length /= 0 then
                     Insert (View, Str);
                  end if;
               end;
            exception
               when Constraint_Error =>
                  --  On windows, it seems that pressing the control key
                  --  generates an event for which Get_String is invalid
                  null;
            end;
      end case;

      return False;
   end On_View_Key_Press_Event;

   ----------------------------------
   -- On_View_Button_Release_Event --
   ----------------------------------

   function On_View_Button_Release_Event
     (Object : access Gtk_Widget_Record'Class;
      Params : Gtk.Arguments.Gtk_Args) return Boolean
   is
      pragma Unreferenced (Params);

      View : constant DAP_Memory_View := DAP_Memory_View (Object);
      Start_Iter : Gtk_Text_Iter;
      End_Iter   : Gtk_Text_Iter;
      Result     : Boolean;
   begin
      if View.Old_Values = null then
         return False;
      end if;

      Get_Selection_Bounds
        (Get_Buffer (View.Editor.View), Start_Iter, End_Iter, Result);

      if Result = False then
         Watch_Cursor_Location (View);
      end if;

      return False;
   end On_View_Button_Release_Event;

   -----------------------
   -- On_Submit_Clicked --
   -----------------------

   procedure On_Submit_Clicked (Object : access Gtk_Widget_Record'Class) is
      View : constant DAP_Memory_View := DAP_Memory_View (Object);
   begin
      Apply_Changes (View);
   end On_Submit_Clicked;

   ----------------------
   -- On_Reset_Clicked --
   ----------------------

   procedure On_Reset_Clicked
     (Object : access Gtk_Widget_Record'Class)
   is
      View : constant DAP_Memory_View := DAP_Memory_View (Object);
   begin
      Stop_Editing (View);
      GNAT.Strings.Free (View.New_Values);

      if View.Old_Values /= null then
         View.New_Values := new String'(View.Old_Values.all);
      end if;

      Update_Display (View);
   end On_Reset_Clicked;

   -----------------------
   -- On_Button_Release --
   -----------------------

   function On_Button_Release
     (Object : access Gtk_Widget_Record'Class) return Boolean
   is
      View : constant DAP_Memory_View := DAP_Memory_View (Object);
   begin
      Update_Display (View);

      return False;
   end On_Button_Release;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access View_Memory_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      use GPS.Kernel.Contexts;
      pragma Unreferenced (Command);

      Name : constant String := Get_Variable_Name (Context.Context, False);
   begin
      if Name /= "" then
         Display_Memory
           (Kernel  => Get_Kernel (Context.Context),
            Address => Name);

      else
         Display_Memory
           (Kernel  => Get_Kernel (Context.Context),
            Address => Entity_Name_Information (Context.Context));
      end if;

      return Commands.Success;
   end Execute;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New (View : out View_Access) is
   begin
      View := new View_Record;
      DAP.Views.Memory.Initialize (View);
   end Gtk_New;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize (View : access View_Record'Class) is
      pragma Suppress (All_Checks);

      Label        : Gtk_Label;
      Flow         : Gtk_Flow_Box;
      Hbox         : Gtk_Hbox;
      Hseparator   : Gtk_Hseparator;
      Arrow        : Gtk_Arrow;
      Hbuttonbox11 : Gtk_Hbutton_Box;
      Adjustment   : Gtk_Adjustment;
      Scrolled     : Gtk_Scrolled_Window;

   begin
      Gtk.Box.Initialize_Vbox (View);

      --  Flowbox containing control widgets

      Gtk_New (Flow);
      Pack_Start (View, Flow, Expand => False);
      Flow.Set_Homogeneous (False);
      Flow.Set_Orientation (Orientation_Horizontal);
      Flow.Set_Can_Focus (False);
      Flow.Set_Selection_Mode (Selection_None);

      --  Location widget

      Gtk_New_Hbox (Hbox);
      Flow.Add (Hbox);

      Gtk_New (Label, "Location");
      Set_Alignment (Label, 0.5, 0.5);
      Set_Padding (Label, 0, 0);
      Set_Justify (Label, Justify_Center);
      Set_Line_Wrap (Label, False);
      Hbox.Pack_Start (Label, False, False, 4);

      Gtk_New (View.Address_Entry);
      Set_Editable (View.Address_Entry, True);
      Set_Max_Length (View.Address_Entry, 0);
      Set_Text (View.Address_Entry, "");
      Set_Visibility (View.Address_Entry, True);
      Set_Name (View.Address_Entry, "memory view adress entry");
      Hbox.Pack_Start (View.Address_Entry, False);

      Gtk_New (View.Address_View, "View");
      Set_Relief (View.Address_View, Relief_Normal);
      Hbox.Pack_Start (View.Address_View, False, False);

      --  Unit size widget

      Gtk_New_Hbox (Hbox);
      Flow.Add (Hbox);

      Gtk_New (Label, "Unit size: ");
      Set_Line_Wrap (Label, False);
      Pack_Start (Hbox, Label, False, False, 4);

      Gtk_New (View.Size);
      View.Size.Append_Text ("Byte");
      View.Size.Append_Text ("Halfword");
      View.Size.Append_Text ("Word");
      View.Size.Set_Active (0);
      Pack_Start (Hbox, View.Size, False, False, 7);
      Set_Name (View.Size, "memory view unit size");

      --  Format widget

      Gtk_New_Hbox (Hbox);
      Flow.Add (Hbox);

      Gtk_New (Label, "Format: ");
      Set_Line_Wrap (Label, False);
      Pack_Start (Hbox, Label, False, False, 4);

      Gtk_New (View.Format);
      View.Format.Append_Text ("Hex");
      View.Format.Append_Text ("Decimal");
      View.Format.Append_Text ("Octal");
      View.Format.Append_Text ("ASCII");
      View.Format.Set_Active (0);
      Pack_Start (Hbox, View.Format, False, False, 7);
      Set_Name (View.Format, "memory view format");

      --  ASCII check button

      Gtk_New (View.Show_Ascii, "Show ASCII");
      Set_Active (View.Show_Ascii, True);
      Set_Name (View.Show_Ascii, "memory view show ascii");
      Flow.Add (View.Show_Ascii);

      --  Page up/down action

      Gtk_New_Hbox (Hbox);
      Flow.Add (Hbox);

      Gtk_New (View.Pgup);
      Set_Relief (View.Pgup, Relief_Normal);
      Pack_Start (Hbox, View.Pgup, False, False, 0);

      Gtk_New (Arrow, Arrow_Up, Shadow_Out);
      Set_Alignment (Arrow, 0.5, 0.5);
      Set_Padding (Arrow, 0, 0);
      Add (View.Pgup, Arrow);

      Gtk_New (View.Pgdn);
      Set_Relief (View.Pgdn, Relief_Normal);
      Pack_Start (Hbox, View.Pgdn, False, False, 0);

      Gtk_New (Arrow, Arrow_Down, Shadow_Out);
      Set_Alignment (Arrow, 0.5, 0.5);
      Set_Padding (Arrow, 0, 0);
      Add (View.Pgdn, Arrow);

      --  Lines spin

      Gtk_New (Adjustment, 16.0, 1.0, 30.0, 1.0, 0.0);
      Gtk_New (View.Lines_Spin, Adjustment, 0.0, 0);
      Pack_Start (Hbox, View.Lines_Spin, False, False, 0);

      --  Scrolled window containing the memory

      Gtk_New (Scrolled);
      Set_Policy
        (Scrolled, Policy_Automatic, Policy_Automatic);
      Pack_Start (View, Scrolled, True, True, 0);

      --  Memory view

      Gtk_New (View.View);
      Set_Editable (View.View, True);
      View.View.Get_Buffer.Set_Text ("");
      Set_Name (View.View, "memory view text");
      Add (Scrolled, View.View);

      Gtk_New_Hseparator (Hseparator);
      Pack_Start (View, Hseparator, False, False, 3);

      --  Buttons to undo/submit changes

      Gtk_New (Hbuttonbox11);
      Set_Spacing (Hbuttonbox11, 30);
      Set_Layout (Hbuttonbox11, Buttonbox_Spread);
      Pack_Start (View, Hbuttonbox11, False, False, 0);

      Gtk_New (View.Reset, "Undo changes");
      Set_Relief (View.Reset, Relief_Normal);
      Add (Hbuttonbox11, View.Reset);
      Set_Sensitive (View.Reset, False);

      Gtk_New (View.Submit, "Submit changes");
      Set_Relief (View.Submit, Relief_Normal);
      Add (Hbuttonbox11, View.Submit);
      Set_Sensitive (View.Submit, False);

      Set_Wrap_Mode (View.View, Wrap_None);
   end Initialize;

   ------------------
   -- Insert_ASCII --
   ------------------

   procedure Insert_ASCII (View : access DAP_Memory_View_Record'Class) is
      Buffer      : constant Gtk_Text_Buffer := Get_Buffer (View.Editor.View);
      Client      : constant DAP.Clients.DAP_Client_Access :=
        DAP.Module.Get_Current_Debugger;
      Endianness  : constant Endian_Type := Client.Get_Endian_Type;
      Start_Mark  : Gtk_Text_Mark;
      Start_Iter  : Gtk_Text_Iter;
      End_Iter    : Gtk_Text_Iter;
      Tag         : Gtk_Text_Tag;
      Index       : Natural;
      Line_Index  : Natural;
   begin
      Get_Iter_At_Mark (Buffer, Start_Iter, Get_Insert (Buffer));
      Line_Index := Natural (Get_Line (Start_Iter)) + 1;
      Insert_At_Cursor (Buffer, Data_ASCII_Separator);

      for Column_Index in 1 .. View.Number_Of_Columns loop
         Index := (Line_Index - 1) *
           (View.Number_Of_Columns * View.Unit_Size)
           + (Column_Index - 1) * View.Unit_Size + 1;

         if View.Old_Values (Index .. Index + View.Unit_Size - 1) /=
           View.New_Values (Index .. Index + View.Unit_Size - 1)
         then
            Tag := View.Modified_Tag;
         else
            Tag := View.Default_Tag;
         end if;

         declare
            S : String (1 .. View.Unit_Size);
         begin
            if Endianness = Little_Endian then
               declare
                  B : constant String (1 .. View.Unit_Size) :=
                        View.New_Values (Index .. Index + View.Unit_Size - 1);
               begin
                  for J in 0 .. View.Unit_Size / 2 - 1 loop
                     S (S'First + J * 2 .. S'First + J * 2 + 1) :=
                       B (B'Last -  J * 2 - 1 .. B'Last -  J * 2);
                  end loop;
               end;
            else
               S := View.New_Values (Index .. Index + View.Unit_Size - 1);
            end if;

            Get_Iter_At_Mark (Buffer, End_Iter, Get_Insert (Buffer));
            Start_Mark := Create_Mark (Buffer, Where => End_Iter);

            Insert
              (Buffer,
               End_Iter,
               Conversion
                 (S, View.Unit_Size, Text, View.Trunc, Is_ASCII => True) &
                 Data_Separator);

            Get_Iter_At_Mark (Buffer, Start_Iter, Start_Mark);
            Apply_Tag (Buffer, Tag, Start_Iter, End_Iter);
            Delete_Mark (Buffer, Start_Mark);
            Place_Cursor (Buffer, End_Iter);
         end;
      end loop;
   end Insert_ASCII;

   ----------------
   -- Conversion --
   ----------------

   function Conversion
     (S        : String;
      Size     : Integer;
      Format   : Display_Type;
      Trunc_At : Integer;
      Is_ASCII : Boolean := False) return String
   is
      pragma Unreferenced (Size);

      Long  : Long_Long_Integer;
      Test  : Integer := S'First;
      Dummy : constant String := "------------------------";

   begin
      Skip_To_String (S, Test, Non_Valid_Character);

      if Test < S'Last then
         if Trunc_At /= -1 and not Is_ASCII then
            return Dummy (1 .. Trunc_At);
         else
            return Non_Valid_Character;
         end if;
      end if;

      Long := Long_Long_Integer'Value (Hex_Header & S & Hex_Footer);

      case Format is
         when Hex =>
            return S;
         when Octal =>
            return To_Standard_Base (Long, 8, Trunc_At);
         when Decimal =>
            return To_Standard_Base (Long, 10, Trunc_At);
         when Text =>
            declare
               Result : String (1 .. S'Length / 2);
               Value  : Integer;
            begin
               for J in 1 .. Result'Last loop
                  Value :=
                    Integer'Value
                    (Hex_Header
                     & S (S'First + 2 * J - 2 .. S'First + 2 * J - 1)
                     & Hex_Footer);

                  if Value > 31 and then Value < 128 then
                     Result (J) := Character'Val (Value);
                  else
                     Result (J) := '.';
                  end if;
               end loop;

               return Result;
            end;
      end case;
   end Conversion;

   ---------------------
   -- Get_Coordinates --
   ---------------------

   procedure Get_Coordinates
     (View     : access DAP_Memory_View_Record'Class;
      Position : Gint;
      Row      : out Integer;
      Column   : out Integer)
   is
      Row_Length : Integer;
      ASCII_Size : Integer := 0;

   begin
      if Get_Active (View.Editor.Show_Ascii) then
         ASCII_Size :=
           Data_ASCII_Separator'Length +
           Line_Base_Size +
           ASCII_Separator'Length * View.Number_Of_Columns;
      end if;

      Row_Length :=
        Address_Length + Address_Separator'Length +
        1 + View.Label_Length +
        (View.Number_Of_Columns * (View.Trunc + Data_Separator'Length)) +
        ASCII_Size + End_Of_Line'Length;

      Row := Integer (Position) / Row_Length;

      Column :=
        (Integer (Position) - Row * Row_Length -
         (Address_Length + Address_Separator'Length + 1 + View.Label_Length))
        + 1;

      if Column <= 0 then
         Column := -1;
         return;
      end if;

      Column := Column / (View.Trunc + Data_Separator'Length);
   end Get_Coordinates;

   ----------------------
   -- Position_To_Bloc --
   ----------------------

   function Position_To_Bloc
     (View     : access DAP_Memory_View_Record'Class;
      Position : Gint) return Integer
   is
      Row    : Integer;
      Column : Integer;

   begin
      Get_Coordinates (View, Position, Row, Column);
      return Column + Row * View.Number_Of_Columns;
   end Position_To_Bloc;

   ----------------------
   -- To_Standard_Base --
   ----------------------

   function To_Standard_Base
     (Address  : Long_Long_Integer;
      Base     : Integer;
      Trunc_At : Integer := -1) return String
   is
      Index   : Integer := 1;
      Result  : String (1 .. 64);
      Mapping : constant Character_Mapping :=
                  To_Mapping ("ABCDEF ", "abcdef0");

   begin
      Put (Result, Address, Base);
      Skip_To_String (Result, Index, Hex_Footer);

      if Index > 3
        and then Index < Result'Length + 1
      then
         Result (Index - 3 .. Index) := "    ";
      end if;

      if Trunc_At = -1 then
         return Translate (Result (1 .. Result'Length - 1), Mapping);
      else
         if Base = 10 then
            return
              Translate
                (Result (Result'Last - Trunc_At + 1 .. Result'Last), Mapping);
         else
            return
              Translate
                (Result (Result'Last - Trunc_At .. Result'Last - 1), Mapping);
         end if;
      end if;
   end To_Standard_Base;

   -----------------
   -- Swap_Blocks --
   -----------------

   procedure Swap_Blocks
     (View : access DAP_Memory_View_Record'Class; Size : Data_Size)
   is
      Index     : Integer := 0;
      Unit_Size : Integer;

   begin
      if View.Old_Values = null
        or else View.New_Values = null
      then
         return;
      end if;

      case Size is
         when Byte =>
            return;
         when Halfword =>
            Unit_Size := 4;
         when Word =>
            Unit_Size := 8;
      end case;

      declare
         Buffer : String (1 .. Unit_Size);
      begin
         while Index <= View.Number_Of_Bytes * 2 - Unit_Size loop
            Buffer (1 .. Unit_Size) :=
              View.Old_Values
                (View.Old_Values'First + Index ..
                     View.Old_Values'First + Index + Unit_Size - 1);

            for J in 1 .. Unit_Size / 2 loop
               View.Old_Values
                 (View.Old_Values'First + Index + (J - 1) * 2
                    .. View.Old_Values'First + Index + (J - 1) * 2 + 1) :=
                 Buffer (Buffer'Last - (J - 1) * 2 - 1
                           .. Buffer'Last - (J - 1) * 2);
            end loop;

            Buffer (1 .. Unit_Size) :=
              View.New_Values
                (View.New_Values'First + Index ..
                     View.New_Values'First + Index + Unit_Size - 1);

            for J in 1 .. Unit_Size / 2 loop
               View.New_Values
                 (View.New_Values'First + Index + (J - 1) * 2
                    .. View.New_Values'First + Index + (J - 1) * 2 + 1) :=
                 Buffer (Buffer'Last - (J - 1) * 2 - 1
                           .. Buffer'Last - (J - 1) * 2);
            end loop;

            Index := Index + Unit_Size;
         end loop;
      end;
   end Swap_Blocks;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Debugger_Available : Action_Filter;
   begin
      Simple_Views.Register_Module (Kernel);

      Debugger_Available := Kernel.Lookup_Filter ("Debugger available");

      GPS.Kernel.Actions.Register_Action
        (Kernel, "examine memory",
         Command     => new View_Memory_Command,
         Description =>
           "Examine the contents of the memory at the location of the"
         & " selected variable",
         Category    => "Debug",
         Filter      => Debugger_Available and
             Kernel.Lookup_Filter ("Debugger not command variable"));

      --  the '%S' and 'debug printable variable' prevent this menu from
      --  showing up in the GVD canvas. Instead, the canvas hard-codes it.
      GPS.Kernel.Modules.UI.Register_Contextual_Menu
        (Kernel,
         Name   => "Debug view memory",
         Label  => "Debug/View memory at address of %S",
         Filter =>  Debugger_Available and
             Lookup_Filter (Kernel, "Debugger printable variable"),
         Action => "examine memory");
   end Register_Module;

end DAP.Views.Memory;
