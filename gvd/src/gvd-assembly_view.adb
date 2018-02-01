------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2000-2018, AdaCore                     --
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

with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Gdk.RGBA;                  use Gdk.RGBA;
with Gdk.Event;                 use Gdk.Event;
with Gdk.Types.Keysyms;         use Gdk.Types.Keysyms;
with Glib;                      use Glib;
with Glib.Convert;
with Glib.Object;               use Glib.Object;
with Glib.Values;

with Gtk.Box;                   use Gtk.Box;
with Gtk.Cell_Renderer_Text;    use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;  use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Handlers;              use Gtk.Handlers;
pragma Elaborate_All (Gtk.Handlers);
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Text_Tag;              use Gtk.Text_Tag;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Widget;                use Gtk.Widget;

with Pango.Font;                use Pango.Font;

with Gtkada.MDI;                use Gtkada.MDI;
with GNATCOLL.VFS;              use GNATCOLL.VFS;

with Debugger;                  use Debugger;
with Debugger_Pixmaps;
with Default_Preferences;       use Default_Preferences;
with Generic_Views;             use Generic_Views;

with GPS.Debuggers;             use GPS.Debuggers;
with GPS.Default_Styles;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Actions;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;          use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Style_Manager;
with GVD.Breakpoints_List;      use GVD.Breakpoints_List;
with GVD.Assembly_Decorators;   use GVD.Assembly_Decorators;
with GVD.Generic_View;          use GVD.Generic_View;
with GVD.Preferences;           use GVD.Preferences;
with GVD.Process;               use GVD.Process;
with GVD.Types;                 use GVD.Types;
with GVD_Module;                use GVD_Module;
with Glib_Values_Utils;         use Glib_Values_Utils;

with Commands;                  use Commands;
with Commands.Interactive;      use Commands.Interactive;
with GNATCOLL.Traces;           use GNATCOLL.Traces;
with Xref;                      use Xref;

package body GVD.Assembly_View is

   Me : constant Trace_Handle := Create ("GPS.DEBUGGING.ASSEMBLY_VIEW");

   type Cache_Data;
   type Cache_Data_Access is access Cache_Data;
   type Cache_Data is record
      Low, High : GVD.Types.Address_Type;
      --  The low and high ranges for this item

      Data      : Disassemble_Elements;
      --  The assembly code for that range

      Next       : Cache_Data_Access;
      Subprogram : Boolean := False;
   end record;
   --  This implements a cache for the assembly code, for specific ranges.
   --  Some debuggers (gdb) might take a long time to output the assembly code
   --  for a specific region, so it is better to keep it once we have it.

   type Assembly_View_Record is new Process_View_Record with
      record
         Tree                : Gtk.Tree_View.Gtk_Tree_View;
         Model               : Gtk.Tree_Store.Gtk_Tree_Store;
         --  The actual contents of the viewer

         Cache               : Cache_Data_Access;
         Current_Range       : Cache_Data_Access;
         --  The range of assembly code being displayed.

         Source_Line_Start   : GVD.Types.Address_Type :=
           GVD.Types.Invalid_Address;

         Source_Line_End     : GVD.Types.Address_Type :=
           GVD.Types.Invalid_Address;

         Decorator           : Assembly_Decorators.Decorator;
      end record;
   type Assembly_View is access all Assembly_View_Record'Class;

   overriding procedure Create_Menu
     (View : not null access Assembly_View_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class);

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
     (View : Assembly_View;
      Line : Natural;
      File : GNATCOLL.VFS.Virtual_File);
   --  Store in the assembly view the range of address that corresponds to the
   --  current source line.

   overriding procedure Update (View : not null access Assembly_View_Record);

   procedure Set_Font
     (View : Assembly_View;
      Font : Pango.Font.Pango_Font_Description);
   --  Set the font used for the box.
   --  This is called by Configure internally.

   procedure Fill_Model
     (View     : Assembly_View;
      Elements : Disassemble_Elements);
   --  Set models data. The Hightlighting is reset.

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
      Reuse_If_Exist     => True,
      Commands_Category  => "",
      Group              => Group_Debugger_Stack,
      Position           => Position_Right,
      Areas              => Gtkada.MDI.Both,
      Initialize         => Initialize,
      Local_Config       => True,
      Local_Toolbar      => True);

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
      Iter     : out Gtk_Tree_Iter;
      Found    : out Boolean);
   --  Return an iterator pointing to the row belong to the Address.
   --  Found indicates whether the address was found.

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

   procedure On_Frame_Changed
     (View : Assembly_View;
      File : GNATCOLL.VFS.Virtual_File;
      From : Natural;
      To   : Natural);
   --  Disasemble subprogram

   function In_Range
     (Address : Address_Type;
      R       : Cache_Data_Access) return Boolean;
   --  Return True if Address is in the range of addresses described by R.

   function Find_In_Cache
     (View    : Assembly_View;
      Address : Address_Type) return Cache_Data_Access;
   --  Return the cached data that contains Address.
   --  null is returned if none is found.

   procedure Meta_Scroll
     (View : Assembly_View;
      Down : Boolean);
   --  The user has asked to see the assembly range outside what is currently
   --  displayed in the assembly editor.

   procedure Meta_Scroll_PC
     (View : Assembly_View);
   --  Scroll to current PC position

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

   --------------
   -- Commands --
   --------------

   type Scroll_Command_Context is new Interactive_Command with record
      Down : Boolean := False;
   end record;
   overriding function Execute
     (Command : access Scroll_Command_Context;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Disassemble next/previuos code block

   type Scroll_PC_Command_Context is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Scroll_PC_Command_Context;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Disassemble $pc code block

   type Disassemble_Subprogram_Command is
     new Interactive_Command with null record;
   overriding function Execute
     (Command : access Disassemble_Subprogram_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Disassemble subprogram

   -------------
   -- Filters --
   -------------

   type Subprogram_Filter is
     new Action_Filter_Record with null record;
   overriding function Filter_Matches_Primitive
     (Filter  : access Subprogram_Filter;
      Context : Selection_Context) return Boolean;

   ---------------
   --  Defaults --
   ---------------

   Invalid_Cache_Data : constant Cache_Data_Access := new Cache_Data'
     (Low        => Invalid_Address,
      High       => Invalid_Address,
      Data       => <>,
      Next       => null,
      Subprogram => False);

   PC_Pixmap_Column     : constant := 0;
   Address_Column       : constant := 1;
   Method_Offset_Column : constant := 2;
   Instr_Column         : constant := 3;
   Opcodes_Column       : constant := 4;
   FG_Color_Column      : constant := 5;
   BG_Color_Column      : constant := 6;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View : not null access Assembly_View_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      K : constant Kernel_Handle := View.Kernel;
   begin
      Append_Menu (Menu, K, Asm_Show_Addresses);
      Append_Menu (Menu, K, Asm_Show_Offset);
      Append_Menu (Menu, K, Asm_Show_Opcodes);
      Append_Menu (Menu, K, Asm_Highlight_Instructions);
   end Create_Menu;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (View : Assembly_View;
      Font : Pango.Font.Pango_Font_Description) is
   begin
      --  Font
      Set_Font (View, Font);
   end Configure;

   ---------------------
   -- Set_Source_Line --
   ---------------------

   procedure Set_Source_Line
     (View : Assembly_View;
      Line : Natural;
      File : GNATCOLL.VFS.Virtual_File)
   is
   begin
      if View /= null then
         Get_Line_Address
           (Visual_Debugger (Get_Process (View)).Debugger,
            Line, File,
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
      Set_Source_Line
        (Get_View (Process), Process.Current_Line, Process.Current_File);
   end Execute;

   --------------
   -- Set_Text --
   --------------

   procedure Fill_Model
     (View     : Assembly_View;
      Elements : Disassemble_Elements)
   is
      use Ada.Strings.Unbounded;

      Model     : Gtk.Tree_Store.Gtk_Tree_Store renames View.Model;
      Row       : Gtk_Tree_Iter;
      Values    : Glib.Values.GValue_Array (1 .. 5);
      Columns   : Columns_Array (Values'Range);
      Last      : Gint := 0;
      Registers : Registers_Set.Set;

   begin
      if View = null then
         return;
      end if;

      Model.Clear;

      if Asm_Highlight_Instructions.Get_Pref then
         for Item of
           Visual_Debugger
             (Get_Process (View)).Debugger.Get_Register_Names
         loop
            Registers.Insert (Item);
         end loop;
      end if;

      for El of Elements loop
         Model.Append (Row, Null_Iter);
         Last := 0;

         declare
            S : constant String := Address_To_String (El.Address);
         begin
            if S /= "" then
               Last           := Last + 1;
               Columns (Last) := Address_Column;
               Values  (Last) := As_String (S);
            end if;
         end;

         if El.Method_Offset /= Null_Unbounded_String then
            Last           := Last + 1;
            Columns (Last) := Method_Offset_Column;
            Values  (Last) := As_String (To_String (El.Method_Offset));
         end if;

         if El.Instr /= Null_Unbounded_String then
            Last           := Last + 1;
            Columns (Last) := Instr_Column;
            Values  (Last) := As_String
              ((if Asm_Highlight_Instructions.Get_Pref
               then View.Decorator.Decorate (To_String (El.Instr), Registers)
               else Glib.Convert.Escape_Text (To_String (El.Instr))));

         end if;

         if El.Opcodes /= Null_Unbounded_String then
            Last           := Last + 1;
            Columns (Last) := Opcodes_Column;
            Values  (Last) := As_String (To_String (El.Opcodes));
         end if;

         Set_And_Clear (Model, Row, Columns (1 .. Last), Values (1 .. Last));
      end loop;
   end Fill_Model;

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

      Modify_Font (View.Tree, Font);
   end Set_Font;

   ---------------
   -- Highlight --
   ---------------

   procedure Highlight (View : access Assembly_View_Record'Class) is
      use Ada.Strings.Unbounded;

      Process    : Visual_Debugger;
      Model      : Gtk.Tree_Store.Gtk_Tree_Store renames View.Model;
      Values     : Glib.Values.GValue_Array (1 .. 3);
      Columns    : Columns_Array (Values'Range);
      Start_Iter : Gtk_Tree_Iter;
      Found      : Boolean;

      Detached   : Gtk.Tree_Model.Gtk_Tree_Model;
      Last       : Address_Type := Invalid_Address;
   begin
      if View = null then
         return;
      end if;

      Detached := View.Tree.Get_Model;
      View.Tree.Set_Model (Null_Gtk_Tree_Model);

      Process := Get_Process (View);

      --  Reset the current highlighting

      Columns := (FG_Color_Column, BG_Color_Column, PC_Pixmap_Column);

      Start_Iter := Model.Get_Iter_First;
      Glib.Values.Init (Values (1), Gdk.RGBA.Get_Type);
      Gdk.RGBA.Set_Value (Values (1), Null_RGBA);
      Glib.Values.Init (Values (2), Gdk.RGBA.Get_Type);
      Gdk.RGBA.Set_Value (Values (2), Null_RGBA);
      Values (3) := As_String ("");

      while Start_Iter /= Null_Iter loop
         Model.Set (Start_Iter, Glib.Gint_Array (Columns), Values);
         Model.Next (Start_Iter);
      end loop;
      Unset (Values);

      --  Highlight address range

      if View.Source_Line_Start /= Invalid_Address
        and then View.Source_Line_End /= Invalid_Address
      then
         Iter_From_Address
           (View, View.Source_Line_Start, Start_Iter,
            Found);

         --  Highlight the new range

         if Found then
            Columns (1) := BG_Color_Column;
            Glib.Values.Init (Values (1), Gdk.RGBA.Get_Type);
            Gdk.RGBA.Set_Value
              (Values (1), Editor_Current_Line_Color.Get_Pref);

            while Start_Iter /= Null_Iter
              and then String_To_Address
                (Model.Get_String (Start_Iter, Address_Column)) <=
                  View.Source_Line_End
            loop
               Model.Set
                 (Start_Iter,
                  Glib.Gint_Array (Columns (1 .. 1)),
                  Values (1 .. 1));

               Model.Next (Start_Iter);
            end loop;
            Unset (Values (1 .. 1));
         end if;
      end if;

         --  Highlight breakpoint lines

      Columns (1) := BG_Color_Column;
      for B of Get_Stored_List_Of_Breakpoints (Process).List loop
         if B.Address /= Invalid_Address then
            Iter_From_Address (View, B.Address, Start_Iter, Found);
            if Found then
               Glib.Values.Init (Values (1), Gdk.RGBA.Get_Type);
               Gdk.RGBA.Set_Value
                 (Values (1),
                  (if not B.Enabled then
                        GPS.Kernel.Style_Manager.Background
                     (GPS.Default_Styles.Debugger_Disabled_Breakpoint_Style)
                   elsif B.Condition /= "" then
                      GPS.Kernel.Style_Manager.Background
                     (GPS.Default_Styles.Debugger_Conditional_Breakpoint_Style)
                   else
                      GPS.Kernel.Style_Manager.Background
                     (GPS.Default_Styles.Debugger_Breakpoint_Style)));

               Set_And_Clear
                 (Model,
                  Start_Iter,
                  Columns (1 .. 1),
                  Values (1 .. 1));
            end if;
         end if;
      end loop;

      --  Highlight PC line

      Iter_From_Address (View, Process.Pc, Start_Iter, Found);
      if Found then
         Model.Set
           (Start_Iter, PC_Pixmap_Column,
            To_String (Debugger_Pixmaps.Current_Line_Pixbuf));

      elsif In_Range (Process.Pc, View.Current_Range) then
         for Index in 1 .. Natural (View.Current_Range.Data.Length) loop
            exit when
              View.Current_Range.Data.Element (Index).Address > Process.Pc;

            Last := View.Current_Range.Data.Element (Index).Address;
         end loop;

         if Last /= Invalid_Address then
            Iter_From_Address (View, Last, Start_Iter, Found);
            if Found then
               Model.Set
                 (Start_Iter, PC_Pixmap_Column,
                  To_String (Debugger_Pixmaps.Current_Line_Inside_Pixbuf));
            end if;
         end if;
      end if;

      View.Tree.Set_Model (Detached);

      if Start_Iter /= Null_Iter then
         View.Tree.Get_Selection.Select_Iter (Start_Iter);
      end if;
   end Highlight;

   -----------------------
   -- Iter_From_Address --
   -----------------------

   procedure Iter_From_Address
     (View     : not null access Assembly_View_Record'Class;
      Address : Address_Type;
      Iter    : out Gtk_Tree_Iter;
      Found   : out Boolean)
   is
      Model : Gtk.Tree_Store.Gtk_Tree_Store renames View.Model;
   begin
      Iter  := Model.Get_Iter_First;
      Found := False;

      while Iter /= Null_Iter loop
         if String_To_Address
           (Model.Get_String (Iter, Address_Column)) = Address
         then
            Found := True;
            return;
         end if;

         Model.Next (Iter);
      end loop;
   end Iter_From_Address;

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

   ------------------------------
   -- Filter_Matches_Primitive --
   ------------------------------

   overriding function Filter_Matches_Primitive
     (Filter  : access Subprogram_Filter;
      Context : Selection_Context) return Boolean
   is
      pragma Unreferenced (Filter);
   begin
      if Has_Entity_Name_Information (Context) then
         declare
            Entity : constant Root_Entity'Class := Get_Entity (Context);
         begin
            return Is_Subprogram (Entity);
         end;
      end if;
      return False;
   end Filter_Matches_Primitive;

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
      Unchecked_Free (Data);
   end Free;

   -----------------
   -- Meta_Scroll --
   -----------------

   procedure Meta_Scroll
     (View : Assembly_View;
      Down : Boolean)
   is
      Address : Address_Type;
   begin
      if View = null
        or else View.Current_Range = null
        or else Assembly_Range_Size.Get_Pref = 0
      then
         return;
      end if;

      if Down then
         if View.Current_Range.High /= Invalid_Address then
            Address := Add_Address
              (View.Current_Range.High, Assembly_Range_Size.Get_Pref);

            if Address /= Invalid_Address then
               On_Frame_Changed (View, View.Current_Range.High, Address);
            end if;
         end if;

      else
         if View.Current_Range.Low /= Invalid_Address then
            Address := Add_Address
              (View.Current_Range.Low, -Assembly_Range_Size.Get_Pref);

            if Address /= Invalid_Address then
               On_Frame_Changed (View, Address, View.Current_Range.Low);
            end if;
         end if;
      end if;

      Highlight (View);
   end Meta_Scroll;

   --------------------
   -- Meta_Scroll_PC --
   --------------------

   procedure Meta_Scroll_PC
     (View : Assembly_View)
   is
      Process : Visual_Debugger;
      Iter    : Gtk_Tree_Iter;
      Path    : Gtk_Tree_Path;
      Found   : Boolean;
   begin
      if View /= null then
         Process := Get_Process (View);

         if Process /= null then
            On_Frame_Changed (View, Process.Pc, Process.Pc);

            Highlight (View);

            Iter_From_Address (View, Process.Pc, Iter, Found);
            if Found then
               Path := View.Model.Get_Path (Iter);
               View.Tree.Scroll_To_Cell (Path, null, True, 0.5, 0.0);
               Path_Free (Path);
            end if;
         end if;
      end if;
   end Meta_Scroll_PC;

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

         when GDK_Home =>
            Meta_Scroll_PC (Assembly_View (View));
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
      S              : Disassemble_Elements;
      S2             : Disassemble_Elements;
      Start, Last    : Address_Type;
      Low, High      : Address_Type;
      Start_In_Range : Boolean := False;
      End_In_Range   : Boolean := False;

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

      if View.Current_Range /= null
        and then View.Current_Range.Subprogram
      then
         Free (View.Current_Range);
      end if;

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
         then
            View.Current_Range.Low := Start;
            View.Current_Range.Data.Prepend (S);

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
           and then Last /= Invalid_Address
         then
            View.Current_Range.High := Last;

            --  Avoid duplicating the first assembly line since it
            --  was already displayed.
            S.Delete_First;
            View.Current_Range.Data.Append (S);

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
         if Assembly_Range_Size.Get_Pref = 0 then
            Get_Machine_Code
              (Process.Debugger,
               Range_Start     => Start,
               Range_End       => Last,
               Code            => S,
               Start_Address   => Start_Address,
               End_Address     => End_Address);
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

                  S2.Delete_First;
                  S.Append (S2);
               end if;
            end if;

            View.Cache := new Cache_Data'
              (Low        => Low,
               High       => High,
               Data       => S,
               Next       => View.Cache,
               Subprogram => False);

            View.Current_Range := View.Cache;
         end if;
      end if;

      Fill_Model (View, View.Current_Range.Data);
   end On_Frame_Changed;

   ----------------------
   -- On_Frame_Changed --
   ----------------------

   procedure On_Frame_Changed
     (View : Assembly_View;
      File : GNATCOLL.VFS.Virtual_File;
      From : Natural;
      To   : Natural)
   is
      Process : Visual_Debugger;
      S       : Disassemble_Elements;

   begin
      if View = null then
         return;
      end if;

      if View.Current_Range /= null
        and then View.Current_Range.Subprogram
      then
         Free (View.Current_Range);
      end if;

      Process := Get_Process (View);

      Get_Machine_Code
        (Process.Debugger,
         File => +Base_Name (File),
         From => From,
         To   => To,
         Code => S);

      if S.Is_Empty then
         View.Current_Range := Invalid_Cache_Data;

      else
         View.Current_Range := new Cache_Data'
           (Low        => S.First_Element.Address,
            High       => S.Last_Element.Address,
            Data       => S,
            Next       => null,
            Subprogram => True);
      end if;

      Fill_Model (View, View.Current_Range.Data);
   end On_Frame_Changed;

   ------------
   -- Update --
   ------------

   overriding procedure Update (View : not null access Assembly_View_Record) is
      Process      : constant Visual_Debugger := Get_Process (View);
      Address_Low  : Address_Type;
      Address_High : Address_Type;
      Size         : Integer;
   begin
      if Process = null then
         return;
      end if;

      Set_Source_Line
        (Assembly_View (View), Process.Current_Line, Process.Current_File);

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
            if Process.Pc < Address_Low
              or else Process.Pc > Address_High
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

            Size := Assembly_Range_Size.Get_Pref;
            if Size = 0 then
               Size := 200;
            end if;

            if Set_Offset (Address_Low, Size) < Address_High then
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

      if Process.Pc < View.Current_Range.Low
        or else Process.Pc > View.Current_Range.High
      then
         --  sometimes "info line" returns addresses of previous line
         --  instead current line where $pc is, so remove Highlight
         --  to don't confuse user
         Trace (Me, "PC not in addresses range");

         View.Source_Line_Start := Invalid_Address;
         View.Source_Line_End   := Invalid_Address;
      end if;

      --  Redo the highlighting

      Highlight (Assembly_View (View));
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
      V   : constant Visual_Debugger := Visual_Debugger (Process);
      Old : constant Assembly_View   := Get_View (Process);
   begin
      --  If we are detaching, clear the old view
      if Old /= null then
         Old.Model.Clear;
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
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Debugger_Stopped : Action_Filter;
   begin
      Invalid_Cache_Data.Data.Append
        ((Address => Invalid_Address,
          Instr   => Ada.Strings.Unbounded.To_Unbounded_String
            ("Couldn't get assembly code"),
          others  => <>));

      Simple_Views.Register_Module (Kernel);
      Simple_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open assembly view",
         Description => -"Open the Assembly view for the debugger");

      Debugger_Breakpoints_Changed_Hook.Add (new On_Breakpoints_Changed);

      Debugger_Stopped := Kernel.Lookup_Filter
        ("Debugger stopped");

      GPS.Kernel.Actions.Register_Action
        (Kernel, "assembly_view disassemble next",
         Command     => new Scroll_Command_Context'
           (Interactive_Command with Down => True),
         Description => "Disassemble next code block",
         Icon_Name   => "gps-debugger-down",
         Category    => -"Debug",
         Filter      => Debugger_Stopped);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "assembly_view disassemble previous",
         Command     => new Scroll_Command_Context'
           (Interactive_Command with Down => False),
         Description => "Disassemble previous code block",
         Icon_Name   => "gps-debugger-up",
         Category    => -"Debug",
         Filter      => Debugger_Stopped);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "assembly_view disassemble pc",
         Command     => new Scroll_PC_Command_Context,
         Description => "Disassemble $pc code block",
         Icon_Name   => "gps-debugger-step",
         Category    => -"Debug",
         Filter      => Debugger_Stopped);

      if GVD.Preferences.Debugger_Kind.Get_Pref = Gdb_MI then
         GPS.Kernel.Actions.Register_Action
           (Kernel, "assembly_view disassemble subprogram",
            Command     => new Disassemble_Subprogram_Command,
            Description => "Disassemble current subprogram",
            Category    => -"Debug",
            Filter      => Debugger_Stopped and
              new Subprogram_Filter);
         GPS.Kernel.Modules.UI.Register_Contextual_Menu
           (Kernel => Kernel,
            Label  => -"Debug/Disassemble subprogram %e",
            Action => "assembly_view disassemble subprogram");
      end if;
   end Register_Module;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Widget : access Assembly_View_Record'Class) return Gtk_Widget
   is
      Hook     : access On_Pref_Changed;
      Scrolled : Gtk_Scrolled_Window;

      Column_Types : constant GType_Array :=
        (PC_Pixmap_Column     => GType_String,
         Address_Column       => GType_String,
         Method_Offset_Column => GType_String,
         Instr_Column         => GType_String,
         Opcodes_Column       => GType_String,
         FG_Color_Column      => Gdk.RGBA.Get_Type,
         BG_Color_Column      => Gdk.RGBA.Get_Type);

      Col           : Gtk_Tree_View_Column;
      Render        : Gtk_Cell_Renderer_Text;
      Pixmap_Render : Gtk_Cell_Renderer_Pixbuf;
      Col_Number    : Gint with Unreferenced;
   begin
      Initialize_Vbox (Widget, Homogeneous => False);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Widget.Pack_Start (Scrolled, Expand => True, Fill => True);

      Gtk_New (Widget.Model, Column_Types);
      Gtk_New (Widget.Tree,  Widget.Model);
      Widget.Tree.Get_Selection.Set_Mode (Selection_Single);
      Widget.Tree.Set_Headers_Visible (False);
      Widget.Tree.Set_Enable_Search (False);
      Widget.Tree.Set_Show_Expanders (False);
      Add (Scrolled, Widget.Tree);

      Gtk_New (Col);
      Col_Number := Widget.Tree.Append_Column (Col);
      Col.Set_Resizable (True);
      Col.Set_Reorderable (False);
      Col.Set_Clickable (False);
      Gtk_New (Pixmap_Render);
      Col.Pack_Start (Pixmap_Render, False);
      Col.Add_Attribute (Pixmap_Render, "icon-name", PC_Pixmap_Column);

      Gtk_New (Col);
      Col_Number := Widget.Tree.Append_Column (Col);
      Col.Set_Resizable (True);
      Col.Set_Reorderable (False);
      Col.Set_Clickable (False);
      Gtk_New (Render);
      Col.Pack_Start (Render, False);
      Col.Add_Attribute (Render, "text", Address_Column);
      Col.Add_Attribute (Render, "foreground-rgba", FG_Color_Column);
      Col.Add_Attribute (Render, "background-rgba", BG_Color_Column);
      if not Asm_Show_Addresses.Get_Pref then
         Col.Set_Visible (False);
      end if;

      Gtk_New (Col);
      Col_Number := Widget.Tree.Append_Column (Col);
      Col.Set_Resizable (False);
      Col.Set_Reorderable (False);
      Col.Set_Clickable (False);
      Gtk_New (Render);
      Col.Pack_Start (Render, False);
      Col.Add_Attribute (Render, "text", Method_Offset_Column);
      Col.Add_Attribute (Render, "foreground-rgba", FG_Color_Column);
      Col.Add_Attribute (Render, "background-rgba", BG_Color_Column);
      if not Asm_Show_Offset.Get_Pref then
         Col.Set_Visible (False);
      end if;

      Gtk_New (Col);
      Col_Number := Widget.Tree.Append_Column (Col);
      Col.Set_Resizable (False);
      Col.Set_Reorderable (False);
      Col.Set_Clickable (False);
      Gtk_New (Render);
      Col.Pack_Start (Render, False);
      Col.Add_Attribute (Render, "markup", Instr_Column);
      Col.Add_Attribute (Render, "foreground-rgba", FG_Color_Column);
      Col.Add_Attribute (Render, "background-rgba", BG_Color_Column);

      Gtk_New (Col);
      Col_Number := Widget.Tree.Append_Column (Col);
      Col.Set_Resizable (False);
      Col.Set_Reorderable (False);
      Col.Set_Clickable (False);
      Gtk_New (Render);
      Col.Pack_Start (Render, False);
      Col.Add_Attribute (Render, "text", Opcodes_Column);
      Col.Add_Attribute (Render, "foreground-rgba", FG_Color_Column);
      Col.Add_Attribute (Render, "background-rgba", BG_Color_Column);
      if not Asm_Show_Opcodes.Get_Pref then
         Col.Set_Visible (False);
      end if;

      Assembly_View_Event_Cb.Object_Connect
        (Widget.Tree, Signal_Key_Press_Event,
         Assembly_View_Event_Cb.To_Marshaller (Key_Press_Cb'Access),
         Widget);

      Configure (Assembly_View (Widget), Default_Style.Get_Pref_Font);

      Hook      := new On_Pref_Changed;
      Hook.View := Assembly_View (Widget);
      Preferences_Changed_Hook.Add (Hook, Watch => Widget);

      Debugger_Location_Changed_Hook.Add
        (new On_Location_Changed, Watch => Widget);

      return Gtk_Widget (Widget.Tree);
   end Initialize;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Scroll_PC_Command_Context;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      View    : constant Assembly_View :=
        Assembly_View (Assembly_MDI_Views.Get_Or_Create_View (Kernel));

   begin
      Meta_Scroll_PC (View);
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Scroll_Command_Context;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      Kernel  : constant Kernel_Handle := Get_Kernel (Context.Context);
      View    : constant Assembly_View :=
        Assembly_View (Assembly_MDI_Views.Get_Or_Create_View (Kernel));

   begin
      Meta_Scroll (View, Down => Command.Down);
      return Commands.Success;
   end Execute;

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
        or else Pref = Preference (Default_Style)
      then
         Set_Font (Self.View, Default_Style.Get_Pref_Font);
      end if;

      if Pref = null
        or else Pref = Preference (Asm_Show_Addresses)
      then
         if Asm_Show_Addresses.Get_Pref then
            Self.View.Tree.Get_Column (Address_Column).Set_Visible (True);
         else
            Self.View.Tree.Get_Column (Address_Column).Set_Visible (False);
         end if;
      end if;

      if Pref = null
        or else Pref = Preference (Asm_Show_Offset)
      then
         if Asm_Show_Offset.Get_Pref then
            Self.View.Tree.Get_Column
              (Method_Offset_Column).Set_Visible (True);
         else
            Self.View.Tree.Get_Column
              (Method_Offset_Column).Set_Visible (False);
         end if;
      end if;

      if Pref = null
        or else Pref = Preference (Asm_Show_Opcodes)
      then
         if Asm_Show_Opcodes.Get_Pref then
            Self.View.Tree.Get_Column (Opcodes_Column).Set_Visible (True);
         else
            Self.View.Tree.Get_Column (Opcodes_Column).Set_Visible (False);
         end if;
      end if;

      Update (Self.View);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Disassemble_Subprogram_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      View     : constant Assembly_View :=
        Assembly_View (Assembly_MDI_Views.Get_Or_Create_View (Kernel));
      Process  : Visual_Debugger;
      File     : Virtual_File;
      From, To : Natural := 0;

   begin

      if View = null then
         return Commands.Failure;
      end if;

      if Has_Entity_Name_Information (Context.Context) then
         declare
            Entity : constant Root_Entity'Class :=
              Get_Entity (Context.Context);
            Loc    : General_Location;
         begin
            Loc  := Entity.Get_Body;
            File := Loc.File;
            From := Loc.Line;
            To   := Entity.End_Of_Scope.Line;
         end;
      end if;

      if (File = No_File or else From = 0)
        and then Has_File_Information (Context.Context)
        and then Has_Line_Information (Context.Context)
      then
         declare
            Files : constant GNATCOLL.VFS.File_Array :=
              File_Information (Context.Context);
         begin
            File := Files (Files'First);
            From := Line_Information (Context.Context);
         end;
      end if;

      Process := Get_Process (View);
      if Process /= null
        and then File /= No_File
      then
         On_Frame_Changed (View, File, From, To);
         Highlight (View);
         return Commands.Success;

      else
         return Commands.Failure;
      end if;
   end Execute;

end GVD.Assembly_View;
