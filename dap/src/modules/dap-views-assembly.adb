------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2022-2023, AdaCore                  --
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

with Ada.Finalization;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Glib;                       use Glib;
with Glib.Convert;
with Glib.Values;
with Glib_Values_Utils;          use Glib_Values_Utils;

with Gdk.Event;                  use Gdk.Event;
with Gdk.RGBA;                   use Gdk.RGBA;
with Gdk.Types.Keysyms;          use Gdk.Types.Keysyms;

with Gtk.Box;                    use Gtk.Box;
with Gtk.Cell_Renderer_Text;     use Gtk.Cell_Renderer_Text;
with Gtk.Cell_Renderer_Pixbuf;   use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Enums;                  use Gtk.Enums;
with Gtk.Handlers;
with Gtk.Menu;
with Gtk.Scrolled_Window;        use Gtk.Scrolled_Window;
with Gtk.Tree_Model;             use Gtk.Tree_Model;
with Gtk.Tree_Store;             use Gtk.Tree_Store;
with Gtk.Tree_View;              use Gtk.Tree_View;
with Gtk.Tree_View_Column;       use Gtk.Tree_View_Column;
with Gtk.Widget;                 use Gtk.Widget;

with Pango.Font;                 use Pango.Font;
with Gtkada.MDI;

with VSS.Characters.Latin;
with VSS.Strings.Conversions;
with VSS.Strings.Cursors.Iterators.Characters;

with GPS.Debuggers;              use GPS.Debuggers;
with GPS.Default_Styles;
with GPS.Kernel.Actions;
with GPS.Kernel.Hooks;           use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;             use GPS.Kernel.MDI;
with GPS.Kernel.Preferences;     use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;         use GPS.Kernel.Scripts;
with GPS.Kernel.Style_Manager;
with Default_Preferences;        use Default_Preferences;

with Commands;                   use Commands;
with Commands.Interactive;       use Commands.Interactive;

with Debugger_Pixmaps;

with DAP.Clients;                use DAP.Clients;
with DAP.Clients.Breakpoint_Managers;
with DAP.Clients.Stack_Trace;    use DAP.Clients.Stack_Trace;
with DAP.Tools;                  use DAP.Tools;
with DAP.Types;                  use DAP.Types;
with DAP.Modules.Preferences;    use DAP.Modules.Preferences;
with DAP.Requests.Disassemble;
with DAP.Types.Breakpoints;      use DAP.Types.Breakpoints;
with DAP.Utils;                  use DAP.Utils;

package body DAP.Views.Assembly is

   type Cache_Data;
   type Cache_Data_Access is access Cache_Data;
   type Cache_Data is record
      Low, High : Address_Type;
      --  The low and high ranges for this item

      Data      : Disassemble_Elements;
      --  The assembly code for that range

      Next       : Cache_Data_Access;
      Subprogram : Boolean := False;
   end record;
   --  This implements a cache for the assembly code, for specific ranges. Some
   --  debuggers (e.g: gdb) might take a long time to output the assembly code
   --  for a specific region, so it is better to keep it once we have it.

   type Direction_Kind is (Prepend, Append, Full);
   --  How to manage next portion of the data

   type Assembly_View_Record is new View_Record with
      record
         Tree          : Gtk.Tree_View.Gtk_Tree_View;
         Model         : Gtk.Tree_Store.Gtk_Tree_Store;
         --  The actual contents of the viewer

         Cache         : Cache_Data_Access;
         Current_Range : Cache_Data_Access;
         --  The range of assembly code being displayed.
      end record;
   type Assembly_View is access all Assembly_View_Record'Class;

   overriding procedure On_Process_Terminated
     (Self : not null access Assembly_View_Record);
   overriding procedure On_Status_Changed
     (Self   : not null access Assembly_View_Record;
      Status : GPS.Debuggers.Debugger_State);
   overriding procedure On_Location_Changed
     (Self : not null access Assembly_View_Record);
   overriding procedure Update (Self : not null access Assembly_View_Record);

   overriding procedure Create_Menu
     (Self : not null access Assembly_View_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class);

   procedure Configure
     (Self : Assembly_View;
      Font : Pango.Font.Pango_Font_Description);
   --  Set the various settings of the assembly view.
   --  Ps_Font_Name is the name of the postscript font that will be used to
   --  display the text. It should be a fixed-width font, which is nice for
   --  source code.

   function Initialize
     (Widget : access Assembly_View_Record'Class) return Gtk_Widget;
   --  Internal initialization function

   procedure Set_Font
     (Self : Assembly_View;
      Font : Pango.Font.Pango_Font_Description);
   --  Set the font used for the box.
   --  This is called by Configure internally.

   procedure Free_Cache (Self : Assembly_View);
   --  Free local cahed data

   procedure Fill_Model
     (Self     : Assembly_View;
      Elements : Disassemble_Elements);
   --  Set models data. The Hightlighting is reset.

   procedure Get_Machine_Code
     (Self      : Assembly_View;
      Start     : Address_Type;
      Last      : Address_Type;
      Direction : Direction_Kind);
   --  Sends DAP request

   package Assembly_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name                     => "Assembly_View",
      View_Name                       => "Assembly",
      Formal_View_Record              => Assembly_View_Record,
      Formal_MDI_Child                => GPS_MDI_Child_Record,
      Reuse_If_Exist                  => True,
      Save_Duplicates_In_Perspectives => False,
      Commands_Category               => "",
      Areas                           => Gtkada.MDI.Both,
      Group                           => Group_Debugger_Stack,
      Position                        => Gtkada.MDI.Position_Right,
      Initialize                      => Initialize,
      Local_Config                    => True,
      Local_Toolbar                   => True);
   subtype Assembly_MDI is Assembly_MDI_Views.View_Access;
   use type Assembly_MDI;

   package Assembly_Views is new DAP.Views.Simple_Views
     (Formal_Views       => Assembly_MDI_Views,
      Formal_View_Record => Assembly_View_Record,
      Formal_MDI_Child   => GPS_MDI_Child_Record);

   package Assembly_View_Event_Cb is
     new Gtk.Handlers.Return_Callback (Assembly_View_Record, Boolean);

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

   procedure Highlight
     (View         : access Assembly_View_Record'Class;
      Scroll_To_Pc : Boolean := True);
   --  Redo the buffer highlighting

   procedure On_Frame_Changed
     (View          : Assembly_View;
      Start_Address : Address_Type);
   --  Called when the assembly code for the address PC needs to be loaded.
   --  This gets the assembly source code for a range starting at PC, and
   --  going up to End_Pc.

   function In_Range
     (Address : Address_Type;
      R       : Cache_Data_Access) return Boolean;
   --  Return True if Address is in the range of addresses described by R.

   function Find_In_Cache
     (View    : Assembly_View;
      Address : Address_Type) return Cache_Data_Access;
   --  Return the cached data that contains Address.
   --  null is returned if none is found.

   procedure Find_Iter_Location_Bounds
     (View         : Assembly_View;
      Current_Iter : Gtk_Tree_Iter;
      Start_Iter   : out Gtk_Tree_Iter;
      End_Iter     : out Gtk_Tree_Iter);
   --  Find the start and end iter at the same location as Current_Iter

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

   type On_Breakpoint_Added_Or_Deleted is
     new Debugger_Breakpoint_Hook_Function
      with null record;
   overriding procedure Execute
      (Self     : On_Breakpoint_Added_Or_Deleted;
       Kernel   : not null access Kernel_Handle_Record'Class;
       Debugger : access Base_Visual_Debugger'Class;
       Id       : Integer);
   --   Called when the breakpoint has been added or deleted

   type On_Breakpoints_Changed is new Debugger_Hooks_Function
      with null record;
   overriding procedure Execute
      (Self     : On_Breakpoints_Changed;
       Kernel   : not null access Kernel_Handle_Record'Class;
       Debugger : access Base_Visual_Debugger'Class);
   --   Called when the breakpoints might have changed

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      View : Assembly_View;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed, to refresh the editor
   --  appropriately.

   type Breakpoint_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Create/delete a breakpoint

   procedure Free (Data : in out Cache_Data_Access);

   procedure Unchecked_Free is
     new Ada.Unchecked_Deallocation (Cache_Data, Cache_Data_Access);

   ---------------------------------
   -- Off_On_Sensitive_Controller --
   ---------------------------------

   type Off_On_Sensitive_Controller
     (View  : access Assembly_View_Record'Class) is new
     Ada.Finalization.Limited_Controlled with null record;
   --  This type makes view's tree insensitive on initialization and restore
   --  sensitivity on destruction. We use it because we don't want to process
   --  too many key events while performing the disassemble operation because
   --  several such operation processing one by one can hung GNAT Studio for
   --  a while.

   overriding procedure Initialize (Self : in out Off_On_Sensitive_Controller);
   overriding procedure Finalize (Self : in out Off_On_Sensitive_Controller);

   -----------------------------
   -- Disassemble_DAP_Request --
   -----------------------------

   type Request is
     new DAP.Requests.Disassemble.Disassemble_DAP_Request with record
      Direction : Direction_Kind;
   end record;
   type Request_Access is access all Request;

   overriding procedure On_Result_Message
     (Self        : in out Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.DisassembleResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access);

   -- Commands --

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
   Instr_Column         : constant := 2;
   Opcodes_Column       : constant := 3;
   FG_Color_Column      : constant := 4;
   BG_Color_Column      : constant := 5;
   File_Column          : constant := 6;
   Line_Column          : constant := 7;
   Symbol_Column        : constant := 8;

   Can_Not_Get : constant String := "Couldn't get assembly code";

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Widget : access Assembly_View_Record'Class) return Gtk_Widget
   is
      Scrolled : Gtk_Scrolled_Window;

      Column_Types : constant GType_Array :=
        (PC_Pixmap_Column => GType_String,
         Address_Column   => GType_String,
         Instr_Column     => GType_String,
         Opcodes_Column   => GType_String,
         FG_Color_Column  => Gdk.RGBA.Get_Type,
         BG_Color_Column  => Gdk.RGBA.Get_Type,
         File_Column      => GType_String,
         Line_Column      => GType_Int,
         Symbol_Column    => GType_String);

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
      Widget.Tree.Set_Enable_Search (False);
      Widget.Tree.Set_Show_Expanders (False);
      Add (Scrolled, Widget.Tree);

      --  PC_Pixmap_Column
      Gtk_New (Col);
      Col_Number := Widget.Tree.Append_Column (Col);
      Col.Set_Resizable (True);
      Col.Set_Reorderable (False);
      Col.Set_Clickable (False);
      Gtk_New (Pixmap_Render);
      Col.Pack_Start (Pixmap_Render, False);
      Col.Add_Attribute (Pixmap_Render, "icon-name", PC_Pixmap_Column);

      --  Address_Column
      Gtk_New (Col);
      Col_Number := Widget.Tree.Append_Column (Col);
      Col.Set_Title ("Address");
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

      --  Instr_Column
      Gtk_New (Col);
      Col_Number := Widget.Tree.Append_Column (Col);
      Col.Set_Title ("Instruction");
      Col.Set_Resizable (True);
      Col.Set_Reorderable (False);
      Col.Set_Clickable (False);
      Gtk_New (Render);
      Col.Pack_Start (Render, False);
      Col.Add_Attribute (Render, "markup", Instr_Column);
      Col.Add_Attribute (Render, "foreground-rgba", FG_Color_Column);
      Col.Add_Attribute (Render, "background-rgba", BG_Color_Column);

      --  Opcodes_Column
      Gtk_New (Col);
      Col_Number := Widget.Tree.Append_Column (Col);
      Col.Set_Title ("Opcode");
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

      Debugger_Breakpoints_Changed_Hook.Add
         (new On_Breakpoints_Changed, Watch => Widget);
      Debugger_Breakpoint_Added_Hook.Add
         (new On_Breakpoint_Added_Or_Deleted, Watch => Widget);
      Debugger_Breakpoint_Deleted_Hook.Add
         (new On_Breakpoint_Added_Or_Deleted, Watch => Widget);

      Preferences_Changed_Hook.Add
        (Obj   =>
            new On_Pref_Changed'
           (Hook_Function with View => Assembly_View (Widget)),
         Watch => Widget);

      return Gtk_Widget (Widget.Tree);
   end Initialize;

   ---------------
   -- Configure --
   ---------------

   procedure Configure
     (Self : Assembly_View;
      Font : Pango.Font.Pango_Font_Description) is
   begin
      --  Font
      Set_Font (Self, Font);
   end Configure;

   ----------------
   -- Fill_Model --
   ----------------

   procedure Fill_Model
     (Self     : Assembly_View;
      Elements : Disassemble_Elements)
   is
      use VSS.Strings;

      Model   : Gtk.Tree_Store.Gtk_Tree_Store renames Self.Model;
      Row     : Gtk_Tree_Iter;
      Values  : Glib.Values.GValue_Array (1 .. 9);
      Columns : Columns_Array (Values'Range);
      Last    : Gint := 0;
   begin
      Model.Clear;

      --  May happens when we don't have an address to disassemble,
      --  for example when we did not start the execution yet.
      if Elements.Is_Empty then
         Model.Append (Row, Null_Iter);
         Columns (1) := Instr_Column;
         Values  (1) := As_String
           ("<b>" & Glib.Convert.Escape_Text (Can_Not_Get) & "</b>");
         Set_And_Clear (Model, Row, Columns (1 .. 1), Values (1 .. 1));
         return;
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

         if not El.Instr.Is_Empty then
            Last           := Last + 1;
            Columns (Last) := Instr_Column;
            if El.Instr = To_Virtual_String (Can_Not_Get) then
               Values  (Last) := As_String
                 ("<b>" & Glib.Convert.Escape_Text (Can_Not_Get) & "</b>");
            else
               Values (Last) := As_String
                 (Get_Markup_For_Language
                    (Self.Kernel,
                     "ASM",
                     Glib.Convert.Escape_Text (To_UTF_8_String (El.Instr))));
            end if;

         end if;

         if not El.Opcodes.Is_Empty then
            Last           := Last + 1;
            Columns (Last) := Opcodes_Column;
            Values  (Last) := As_String (To_UTF_8_String (El.Opcodes));
         end if;

         if El.File /= No_File then
            Last           := Last + 1;
            Columns (Last) := File_Column;
            Values  (Last) := As_String (Display_Full_Name (El.File));
         end if;

         Last           := Last + 1;
         Columns (Last) := Line_Column;
         Values  (Last) := As_Int (Gint (El.Line));

         if not El.Symbol.Is_Empty then
            Last           := Last + 1;
            Columns (Last) := Symbol_Column;
            Values  (Last) := As_String (To_UTF_8_String (El.Symbol));
         end if;

         Set_And_Clear (Model, Row, Columns (1 .. Last), Values (1 .. Last));
      end loop;
   end Fill_Model;

   ----------------
   -- Free_Cache --
   ----------------

   procedure Free_Cache (Self : Assembly_View) is
      Prev, Tmp : Cache_Data_Access;
   begin
      if Self.Current_Range /= Invalid_Cache_Data then
         Tmp := Self.Cache;
         while Tmp /= null
           and then Tmp /= Self.Current_Range
         loop
            Prev := Tmp;
            Tmp  := Tmp.Next;
         end loop;

         if Tmp /= null then
            if Prev /= null then
               Prev.Next := Tmp.Next;
            else
               Self.Cache := Tmp.Next;
            end if;
            Free (Tmp);
         end if;
      end if;

      Self.Current_Range := null;
   end Free_Cache;

   ----------
   -- Free --
   ----------

   procedure Free (Data : in out Cache_Data_Access) is
   begin
      Unchecked_Free (Data);
   end Free;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (Self : not null access Assembly_View_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      K : constant Kernel_Handle := Self.Kernel;
   begin
      Append_Menu (Menu, K, Asm_Show_Addresses);
      Append_Menu (Menu, K, Asm_Show_Opcodes);
   end Create_Menu;

   ------------------
   -- Key_Press_Cb --
   ------------------

   function Key_Press_Cb
     (View  : access Assembly_View_Record'Class;
      Event : Gdk_Event) return Boolean is
   begin
      case Get_Key_Val (Event) is
         when GDK_Page_Down =>
            declare
               C : Off_On_Sensitive_Controller (View) with Unreferenced;
            begin
               Meta_Scroll_Down (Assembly_View (View));
            end;
            return True;

         when GDK_Page_Up =>
            declare
               C : Off_On_Sensitive_Controller (View) with Unreferenced;
            begin
               Meta_Scroll_Up (Assembly_View (View));
            end;
            return True;

         when GDK_Home =>
            Meta_Scroll_PC (Assembly_View (View));
            return True;

         when others => null;
      end case;

      return False;
   end Key_Press_Cb;

   ---------------------------
   -- On_Process_Terminated --
   ---------------------------

   overriding procedure On_Process_Terminated
     (Self : not null access Assembly_View_Record) is
   begin
      Clear (-Get_Model (Self.Tree));
      Free_Cache (Assembly_View (Self));
   end On_Process_Terminated;

   -----------------------
   -- On_Status_Changed --
   -----------------------

   overriding procedure On_Status_Changed
     (Self   : not null access Assembly_View_Record;
      Status : GPS.Debuggers.Debugger_State)
   is
      use GPS.Debuggers;
   begin
      if Status = Debug_Busy then
         --  The debugger is now executing a command that will likely change
         --  the current stack trace. While it is executing, we do not want to
         --  keep a visible call stack displayed.

         Self.Current_Range := Invalid_Cache_Data;
         Assembly_View (Self).Fill_Model (Self.Current_Range.Data);
      end if;
   end On_Status_Changed;

   --------------
   -- Set_Font --
   --------------

   procedure Set_Font
     (Self : Assembly_View;
      Font : Pango_Font_Description) is
   begin
      if Self = null then
         return;
      end if;

      Modify_Font (Self.Tree, Font);
   end Set_Font;

   -----------------------
   -- Iter_From_Address --
   -----------------------

   procedure Iter_From_Address
     (View    : not null access Assembly_View_Record'Class;
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

   -------------------------------
   -- Find_Iter_Location_Bounds --
   -------------------------------

   procedure Find_Iter_Location_Bounds
     (View         : Assembly_View;
      Current_Iter : Gtk_Tree_Iter;
      Start_Iter   : out Gtk_Tree_Iter;
      End_Iter     : out Gtk_Tree_Iter)
   is
      use Ada.Strings.Unbounded;
      Model        : Gtk.Tree_Store.Gtk_Tree_Store renames View.Model;
      Current_File : Unbounded_String;
      Current_Line : Gint := 0;

      function Has_File_Information (Iter : Gtk_Tree_Iter) return Boolean;
      --  Check if Iter contains file information

      function Compare_File_Information (Iter : Gtk_Tree_Iter) return Boolean;
      --  Check if we have the same information at the stored one

      --------------------------
      -- Has_File_Information --
      --------------------------

      function Has_File_Information (Iter : Gtk_Tree_Iter) return Boolean is
      begin
         return Model.Get_String (Iter, File_Column) /= ""
           and then Model.Get_Int (Iter, Line_Column) /= 0;
      end Has_File_Information;

      ------------------------------
      -- Compare_File_Information --
      ------------------------------

      function Compare_File_Information (Iter : Gtk_Tree_Iter) return Boolean
      is
      begin
         return Model.Get_String (Iter, File_Column) = To_String (Current_File)
           and then Model.Get_Int (Iter, Line_Column) /= Current_Line;
      end Compare_File_Information;
   begin
      Start_Iter := Current_Iter;
      --  Find the first previously Iter with File Information
      while Start_Iter /= Null_Iter
        and then not Has_File_Information (Start_Iter)
      loop
         Model.Previous (Start_Iter);
      end loop;

      if Start_Iter = Null_Iter then
         return;
      end if;

      --  Save the File Information
      Current_File :=
        To_Unbounded_String (Model.Get_String (Start_Iter, File_Column));
      Current_Line := Model.Get_Int (Start_Iter, Line_Column);

      --  Find the first previous Iter with the exact same File Information
      while Start_Iter /= Null_Iter
        and then Has_File_Information (Start_Iter)
        and then Compare_File_Information (Start_Iter)
      loop
         Model.Previous (Start_Iter);
      end loop;

      --  Find the last next Iter with the same File Information
      End_Iter := Current_Iter;
      while End_Iter /= Null_Iter
        and then
          (not Has_File_Information (End_Iter)
           or else not Compare_File_Information (End_Iter))
      loop
         Model.Next (End_Iter);
      end loop;
   end Find_Iter_Location_Bounds;

   ---------------
   -- Highlight --
   ---------------

   procedure Highlight
     (View         : access Assembly_View_Record'Class;
      Scroll_To_Pc : Boolean := True)
   is
      use DAP.Types.Breakpoints;

      Client   : constant DAP.Clients.DAP_Client_Access := Get_Client (View);
      Model    : Gtk.Tree_Store.Gtk_Tree_Store renames View.Model;
      Values   : Glib.Values.GValue_Array (1 .. 3);
      Columns  : Columns_Array (Values'Range);
      Iter     : Gtk_Tree_Iter;
      Found    : Boolean;

      Detached : Gtk.Tree_Model.Gtk_Tree_Model;
      Last     : Address_Type := Invalid_Address;

      First_Visible_Line_Iter : Gtk_Tree_Iter := Null_Iter;
      Selected_Line_Iter      : Gtk_Tree_Iter := Null_Iter;
   begin
      if View = null
        or else Client = null
      then
         return;
      end if;

      if not Scroll_To_Pc then
         --  Detaching the model will reset the scrolling and the selection so
         --  store them now.
         declare
            From : Gtk_Tree_Path  := Null_Gtk_Tree_Path;
            To   : Gtk_Tree_Path  := Null_Gtk_Tree_Path;
            M    : Gtk_Tree_Model := Null_Gtk_Tree_Model;
         begin
            View.Tree.Get_Visible_Range (From, To, Found);
            if Found then
               First_Visible_Line_Iter := Get_Iter (View.Tree.Get_Model, From);
               Path_Free (From);
               Path_Free (To);
            end if;

            View.Tree.Get_Selection.Get_Selected (M, Selected_Line_Iter);
         end;
      end if;

      Detached := View.Tree.Get_Model;
      View.Tree.Set_Model (Null_Gtk_Tree_Model);

      --  Reset the current highlighting
      Columns := (FG_Color_Column, BG_Color_Column, PC_Pixmap_Column);
      Glib.Values.Init (Values (1), Gdk.RGBA.Get_Type);
      Gdk.RGBA.Set_Value (Values (1), Null_RGBA);
      Glib.Values.Init (Values (2), Gdk.RGBA.Get_Type);
      Gdk.RGBA.Set_Value (Values (2), Null_RGBA);
      Values (3) := As_String ("");

      Iter := Model.Get_Iter_First;
      while Iter /= Null_Iter loop
         Model.Set (Iter, Glib.Gint_Array (Columns), Values);
         Model.Next (Iter);
      end loop;
      Unset (Values);

      --  Highlight breakpoint lines
      Columns (1) := BG_Color_Column;
      for Data of Client.Get_Breakpoints_Manager.Get_Breakpoints loop
         if Data.Kind in On_Line | On_Instruction then
            Iter_From_Address
              (View    => View,
               Address => Data.Location.Address,
               Iter    => Iter,
               Found   => Found);

            if Found then
               Glib.Values.Init (Values (1), Gdk.RGBA.Get_Type);
               Gdk.RGBA.Set_Value
                 (Values (1),
                  (if not Data.Enabled then
                        GPS.Kernel.Style_Manager.Background
                     (GPS.Default_Styles.Debugger_Disabled_Breakpoint_Style)
                   elsif not Data.Condition.Is_Empty then
                      GPS.Kernel.Style_Manager.Background
                     (GPS.Default_Styles.Debugger_Conditional_Breakpoint_Style)
                   else
                      GPS.Kernel.Style_Manager.Background
                     (GPS.Default_Styles.Debugger_Breakpoint_Style)));

               Set_And_Clear
                 (Model,
                  Iter,
                  Columns (1 .. 1),
                  Values (1 .. 1));
            end if;
         end if;
      end loop;

      --  Highlight PC line
      Iter_From_Address
        (View, Client.Get_Stack_Trace.Get_Current_Address, Iter, Found);

      if Found then
         Model.Set
           (Iter, PC_Pixmap_Column,
            Ada.Strings.Unbounded.To_String
              (Debugger_Pixmaps.Current_Line_Pixbuf));

      elsif In_Range
        (Client.Get_Stack_Trace.Get_Current_Address, View.Current_Range)
      then
         for Index in 1 .. Natural (View.Current_Range.Data.Length) loop
            exit when View.Current_Range.Data.Element
              (Index).Address > Client.Get_Stack_Trace.Get_Current_Address;

            Last := View.Current_Range.Data.Element (Index).Address;
         end loop;

         if Last /= Invalid_Address then
            Iter_From_Address (View, Last, Iter, Found);
            if Found then
               Model.Set
                 (Iter, PC_Pixmap_Column,
                  Ada.Strings.Unbounded.To_String
                    (Debugger_Pixmaps.Current_Line_Inside_Pixbuf));
            end if;
         end if;
      end if;

      --  Highlight the instructions at the same line as PC
      if Iter /= Null_Iter then
         declare
            Start_Iter : Gtk_Tree_Iter;
            End_Iter   : Gtk_Tree_Iter;
         begin
            Find_Iter_Location_Bounds
              (Assembly_View (View), Iter, Start_Iter, End_Iter);

            Columns (1) := BG_Color_Column;
            Glib.Values.Init (Values (1), Gdk.RGBA.Get_Type);
            Gdk.RGBA.Set_Value
              (Values (1), Debugger_Current_Line_Color.Get_Pref);

            Iter := Start_Iter;
            while Iter /= Null_Iter and then Iter /= End_Iter loop
               Model.Set
                 (Iter,
                  Glib.Gint_Array (Columns (1 .. 1)),
                  Values (1 .. 1));

               Model.Next (Iter);
            end loop;
            Unset (Values (1 .. 1));
         end;
      end if;

      View.Tree.Set_Model (Detached);

      if Scroll_To_Pc then
         if Iter /= Null_Iter then
            View.Tree.Get_Selection.Select_Iter (Iter);

            declare
               P : Gtk_Tree_Path;
            begin
               P := Get_Path (View.Tree.Get_Model, Iter);
               View.Tree.Scroll_To_Cell (P, null, True, 0.0, 0.0);
               Path_Free (P);
            end;
         end if;

      else
         --  Restore the scrolling and the selection
         if First_Visible_Line_Iter /= Null_Iter then
            declare
               P : Gtk_Tree_Path;
            begin
               P := Get_Path (View.Tree.Get_Model, First_Visible_Line_Iter);
               View.Tree.Scroll_To_Cell (P, null, True, 0.0, 0.0);
               Path_Free (P);
            end;
         end if;

         if Selected_Line_Iter /= Null_Iter then
            View.Tree.Get_Selection.Select_Iter (Selected_Line_Iter);
         end if;
      end if;
   end Highlight;

   ----------------------
   -- Get_Machine_Code --
   ----------------------

   procedure Get_Machine_Code
     (Self      : Assembly_View;
      Start     : Address_Type;
      Last      : Address_Type;
      Direction : Direction_Kind)
   is
      Client : constant DAP.Clients.DAP_Client_Access := Get_Client (Self);
      Req    : Request_Access;
   begin
      if Client = null then
         return;
      end if;

      Req := new Request (Self.Kernel);

      Req.Direction := Direction;
      Req.Parameters.arguments.memoryReference :=
        VSS.Strings.Conversions.To_Virtual_String (Address_To_String (Start));
      Req.Parameters.arguments.instructionCount :=
        Integer (Address_To_Integer (Last) - Address_To_Integer (Start));
      Req.Parameters.arguments.resolveSymbols := False;

      Client.Enqueue (DAP.Requests.DAP_Request_Access (Req));
   end Get_Machine_Code;

   -----------------------
   -- On_Result_Message --
   -----------------------

   overriding procedure On_Result_Message
     (Self        : in out Request;
      Client      : not null access DAP.Clients.DAP_Client'Class;
      Result      : in out DAP.Tools.DisassembleResponse;
      New_Request : in out DAP.Requests.DAP_Request_Access)
   is
      pragma Unreferenced (New_Request);

      View : constant Assembly_MDI := Assembly_MDI_Views.Retrieve_View
        (Self.Kernel, True);
      S    : Disassemble_Elements;

      function Format_Opcodes
        (S : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String;
      --  Split S in pair of bytes separated by whitespaces.

      --------------------
      -- Format_Opcodes --
      --------------------

      function Format_Opcodes
        (S : VSS.Strings.Virtual_String) return VSS.Strings.Virtual_String
      is
         Result : VSS.Strings.Virtual_String;
         I      : VSS.Strings.Cursors.Iterators.Characters.
           Character_Iterator := S.At_First_Character;
         Count  : Natural := 0;
         Dummy  : Boolean;
      begin
         while I.Has_Element loop
            Result.Append (I.Element);
            Count := Count + 1;

            if Count = 2 then
               Result.Append (VSS.Characters.Latin.Space);
               Count := 0;
            end if;
            Dummy := I.Forward;
         end loop;

         return Result;
      end Format_Opcodes;

   begin
      if View = null then
         return;
      end if;

      if Result.a_body.Is_Set
        and then Length (Result.a_body.Value.instructions) > 0
      then
         for Index in 1 .. Length (Result.a_body.Value.instructions) loop
            declare
               Line : constant DisassembledInstruction_Variable_Reference :=
                 Get_DisassembledInstruction_Variable_Reference
                   (Result.a_body.Value.instructions, Index);
            begin
               S.Append
                 (Disassemble_Element'
                    (Address       =>
                         String_To_Address (To_UTF8 (Line.address)),
                     Instr         => Line.instruction,
                     Opcodes       => Format_Opcodes (Line.instructionBytes),
                     Symbol        => Line.symbol,
                     File          =>
                       (if Line.location.Is_Set
                        then To_File (Line.location.Value.path)
                        else GNATCOLL.VFS.No_File),
                     Line    => (if Line.line.Is_Set
                                 then Line.line.Value
                                 else 0)));
            end;
         end loop;

         if Self.Direction = Prepend then
            View.Current_Range.Low :=
              Disassemble_Element_Vectors.First_Element (S).Address;
            View.Current_Range.Data.Prepend (S);

         elsif Self.Direction = Append then
            View.Current_Range.High :=
              Disassemble_Element_Vectors.Last_Element (S).Address;

            --  Avoid duplicating the first assembly line since it
            --  was already displayed.
            S.Delete_First;
            View.Current_Range.Data.Append (S);

         else
            View.Cache := new Cache_Data'
              (Low        =>
                 Disassemble_Element_Vectors.First_Element (S).Address,
               High       =>
                 Disassemble_Element_Vectors.Last_Element (S).Address,
               Data       => S,
               Next       => View.Cache,
               Subprogram => False);

            View.Current_Range := View.Cache;
         end if;

         Assembly_View (View).Fill_Model (View.Current_Range.Data);
         Assembly_View (View).Highlight;

      else
         --  If both are null, this means that the server couldn't get the
         --  assembly at all, and there's no point in trying again afterwards.
         View.Current_Range := Invalid_Cache_Data;
         Assembly_View (View).Free_Cache;
      end if;
   end On_Result_Message;

   ----------------------
   -- On_Frame_Changed --
   ----------------------

   procedure On_Frame_Changed
     (View          : Assembly_View;
      Start_Address : Address_Type)
   is
      End_Address    : Address_Type;
      Size           : Integer;
      Start_In_Range : Boolean := False;
      End_In_Range   : Boolean := False;
   begin
      if View = null then
         return;
      end if;

      if View.Current_Range /= null
        and then View.Current_Range.Subprogram
      then
         Free (View.Current_Range);
      end if;

      if Start_Address = Invalid_Address then
         View.Fill_Model (Invalid_Cache_Data.Data);
         return;
      end if;

      --  Is the range already visible ?
      Size := Assembly_Range_Size.Get_Pref;
      if Size = 0 then
         Size := 200;
      end if;

      End_Address := Set_Offset (Start_Address, Size);

      if View.Current_Range /= null then
         if View.Current_Range.Low = Invalid_Address
           or else View.Current_Range.High = Invalid_Address
         then
            Free_Cache (View);

         else
            Start_In_Range := In_Range (Start_Address, View.Current_Range);
            End_In_Range   := In_Range (End_Address, View.Current_Range);
         end if;
      end if;

      if Start_In_Range
        and then End_In_Range
      then
         View.Highlight;
         return;
      end if;

      --  Should we prepend to the current buffer ?
      if End_In_Range then
         View.Get_Machine_Code
           (Start_Address, View.Current_Range.Low, Prepend);

      --  Should we append to the current buffer ?
      elsif Start_In_Range then
         View.Get_Machine_Code
           (View.Current_Range.High, Add_Address (End_Address, 1), Append);

      else
         View.Current_Range := null;
      end if;

      --  Else get a whole new range (minimum size Assembly_Range_Size)
      if View.Current_Range = null then
         View.Current_Range := Find_In_Cache (View, Start_Address);
      end if;

      if View.Current_Range = null then
         View.Get_Machine_Code (Start_Address, End_Address, Full);
      else
         View.Fill_Model (View.Current_Range.Data);
         View.Highlight;
      end if;
   end On_Frame_Changed;

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
      then
         return;
      end if;

      if Down then
         if View.Current_Range.High /= Invalid_Address then
            On_Frame_Changed (View, View.Current_Range.High);
         end if;

      else
         if View.Current_Range.Low /= Invalid_Address then
            Address := Add_Address
              (View.Current_Range.Low,
               (if Assembly_Range_Size.Get_Pref = 0
                then -200
                else -Assembly_Range_Size.Get_Pref));

            if Address /= Invalid_Address then
               On_Frame_Changed (View, Address);
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
      Client : constant DAP.Clients.DAP_Client_Access := Get_Client (View);
      Iter   : Gtk_Tree_Iter;
      Path   : Gtk_Tree_Path;
      Found  : Boolean;
   begin
      if View /= null
        and then Client /= null
      then
         On_Frame_Changed (View, Client.Get_Stack_Trace.Get_Current_Address);

         Iter_From_Address
           (View, Client.Get_Stack_Trace.Get_Current_Address, Iter, Found);

         if Found then
            Path := View.Model.Get_Path (Iter);
            View.Tree.Scroll_To_Cell (Path, null, True, 0.5, 0.0);
            Path_Free (Path);
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

   -------------------------
   -- On_Location_Changed --
   -------------------------

   overriding procedure On_Location_Changed
     (Self : not null access Assembly_View_Record)
   is
      Client : constant DAP.Clients.DAP_Client_Access := Get_Client (Self);
   begin
      if Client /= null then
         Assembly_View (Self).On_Frame_Changed
           (Client.Get_Stack_Trace.Get_Current_Address);
      end if;
   end On_Location_Changed;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Kernel);
      Do_Update : Boolean := False;
   begin
      if Pref = null
        or else Pref = Preference (Default_Style)
      then
         Do_Update := True;
         Set_Font (Self.View, Default_Style.Get_Pref_Font);
      end if;

      if Pref = null
        or else Pref = Preference (Asm_Show_Addresses)
      then
         Do_Update := True;
         if Asm_Show_Addresses.Get_Pref then
            Self.View.Tree.Get_Column (Address_Column).Set_Visible (True);
         else
            Self.View.Tree.Get_Column (Address_Column).Set_Visible (False);
         end if;
      end if;

      if Pref = null
        or else Pref = Preference (Asm_Show_Opcodes)
      then
         Do_Update := True;
         if Asm_Show_Opcodes.Get_Pref then
            Self.View.Tree.Get_Column (Opcodes_Column).Set_Visible (True);
         else
            Self.View.Tree.Get_Column (Opcodes_Column).Set_Visible (False);
         end if;
      end if;

      if Pref = null
        or else Pref = Preference (Keywords_Style)
        or else Pref = Preference (Numbers_Style)
      then
         Do_Update := True;
         Free_Cache (Self.View);
      end if;

      if Do_Update then
         Update (Self.View);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self     : On_Breakpoint_Added_Or_Deleted;
       Kernel   : not null access Kernel_Handle_Record'Class;
       Debugger : access Base_Visual_Debugger'Class;
       Id       : Integer)
   is
      pragma Unreferenced (Self);
      View : constant Assembly_View :=
        Assembly_View (Assembly_MDI_Views.Retrieve_View (Kernel, True));
   begin
      if View /= null then
         View.Highlight (False);
      end if;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
      (Self     : On_Breakpoints_Changed;
       Kernel   : not null access Kernel_Handle_Record'Class;
       Debugger : access Base_Visual_Debugger'Class)
   is
      pragma Unreferenced (Self);
      View : constant Assembly_View := Assembly_View
        (Assembly_MDI_Views.Retrieve_View (Kernel, True));
   begin
      if View /= null then
         View.Highlight (False);
      end if;
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

   ------------
   -- Update --
   ------------

   overriding procedure Update (Self : not null access Assembly_View_Record) is
      use DAP.Clients;

      Client : constant DAP.Clients.DAP_Client_Access := Get_Client (Self);
   begin
      if Client = null then
         Self.Current_Range := Invalid_Cache_Data;
         Assembly_View (Self).Free_Cache;

      else
         Assembly_View (Self).On_Frame_Changed
           (Client.Get_Stack_Trace.Get_Current_Address);
      end if;
   end Update;

   ----------------
   -- Initialize --
   ----------------

   overriding procedure Initialize
     (Self : in out Off_On_Sensitive_Controller) is
   begin
      Self.View.Tree.Set_Sensitive (False);
   end Initialize;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (Self : in out Off_On_Sensitive_Controller) is
   begin
      Self.View.Tree.Set_Sensitive (True);
      Self.View.Tree.Grab_Focus;
   end Finalize;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Breakpoint_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      View     : constant Assembly_View :=
        Assembly_View (Assembly_MDI_Views.Get_Or_Create_View (Kernel));
      Model    : Gtk.Tree_Model.Gtk_Tree_Model;
      Iter     : Gtk.Tree_Model.Gtk_Tree_Iter;
   begin
      View.Tree.Get_Selection.Get_Selected (Model, Iter);

      if Iter = Null_Iter then
         return Commands.Success;
      end if;

      declare
         Client : constant DAP.Clients.DAP_Client_Access := Get_Client (View);
         Str    : constant String := Get_String (Model, Iter, Address_Column);
      begin
         if Client = null or else Str = "" then
            return Commands.Success;
         end if;

         Client.Get_Breakpoints_Manager.Toggle_Instruction_Breakpoint
           (String_To_Address (Str));
      end;

      return Commands.Success;
   end Execute;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Debugger_Available : Action_Filter;
   begin
      Invalid_Cache_Data.Data.Append
        (Disassemble_Element'
           (Address => Invalid_Address,
            Instr   => To_Virtual_String (Can_Not_Get),
            others  => <>));

      Assembly_Views.Register_Module (Kernel);
      Assembly_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open assembly view",
         Description => "Open the Assembly view for the debugger");

      Debugger_Available := Kernel.Lookup_Filter ("Debugger available");

      GPS.Kernel.Actions.Register_Action
        (Kernel, "assembly_view disassemble next",
         Command     => new Scroll_Command_Context'
           (Interactive_Command with Down => True),
         Description => "Disassemble next code block",
         Icon_Name   => "gps-debugger-down-symbolic",
         Category    => "Debug",
         Filter      => Debugger_Available);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "assembly_view disassemble previous",
         Command     => new Scroll_Command_Context'
           (Interactive_Command with Down => False),
         Description => "Disassemble previous code block",
         Icon_Name   => "gps-debugger-up-symbolic",
         Category    => "Debug",
         Filter      => Debugger_Available);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "assembly_view disassemble pc",
         Command     => new Scroll_PC_Command_Context,
         Description => "Disassemble $pc code block",
         Icon_Name   => "gps-debugger-step-symbolic",
         Category    => "Debug",
         Filter      => Debugger_Available);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "assembly_view toggle breakpoint",
         Command     => new Breakpoint_Command,
         Description => "Create/delete a breakpoint on address",
         Icon_Name   => "gps-emblem-debugger-current",
         Category    => "Debug",
         Filter      => Debugger_Available);
   end Register_Module;

end DAP.Views.Assembly;
