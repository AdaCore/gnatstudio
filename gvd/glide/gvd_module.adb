-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                     Copyright (C) 2001-2002                       --
--                             ACT-Europe                            --
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
with Glib.Object;             use Glib.Object;
with Glib.Values;             use Glib.Values;
with Gdk.Drawable;            use Gdk.Drawable;
with Gdk.Font;                use Gdk.Font;
with Gdk.Pixmap;              use Gdk.Pixmap;
with Gdk.Types;               use Gdk.Types;
with Gdk.Types.Keysyms;       use Gdk.Types.Keysyms;
with Gtk.Bin;                 use Gtk.Bin;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Handlers;            use Gtk.Handlers;
with Gtk.Label;               use Gtk.Label;
with Gtk.Main;                use Gtk.Main;
with Gtk.Menu;                use Gtk.Menu;
with Gtk.Menu_Item;           use Gtk.Menu_Item;
with Gtk.Pixmap;              use Gtk.Pixmap;
with Gtk.Stock;               use Gtk.Stock;
with Gtk.Toolbar;             use Gtk.Toolbar;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Window;              use Gtk.Window;
with Gtkada.Handlers;         use Gtkada.Handlers;
with Gtkada.MDI;              use Gtkada.MDI;
with Factory_Data;            use Factory_Data;

with Display_Items;           use Display_Items;
with Items;                   use Items;
with GVD.Canvas;              use GVD.Canvas;
with GVD.Dialogs;             use GVD.Dialogs;
with GVD.Menu;                use GVD.Menu;
with GVD.Types;               use GVD.Types;
with GVD.Toolbar;             use GVD.Toolbar;
with GVD.Process;             use GVD.Process;
with Process_Proxies;         use Process_Proxies;
with Debugger;                use Debugger;
with Language;                use Language;
with Language_Handlers;       use Language_Handlers;
with Basic_Types;             use Basic_Types;
with GUI_Utils;               use GUI_Utils;
with Prj_API;                 use Prj_API;
with Prj;                     use Prj;
with GNAT.Directory_Operations; use GNAT.Directory_Operations;
with GNAT.OS_Lib;

with Glide_Page;              use Glide_Page;
with Glide_Main_Window;       use Glide_Main_Window;
with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Console;    use Glide_Kernel.Console;
with Glide_Kernel.Modules;    use Glide_Kernel.Modules;
with Glide_Kernel.Project;    use Glide_Kernel.Project;
with Glide_Intl;              use Glide_Intl;
with Pixmaps_IDE;             use Pixmaps_IDE;
with Traces;                  use Traces;

with Ada.Exceptions;          use Ada.Exceptions;

with Generic_List;
with Debugger_Pixmaps;        use Debugger_Pixmaps;
with Commands;                use Commands;
with Commands.Debugger;       use Commands.Debugger;

package body GVD_Module is

   Me : Debug_Handle := Create ("Debugger");

   Max_Tooltip_Width : constant := 400;
   Max_Tooltip_Height : constant := 300;
   --  Maximum size to use for the tooltip windows

   type File_Line_Record is record
      File : String_Access;
      Line : Integer;
   end record;

   procedure Free (X : in out File_Line_Record);
   --  Free memory associated to X.

   package File_Line_List is new Generic_List (File_Line_Record);

   type GVD_Module_User_Data is new Module_ID_Record with record
      Kernel           : Kernel_Handle;

      Unexplored_Lines : File_Line_List.List := File_Line_List.Null_List;
      --  The list of lines which are currently revealed in the editor
      --  but the status of which has not yet been queried from the debugger.

      List_Modified    : Boolean := False;
      --  Set to True when the list has been modified by a callback.

      Initialize_Menu  : Gtk_Menu;
   end record;

   procedure GVD_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Generate the contextual menu entries for contextual menus in other
   --  modules than the debugger.

   procedure Tooltip_Handler
     (Sel_Context : access Selection_Context'Class;
      Pixmap      : out Gdk.Gdk_Pixmap;
      Width       : out Gint;
      Height      : out Gint);
   --  Create a pixmap suitable for a tooltip, if debugger has been initialized

   procedure Set_Sensitive
     (Kernel    : Kernel_Handle;
      Sensitive : Boolean);
   --  Change the sensitive state of the debugger menu items and toolbar
   --  buttons

   generic
      with procedure Debug_Command
        (Object : Data_Type_Access;
         Action : Glib.Guint;
         Widget : Limited_Widget);
   procedure Generic_Debug_Command
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Generic procedure used for most debugger callbacks.

   procedure On_View_Changed
     (K : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Called every time the project view changes, to recompute the dynamic
   --  menus.

   ---------------------------
   -- Generic_Debug_Command --
   ---------------------------

   procedure Generic_Debug_Command
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Top  : constant Glide_Window := Glide_Window (Get_Main_Window (Kernel));
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      if Page.Debugger = null then
         return;
      end if;

      Debug_Command (Top.all'Access, 0, Null_Widget);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Generic_Debug_Command;

   --------------------
   -- Menu Callbacks --
   --------------------

   procedure On_Debug_Init
     (Kernel : access GObject_Record'Class; Data : File_Project_Record);
   --  Debug->Initialize

   procedure On_Connect_To_Board
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Debug->Connect to Board

   procedure On_Debug_Executable is new
     Generic_Debug_Command (GVD.Menu.On_Open_Program);
   --  Debug->Debug->Load File

   procedure On_Add_Symbols is new
     Generic_Debug_Command (GVD.Menu.On_Add_Symbols);
   --  Debug->Debug->Add Symbols

   procedure On_Attach is new
     Generic_Debug_Command (GVD.Menu.On_Attach_To_Process);
   --  Debug->Debug->Attach

   procedure On_Detach is new
     Generic_Debug_Command (GVD.Menu.On_Detach_Process);
   --  Debug->Debug->Detach

   procedure On_Load_Core is new
     Generic_Debug_Command (GVD.Menu.On_Open_Core_Dump);
   --  Debug->Debug->Debug Core File

   procedure On_Kill is new
     Generic_Debug_Command (GVD.Menu.On_Kill);
   --  Debug->Debug->Kill

   --  ??? procedure On_Session_Open is new
   --    Generic_Debug_Command (GVD.Menu.On_Open_Session);
   --  Debug->Session->Open

   --  ??? procedure On_Session_Save is new
   --    Generic_Debug_Command (GVD.Menu.On_Save_Session_As);
   --  Debug->Session->Save As

   procedure On_Command_History is new
     Generic_Debug_Command (GVD.Menu.On_Command_History);
   --  Debug->Session->Command History

   procedure On_Threads is new
     Generic_Debug_Command (GVD.Menu.On_Threads);
   --  Debug->Data->Threads

   procedure On_Tasks is new
     Generic_Debug_Command (GVD.Menu.On_Tasks);
   --  Debug->Data->Tasks

   procedure On_Assembly is new
     Generic_Debug_Command (GVD.Menu.On_Tasks);
   --  ??? Debug->Data->Assembly

   procedure On_Edit_Breakpoints is new
     Generic_Debug_Command (GVD.Menu.On_Edit_Breakpoints);
   --  Debug->Data->Edit Breakpoints

   procedure On_Examine_Memory is new
     Generic_Debug_Command (GVD.Menu.On_Examine_Memory);
   --  Debug->Data->Examine Memory

   procedure On_Display_Locals is new
     Generic_Debug_Command (GVD.Menu.On_Display_Local_Variables);
   --  Debug->Data->Display Local Variables

   procedure On_Display_Args is new
     Generic_Debug_Command (GVD.Menu.On_Display_Arguments);
   --  Debug->Data->Display Arguments

   procedure On_Display_Regs is new
     Generic_Debug_Command (GVD.Menu.On_Display_Registers);
   --  Debug->Data->Display Registers

   procedure On_Display_Expression is new
     Generic_Debug_Command (GVD.Menu.On_Display_Expression);
   --  Debug->Data->Display Any Expression

   procedure On_Data_Refresh is new
     Generic_Debug_Command (GVD.Menu.On_Refresh);
   --  Debug->Data->Refresh

   procedure On_Data_Show is new
     Generic_Debug_Command (GVD.Menu.On_Show);
   --  Debug->Data->Show

   procedure On_Start is new
     Generic_Debug_Command (GVD.Menu.On_Run);
   --  Debug->On_Start menu

   procedure On_Step is new
     Generic_Debug_Command (GVD.Menu.On_Step);
   --  Debug->Step menu

   procedure On_Step_Instruction is new
     Generic_Debug_Command (GVD.Menu.On_Step_Instruction);
   --  Debug->Step Instruction menu

   procedure On_Next is new
     Generic_Debug_Command (GVD.Menu.On_Next);
   --  Debug->Next menu

   procedure On_Next_Instruction is new
     Generic_Debug_Command (GVD.Menu.On_Next_Instruction);
   --  Debug->Next Instruction menu

   procedure On_Finish is new
     Generic_Debug_Command (GVD.Menu.On_Finish);
   --  Debug->Finish menu

   procedure On_Continue is new
     Generic_Debug_Command (GVD.Menu.On_Continue);
   --  Debug->Continue menu

   procedure On_Interrupt is new
     Generic_Debug_Command (GVD.Menu.On_Interrupt);
   --  Debug->Interrupt

   procedure On_Debugger_Terminate
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Terminate

   -------------------------------
   -- Contextual Menu Callbacks --
   -------------------------------

   procedure Set_Breakpoint
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Set a breakpoint on a specific line.

   procedure Set_Subprogram_Breakpoint
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Set a breakpoint at the beginning of a specified subprogram.

   procedure Till_Breakpoint
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Set a temporary breakpoint on a line, and continue execution.

   procedure Print_Variable
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callback for the "print" contextual menu items.

   procedure Graph_Display_Variable
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callback for the "display" contextual menu items.

   procedure View_Into_Memory
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Callback for the "view memory at address of" contextual menu item.

   procedure Show_Current_Line_Menu
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access);
   --  Display the current file and current line in the editor.

   -----------------------
   -- Toolbar Callbacks --
   -----------------------

   procedure On_Start_Continue (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "start/continue" button

   --------------------
   -- Misc Callbacks --
   --------------------

   procedure On_Destroy_Window (Object : access Gtk_Widget_Record'Class);
   --  Callback for the "destroy" signal to clean up the debugger.

   procedure Lines_Revealed_Cb
     (Widget  : access Gtk_Widget_Record'Class;
      Args    : GValues);
   --  Callback for the "lines_revealed" signal.

   function Idle_Reveal_Lines return Boolean;
   --  Idle/Timeout function to query the line information from the debugger.

   -------------------------
   -- On_Connect_To_Board --
   -------------------------

   procedure On_Connect_To_Board
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top  : constant Glide_Window := Glide_Window (Get_Main_Window (Kernel));
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      if Page.Debugger = null then
         return;
      end if;

      --  ??? Should open a single dialog to set both target and protocol

      declare
         Target : constant String := Simple_Entry_Dialog
           (Top, -"Selection", -"Enter target name:",
            Win_Pos_Mouse, "Debug_Target");

      begin
         if Target = ASCII.NUL & "" then
            return;
         end if;

         declare
            Protocol : constant String := Simple_Entry_Dialog
              (Top, -"Selection", -"Enter protocol:",
               Win_Pos_Mouse, "Debug_Protocol");

         begin
            if Protocol = ASCII.NUL & "" then
               return;
            end if;

            Page.Descriptor.Remote_Target := new String' (Target);
            Page.Descriptor.Protocol := new String' (Protocol);
            Connect_To_Target
              (Page.Debugger,
               Page.Descriptor.Remote_Target.all,
               Page.Descriptor.Protocol.all,
               GVD.Types.Visible);
         end;
      end;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Connect_To_Board;

   --------------------
   -- GVD_Contextual --
   --------------------

   procedure GVD_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      pragma Unreferenced (Object);

      Entity   : Entity_Selection_Context_Access;
      Mitem    : Gtk_Menu_Item;
      Submenu  : Gtk_Menu;
      Line     : Integer;
      Debugger : Debugger_Access;
      Lang     : Language_Access;

   begin
      if Context.all in Entity_Selection_Context'Class then
         Entity   := Entity_Selection_Context_Access (Context);
         Debugger := Get_Current_Process
           (Get_Main_Window (Get_Kernel (Context))).Debugger;

         if Debugger /= null then
            Lang := Get_Language_From_File
              (Get_Language_Handler (Get_Kernel (Context)),
               File_Information (Entity));

            Gtk_New (Mitem, Label => -"Debug");
            Gtk_New (Submenu);
            Set_Submenu (Mitem, Gtk_Widget (Submenu));
            Append (Menu, Mitem);

            if Has_Entity_Name_Information (Entity) then
               declare
                  Ent : constant String := Entity_Name_Information (Entity);
               begin
                  Gtk_New (Mitem, -"Print " & Ent);
                  Append (Submenu, Mitem);
                  Context_Callback.Connect
                    (Mitem, "activate",
                     Context_Callback.To_Marshaller (Print_Variable'Access),
                     Selection_Context_Access (Context));
                  Gtk_New (Mitem, -"Display " & Ent);
                  Append (Submenu, Mitem);
                  Context_Callback.Connect
                    (Mitem, "activate",
                     Context_Callback.To_Marshaller
                       (Graph_Display_Variable'Access),
                     Selection_Context_Access (Context));

                  if Lang /= null then
                     declare
                        Ent_Deref : constant String :=
                          Dereference_Name (Lang, Ent);
                     begin
                        Gtk_New (Mitem, -"Print " & Ent_Deref);
                        Append (Submenu, Mitem);
                        Context_Callback.Connect
                          (Mitem, "activate",
                           Context_Callback.To_Marshaller
                             (Print_Variable'Access),
                           Selection_Context_Access (Context));
                        Gtk_New (Mitem, -"Display " & Ent_Deref);
                        Append (Submenu, Mitem);
                        Context_Callback.Connect
                          (Mitem, "activate",
                           Context_Callback.To_Marshaller
                             (Graph_Display_Variable'Access),
                           Selection_Context_Access (Context));
                     end;
                  end if;

                  Gtk_New (Mitem, -"View Memory at &" & Ent);
                  Append (Submenu, Mitem);
                  Context_Callback.Connect
                    (Mitem, "activate",
                     Context_Callback.To_Marshaller (View_Into_Memory'Access),
                     Selection_Context_Access (Context));
                  Gtk_New (Mitem);
                  Append (Submenu, Mitem);
                  Gtk_New (Mitem, -"Set breakpoint on " & Ent);
                  Append (Submenu, Mitem);
                  Context_Callback.Connect
                    (Mitem, "activate",
                     Context_Callback.To_Marshaller
                       (Set_Subprogram_Breakpoint'Access),
                     Selection_Context_Access (Context));
               end;
            end if;

            if Has_Line_Information (Entity) then
               Line := Line_Information (Entity);
               Gtk_New (Mitem, -"Set breakpoint on line" & Line'Img);
               Append (Submenu, Mitem);
               Context_Callback.Connect
                 (Mitem, "activate",
                  Context_Callback.To_Marshaller (Set_Breakpoint'Access),
                  Selection_Context_Access (Context));
               Gtk_New (Mitem, -"Continue until line" & Line'Img);
               Append (Submenu, Mitem);
               Context_Callback.Connect
                 (Mitem, "activate",
                  Context_Callback.To_Marshaller (Till_Breakpoint'Access),
                  Selection_Context_Access (Context));
               Gtk_New (Mitem);
               Append (Submenu, Mitem);
            end if;

            Gtk_New (Mitem, -"Show Current Location");
            Append (Submenu, Mitem);
            Context_Callback.Connect
              (Mitem, "activate",
               Context_Callback.To_Marshaller (Show_Current_Line_Menu'Access),
               Selection_Context_Access (Context));
         end if;
      end if;
   end GVD_Contextual;

   ---------------------
   -- Tooltip_Handler --
   ---------------------

   procedure Tooltip_Handler
     (Sel_Context : access Selection_Context'Class;
      Pixmap      : out Gdk.Gdk_Pixmap;
      Width       : out Gint;
      Height      : out Gint)
   is
      Selection      : Entity_Selection_Context_Access;
      Debugger       : Debugger_Process_Tab;
      Lang           : Language_Access;
      Kernel         : Kernel_Handle;

      Value          : Basic_Types.String_Access;
      Context        : Items.Drawing_Context;
      Chars_Per_Line : Gint;
      Index          : Natural;
      Line           : Gint;
      Max            : Natural;
      W              : Gint;

   begin
      Pixmap := null;
      Width  := 0;
      Height := 0;

      if Sel_Context.all not in Entity_Selection_Context'Class then
         return;
      end if;

      Kernel    := Get_Kernel (Sel_Context);
      Selection := Entity_Selection_Context_Access (Sel_Context);
      Debugger  := Get_Current_Process (Get_Main_Window (Kernel));

      if Debugger.Debugger = null
        or else not Has_Entity_Name_Information (Selection)
        or else Command_In_Process (Get_Process (Debugger.Debugger))
      then
         return;
      end if;

      Push_State (Kernel, Busy);
      Lang := Get_Language_From_File
        (Get_Language_Handler (Kernel), File_Information (Selection));

      declare
         Variable_Name : constant String :=
           Entity_Name_Information (Selection);
      begin
         if Variable_Name /= ""
           and then Can_Tooltip_On_Entity (Lang, Variable_Name)
         then
            Value := new String' (Value_Of (Debugger.Debugger, Variable_Name));

            if Value.all = "" then
               Free (Value);
               Pop_State (Kernel);
               return;
            end if;

         else
            Pop_State (Kernel);
            return;
         end if;

         Context := Create_Tooltip_Drawing_Context
           (Debugger.Data_Canvas, Null_Pixmap);
         Chars_Per_Line :=
           Max_Tooltip_Width / Char_Width (Context.Font, Character' ('m'));

         Height := Get_Ascent (Context.Font) + Get_Descent (Context.Font);

         if Value'Length > Chars_Per_Line then
            Width := Gint'Min
              (Max_Tooltip_Width,
               Chars_Per_Line * Char_Width (Context.Font, Character' ('m'))
               + 4);
            Height := Gint'Min
              (Max_Tooltip_Height,
               (1 + Value'Length / Chars_Per_Line) * Height + 2);
         else
            Width := Gint'Min
              (Max_Tooltip_Width, String_Width (Context.Font, Value.all) + 4);
         end if;
      end;

      if Width /= 0 and then Height /= 0 then
         Gdk.Pixmap.Gdk_New
           (Pixmap, Get_Window (Gtk_Window (Debugger.Window)), Width, Height);
         Context := Create_Tooltip_Drawing_Context
           (Debugger.Data_Canvas, Pixmap);

         Draw_Rectangle
           (Pixmap,
            Get_Box_Context (GVD_Canvas (Debugger.Data_Canvas)).Thaw_Bg_GC,
            Filled => True,
            X      => 0,
            Y      => 0,
            Width  => Width - 1,
            Height => Height - 1);

         Index := Value'First;
         Line  := 0;
         W     := 0;

         while Index <= Value'Last loop
            Max := Index + Natural (Chars_Per_Line) - 1;

            if Max > Value'Last then
               Max := Value'Last;
            end if;

            Draw_Text
              (Pixmap, Context.Font, Context.GC,
               2, Line *
               (Get_Ascent (Context.Font) + Get_Descent (Context.Font))
               + Get_Ascent (Context.Font),
               Value (Index .. Max));
            W := Gint'Max
              (W, String_Width (Context.Font, Value (Index .. Max)));
            Index := Max + 1;
            Line := Line + 1;
         end loop;

         Width := W + 4;

         Draw_Rectangle
           (Pixmap,
            Context.GC,
            Filled => False,
            X      => 0,
            Y      => 0,
            Width  => Width - 1,
            Height => Height - 1);
      end if;

      Free (Value);
      Pop_State (Kernel);

   exception
      when Language.Unexpected_Type | Constraint_Error =>
         Pop_State (Kernel);
      when E : others =>
         Pop_State (Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Tooltip_Handler;

   -------------------
   -- Set_Sensitive --
   -------------------

   procedure Set_Sensitive
     (Kernel    : Kernel_Handle;
      Sensitive : Boolean)
   is
      Top          : constant Glide_Window :=
        Glide_Window (Get_Main_Window (Kernel));
      Debug        : constant String := '/' & (-"Debug") & '/';
      Debug_Sub    : constant String := Debug & (-"Debug") & '/';
      Data_Sub     : constant String := Debug & (-"Data") & '/';
      Session_Sub  : constant String := Debug & (-"Session") & '/';

   begin
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Initialize")), not Sensitive);

      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug_Sub & (-"Connect to Board...")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug_Sub & (-"Load File...")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug_Sub & (-"Add Symbols...")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug_Sub & (-"Attach...")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug_Sub & (-"Detach")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug_Sub & (-"Debug Core File...")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug_Sub & (-"Kill")), Sensitive);

      --  ??? Set_Sensitive (Find_Menu_Item
      --    (Kernel, Session_Sub & (-"Open...")), Sensitive);
      --  ??? Set_Sensitive (Find_Menu_Item
      --    (Kernel, Session_Sub & (-"Save As...")), Sensitive);

      Set_Sensitive (Find_Menu_Item
        (Kernel, Session_Sub & (-"Command History")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Data_Sub & (-"Call Stack")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Data_Sub & (-"Threads")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Data_Sub & (-"Tasks")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Data_Sub & (-"Assembly")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Data_Sub & (-"Edit Breakpoints")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Data_Sub & (-"Examine Memory")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Data_Sub & (-"Display Local Variables")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Data_Sub & (-"Display Arguments")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Data_Sub & (-"Display Registers")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Data_Sub & (-"Display Any Expression...")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Data_Sub & (-"Refresh")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Data_Sub & (-"Show")), Sensitive);
      Set_Sensitive (Find_Menu_Item (Kernel, Debug & (-"Start")), Sensitive);
      Set_Sensitive (Find_Menu_Item (Kernel, Debug & (-"Step")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Step Instruction")), Sensitive);
      Set_Sensitive (Find_Menu_Item (Kernel, Debug & (-"Next")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Next Instruction")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Finish")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Continue")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Interrupt")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Debug & (-"Terminate")), Sensitive);

      Set_Sensitive (Top.Cont_Button, Sensitive);
      Set_Sensitive (Top.Step_Button, Sensitive);
      Set_Sensitive (Top.Next_Button, Sensitive);
      Set_Sensitive (Top.Finish_Button, Sensitive);
   end Set_Sensitive;

   -------------------
   -- On_Debug_Init --
   -------------------

   procedure On_Debug_Init
     (Kernel : access GObject_Record'Class; Data : File_Project_Record)
   is
      K : Kernel_Handle := Kernel_Handle (Kernel);

      Top  : constant Glide_Window := Glide_Window (Get_Main_Window (K));
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      Push_State (K, Busy);

      --  Initial the debugger if necessary
      if Page.Debugger = null then
         Configure (Page, Gdb_Type, "", (1 .. 0 => null), "");
         Set_Sensitive (K, True);
         Page.Destroy_Id := Widget_Callback.Object_Connect
           (Top, "destroy",
            Widget_Callback.To_Marshaller (On_Destroy_Window'Access),
            Page);
      end if;

      --  Load a file if necessary
      if Data.File /= "" then
         declare
            Full : constant String := Executables_Directory (Data.Project);
         begin
            Set_Executable (Page.Debugger, Full & Data.File, Mode => Hidden);
         exception
            when Executable_Not_Found =>
               Insert (K, "File not found: " & Full & Data.File);
         end;
      end if;

      Pop_State (K);

   exception
      when E : others =>
         Pop_State (K);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Debug_Init;

   ---------------------------
   -- On_Debugger_Terminate --
   ---------------------------

   procedure On_Debugger_Terminate
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (Widget);

      Top   : constant Glide_Window := Glide_Window (Get_Main_Window (Kernel));
      Page  : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      MDI   : constant MDI_Window := Page.Process_Mdi;

      use Debugger;

   begin
      if Page.Debugger /= null then
         Gtk.Handlers.Disconnect (Top, Page.Destroy_Id);
         Push_State (Kernel, Busy);
         Close (Page.Debugger);
         Page.Debugger := null;
         Close (MDI, Page.Command_Scrolledwindow);
         Close (MDI, Page.Data_Paned);
         Set_Sensitive (Kernel, False);
         Pop_State (Kernel);
      end if;

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Debugger_Terminate;

   --------------------
   -- Set_Breakpoint --
   --------------------

   procedure Set_Breakpoint
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Entity    : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Debugger : constant Debugger_Access :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context))).Debugger;
      Num      : Breakpoint_Identifier;

   begin
      Num := Break_Source
        (Debugger,
         File_Information (Entity),
         Line_Information (Entity),
         Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Set_Breakpoint;

   -------------------------------
   -- Set_Subprogram_Breakpoint --
   -------------------------------

   procedure Set_Subprogram_Breakpoint
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Entity    : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Debugger : constant Debugger_Access :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context))).Debugger;
      Num      : Breakpoint_Identifier;

   begin
      Num := Break_Subprogram
        (Debugger,
         Entity_Name_Information (Entity),
         Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Set_Subprogram_Breakpoint;

   ---------------------
   -- Till_Breakpoint --
   ---------------------

   procedure Till_Breakpoint
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget);

      Entity    : constant Entity_Selection_Context_Access :=
        Entity_Selection_Context_Access (Context);
      Debugger : constant Debugger_Access :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context))).Debugger;
      Num      : Breakpoint_Identifier;

   begin
      Num := Break_Source
        (Debugger,
         File_Information (Entity),
         Line_Information (Entity),
         Temporary => True);
      Continue (Debugger, Mode => GVD.Types.Visible);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Till_Breakpoint;

   --------------------
   -- Print_Variable --
   --------------------

   procedure Print_Variable
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      Debugger : constant Debugger_Access :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context))).Debugger;
      Name     : constant String :=
        Get_Text (Gtk_Label (Get_Child (Gtk_Bin (Widget))));
      Print    : constant String := -"Print ";

   begin
      Print_Value (Debugger, Name (Name'First + Print'Length .. Name'Last));

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Print_Variable;

   ----------------------------
   -- Graph_Display_Variable --
   ----------------------------

   procedure Graph_Display_Variable
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      Process : constant Debugger_Process_Tab :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context)));
      Name    : constant String :=
        Get_Text (Gtk_Label (Get_Child (Gtk_Bin (Widget))));
      Display : constant String := -"Display ";

   begin
      Process_User_Command
        (Process,
         "graph display " & Name (Name'First + Display'Length .. Name'Last),
         Output_Command => True);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Graph_Display_Variable;

   ----------------------
   -- View_Into_Memory --
   ----------------------

   procedure View_Into_Memory
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget, Context);

   begin
      --  ???
      null;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end View_Into_Memory;

   ----------------------------
   -- Show_Current_Line_Menu --
   ----------------------------

   procedure Show_Current_Line_Menu
     (Widget  : access GObject_Record'Class;
      Context : Selection_Context_Access)
   is
      pragma Unreferenced (Widget, Context);
   begin
      --  ???
      null;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end Show_Current_Line_Menu;

   -----------------------
   -- On_Start_Continue --
   -----------------------

   procedure On_Start_Continue (Object : access Gtk_Widget_Record'Class) is
      Tab : constant Debugger_Process_Tab := Get_Current_Process (Object);
   begin
      if Tab /= null and then Tab.Debugger /= null then
         if Is_Started (Tab.Debugger) then
            Continue (Tab.Debugger, Mode => GVD.Types.Visible);
         else
            Start (Tab.Debugger, Mode => GVD.Types.Visible);
         end if;
      end if;

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Start_Continue;

   -----------------------
   -- On_Destroy_Window --
   -----------------------

   procedure On_Destroy_Window (Object : access Gtk_Widget_Record'Class) is
      Page : constant Glide_Page.Glide_Page := Glide_Page.Glide_Page (Object);
   begin
      Close (Page.Debugger);
   end On_Destroy_Window;

   ------------------------
   --  Idle_Reveal_Lines --
   ------------------------

   function Idle_Reveal_Lines return Boolean is
      Kind         : Line_Kind;
      C            : Set_Breakpoint_Command_Access;
      File_Line    : File_Line_Record;
      Debugger     : constant Debugger_Access :=
        Get_Current_Process
        (Get_Main_Window (GVD_Module_User_Data
                          (GVD_Module_ID.all).Kernel)).Debugger;
      --  ??? Should attach the right debugger with GVD_Module_Id.

   begin
      if File_Line_List.Is_Empty
        (GVD_Module_User_Data (GVD_Module_ID.all).Unexplored_Lines)
      then
         return False;

      elsif Command_In_Process (Get_Process (Debugger)) then
         return True;
      end if;

      File_Line := File_Line_List.Head
        (GVD_Module_User_Data (GVD_Module_ID.all).Unexplored_Lines);

      Kind := Line_Contains_Code
        (Debugger, File_Line.File.all, File_Line.Line);

      if GVD_Module_User_Data (GVD_Module_ID.all).List_Modified then
         GVD_Module_User_Data (GVD_Module_ID.all).List_Modified := False;
         return True;
      end if;

      --  ??? we could make smart use of the case Kind = No_More_Code
      --  Clear the list and return False.
      declare
         L : Integer := File_Line.Line;
         A : Line_Information_Array (L .. L);
      begin
         if Kind = Have_Code then
            Create (C,
                    GVD_Module_User_Data (GVD_Module_ID.all).Kernel,
                    Debugger,
                    Set,
                    File_Line.File.all,
                    File_Line.Line);
            A (L).Image := Line_Has_Code_Pixbuf;
            A (L).Associated_Command := Command_Access (C);
         end if;

         Add_Line_Information
           (GVD_Module_User_Data (GVD_Module_ID.all).Kernel,
            File_Line.File.all,
            GVD_Module_Name & "/Line Information",
            new Line_Information_Array' (A));
      end;

      File_Line_List.Next
        (GVD_Module_User_Data (GVD_Module_ID.all).Unexplored_Lines);

      if File_Line_List.Is_Empty
        (GVD_Module_User_Data (GVD_Module_ID.all).Unexplored_Lines)
      then
         return False;
      end if;

      return True;
   end Idle_Reveal_Lines;

   -----------------------
   -- Lines_Revealed_Cb --
   -----------------------

   procedure Lines_Revealed_Cb
     (Widget  : access Gtk_Widget_Record'Class;
      Args    : GValues)
   is
      pragma Unreferenced (Widget);
      Context      : Selection_Context_Access :=
        To_Selection_Context_Access (Get_Address (Nth (Args, 1)));
      Area_Context : File_Area_Context_Access;
      Timeout_Id   : Timeout_Handler_Id;
      Process      : constant Debugger_Process_Tab :=
        Get_Current_Process (Get_Main_Window (Get_Kernel (Context)));

   begin
      if Process = null or else Process.Debugger = null then
         return;
      end if;

      if Context.all in File_Area_Context'Class then
         Area_Context := File_Area_Context_Access (Context);

         declare
            File : constant String := Directory_Information (Area_Context) &
              File_Information (Area_Context);
            Line1, Line2 : Integer;

         begin
            Get_Area (Area_Context, Line1, Line2);

            if File_Line_List.Is_Empty
              (GVD_Module_User_Data (GVD_Module_ID.all).Unexplored_Lines)
            then
               Timeout_Id := Timeout_Add
                 (1, Idle_Reveal_Lines'Access);
            else
               GVD_Module_User_Data (GVD_Module_ID.all).List_Modified := True;
               File_Line_List.Free
                 (GVD_Module_User_Data (GVD_Module_ID.all).Unexplored_Lines);
               GVD_Module_User_Data (GVD_Module_ID.all).Unexplored_Lines
                 := File_Line_List.Null_List;
            end if;

            for J in Line1 .. Line2 loop
               File_Line_List.Append
                 (GVD_Module_User_Data (GVD_Module_ID.all).Unexplored_Lines,
                  (new String' (File), J));
               --  ??? We might want to use a LIFO structure here
               --  instead of FIFO, so that the lines currently shown
               --  are displayed first.
            end loop;
         end;
      end if;
   end Lines_Revealed_Cb;

   ---------------------
   -- On_View_Changed --
   ---------------------

   procedure On_View_Changed
     (K : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      pragma Unreferenced (K);
      Mitem : Gtk_Menu_Item;
      Menu : Gtk_Menu renames
        GVD_Module_User_Data (GVD_Module_ID.all).Initialize_Menu;
      Iter : Imported_Project_Iterator := Start (Get_Project (Kernel));

   begin
      --  Remove all existing menus
      Remove_All_Children (Menu);

      --  Specific entry to start the debugger without any main program
      Gtk_New (Mitem, -"<none>");
      Append (Menu, Mitem);
      File_Project_Cb.Object_Connect
        (Mitem, "activate",
         File_Project_Cb.To_Marshaller (On_Debug_Init'Access),
         Slot_Object => Kernel,
         User_Data => File_Project_Record'
           (Length  => 0,
            Project => Get_Project_View (Kernel),
            File    => ""));

      --  Add all the main units from all the imported projects.
      while Current (Iter) /= No_Project loop
         declare
            Mains : GNAT.OS_Lib.Argument_List := Get_Attribute_Value
              (Current (Iter), Attribute_Name => Main_Attribute);
         begin
            for M in Mains'Range loop
               declare
                  Exec : constant String := Base_Name (Mains (M).all,
                    GNAT.Directory_Operations.File_Extension (Mains (M).all));
               begin
                  Gtk_New (Mitem, Exec);
                  Append (Menu, Mitem);
                  File_Project_Cb.Object_Connect
                    (Mitem, "activate",
                     File_Project_Cb.To_Marshaller (On_Debug_Init'Access),
                     Slot_Object => Kernel,
                     User_Data => File_Project_Record'
                       (Length  => Exec'Length,
                        Project => Current (Iter),
                        File    => Exec));
               end;
            end loop;
            Free (Mains);
         end;

         Next (Iter);
      end loop;

      Show_All (Menu);

   exception
      when E : others =>
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_View_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class)
   is
      Toolbar      : constant Gtk_Toolbar := Get_Toolbar (Kernel);
      Window       : constant Gtk_Window  := Get_Main_Window (Kernel);
      Top          : constant Glide_Window := Glide_Window (Window);
      Debug        : constant String := '/' & (-"Debug") & '/';
      Debug_Sub    : constant String := Debug & (-"Debug") & '/';
      Data_Sub     : constant String := Debug & (-"Data") & '/';
      Session_Sub  : constant String := Debug & (-"Session") & '/';
      Mitem        : Gtk_Menu_Item;
      Menu         : Gtk_Menu;
      --  ??? Should get the right process
   begin
      GVD_Module_ID := new GVD_Module_User_Data;
      GVD_Module_User_Data (GVD_Module_ID.all).Kernel :=
        Kernel_Handle (Kernel);

      Register_Module
        (Module                  => GVD_Module_ID,
         Kernel                  => Kernel,
         Module_Name             => GVD_Module_Name,
         Priority                => Default_Priority + 20,
         Contextual_Menu_Handler => GVD_Contextual'Access,
         Tooltip_Handler         => Tooltip_Handler'Access);

      --  Dynamic Initialize menu
      Mitem := Register_Menu (Kernel, Debug, -"Initialize", "", null,
                              Ref_Item => -"Data");
      Gtk_New (Menu);
      Set_Submenu (Mitem, Menu);
      GVD_Module_User_Data (GVD_Module_ID.all).Initialize_Menu := Menu;
      Kernel_Callback.Connect
        (Kernel, "project_view_changed",
         Kernel_Callback.To_Marshaller (On_View_Changed'Access),
         User_Data => Kernel_Handle (Kernel));

      --  Add debugger menus

      Register_Menu (Kernel, Debug_Sub, Ref_Item => -"Data");
      Register_Menu (Kernel, Debug_Sub, -"Connect to Board...", "",
                     On_Connect_To_Board'Access, Sensitive => False);
      Register_Menu (Kernel, Debug_Sub, -"Load File...", "",
                     On_Debug_Executable'Access, Sensitive => False);
      Register_Menu (Kernel, Debug_Sub, -"Add Symbols...", "",
                     On_Add_Symbols'Access, Sensitive => False);
      Register_Menu (Kernel, Debug_Sub, -"Attach...", "",
                     On_Attach'Access, Sensitive => False);
      Register_Menu (Kernel, Debug_Sub, -"Detach", "",
                     On_Detach'Access, Sensitive => False);
      Register_Menu (Kernel, Debug_Sub, -"Debug Core File...", "",
                     On_Load_Core'Access, Sensitive => False);
      Register_Menu (Kernel, Debug_Sub, -"Kill", "",
                     On_Kill'Access, Sensitive => False);

      Register_Menu (Kernel, Session_Sub, -"Open...", Stock_Open,
                     null, Sensitive => False);
      Register_Menu (Kernel, Session_Sub, -"Save As...", Stock_Save_As,
                     null, Sensitive => False);
      Register_Menu (Kernel, Session_Sub, -"Command History", Stock_Index,
                     On_Command_History'Access, Sensitive => False);

      Set_Sensitive (Find_Menu_Item
        (Kernel, Data_Sub & (-"Call Stack")), False);

      Register_Menu (Kernel, Data_Sub, -"Threads", "",
                     On_Threads'Access, Sensitive => False);
      Register_Menu (Kernel, Data_Sub, -"Tasks", "",
                     On_Tasks'Access, Sensitive => False);
      Register_Menu (Kernel, Data_Sub, -"Assembly", "",
                     On_Assembly'Access, Sensitive => False);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Data_Sub, Mitem);
      Register_Menu (Kernel, Data_Sub, -"Edit Breakpoints", "",
                     On_Edit_Breakpoints'Access, Sensitive => False);
      Register_Menu (Kernel, Data_Sub, -"Examine Memory", "",
                     On_Examine_Memory'Access, Sensitive => False);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Data_Sub, Mitem);
      Register_Menu (Kernel, Data_Sub, -"Display Local Variables", "",
                     On_Display_Locals'Access,
                     GDK_L, Mod1_Mask, Sensitive => False);
      Register_Menu (Kernel, Data_Sub, -"Display Arguments", "",
                     On_Display_Args'Access,
                     GDK_U, Mod1_Mask, Sensitive => False);
      Register_Menu (Kernel, Data_Sub, -"Display Registers", "",
                     On_Display_Regs'Access, Sensitive => False);
      Register_Menu (Kernel, Data_Sub, -"Display Any Expression...", "",
                     On_Display_Expression'Access, Sensitive => False);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Data_Sub, Mitem);
      Register_Menu (Kernel, Data_Sub, -"Refresh", Stock_Refresh,
                     On_Data_Refresh'Access,
                     GDK_L, Control_Mask, Sensitive => False);
      Register_Menu (Kernel, Data_Sub, -"Show", "",
                     On_Data_Show'Access, Sensitive => False);

      Gtk_New (Mitem);
      Register_Menu (Kernel, Debug, Mitem);

      Register_Menu (Kernel, Debug, -"Start", "",
                     On_Start'Access, GDK_F2, Sensitive => False);
      Register_Menu (Kernel, Debug, -"Step", "",
                     On_Step'Access, GDK_F5, Sensitive => False);
      Register_Menu (Kernel, Debug, -"Step Instruction", "",
                     On_Step_Instruction'Access,
                     GDK_F5, Shift_Mask, Sensitive => False);
      Register_Menu (Kernel, Debug, -"Next", "",
                     On_Next'Access, GDK_F6, Sensitive => False);
      Register_Menu (Kernel, Debug, -"Next Instruction", "",
                     On_Next_Instruction'Access,
                     GDK_F6, Shift_Mask, Sensitive => False);
      Register_Menu (Kernel, Debug, -"Finish", "",
                     On_Finish'Access, GDK_F7, Sensitive => False);
      Register_Menu (Kernel, Debug, -"Continue", "",
                     On_Continue'Access, GDK_F8, Sensitive => False);
      Register_Menu (Kernel, Debug, -"Interrupt", Stock_Stop,
                     On_Interrupt'Access, GDK_Escape, Sensitive => False);
      Gtk_New (Mitem);
      Register_Menu (Kernel, Debug, Mitem);
      Register_Menu (Kernel, Debug, -"Terminate", "",
                     On_Debugger_Terminate'Access, Sensitive => False);

      --  Add debugger buttons in the toolbar

      Append_Space (Toolbar);
      Top.Cont_Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Tooltip_Text => -"Start/Continue the debugged program",
         Icon => Gtk_Widget (Create_Pixmap (run_xpm, Window)));
      Widget_Callback.Object_Connect
        (Top.Cont_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Start_Continue'Access),
         Window);
      Set_Sensitive (Top.Cont_Button, False);

      Top.Step_Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Tooltip_Text =>
           -"Step program until it reaches a different source line",
         Icon => Gtk_Widget (Create_Pixmap (step_xpm, Window)));
      Widget_Callback.Object_Connect
        (Top.Step_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Step'Access), Window);
      Set_Sensitive (Top.Step_Button, False);

      Top.Next_Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Tooltip_Text => -"Step program, proceeding through subroutine calls",
         Icon => Gtk_Widget (Create_Pixmap (next_xpm, Window)));
      Widget_Callback.Object_Connect
        (Top.Next_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Next'Access), Window);
      Set_Sensitive (Top.Next_Button, False);

      Top.Finish_Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Tooltip_Text => -"Execute until selected stack frame returns",
         Icon => Gtk_Widget (Create_Pixmap (finish_xpm, Window)));
      Widget_Callback.Object_Connect
        (Top.Finish_Button, "clicked",
         Widget_Callback.To_Marshaller (On_Finish'Access), Window);
      Set_Sensitive (Top.Finish_Button, False);

      Widget_Callback.Object_Connect
        (Kernel,
         Source_Lines_Revealed_Signal,
         Lines_Revealed_Cb'Access,
         Top);

      Init_Graphics;
   end Register_Module;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out File_Line_Record) is
   begin
      Free (X.File);
   end Free;

end GVD_Module;
