-----------------------------------------------------------------------
--                   GVD - The GNU Visual Debugger                   --
--                                                                   --
--                         Copyright (C) 2001                        --
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

with Glib.Object;             use Glib.Object;
with Gdk.Types;               use Gdk.Types;
with Gdk.Types.Keysyms;       use Gdk.Types.Keysyms;
with Gtk.Bin;                 use Gtk.Bin;
with Gtk.Enums;               use Gtk.Enums;
with Gtk.Label;               use Gtk.Label;
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

with GVD.Menu;                use GVD.Menu;
with GVD.Types;               use GVD.Types;
with GVD.Toolbar;             use GVD.Toolbar;
with GVD.Process;             use GVD.Process;
with Debugger;                use Debugger;
with Language;                use Language;

with Glide_Page;              use Glide_Page;
with Glide_Main_Window;       use Glide_Main_Window;
with Glide_Kernel;            use Glide_Kernel;
with Glide_Kernel.Modules;    use Glide_Kernel.Modules;
with Glide_Intl;              use Glide_Intl;
with Pixmaps_IDE;             use Pixmaps_IDE;
with Traces;                  use Traces;

with Ada.Exceptions;          use Ada.Exceptions;

package body GVD_Module is

   GVD_Module_ID : Module_ID;

   Me : Debug_Handle := Create ("Debugger");

   procedure Initialize_Module
     (Kernel : access Glide_Kernel.Kernel_Handle_Record'Class);
   --  Initialization function for the module

   procedure GVD_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class);
   --  Generate the contextual menu entries for contextual menus in other
   --  modules than the debugger.

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
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle);
   --  Debug->Initialize

   procedure On_Connect_To_Board is new
     Generic_Debug_Command (GVD.Menu.On_Add_Symbols);
   --  ??? Debug->Debug->Connect to Board

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

   procedure On_Session_Open is new
     Generic_Debug_Command (GVD.Menu.On_Open_Session);
   --  Debug->Session->Open

   procedure On_Session_Save is new
     Generic_Debug_Command (GVD.Menu.On_Save_Session_As);
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
   -- GVD_Contextual --
   --------------------

   procedure GVD_Contextual
     (Object  : access GObject_Record'Class;
      Context : access Selection_Context'Class;
      Menu    : access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      Entity   : Entity_Selection_Context_Access;
      Mitem    : Gtk_Menu_Item;
      Line     : Integer;
      Debugger : Debugger_Access;
      Lang     : Language_Access;

   begin
      if Context.all in Entity_Selection_Context'Class then
         Entity   := Entity_Selection_Context_Access (Context);
         Debugger := Get_Current_Process
           (Get_Main_Window (Get_Kernel (Context))).Debugger;

         if Debugger /= null then
            Lang := Get_Language (Debugger);
            --  ??? Should query the project module instead of the debugger
            --  Lang := Get_Language (File_Information (Entity));

            if Has_Entity_Name_Information (Entity) then
               declare
                  Ent : constant String := Entity_Name_Information (Entity);
               begin
                  Gtk_New (Mitem);
                  Append (Menu, Mitem);
                  Gtk_New (Mitem, -"Print " & Ent);
                  Append (Menu, Mitem);
                  Context_Callback.Connect
                    (Mitem, "activate",
                     Context_Callback.To_Marshaller (Print_Variable'Access),
                     Selection_Context_Access (Context));
                  Gtk_New (Mitem, -"Display " & Ent);
                  Append (Menu, Mitem);
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
                        Append (Menu, Mitem);
                        Context_Callback.Connect
                          (Mitem, "activate",
                           Context_Callback.To_Marshaller
                             (Print_Variable'Access),
                           Selection_Context_Access (Context));
                        Gtk_New (Mitem, -"Display " & Ent_Deref);
                        Append (Menu, Mitem);
                        Context_Callback.Connect
                          (Mitem, "activate",
                           Context_Callback.To_Marshaller
                             (Graph_Display_Variable'Access),
                           Selection_Context_Access (Context));
                     end;
                  end if;

                  Gtk_New (Mitem, -"View Memory at &" & Ent);
                  Append (Menu, Mitem);
                  Context_Callback.Connect
                    (Mitem, "activate",
                     Context_Callback.To_Marshaller (View_Into_Memory'Access),
                     Selection_Context_Access (Context));
                  Gtk_New (Mitem);
                  Append (Menu, Mitem);
                  Gtk_New (Mitem, -"Set breakpoint on " & Ent);
                  Append (Menu, Mitem);
                  Context_Callback.Connect
                    (Mitem, "activate",
                     Context_Callback.To_Marshaller
                       (Set_Subprogram_Breakpoint'Access),
                     Selection_Context_Access (Context));
               end;
            end if;

            if Has_Line_Information (Entity) then
               if not Has_Entity_Name_Information (Entity) then
                  Gtk_New (Mitem);
                  Append (Menu, Mitem);
               end if;

               Line := Line_Information (Entity);
               Gtk_New (Mitem, -"Set breakpoint on line" & Line'Img);
               Append (Menu, Mitem);
               Context_Callback.Connect
                 (Mitem, "activate",
                  Context_Callback.To_Marshaller (Set_Breakpoint'Access),
                  Selection_Context_Access (Context));
               Gtk_New (Mitem, -"Continue until line" & Line'Img);
               Append (Menu, Mitem);
               Context_Callback.Connect
                 (Mitem, "activate",
                  Context_Callback.To_Marshaller (Till_Breakpoint'Access),
                  Selection_Context_Access (Context));
            end if;

            Gtk_New (Mitem);
            Append (Menu, Mitem);
            Gtk_New (Mitem, -"Show Current Location");
            Append (Menu, Mitem);
            Context_Callback.Connect
              (Mitem, "activate",
               Context_Callback.To_Marshaller (Show_Current_Line_Menu'Access),
               Selection_Context_Access (Context));
         end if;
      end if;
   end GVD_Contextual;

----------------------
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
      Set_Sensitive (Find_Menu_Item
        (Kernel, Session_Sub & (-"Open...")), Sensitive);
      Set_Sensitive (Find_Menu_Item
        (Kernel, Session_Sub & (-"Save As...")), Sensitive);
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
   --------------------

   procedure On_Debug_Init
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Top  : constant Glide_Window := Glide_Window (Get_Main_Window (Kernel));
      Page : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      use Debugger;

   begin
      if Page.Debugger = null then
         Push_State (Kernel, Busy);
         Configure (Page, Gdb_Type, "", (1 .. 0 => null), "");
         Set_Sensitive (Kernel, True);
         Pop_State (Kernel);
      end if;

   exception
      when E : others =>
         Pop_State (Kernel);
         Trace (Me, "Unexpected exception: " & Exception_Information (E));
   end On_Debug_Init;

   ---------------------------
   -- On_Debugger_Terminate --
   ---------------------------

   procedure On_Debugger_Terminate
     (Widget : access GObject_Record'Class; Kernel : Kernel_Handle)
   is
      Top   : constant Glide_Window := Glide_Window (Get_Main_Window (Kernel));
      Page  : constant Glide_Page.Glide_Page :=
        Glide_Page.Glide_Page (Get_Current_Process (Top));
      MDI   : constant MDI_Window := Page.Process_Mdi;

      use Debugger;

   begin
      if Page.Debugger /= null then
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
   -- Initialize_Module --
   -----------------------

   procedure Initialize_Module
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

   begin
      --  Add debugger menus

      Register_Menu (Kernel, Debug, -"Initialize", "", On_Debug_Init'Access,
                     Ref_Item => -"Data");
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
                     On_Session_Open'Access, Sensitive => False);
      Register_Menu (Kernel, Session_Sub, -"Save As...", Stock_Save_As,
                     On_Session_Save'Access, Sensitive => False);
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
        (Top.Step_Button, "clicked", On_Step'Access, Window);
      Set_Sensitive (Top.Step_Button, False);
      Top.Next_Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Tooltip_Text => -"Step program, proceeding through subroutine calls",
         Icon => Gtk_Widget (Create_Pixmap (next_xpm, Window)));
      Widget_Callback.Object_Connect
        (Top.Next_Button, "clicked", On_Next'Access, Window);
      Set_Sensitive (Top.Next_Button, False);
      Top.Finish_Button := Append_Element
        (Toolbar => Toolbar,
         The_Type => Toolbar_Child_Button,
         Tooltip_Text => -"Execute until selected stack frame returns",
         Icon => Gtk_Widget (Create_Pixmap (finish_xpm, Window)));
      Widget_Callback.Object_Connect
        (Top.Finish_Button, "clicked", On_Finish'Access, Window);
      Set_Sensitive (Top.Finish_Button, False);
   end Initialize_Module;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module is
   begin
      GVD_Module_ID := Register_Module
        (Module_Name             => GVD_Module_Name,
         Priority                => Default_Priority,
         Initializer             => Initialize_Module'Access,
         Contextual_Menu_Handler => GVD_Contextual'Access);
   end Register_Module;

end GVD_Module;
