-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2006                         --
--                              AdaCore                              --
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

with Ada.Exceptions;            use Ada.Exceptions;
with Ada.Strings.Unbounded;     use Ada.Strings; use Ada.Strings.Unbounded;

with GNAT.OS_Lib;               use GNAT.OS_Lib;

with Glib;                      use Glib;
with Gdk.Drawable;              use Gdk.Drawable;
with Gdk.Event;                 use Gdk.Event;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Style;                 use Gtk.Style;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Canvas;             use Gtkada.Canvas;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;
with Pango.Layout;              use Pango.Layout;

with Browsers.Canvas;           use Browsers.Canvas;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GPS.Kernel.Standard_Hooks; use GPS.Kernel.Standard_Hooks;
with String_Hash;
with String_List_Utils;         use String_List_Utils;
with Traces;                    use Traces;
with VFS;                       use VFS;

package body Browsers.Revisions is

   package SL renames String_List_Utils.String_List;

   procedure Free (L : in out SL.List);
   --  Free the string list

   package Syms_Table is new String_Hash (SL.List, Free, SL.Null_List);
   use Syms_Table;

   type Revision_Browser_Record is new Browsers.Canvas.General_Browser_Record
   with record
      Syms : String_Hash_Table.HTable;
      File : Virtual_File;
   end record;

   type Revision_Browser is access all Revision_Browser_Record'Class;

   procedure Free (Browser : in out Revision_Browser);
   --  Free the revision browser

   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class);
   --  The browser is destroyed, release memory used

   package Browser_Table is new String_Hash (Revision_Browser, Free, null);

   package BT renames Browser_Table.String_Hash_Table;

   type Revision_Browser_Module is new Module_ID_Record with record
      Table : BT.HTable;
   end record;

   Revision_Browser_Module_ID : Module_ID;

   type Log_Data is record
      Revision : Unbounded_String;
      Author   : Unbounded_String;
      Date     : Unbounded_String;
      Log      : Unbounded_String;
   end record;

   type Browser_Revision_Vertex is new Arrow_Item_Record with record
      Browser  : Revision_Browser;
      Data     : Log_Data;
      Refs     : Xref_List; -- the data except the symbolic names
      Sym_Refs : Xref_List; -- the symbolic names
   end record;
   type Browser_Revision_Vertex_Access is access all Browser_Revision_Vertex;

   type Show_Log_Data is new Active_Area_Callback with record
      Vertex : Browser_Revision_Vertex_Access;
   end record;
   type Show_Log_Data_Access is access all Show_Log_Data;

   function Call
     (Callback : Show_Log_Data;
      Event    : Gdk.Event.Gdk_Event) return Boolean;
   --  Show the corresponding log data

   procedure Contextual_Factory
     (Item    : access Browser_Revision_Vertex;
      Context : in out Selection_Context;
      Browser : access General_Browser_Record'Class;
      Event   : Gdk_Event;
      Menu    : Gtk_Menu);
   --  See doc for inherited subprogram

   procedure Resize_And_Draw
     (Item             : access Browser_Revision_Vertex;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango_Layout_Record'Class);
   --  See doc for inherited subprogram

   procedure Gtk_New
     (V       : out Browser_Revision_Vertex_Access;
      Browser : access Revision_Browser_Record'Class;
      Data    : Log_Data);
   --  Create a new project vertex

   function Create_Revision_Browser
     (Kernel : access Kernel_Handle_Record'Class) return Revision_Browser;
   --  Create the revision browser

   procedure Add_Log_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands

   procedure Add_Link_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands

   procedure Add_Revision_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands

   function Open_Revision_Browser
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File) return Revision_Browser;
   --  Open the Revision Browser associated with File, create it if needed

   function "+"
     (Str : String) return Unbounded_String renames To_Unbounded_String;

   function Add_Log_If_Not_Present
     (Browser : Revision_Browser;
      Log     : Log_Data) return Boolean;
   --  Add log data into Browser if not already present

   function Add_Link_If_Not_Present
     (Browser      : Revision_Browser;
      Log_1, Log_2 : Log_Data) return Boolean;
   --  Add a link between Log_1 and Long_2 if not already present

   function Find_Revision
     (Browser : access Revision_Browser_Record'Class;
      Log     : Log_Data) return Browser_Revision_Vertex_Access;
   --  Returns the revision if already in the browser, null otherwise

   ----------
   -- Free --
   ----------

   procedure Free (L : in out SL.List) is
      pragma Unreferenced (L);
   begin
      null;
   end Free;

   procedure Free (Browser : in out Revision_Browser) is
      pragma Unreferenced (Browser);
   begin
      null;
   end Free;

   ------------------------------
   -- Add_Link_Command_Handler --
   ------------------------------

   procedure Add_Link_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      Kernel       : constant Kernel_Handle := Get_Kernel (Data);
      File         : constant Virtual_File := Create (Nth_Arg (Data, 1));
      Browser      : constant Revision_Browser :=
                       Open_Revision_Browser (Kernel, File);
      Rev_1        : constant String := Nth_Arg (Data, 2);
      Rev_2        : constant String := Nth_Arg (Data, 3);
      Log_1, Log_2 : Log_Data;
   begin
      Log_1.Revision := +Rev_1;
      Log_2.Revision := +Rev_2;

      if Add_Link_If_Not_Present (Browser, Log_1, Log_2) then
         Layout (Browser, Force => False);
         Refresh_Canvas (Get_Canvas (Browser));
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Add_Link_Command_Handler;

   -----------------------------
   -- Add_Log_Command_Handler --
   -----------------------------

   procedure Add_Log_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Data);
      File    : constant Virtual_File := Create (Nth_Arg (Data, 1));
      Log     : constant Log_Data :=
                  (Revision => +Nth_Arg (Data, 2),
                   Author   => +Nth_Arg (Data, 3),
                   Date     => +Nth_Arg (Data, 4),
                   Log      => +Nth_Arg (Data, 5));
      Browser : constant Revision_Browser :=
                    Open_Revision_Browser (Kernel, File);
   begin
      if Add_Log_If_Not_Present (Browser, Log) then
         Layout (Browser, Force => False);
         Refresh_Canvas (Get_Canvas (Browser));
      end if;
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Add_Log_Command_Handler;

   ----------------------------------
   -- Add_Revision_Command_Handler --
   ----------------------------------

   procedure Add_Revision_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      Kernel  : constant Kernel_Handle := Get_Kernel (Data);
      File    : constant Virtual_File := Create (Nth_Arg (Data, 1));
      Browser : constant Revision_Browser :=
                  Open_Revision_Browser (Kernel, File);
      Rev     : constant String := Nth_Arg (Data, 2);
      Sym     : constant String := Nth_Arg (Data, 3);
      Key     : constant String := Full_Name (File, True).all & "$" & Rev;
      List    : SL.List;
   begin
      List := String_Hash_Table.Get (Browser.Syms, Key);

      SL.Append (List, Sym);

      String_Hash_Table.Set (Browser.Syms, Key, List);
   exception
      when E : others =>
         Trace (Exception_Handle,
                "Unexpected exception " & Exception_Information (E));
   end Add_Revision_Command_Handler;

   -----------------------------
   -- Add_Link_If_Not_Present --
   -----------------------------

   function Add_Link_If_Not_Present
     (Browser      : Revision_Browser;
      Log_1, Log_2 : Log_Data) return Boolean
   is
      Rev_1_Vertex : constant Browser_Revision_Vertex_Access :=
                       Find_Revision (Browser, Log_1);
      Rev_2_Vertex : constant Browser_Revision_Vertex_Access :=
                       Find_Revision (Browser, Log_2);
      Link         : Browser_Link;
   begin
      if Rev_1_Vertex /= null and then Rev_2_Vertex /= null
        and then
          not Has_Link (Get_Canvas (Browser), Rev_1_Vertex, Rev_2_Vertex)
      then
         Link := new Browser_Link_Record;
         Add_Link (Get_Canvas (Browser), Link, Rev_1_Vertex, Rev_2_Vertex);
         return True;
      end if;
      return False;
   end Add_Link_If_Not_Present;

   ----------------------------
   -- Add_Log_If_Not_Present --
   ----------------------------

   function Add_Log_If_Not_Present
     (Browser : Revision_Browser;
      Log     : Log_Data) return Boolean
   is
      Rev_Vertex : Browser_Revision_Vertex_Access :=
                     Find_Revision (Browser, Log);
   begin
      if Rev_Vertex = null then
         Gtk_New (Rev_Vertex, Browser, Log);
         Put (Get_Canvas (Browser), Rev_Vertex);
         Refresh (Rev_Vertex);
         return True;
      end if;
      return False;
   end Add_Log_If_Not_Present;

   ----------
   -- Call --
   ----------

   function Call
     (Callback : Show_Log_Data;
      Event    : Gdk.Event.Gdk_Event) return Boolean
   is
      pragma Unreferenced (Event);
      Kernel  : constant Kernel_Handle := Get_Kernel (Callback.Vertex.Browser);
      F_Name  : constant String :=
                  Base_Name (Callback.Vertex.Browser.File) & "$log$" &
                    To_String (Callback.Vertex.Data.Revision);
      Log     : constant String :=
                  To_String (Callback.Vertex.Data.Log);
      Success : Boolean;
      Result  : Integer;
      pragma Unreferenced (Result);
      Tmp     : Virtual_File;
      File    : File_Descriptor;
   begin
      File := Create_New_File (F_Name, Binary);
      Result := Write (File, Log'Address, Log'Length);
      Close (File);

      Tmp := Create (F_Name);
      Open_File_Editor (Kernel, Tmp);

      Delete_File (F_Name, Success);
      return True;
   end Call;

   ------------------------
   -- Contextual_Factory --
   ------------------------

   procedure Contextual_Factory
     (Item    : access Browser_Revision_Vertex;
      Context : in out Selection_Context;
      Browser : access General_Browser_Record'Class;
      Event   : Gdk.Event.Gdk_Event;
      Menu    : Gtk.Menu.Gtk_Menu)
   is
      pragma Unreferenced (Browser, Event, Menu);
   begin
      Set_File_Information
        (Context,
         File     => Item.Browser.File,
         Revision => To_String (Item.Data.Revision));
   end Contextual_Factory;

   -----------------------------
   -- Create_Revision_Browser --
   -----------------------------

   function Create_Revision_Browser
     (Kernel : access Kernel_Handle_Record'Class) return Revision_Browser
   is
      Browser : Revision_Browser;
   begin
      Browser := new Revision_Browser_Record;
      Initialize (Browser, Kernel, Create_Toolbar => False);
      Set_Layout_Orientation (Get_Canvas (Browser), Vertical_Layout => True);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => Browser,
         Object          => Browser,
         ID              => Revision_Browser_Module_ID,
         Context_Func    => Default_Browser_Context_Factory'Access);
      return Browser;
   end Create_Revision_Browser;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class) is
      Browser : constant Revision_Browser := Revision_Browser (Widget);
   begin
      String_Hash_Table.Reset (Browser.Syms);
      BT.Remove
        (Revision_Browser_Module (Revision_Browser_Module_ID.all).Table,
         Base_Name (Browser.File));
   end On_Destroy;

   -------------------
   -- Find_Revision --
   -------------------

   function Find_Revision
     (Browser : access Revision_Browser_Record'Class;
      Log     : Log_Data) return Browser_Revision_Vertex_Access
   is
      Iter : Item_Iterator := Start (Get_Canvas (Browser));
      Item : Canvas_Item;
   begin
      loop
         Item := Get (Iter);
         exit when Item = null
           or else Browser_Revision_Vertex_Access (Item).Data.Revision =
                     Log.Revision;
         Next (Iter);
      end loop;
      return Browser_Revision_Vertex_Access (Item);
   end Find_Revision;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (V       : out Browser_Revision_Vertex_Access;
      Browser : access Revision_Browser_Record'Class;
      Data    : Log_Data)
   is
      use type SL.List_Node;
      Revision : constant String := To_String (Data.Revision);
      Show_Log : Show_Log_Data_Access;
      File     : constant Virtual_File := Browser.File;
      Rev      : constant String := To_String (Data.Revision);
      Key      : constant String := Full_Name (File, True).all & "$" & Rev;
      List     : SL.List;
      Node     : SL.List_Node;
   begin
      V := new Browser_Revision_Vertex;
      V.Browser := Revision_Browser (Browser);
      V.Data    := Data;

      Initialize (V, Browser, Revision, null, null);

      Set_Children_Shown (V, True);
      Set_Parents_Shown (V, True);

      Show_Log := new Show_Log_Data;
      Show_Log.Vertex := V;

      Add_Line (V.Refs, Base_Name (File));
      Add_Line
        (V.Refs,
         "Revision: @" & Rev & '@',
         Callback => (1 => Active_Area_Cb (Show_Log)));
      Add_Line (V.Refs, To_String (V.Data.Date));
      Add_Line (V.Refs, To_String (V.Data.Author));

      List := String_Hash_Table.Get (Browser.Syms, Key);
      Node := SL.First (List);

      while Node /= SL.Null_Node loop
         Add_Line (V.Sym_Refs, SL.Data (Node));
         Node := SL.Next (Node);
      end loop;
   end Gtk_New;

   ---------------------------
   -- Open_Revision_Browser --
   ---------------------------

   function Open_Revision_Browser
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File) return Revision_Browser
   is
      B_Name  : constant String := Base_Name (File);
      Title   : constant String := -"Revision Browser - " & B_Name;
      Browser : Revision_Browser;
      Child   : MDI_Child;
   begin
      Browser := BT.Get
        (Revision_Browser_Module (Revision_Browser_Module_ID.all).Table,
         B_Name);

      if Browser = null then
         Browser := Create_Revision_Browser (Kernel);
         Browser.File := File;

         BT.Set
           (Revision_Browser_Module (Revision_Browser_Module_ID.all).Table,
            B_Name,
            Browser);

         Gtk_New (GPS_MDI_Child (Child), Browser,
                  Default_Width  => Get_Pref (Default_Widget_Width),
                  Default_Height => Get_Pref (Default_Widget_Height),
                  Group          => Group_Graphs,
                  Module         => Revision_Browser_Module_ID);
         Set_Title (Child, -Title);
         Put (Get_MDI (Kernel), GPS_MDI_Child (Child));
         Set_Focus_Child (Child);

         Widget_Callback.Connect (Browser, "destroy", On_Destroy'Access);
      end if;

      Add_Navigation_Location (Kernel, -Title);

      return Browser;
   end Open_Revision_Browser;

   ---------------------
   -- Resize_And_Draw --
   ---------------------

   procedure Resize_And_Draw
     (Item             : access Browser_Revision_Vertex;
      Width, Height    : Glib.Gint;
      Width_Offset     : Glib.Gint;
      Height_Offset    : Glib.Gint;
      Xoffset, Yoffset : in out Glib.Gint;
      Layout           : access Pango_Layout_Record'Class)
   is
      Ref_W1, Ref_W2, Ref_H, Y    : Gint;
      S_Ref_W1, S_Ref_W2, S_Ref_H : Gint;
   begin
      Get_Pixel_Size
        (Get_Browser (Item), Item.Refs, Ref_W1, Ref_W2, Ref_H,  Layout);
      Get_Pixel_Size
        (Get_Browser (Item),
         Item.Sym_Refs, S_Ref_W1, S_Ref_W2, S_Ref_H,  Layout);

      Resize_And_Draw
        (Arrow_Item_Record (Item.all)'Access,
         Gint'Max
           (Gint'Max (Ref_W1 + Ref_W2 + 2 * Margin,
            S_Ref_W1 + S_Ref_W2 + 2 * Margin),
            Width),
         Height + Ref_H + S_Ref_H + 5,
         Width_Offset, Height_Offset, Xoffset, Yoffset, Layout);

      Y := Yoffset + 1;
      Display_Lines (Item, Item.Refs, Margin + Xoffset, Y, Ref_W1, Layout);

      if S_Ref_H /= 0 then
         Y := Y + 2;
         Draw_Line
           (Drawable => Pixmap (Item),
            GC       => Get_Black_GC (Get_Style (Get_Browser (Item))),
            X1       => 0,
            Y1       => Y,
            X2       => Get_Coord (Item).Width,
            Y2       => Y);
         Y := Y + 2;

         Display_Lines
           (Item, Item.Sym_Refs, Margin + Xoffset, Y, Ref_W1, Layout);
      end if;
   end Resize_And_Draw;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Revision_Class : constant Class_Type := New_Class (Kernel, "Revision");
   begin
      Revision_Browser_Module_ID := new Revision_Browser_Module;

      Register_Module
        (Module      => Revision_Browser_Module_ID,
         Kernel      => Kernel,
         Module_Name => Revision_Browser_Module_Name,
         Priority    => Default_Priority);

      Register_Command
        (Kernel, "add_log",
         Handler       => Add_Log_Command_Handler'Access,
         Minimum_Args  => 5,
         Maximum_Args  => 5,
         Class         => Revision_Class,
         Static_Method => True);

      Register_Command
        (Kernel, "add_link",
         Handler       => Add_Link_Command_Handler'Access,
         Minimum_Args  => 3,
         Maximum_Args  => 3,
         Class         => Revision_Class,
         Static_Method => True);

      Register_Command
        (Kernel, "add_revision",
         Handler       => Add_Revision_Command_Handler'Access,
         Minimum_Args  => 3,
         Maximum_Args  => 3,
         Class         => Revision_Class,
         Static_Method => True);
   end Register_Module;

end Browsers.Revisions;
