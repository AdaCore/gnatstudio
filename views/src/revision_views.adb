-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2006-2007                      --
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

with Ada.Strings.Unbounded;     use Ada.Strings; use Ada.Strings.Unbounded;
with Ada.Unchecked_Conversion;
with System;

with GNAT.Scripts;              use GNAT.Scripts;
with GNAT.Strings;              use GNAT.Strings;

with Glib;                      use Glib;
with Glib.Object;               use Glib.Object;
with Gdk.Color;                 use Gdk.Color;
with Gdk.Event;                 use Gdk.Event;
with Gtk.Cell_Renderer;         use Gtk.Cell_Renderer;
use Gtk.Cell_Renderer.Cell_Renderer_List;
with Gtk.Enums;                 use Gtk.Enums;
with Gtk.Menu;                  use Gtk.Menu;
with Gtk.Object;                use Gtk.Object;
with Gtk.Scrolled_Window;       use Gtk.Scrolled_Window;
with Gtk.Tree_Model;            use Gtk.Tree_Model;
with Gtk.Tree_Store;            use Gtk.Tree_Store;
with Gtk.Tree_View;             use Gtk.Tree_View;
with Gtk.Tree_View_Column;      use Gtk.Tree_View_Column;
with Gtk.Widget;                use Gtk.Widget;
with Gtkada.Handlers;           use Gtkada.Handlers;
with Gtkada.MDI;                use Gtkada.MDI;

with Basic_Types;               use Basic_Types;
with Generic_Views;
with GPS.Intl;                  use GPS.Intl;
with GPS.Kernel;                use GPS.Kernel;
with GPS.Kernel.Contexts;       use GPS.Kernel.Contexts;
with GPS.Kernel.MDI;            use GPS.Kernel.MDI;
with GPS.Kernel.Modules;        use GPS.Kernel.Modules;
with GPS.Kernel.Preferences;    use GPS.Kernel.Preferences;
with GPS.Kernel.Scripts;        use GPS.Kernel.Scripts;
with GUI_Utils;                 use GUI_Utils;
with String_Hash;
with String_List_Utils;         use String_List_Utils;
with String_Utils;              use String_Utils;
with Traces;                    use Traces;
with VFS;                       use VFS;

package body Revision_Views is

   Root_Color_Name : constant String := "blue";

   Revision_Column : constant := 0;
   Author_Column   : constant := 1;
   Info_Column     : constant := 2;
   Date_Column     : constant := 3;
   Log_Column      : constant := 4;
   Link_Column     : constant := 5;
   Color_Column    : constant := 6;
   Rev_Info_Column : constant := 7;

   package SL renames String_List_Utils.String_List;

   procedure Free (L : in out SL.List);
   --  Free the string list

   package Syms_Table is new String_Hash (SL.List, Free, SL.Null_List);
   use Syms_Table;

   type Mode_Kind is (Link, Branch, Filter_Out);

   type Revision_View_Record is new Generic_Views.View_Record with record
      Kernel       : Kernel_Handle;
      Tree         : Gtk_Tree_View;
      Prev1, Prev2 : Unbounded_String;
      Parent       : Gtk_Tree_Iter := Null_Iter;
      Mode         : Mode_Kind := Link;
      Syms         : String_Hash_Table.HTable;
      File         : Virtual_File;
      Root_Color   : Gdk_Color;
      Child        : MDI_Child;
   end record;

   type Revision_View is access all Revision_View_Record'Class;

   procedure Initialize
     (View   : access Revision_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class);
   --  Create a new view

   procedure Free (View : in out Revision_View);
   --  Free the revision browser

   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class);
   --  The browser is destroyed, release memory used

   package View_Table is new String_Hash (Revision_View, Free, null);

   package BT renames View_Table.String_Hash_Table;

   type Revision_View_Module is new Module_ID_Record with record
      Table : BT.HTable;
   end record;

   Revision_View_Module_ID : Module_ID;

   type Log_Data is record
      Revision : Unbounded_String;
      Author   : Unbounded_String;
      Date     : Unbounded_String;
      Log      : Unbounded_String;
   end record;

   type Line_Data is record
      Log  : Log_Data;
      Link : Boolean;
   end record;

   procedure View_Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu);

   function Create_Revision_View
     (Kernel : access Kernel_Handle_Record'Class) return Revision_View;
   --  Create the revision browser

   procedure Clear_View_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands

   procedure Add_Log_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands

   procedure Add_Link_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands

   procedure Add_Revision_Command_Handler
     (Data : in out Callback_Data'Class; Command : String);
   --  Handle shell commands

   function Open_Revision_View
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File) return Revision_View;
   --  Open the Revision Browser associated with File, create it if needed

   function "+"
     (Str : String) return Unbounded_String renames To_Unbounded_String;

   procedure Add_Log_If_Not_Present
     (View : Revision_View; Log : Log_Data; Expand : Boolean);
   --  Add log data into Browser if not already present

   procedure Add_Link_If_Not_Present
     (View : Revision_View; Log_1, Log_2 : Log_Data);
   --  Add a link between Log_1 and Long_2 if not already present. Note that
   --  the algorithm here expect that a link is given as soon as possible for
   --  each new node inserted into the view.
   --  ??? It would be possible to remove this limitation by first building a
   --  linked list of nodes and then mapping it into the revision view. Not
   --  worth the work for now as it is ok for CVS and Subversion.

   function Find_Revision
     (View : access Revision_View_Record'Class;
      Log  : Log_Data) return Gtk_Tree_Iter;
   --  Returns the revision if already in the browser, null otherwise

   procedure Fill_Info
     (View : access Revision_View_Record'Class;
      Iter : Gtk_Tree_Iter;
      Line : Line_Data);
   --  Fill Iter information using Log

   function Get_Data_From_Iter
     (View : access Revision_View_Record'Class;
      Iter : Gtk_Tree_Iter) return Line_Data;
   --  Get the Data at Iter

   ----------
   -- Free --
   ----------

   procedure Free (L : in out SL.List) is
      pragma Unreferenced (L);
   begin
      null;
   end Free;

   procedure Free (View : in out Revision_View) is
      pragma Unreferenced (View);
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
      Rev_1        : constant String := Nth_Arg (Data, 2);
      Rev_2        : constant String := Nth_Arg (Data, 3);
      View         : constant Revision_View :=
                       Open_Revision_View (Kernel, File);
      Log_1, Log_2 : Log_Data;
   begin
      Log_1.Revision := +Rev_1;
      Log_2.Revision := +Rev_2;

      Add_Link_If_Not_Present (View, Log_1, Log_2);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_Link_Command_Handler;

   -----------------------------
   -- Add_Log_Command_Handler --
   -----------------------------

   procedure Add_Log_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      File   : constant Virtual_File := Create (Nth_Arg (Data, 1));
      Log    : constant Log_Data :=
                 (Revision => +Nth_Arg (Data, 2),
                  Author   => +Nth_Arg (Data, 3),
                  Date     => +Nth_Arg (Data, 4),
                  Log      => +Unprotect (Nth_Arg (Data, 5)));
      Expand : constant Boolean := Nth_Arg (Data, 6, False);
      View   : constant Revision_View := Open_Revision_View (Kernel, File);
   begin
      Add_Log_If_Not_Present (View, Log, Expand);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_Log_Command_Handler;

   ----------------------------------
   -- Add_Revision_Command_Handler --
   ----------------------------------

   procedure Add_Revision_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      Kernel : constant Kernel_Handle := Get_Kernel (Data);
      File   : constant Virtual_File := Create (Nth_Arg (Data, 1));
      View   : constant Revision_View :=
                 Open_Revision_View (Kernel, File);
      Rev    : constant String := Nth_Arg (Data, 2);
      Sym    : constant String := Nth_Arg (Data, 3);
      Key    : constant String := Full_Name (File, True).all & "$" & Rev;
      List   : SL.List;
   begin
      List := String_Hash_Table.Get (View.Syms, Key);

      SL.Append (List, Sym);

      String_Hash_Table.Set (View.Syms, Key, List);
   exception
      when E : others => Trace (Exception_Handle, E);
   end Add_Revision_Command_Handler;

   -----------------------------
   -- Add_Link_If_Not_Present --
   -----------------------------

   procedure Add_Link_If_Not_Present
     (View : Revision_View; Log_1, Log_2 : Log_Data)
   is
      Model : constant Gtk_Tree_Store :=
                Gtk_Tree_Store (Get_Model (View.Tree));

      procedure Move (From : in out Gtk_Tree_Iter; To : Gtk_Tree_Iter);
      --  Move line pointed by From into To

      ----------
      -- Move --
      ----------

      procedure Move (From : in out Gtk_Tree_Iter; To : Gtk_Tree_Iter) is
         Line : constant Line_Data := Get_Data_From_Iter (View, From);
      begin
         Fill_Info (View, To, Line);
         Remove (Model, From);
      end Move;

      Rev_1 : Gtk_Tree_Iter := Find_Revision (View, Log_1);
      Rev_2 : constant Gtk_Tree_Iter := Find_Revision (View, Log_2);
      Iter  : Gtk_Tree_Iter;
   begin
      case View.Mode is
         when Filter_Out =>
            --  No more output
            null;

         when Branch =>
            --  A branch, back track and reparent all nodes until an orphan is
            --  found. This node is the end of the current parsed branch.

            declare
               Path     : Gtk_Tree_Path;
               Tmp      : Gtk_Tree_Iter;
               Has_Link : Boolean;
               Parent   : Gtk_Tree_Iter := Rev_2;
            begin
               Path := Get_Path (Model, Rev_1);

               loop
                  Append (Model, Iter, Parent);

                  if Parent = Rev_2 then
                     --  The first node becomes the parent of the next
                     --  reparented nodes.
                     Parent := Iter;
                  end if;

                  Tmp := Get_Iter (Model, Path);
                  Has_Link := Get_Boolean (Model, Tmp, Link_Column);

                  exit when not Prev (Path);

                  Move (Tmp, Iter);

                  exit when not Has_Link;
                  --  We have reached the branch leaf
               end loop;
               Path_Free (Path);
            end;

            View.Mode := Filter_Out;

         when Link =>
            --  Add link information

            if Rev_1 /= Null_Iter and then Rev_2 /= Null_Iter then
               --  Check if we need to reparent Rev_1 under Rev_2

               if To_String (Log_1.Revision) /= To_String (View.Prev2) then
                  Append (Model, Iter, Rev_2);
                  Move (Rev_1, Iter);
                  View.Parent := Rev_2;
               else
                  Set (Model, Rev_2, Link_Column, True);
               end if;
            end if;
      end case;
   end Add_Link_If_Not_Present;

   ----------------------------
   -- Add_Log_If_Not_Present --
   ----------------------------

   procedure Add_Log_If_Not_Present
     (View : Revision_View; Log : Log_Data; Expand : Boolean)
   is
      Model : constant Gtk_Tree_Store :=
                Gtk_Tree_Store (Get_Model (View.Tree));
      Iter  : Gtk_Tree_Iter := Find_Revision (View, Log);
   begin
      if Iter = Null_Iter then
         Append (Model, Iter, View.Parent);
         Fill_Info (View, Iter, (Log, False));
         View.Prev2 := View.Prev1;
         View.Prev1 := Log.Revision;
         View.Mode := Link;

         if Expand then
            declare
               Path : Gtk_Tree_Path;
               Tmp  : Boolean;
               pragma Warnings (Off, Tmp);
            begin
               Path := Get_Path (Model, Iter);
               Tmp := Expand_Row (View.Tree, Path, Open_All => True);
               Path_Free (Path);
            end;
         end if;

      elsif View.Mode = Link then
         --  We are in link mode and we reached an existing node. We found a
         --  branch.
         View.Mode := Branch;
      end if;
   end Add_Log_If_Not_Present;

   -------------------------------
   -- Clear_Log_Command_Handler --
   -------------------------------

   procedure Clear_View_Command_Handler
     (Data : in out Callback_Data'Class; Command : String)
   is
      pragma Unreferenced (Command);
      File : constant Virtual_File := Create (Nth_Arg (Data, 1));
      View : constant Revision_View := BT.Get
        (Revision_View_Module (Revision_View_Module_ID.all).Table,
         Base_Name (File));
   begin
      if View /= null then
         Destroy (View);
      end if;
   exception
      when E : others => Trace (Exception_Handle, E);
   end Clear_View_Command_Handler;

   --------------------------
   -- View_Context_Factory --
   --------------------------

   procedure View_Context_Factory
     (Context      : in out Selection_Context;
      Kernel       : access Kernel_Handle_Record'Class;
      Event_Widget : access Gtk.Widget.Gtk_Widget_Record'Class;
      Object       : access Glib.Object.GObject_Record'Class;
      Event        : Gdk.Event.Gdk_Event;
      Menu         : Gtk.Menu.Gtk_Menu)
   is
      pragma Unreferenced (Event_Widget, Kernel, Menu);

      function Get_Parent_Revision (Iter : Gtk_Tree_Iter) return String;
      --  Return the revision for Iter's parent

      V     : constant Revision_View := Revision_View (Object);
      Model : constant Gtk_Tree_Model := Get_Model (V.Tree);

      -------------------------
      -- Get_Parent_Revision --
      -------------------------

      function Get_Parent_Revision (Iter : Gtk_Tree_Iter) return String is
         J : Gtk_Tree_Iter;
      begin
         Iter_Copy (Iter, J);
         --  Climb the tree until finding a revision node corresponding to the
         --  current iter.

         Look_For_Revision : while J /= Null_Iter loop
            declare
               Rev : constant String :=
                       Get_String (Model, J, Rev_Info_Column);
            begin
               if Rev /= "" then
                  return Rev;
               end if;

               J := Parent (Model, J);
            end;
         end loop Look_For_Revision;

         return "";
      end Get_Parent_Revision;

      Iter : Gtk_Tree_Iter;
      Rev  : Unbounded_String;
      Tag  : Unbounded_String;

   begin
      Iter := Find_Iter_For_Event (V.Tree, Model, Event);

      if Iter = Null_Iter then
         return;
      end if;

      declare
         R : constant String := Get_String (Model, Iter, Rev_Info_Column);
      begin
         if R /= "" then
            if Has_Child (Model, Iter) then
               --  We are on a revision node
               Rev := To_Unbounded_String (R);

            else
               --  We are on a tag/branch node
               Tag := To_Unbounded_String (R);
               Rev := To_Unbounded_String (Get_Parent_Revision (Iter));
            end if;

         else
            Rev := To_Unbounded_String (Get_Parent_Revision (Iter));
         end if;
      end;

      Set_File_Information
        (Context,
         File     => V.File,
         Revision => To_String (Rev),
         Tag      => To_String (Tag));
   end View_Context_Factory;

   -----------------------------
   -- Create_Revision_Browser --
   -----------------------------

   function Create_Revision_View
     (Kernel : access Kernel_Handle_Record'Class) return Revision_View
   is
      View : Revision_View;
   begin
      View := new Revision_View_Record;
      Initialize (View, Kernel);

      Register_Contextual_Menu
        (Kernel          => Kernel,
         Event_On_Widget => View.Tree,
         Object          => View,
         ID              => Revision_View_Module_ID,
         Context_Func    => View_Context_Factory'Access);
      return View;
   end Create_Revision_View;

   ---------------
   -- Fill_Info --
   ---------------

   procedure Fill_Info
     (View : access Revision_View_Record'Class;
      Iter : Gtk_Tree_Iter;
      Line : Line_Data)
   is
      Model : constant Gtk_Tree_Store :=
                Gtk_Tree_Store (Get_Model (View.Tree));
      function To_Proxy is new
        Ada.Unchecked_Conversion (System.Address, C_Proxy);

      Child : Gtk_Tree_Iter;
      Info  : Unbounded_String;
      --  The info column contains the date plus tags/branches
   begin
      Set (Model, Iter, Color_Column, To_Proxy (View.Root_Color'Address));
      Set (Model, Iter, Revision_Column, To_String (Line.Log.Revision));
      Set (Model, Iter, Rev_Info_Column, To_String (Line.Log.Revision));
      Set (Model, Iter, Author_Column, To_String (Line.Log.Author));
      Set (Model, Iter, Date_Column, To_String (Line.Log.Date));
      Set (Model, Iter, Log_Column, To_String (Line.Log.Log));
      Set (Model, Iter, Link_Column, Line.Link);
      Info := Line.Log.Date;

      --  Create log entry

      Append (Model, Child, Iter);
      Set (Model, Child, Color_Column, C_Proxy'(null));
      Set (Model, Child, Info_Column, To_String (Line.Log.Log));

      --  Tags & Branches

      declare
         use type SL.List_Node;
         Rev   : constant String := To_String (Line.Log.Revision);
         Key   : constant String :=
                   Full_Name (View.File, True).all & "$" & Rev;
         List  : SL.List;
         Node  : SL.List_Node;
         First : Boolean := True;
      begin
         List := String_Hash_Table.Get (View.Syms, Key);

         if not SL.Is_Empty (List) then
            Append (Info, " (");
            Node := SL.First (List);

            while Node /= SL.Null_Node loop
               Append (Model, Child, Iter);
               Set (Model, Child, Info_Column, "tag: " & SL.Data (Node));
               Set (Model, Child, Rev_Info_Column, SL.Data (Node));
               if First then
                  Append (Info, SL.Data (Node));
                  First := False;
               else
                  Append (Info, " " & SL.Data (Node));
               end if;
               Node := SL.Next (Node);
            end loop;

            Append (Info, ")");
         end if;
      end;

      Set (Model, Iter, Info_Column, To_String (Info));
   end Fill_Info;

   ------------------------
   -- Get_Data_From_Iter --
   ------------------------

   function Get_Data_From_Iter
     (View : access Revision_View_Record'Class;
      Iter : Gtk_Tree_Iter) return Line_Data
   is
      Model : constant Gtk_Tree_Store :=
                Gtk_Tree_Store (Get_Model (View.Tree));
      Log   : Log_Data;
   begin
      Log.Revision := +Get_String (Model, Iter, Rev_Info_Column);
      Log.Author := +Get_String (Model, Iter, Author_Column);
      Log.Date := +Get_String (Model, Iter, Date_Column);
      Log.Log := +Get_String (Model, Iter, Log_Column);
      return (Log, Get_Boolean (Model, Iter, Link_Column));
   end Get_Data_From_Iter;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (View   : access Revision_View_Record'Class;
      Kernel : access Kernel_Handle_Record'Class)
   is
      procedure Set_Attribute (Col : Gint);
      --  Set column attribute

      -------------------
      -- Set_Attribute --
      -------------------

      procedure Set_Attribute (Col : Gint) is
         List : Cell_Renderer_List.Glist;
      begin
         List := Get_Cell_Renderers (Get_Column (View.Tree, Col));
         Add_Attribute
           (Get_Column (View.Tree, Col),
            Cell_Renderer_List.Get_Data (List),
            "foreground_gdk", Color_Column);
         Free (List);
      end Set_Attribute;

      Names   : GNAT.Strings.String_List :=
                  (1 => new String'(-"Revision"),
                   2 => new String'(-"Author"),
                   3 => new String'(-"Date / Log"));
      Success : Boolean;
   begin
      View.Kernel := Kernel_Handle (Kernel);
      Gtk.Scrolled_Window.Initialize (View);
      Set_Policy (View, Policy_Automatic, Policy_Automatic);

      View.Tree := Create_Tree_View
        (Column_Types       => (Revision_Column => GType_String,
                                Author_Column   => GType_String,
                                Info_Column     => GType_String,
                                Date_Column     => GType_String,
                                Log_Column      => GType_String,
                                Link_Column     => GType_Boolean,
                                Color_Column    => Gdk_Color_Type,
                                Rev_Info_Column => GType_String),
         Column_Names       => Names,
         Show_Column_Titles => True,
         Sortable_Columns   => True,
         Initial_Sort_On    => Date_Column + 1);
      Add (View, View.Tree);

      View.Root_Color := Parse (Root_Color_Name);
      Alloc_Color
        (Get_Default_Colormap, View.Root_Color, False, True, Success);

      Set_Attribute (Revision_Column);
      Set_Attribute (Author_Column);
      Set_Attribute (Info_Column);

      Free (Names);
   end Initialize;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy (Widget : access Gtk_Widget_Record'Class) is
      View : constant Revision_View := Revision_View (Widget);
   begin
      String_Hash_Table.Reset (View.Syms);
      BT.Remove
        (Revision_View_Module (Revision_View_Module_ID.all).Table,
         Base_Name (View.File));
   end On_Destroy;

   -------------------
   -- Find_Revision --
   -------------------

   function Find_Revision
     (View : access Revision_View_Record'Class;
      Log  : Log_Data) return Gtk_Tree_Iter
   is
      Model  : constant Gtk_Tree_Store :=
                 Gtk_Tree_Store (Get_Model (View.Tree));
      Rev    : constant String := To_String (Log.Revision);
      Result : Gtk_Tree_Iter := Null_Iter;

      procedure Iterate (Iter : Gtk_Tree_Iter);
      --  Parse recursively the tree model

      -------------
      -- Iterate --
      -------------

      procedure Iterate (Iter : Gtk_Tree_Iter) is
         Quit : Boolean := False;
         J    : Gtk_Tree_Iter;
      begin
         Iter_Copy (Iter, J);

         while not Quit and then J /= Null_Iter loop
            if Has_Child (Model, J) then
               Iterate (Children (Model, J));
            end if;

            if Get_String (Model, J, Rev_Info_Column) = Rev then
               Quit := True;
               Iter_Copy (J, Result);
               return;
            end if;

            Next (Model, J);
         end loop;
      end Iterate;

   begin
      Iterate (Get_Iter_First (Model));
      return Result;
   end Find_Revision;

   ---------------------------
   -- Open_Revision_Browser --
   ---------------------------

   function Open_Revision_View
     (Kernel : access Kernel_Handle_Record'Class;
      File   : Virtual_File) return Revision_View
   is
      B_Name : constant String := Base_Name (File);
      Title  : constant String := -"Revision View - " & B_Name;
      View   : Revision_View;
      Child  : MDI_Child;

   begin
      View := BT.Get
        (Revision_View_Module (Revision_View_Module_ID.all).Table,
         B_Name);

      if View = null then
         View := Create_Revision_View (Kernel);
         View.File := File;

         BT.Set
           (Revision_View_Module (Revision_View_Module_ID.all).Table,
            B_Name,
            View);

         Gtk_New (GPS_MDI_Child (Child), View,
                  Focus_Widget   => Gtk_Widget (View.Tree),
                  Default_Width  => Get_Pref (Default_Widget_Width),
                  Default_Height => Get_Pref (Default_Widget_Height),
                  Group          => Group_Consoles,
                  Module         => Revision_View_Module_ID);
         View.Child := Child;
         Set_Title (Child, -Title);
         Put (Get_MDI (Kernel), GPS_MDI_Child (Child));
         Set_Focus_Child (Child);

         Widget_Callback.Connect (View, Signal_Destroy, On_Destroy'Access);
      end if;

      if View.Child /= null then
         Gtkada.MDI.Raise_Child (View.Child);
      end if;

      return View;
   end Open_Revision_View;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Revision_Class : constant Class_Type := New_Class (Kernel, "Revision");
   begin
      Revision_View_Module_ID := new Revision_View_Module;

      Register_Module
        (Module      => Revision_View_Module_ID,
         Kernel      => Kernel,
         Module_Name => Revision_View_Module_Name,
         Priority    => Default_Priority);

      Register_Command
        (Kernel, "add_log",
         Handler       => Add_Log_Command_Handler'Access,
         Minimum_Args  => 5,
         Maximum_Args  => 6,
         Class         => Revision_Class,
         Static_Method => True);

      Register_Command
        (Kernel, "clear_view",
         Handler       => Clear_View_Command_Handler'Access,
         Minimum_Args  => 1,
         Maximum_Args  => 1,
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

end Revision_Views;
