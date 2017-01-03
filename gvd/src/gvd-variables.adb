------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2016-2017, AdaCore                     --
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

with Ada.Containers.Vectors;
with Ada.Strings.Unbounded;    use Ada.Strings.Unbounded;
with Commands.Interactive;     use Commands, Commands.Interactive;
with Debugger;                 use Debugger;
with Default_Preferences;      use Default_Preferences;
with Gdk.Event;                use Gdk.Event;
with Gdk.RGBA;                 use Gdk.RGBA;
with Generic_Views;            use Generic_Views;
with Glib;                     use Glib;
with Glib.Object;              use Glib.Object;
with Glib.Values;              use Glib.Values;
with Glib_Values_Utils;        use Glib_Values_Utils;
with GNAT.Regpat;              use GNAT.Regpat;
with GNATCOLL.JSON;
with GNATCOLL.Traces;          use GNATCOLL.Traces;
with GNATCOLL.Utils;           use GNATCOLL.Utils;
with GPS.Intl;                 use GPS.Intl;
with GPS.Debuggers;            use GPS.Debuggers;
with GPS.Kernel.Actions;       use GPS.Kernel.Actions;
with GPS.Kernel.Hooks;         use GPS.Kernel.Hooks;
with GPS.Kernel.MDI;           use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;    use GPS.Kernel.Modules.UI;
with GPS.Kernel;               use GPS.Kernel;
with GPS.Kernel.Contexts;      use GPS.Kernel.Contexts;
with GPS.Kernel.Preferences;   use GPS.Kernel.Preferences;
with GPS.Kernel.Properties;    use GPS.Kernel.Properties;
with GPS.Properties;           use GPS.Properties;
with GPS.Search;               use GPS.Search;
with Gtk.Box;                  use Gtk.Box;
with Gtk.Cell_Renderer_Pixbuf; use Gtk.Cell_Renderer_Pixbuf;
with Gtk.Cell_Renderer_Text;   use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                use Gtk.Enums;
with Gtk.Menu;                 use Gtk.Menu;
with Gtk.Scrolled_Window;      use Gtk.Scrolled_Window;
with Gtk.Toolbar;              use Gtk.Toolbar;
with Gtk.Tree_Model;           use Gtk.Tree_Model;
with Gtk.Tree_Store;           use Gtk.Tree_Store;
with Gtk.Tree_View_Column;     use Gtk.Tree_View_Column;
with Gtk.Widget;               use Gtk.Widget;
with Gtkada.MDI;               use Gtkada.MDI;
with Gtkada.Style;             use Gtkada.Style;
with Gtkada.Tree_View;         use Gtkada.Tree_View;
with GUI_Utils;                use GUI_Utils;
with GVD.Contexts;             use GVD.Contexts;
with GVD.Generic_View;         use GVD.Generic_View;
with GVD.Items;                use GVD.Items;
with GVD_Module;               use GVD_Module;
with GVD.Preferences;          use GVD.Preferences;
with GVD.Process;              use GVD.Process;
with Items;                    use Items;
with Items.Simples;            use Items.Simples;
with Language;                 use Language;
with Language.Icons;           use Language.Icons;
with GPS.Dialogs;              use GPS.Dialogs;
with System;
with System.Storage_Elements;  use System.Storage_Elements;
with XML_Utils;                use XML_Utils;

package body GVD.Variables is
   Me : constant Trace_Handle := Create ("Variables");

   Show_Types : Boolean_Preference;

   type Item_ID is new Natural;
   Unknown_Id : constant Item_ID := Item_ID'First;
   type Item is record
      Id    : Item_ID;   --  unique id
      Info  : Item_Info;

      Nested : Boolean := False;
      --  True if this is a nested item, as opposed to one that appears as a
      --  root item in the tree.
   end record;
   No_Item : constant Item := (Unknown_Id, No_Item_Info, False);

   package Item_Vectors is new Ada.Containers.Vectors (Positive, Item);

   function Deep_Copy (X : Item_Vectors.Vector) return Item_Vectors.Vector;
   --  Make a deep copy of X

   type Variable_Tree_View_Record is new Gtkada.Tree_View.Tree_View_Record with
      record
         Process : Visual_Debugger;
         Pattern : Search_Pattern_Access;
         Items   : Item_Vectors.Vector;
      end record;
   type Variable_Tree_View is access all Variable_Tree_View_Record'Class;
   overriding function Is_Visible
     (Self   : not null access Variable_Tree_View_Record;
      Iter   : Gtk_Tree_Iter) return Boolean;
   overriding procedure Add_Children
     (Self       : not null access Variable_Tree_View_Record;
      Store_Iter : Gtk_Tree_Iter);

   type Tree_Row_Id is record
      Typ : System.Address;
      --  The corresponding access Generic_Type'Class.
      --  Do not attempt to dereference, the type might have been freed since.
   end record;
   function Get_Id
     (Self : not null access Variable_Tree_View_Record'Class;
      Iter : Gtk_Tree_Iter) return Tree_Row_Id;
   function Hash (Element : Tree_Row_Id) return Ada.Containers.Hash_Type;
   package Expansions is new Expansion_Support
     (Variable_Tree_View_Record, Tree_Row_Id, Get_Id, Hash);
   --  An Id that uniquely identifies each row of the tree view

   type GVD_Variable_View_Record is new Process_View_Record with record
      Tree    : Variable_Tree_View;
      Ids     : Item_ID := Unknown_Id;  --  to compute unique ids for items
   end record;

   function Initialize
     (Self   : access GVD_Variable_View_Record'Class) return Gtk_Widget;
   procedure Set_View
     (Process : not null access Base_Visual_Debugger'Class;
      View    : access GVD_Variable_View_Record'Class := null);
   function Get_View
     (Process : not null access Base_Visual_Debugger'Class)
      return access GVD_Variable_View_Record'Class;
   --  See documentation in GVD.Generic_Views for these subprograms.

   procedure Clear (Self : not null access GVD_Variable_View_Record'Class);
   --  Clear the contents of Self

   overriding procedure Update
     (Self : not null access GVD_Variable_View_Record);
   overriding procedure On_Attach
     (Self    : not null access GVD_Variable_View_Record;
      Process : not null access Base_Visual_Debugger'Class);
   overriding procedure On_Detach
     (Self    : not null access GVD_Variable_View_Record;
      Process : not null access Base_Visual_Debugger'Class);
   overriding procedure Create_Toolbar
     (Self    : not null access GVD_Variable_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class);
   overriding procedure Create_Menu
     (View    : not null access GVD_Variable_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class);
   overriding procedure Filter_Changed
     (Self    : not null access GVD_Variable_View_Record;
      Pattern : in out Search_Pattern_Access);

   type Variables_Property_Record is new Property_Record with record
      Items : Item_Vectors.Vector;
   end record;
   overriding procedure Save
     (Self  : access Variables_Property_Record;
      Value : in out GNATCOLL.JSON.JSON_Value);
   overriding procedure Load
     (Self  : in out Variables_Property_Record;
      Value : GNATCOLL.JSON.JSON_Value);
   --  Saving and loading which variables are displayed for a given executable

   type Variable_MDI_Child_Record is new GPS_MDI_Child_Record with null record;
   overriding function Build_Context
     (Self  : not null access Variable_MDI_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return GPS.Kernel.Selection_Context;

   package Variable_MDI_Views is new Generic_Views.Simple_Views
     (Module_Name        => "Debugger_Variables",
      View_Name          => -"Debugger Variables",
      Formal_View_Record => GVD_Variable_View_Record,
      Formal_MDI_Child   => Variable_MDI_Child_Record,
      Reuse_If_Exist     => False,
      Commands_Category  => "",
      Local_Toolbar      => True,
      Local_Config       => True,
      Areas              => Gtkada.MDI.Sides_Only,
      Group              => GPS.Kernel.MDI.Group_Graphs,
      Position           => Position_Top,
      Initialize         => Initialize);
   package Variable_Views is new GVD.Generic_View.Simple_Views
     (Formal_View_Record => GVD_Variable_View_Record,
      Formal_MDI_Child   => Variable_MDI_Child_Record,
      Views              => Variable_MDI_Views,
      Get_View           => Get_View,
      Set_View           => Set_View);
   use type Variable_MDI_Views.View_Access;
   subtype GVD_Variable_View is Variable_MDI_Views.View_Access;

   procedure Execute_In_Debugger
     (Context : Interactive_Command_Context;
      Cmd     : String);
   --  Execute a command in the debugger

   type On_Command is new Debugger_String_Hooks_Function with null record;
   overriding function Execute
     (Self    : On_Command;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Process : access GPS.Debuggers.Base_Visual_Debugger'Class;
      Command : String) return String;
   --  Parse and process a "tree print" or "tree display" commands

   type Tree_Display_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Tree_Display_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Tree_Undisplay_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Tree_Undisplay_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Tree_Clear_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Tree_Clear_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Tree_Local_Vars_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Tree_Local_Vars_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Tree_Arguments_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Tree_Arguments_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Tree_Expression_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Tree_Expression_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type Tree_Registers_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Tree_Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;

   type On_Pref_Changed is new Preferences_Hooks_Function with null record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);

   Tree_Cmd_Format : constant Pattern_Matcher := Compile
     ("tree\s+((?:un)?display)\s+"    --  paren 1: type of command
      & "(?:"
      &   "`([^`]+)`"          --  paren 2: `command`
      &   "\s*(split)?"        --  paren 3: whether to split
      &   "|"
      &   "(\S+)"              --  paren 4: varname
      & ")",
      Case_Insensitive);
   Tree_Cmd_Display      : constant := 1;
   Tree_Cmd_Command      : constant := 2;
   Tree_Cmd_Split        : constant := 3;
   Tree_Cmd_Varname      : constant := 4;
   Tree_Cmd_Max_Paren    : constant := 4;   --  number of parenthesis

   Column_Descr          : constant := 0;
   Column_Icon           : constant := 1;
   Column_Id             : constant := 2;   --  integer id for the variable
   Column_Fg             : constant := 3;
   Column_Generic_Type   : constant := 4;   --  address of Generic_Type'Class
   Column_Full_Name      : constant := 5;

   procedure Item_From_Iter
     (Self        : not null access GVD_Variable_View_Record'Class;
      Filter_Iter : in out Gtk_Tree_Iter;
      It          : out Item);
   --  Convert from a row in the variables view to an item.
   --  If Filter_Iter is Null_Iter, the currently selection is used (and
   --  Filter_Iter is updated accordingly)

   procedure Add_Row
     (Self              : not null access Variable_Tree_View_Record'Class;
      Entity            : access Generic_Type'Class;
      Name, Full_Name   : String;
      Parent            : Gtk_Tree_Iter;
      Id                : Item_ID;
      Lang              : not null Language_Access);
   --  Adddd a new row in the tree to represent a variable

   ------------
   -- Get_Id --
   ------------

   function Get_Id
     (Self : not null access Variable_Tree_View_Record'Class;
      Iter : Gtk_Tree_Iter) return Tree_Row_Id is
   begin
      return (Typ => Get_Address (+Self.Model, Iter, Column_Generic_Type));
   end Get_Id;

   ----------
   -- Hash --
   ----------

   function Hash (Element : Tree_Row_Id) return Ada.Containers.Hash_Type is
   begin
      return Ada.Containers.Hash_Type
        (To_Integer (Element.Typ)
         mod Integer_Address (Ada.Containers.Hash_Type'Last));
   end Hash;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Self  : access Variables_Property_Record;
      Value : in out GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;

      Values : JSON_Array;
   begin
      Trace (Me, "Saving variable view to xml, has items ?"
             & Self.Items.Length'Img);

      for Item of Self.Items loop
         if not Item.Nested then
            declare
               Value : constant JSON_Value := Create_Object;
            begin
               if Item.Info.Cmd /= "" then
                  Value.Set_Field ("tag", "cmd");
                  Value.Set_Field ("value", To_String (Item.Info.Cmd));
                  Value.Set_Field ("split", Item.Info.Split_Lines);

               else
                  Value.Set_Field ("tag", "variable");
                  Value.Set_Field ("value", To_String (Item.Info.Varname));
               end if;
               Append (Values, Value);
            end;
         end if;
      end loop;
      Value.Set_Field ("value", Values);
   end Save;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Self  : in out Variables_Property_Record;
      Value : GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;

      It     : Item;
      Values : constant JSON_Array := Value.Get ("value");
   begin
      Trace (Me, "Loading variable view from JSON, has items ?"
             &  Boolean'Image (Length (Values) > 0));

      for Index in 1 .. Length (Values) loop
         declare
            V : constant JSON_Value := Get (Values, Index);
         begin
            if String'(V.Get ("tag")) = "cmd" then
               It :=
                 (Info   => Wrap_Debugger_Command
                    (V.Get ("value"),
                     Split_Lines => V.Get ("split")),
                  Nested => False,
                  Id     => Unknown_Id);
               Self.Items.Prepend (It);

            elsif String'(V.Get ("tag")) = "variable" then
               It :=
                 (Info   => Wrap_Variable (V.Get ("value")),
                  Nested => False,
                  Id     => Unknown_Id);
               Self.Items.Prepend (It);
            end if;
         end;
      end loop;
   end Load;

   -------------------------
   -- Execute_In_Debugger --
   -------------------------

   procedure Execute_In_Debugger
     (Context : Interactive_Command_Context;
      Cmd     : String)
   is
      Process  : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Get_Kernel (Context.Context)));
   begin
      if Process /= null then
         Process_User_Command (Process, Cmd, Output_Command => True);
      end if;
   end Execute_In_Debugger;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Tree_Display_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Name     : constant String :=
        Get_Variable_Name (Context.Context, Dereference => False);
   begin
      if Name /= "" then
         Execute_In_Debugger (Context, "tree display " & Name);
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Tree_Clear_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View : constant GVD_Variable_View :=
        Variable_MDI_Views.Retrieve_View (Get_Kernel (Context.Context));
   begin
      if View /= null then
         View.Clear;
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Tree_Undisplay_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View : constant GVD_Variable_View :=
        Variable_MDI_Views.Retrieve_View (Get_Kernel (Context.Context));
      It   : Item;
      Filter_Iter : Gtk_Tree_Iter;
   begin
      if View /= null then
         Filter_Iter := Null_Iter;
         Item_From_Iter (View, Filter_Iter => Filter_Iter, It => It);
         if It.Info.Cmd /= Null_Unbounded_String then
            Execute_In_Debugger
              (Context, "tree undisplay `" & To_String (It.Info.Cmd) & "`");
         elsif It.Info.Varname /= Null_Unbounded_String then
            Execute_In_Debugger
              (Context, "tree undisplay " & To_String (It.Info.Varname));
         end if;
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Tree_Local_Vars_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Process  : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Get_Kernel (Context.Context)));
   begin
      Execute_In_Debugger
        (Context, "tree display `" & Process.Debugger.Info_Locals & "` split");
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Tree_Expression_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Kernel   : constant Kernel_Handle := Get_Kernel (Context.Context);
      Is_Func    : aliased Boolean;
      Expression : constant String := Display_Text_Input_Dialog
        (Kernel        => Kernel,
         Title         => -"Display the value of an expression",
         Message       => -"Enter an expression to display:",
         Key           => "gvd_display_expression_dialog",
         Check_Msg     => -"Expression is a debugger command",
         Key_Check     => "expression_subprogram_debugger",
         Button_Active => Is_Func'Unchecked_Access);
   begin
      if Expression /= "" then
         if Is_Func then
            Execute_In_Debugger (Context, "tree display `" & Expression & "`");
         else
            Execute_In_Debugger (Context, "tree display " & Expression);
         end if;
      end if;
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Tree_Arguments_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Process  : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Get_Kernel (Context.Context)));
   begin
      Execute_In_Debugger
        (Context, "tree display `" & Process.Debugger.Info_Args & "` split");
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Tree_Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      Process  : constant Visual_Debugger :=
        Visual_Debugger (Get_Current_Debugger (Get_Kernel (Context.Context)));
   begin
      Execute_In_Debugger
        (Context,
         "tree display `" & Process.Debugger.Info_Registers & "` split");
      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Self    : On_Command;
      Kernel  : not null access Kernel_Handle_Record'Class;
      Process : access GPS.Debuggers.Base_Visual_Debugger'Class;
      Command : String) return String
   is
      pragma Unreferenced (Self);
      M         : Match_Array (0 .. Tree_Cmd_Max_Paren);
      It        : Item;
      View      : GVD_Variable_View;

      function Extract (Paren : Natural) return String
         is (Command (M (Paren).First .. M (Paren).Last));
      --  Extract one of the named grouped from the regexp

   begin
      if Process = null or else not Starts_With (Command, "tree ") then
         return "";
      end if;

      Match (Tree_Cmd_Format, Command, M);
      if M (0) = GNAT.Regpat.No_Match then
         return "";
      end if;

      Variable_Views.Attach_To_View
        (Process, Kernel, Create_If_Necessary => True);
      View := Get_View (Process);

      if M (Tree_Cmd_Command) /= GNAT.Regpat.No_Match then
         It.Info := Wrap_Debugger_Command
           (Extract (Tree_Cmd_Command),
            Split_Lines => M (Tree_Cmd_Split) /= GNAT.Regpat.No_Match);
      elsif M (Tree_Cmd_Varname) /= GNAT.Regpat.No_Match then
         It.Info := Wrap_Variable (Extract (Tree_Cmd_Varname));
      else
         return "";  --  Should not happen
      end if;

      It.Nested := False;

      declare
         Cmd  : constant String := Extract (Tree_Cmd_Display);
         Curs : Item_Vectors.Cursor;
      begin
         if Cmd = "display" then
            View.Ids := View.Ids + 1;
            It.Id := View.Ids;
            View.Tree.Items.Append (It);

         elsif Cmd = "undisplay" then
            Curs := View.Tree.Items.First;
            while Item_Vectors.Has_Element (Curs) loop
               if Is_Same (Item_Vectors.Element (Curs).Info, It.Info) then
                  View.Tree.Items.Delete (Curs);
                  exit;
               end if;
               Item_Vectors.Next (Curs);
            end loop;
         end if;
      end;

      Update (View);

      return Command_Intercepted;  --  command was processed
   end Execute;

   ---------------
   -- On_Attach --
   ---------------

   overriding procedure On_Attach
     (Self    : not null access GVD_Variable_View_Record;
      Process : not null access Base_Visual_Debugger'Class)
   is
      V : constant Visual_Debugger := Visual_Debugger (Process);
      Found    : Boolean;
      Property : Variables_Property_Record;
   begin
      Self.Tree.Process := V;

      if Preserve_State_On_Exit.Get_Pref then
         Get_Property
           (Property,
            Get_Executable (V.Debugger),
            Name  => "debugger_variables",
            Found => Found);
         if Found then
            Self.Tree.Items := Property.Items;

            for It of Self.Tree.Items loop
               Self.Ids := Self.Ids + 1;
               It.Id := Self.Ids;
            end loop;

            Update (Self);
         end if;
      end if;
   end On_Attach;

   ---------------
   -- Deep_Copy --
   ---------------

   function Deep_Copy (X : Item_Vectors.Vector) return Item_Vectors.Vector is
      Result : Item_Vectors.Vector;
      It     : Item;
   begin
      for E of X loop
         It := E;
         if E.Info.Entity /= null then
            It.Info.Entity := Clone (E.Info.Entity.all);
         end if;
         Result.Append (It);
      end loop;

      return Result;
   end Deep_Copy;

   ---------------
   -- On_Detach --
   ---------------

   overriding procedure On_Detach
     (Self    : not null access GVD_Variable_View_Record;
      Process : not null access Base_Visual_Debugger'Class)
   is
      Property : access Variables_Property_Record;
   begin
      if Preserve_State_On_Exit.Get_Pref then
         Property := new Variables_Property_Record;
         Property.Items := Deep_Copy (Self.Tree.Items);
         Set_Property
           (Kernel     => Self.Kernel,
            File       => Get_Executable (Visual_Debugger (Process).Debugger),
            Name       => "debugger_variables",
            Property   => Property,
            Persistent => True);
      end if;

      Self.Clear;
      Self.Tree.Process := null;
   end On_Detach;

   -----------
   -- Clear --
   -----------

   procedure Clear (Self : not null access GVD_Variable_View_Record'Class) is
   begin
      for It of Self.Tree.Items loop
         Free (It.Info);
      end loop;

      Self.Tree.Items.Clear;
      Self.Tree.Model.Clear;
   end Clear;

   ------------------
   -- Add_Children --
   ------------------

   overriding procedure Add_Children
     (Self       : not null access Variable_Tree_View_Record;
      Store_Iter : Gtk_Tree_Iter)
   is
      Full_Name   : constant String :=
        Get_String (Self.Model, Store_Iter, Column_Full_Name);
      Lang        : constant access Language_Root'Class :=
        Get_Language (Self.Process.Debugger);
      Deref       : constant String := Lang.Dereference_Name (Full_Name);
      It          : Item;

      procedure Add (It : in out Item);
      --  Add a new row for It

      procedure Add (It : in out Item) is
      begin
         --  Nested items are not updated in Update, so we need to do it now
         if It.Nested then
            Update (It.Info, Self.Process);
         end if;

         Add_Row
           (Self      => Self,
            Entity    => It.Info.Entity,
            Name      => It.Info.Name,
            Full_Name => It.Info.Name,
            Parent    => Store_Iter,
            Id        => It.Id,
            Lang      => Lang);
      end Add;

   begin
      --  Check whether we already have this item in the list.
      --  We expect this list to be relatively short (less than 1000 elements
      --  in general), so no need for a more elaborate data structure.

      for It2 of Self.Items loop
         if It2.Info.Varname = Deref then
            Add (It2);
            return;
         end if;
      end loop;

      It :=
        (Info   => Wrap_Variable (Deref),
         Nested => True,
         Id     => Unknown_Id);
      Self.Items.Append (It);
      Add (It);
   end Add_Children;

   -------------
   -- Add_Row --
   -------------

   procedure Add_Row
     (Self              : not null access Variable_Tree_View_Record'Class;
      Entity            : access Generic_Type'Class;
      Name, Full_Name   : String;
      Parent            : Gtk_Tree_Iter;
      Id                : Item_ID;
      Lang              : not null Language_Access)
   is
      Row   : Gtk_Tree_Iter;
      Ent   : Generic_Type_Access;
      Dummy : Boolean;

      Fg    : constant String := To_String (Default_Style.Get_Pref_Fg);
      Contrast : constant String :=
        To_Hex (Shade_Or_Lighten (Default_Style.Get_Pref_Fg));

      function Display_Type_Name return String with Inline;
      function Display_Name return String with Inline;
      --  Return the display name or type name

      function Display_Name return String is
      begin
         if Name = "" then
            return "";
         else
            return "<b>" & XML_Utils.Protect (Name) & "</b> = ";
         end if;
      end Display_Name;

      function Display_Type_Name return String is
         T : constant String :=
           (if Entity = null then "" else Entity.Get_Type_Name (Lang));
      begin
         if T'Length = 0 or else not Show_Types.Get_Pref then
            return "";
         else
            return "<span foreground=""" & Contrast & """>("
              & XML_Utils.Protect (T) & ")</span> ";
         end if;
      end Display_Type_Name;

      Descr : constant String :=
        Display_Name & Display_Type_Name
        & (if Entity = null
           then "" else XML_Utils.Protect (Entity.Get_Simple_Value));

   begin
      Self.Model.Append (Iter => Row, Parent => Parent);
      Set_And_Clear
        (Self.Model,
         Iter   => Row,
         Values =>
           (Column_Descr        => As_String (Descr),
            Column_Icon         => As_String
              (if Parent = Null_Iter
               then Stock_From_Category
                 (Is_Declaration => False,
                  Visibility     => Language.Visibility_Public,
                  Category       => Language.Cat_Function)
               else ""),
            Column_Id           => As_Int (Gint (Id)),
            Column_Generic_Type => As_Pointer
              ((if Entity = null
               then System.Null_Address else Entity.all'Address)),
            Column_Fg           => As_String
              (if Entity /= null and then Entity.Is_Changed
               then Change_Color.Get_Pref else Fg),
            Column_Full_Name    => As_String (Full_Name)
           ));

      if Entity /= null then
         declare
            Iter : Generic_Iterator'Class := Entity.Start;
         begin
            while not Iter.At_End loop
               Ent := Iter.Data;
               if Ent /= null then
                  Add_Row
                    (Self,
                     Entity    => Ent,
                     Name      => Iter.Field_Name (Lang, ""),
                     Full_Name => Iter.Field_Name (Lang, Full_Name),
                     Parent    => Row,
                     Id        => Unknown_Id,
                     Lang      => Lang);
               end if;
               Iter.Next;
            end loop;
         end;

         --  Make sure access types are expandable.
         if Entity.all in Access_Type'Class then
            Self.Set_Might_Have_Children (Row);
         end if;
      end if;
   end Add_Row;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Self : not null access GVD_Variable_View_Record)
   is
      Process   : constant Visual_Debugger :=
        Visual_Debugger (Get_Process (Self));
      Lang      : Language.Language_Access;
      Expansion : Expansions.Expansion_Status;
   begin
      if Process = null then
         return;
      end if;

      if Process.Debugger /= null then
         --  Compute the new value for all the items
         for Item of Self.Tree.Items loop
            if not Item.Nested and then Item.Info.Auto_Refresh then
               Update (Item.Info, Process);
            end if;
         end loop;

         Lang := Get_Language (Process.Debugger);
      end if;

      --  Now display them, and preserve the expansion of items

      Expansions.Get_Expansion_Status (Self.Tree, Expansion);
      Self.Tree.Model.Clear;

      if Process.Debugger /= null and then not Self.Tree.Items.Is_Empty then
         for It of Self.Tree.Items loop
            if not It.Nested then
               Self.Tree.Add_Row
                 (Entity    => It.Info.Entity,
                  Name      => It.Info.Name,
                  Full_Name => It.Info.Name,
                  Parent    => Null_Iter,
                  Id        => It.Id,
                  Lang      => Lang);
            end if;
         end loop;
         Expansions.Set_Expansion_Status (Self.Tree, Expansion);
      end if;
   end Update;

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self   : access GVD_Variable_View_Record'Class) return Gtk_Widget
   is
      Scrolled : Gtk_Scrolled_Window;
      Col      : Gtk_Tree_View_Column;
      Dummy    : Gint;
      Text     : Gtk_Cell_Renderer_Text;
      Pixbuf   : Gtk_Cell_Renderer_Pixbuf;
      Pref     : access On_Pref_Changed;
   begin
      Gtk.Box.Initialize_Vbox (Self);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Self.Pack_Start (Scrolled, Expand => True, Fill => True);

      Self.Tree := new Variable_Tree_View_Record;
      Initialize
        (Self.Tree,
         Column_Types     => (Column_Descr        => GType_String,
                              Column_Icon         => GType_String,
                              Column_Id           => GType_Int,
                              Column_Fg           => GType_String,
                              Column_Generic_Type => GType_Pointer,
                              Column_Full_Name    => GType_String),
         Filtered         => True,
         Set_Visible_Func => True);

      Scrolled.Add (Self.Tree);

      Self.Tree.Set_Headers_Visible (False);
      Self.Tree.Set_Enable_Search (True);
      Setup_Contextual_Menu (Self.Kernel, Self.Tree);

      Gtk_New (Col);
      Col.Set_Resizable (True);
      Col.Set_Reorderable (True);
      Dummy := Self.Tree.Append_Column (Col);

      Gtk_New (Pixbuf);
      Col.Pack_Start (Pixbuf, False);
      Col.Add_Attribute (Pixbuf, "icon-name", Column_Icon);

      Gtk_New (Text);
      Col.Pack_Start (Text, False);
      Col.Add_Attribute (Text, "markup", Column_Descr);
      Col.Add_Attribute (Text, "foreground", Column_Fg);

      Pref := new On_Pref_Changed;
      Pref.Execute (Self.Kernel, null);
      Preferences_Changed_Hook.Add (Pref, Watch => Self);

      Self.Show_All;
      return Gtk_Widget (Self.Tree);
   end Initialize;

   --------------
   -- Set_View --
   --------------

   procedure Set_View
     (Process : not null access Base_Visual_Debugger'Class;
      View    : access GVD_Variable_View_Record'Class := null)
   is
      Old : constant GVD_Variable_View := Get_View (Process);
   begin
      Visual_Debugger (Process).Variables_View := Abstract_View_Access (View);

      --  If we are detaching, clear the old view
      if View = null and then Old /= null then
         On_Process_Terminated (Old);
      end if;
   end Set_View;

   --------------
   -- Get_View --
   --------------

   function Get_View
     (Process : not null access Base_Visual_Debugger'Class)
      return access GVD_Variable_View_Record'Class is
   begin
      return GVD_Variable_View (Visual_Debugger (Process).Variables_View);
   end Get_View;

   --------------------
   -- Item_From_Iter --
   --------------------

   procedure Item_From_Iter
     (Self        : not null access GVD_Variable_View_Record'Class;
      Filter_Iter : in out Gtk_Tree_Iter;
      It          : out Item)
   is
      Store_Iter, Parent : Gtk_Tree_Iter;
      Id                 : Item_ID;
      M                  : Gtk_Tree_Model;
   begin
      if Filter_Iter = Null_Iter then
         Self.Tree.Get_Selection.Get_Selected (M, Filter_Iter);
         Store_Iter := Self.Tree.Convert_To_Store_Iter (Filter_Iter);
      else
         Store_Iter := Self.Tree.Convert_To_Store_Iter (Filter_Iter);
      end if;

      Find_Item_Id :
      while Store_Iter /= Null_Iter loop
         Parent := Self.Tree.Model.Parent (Store_Iter);
         if Parent = Null_Iter then
            Id := Item_ID (Self.Tree.Model.Get_Int (Store_Iter, Column_Id));

            for It2 of Self.Tree.Items loop
               if It2.Id = Id then
                  It := It2;
                  return;
               end if;
            end loop;
         end if;

         Store_Iter := Parent;
      end loop Find_Item_Id;

      It := No_Item;
   end Item_From_Iter;

   -------------------
   -- Build_Context --
   -------------------

   overriding function Build_Context
     (Self  : not null access Variable_MDI_Child_Record;
      Event : Gdk.Event.Gdk_Event := null)
      return GPS.Kernel.Selection_Context
   is
      View : constant GVD_Variable_View :=
        Variable_MDI_Views.View_From_Child (Self);
      Context : Selection_Context;
      It      : Item;
      Filter_Iter : Gtk_Tree_Iter;
   begin
      Context := GPS_MDI_Child_Record (Self.all).Build_Context (Event);

      if Event /= null then
         Filter_Iter := Find_Iter_For_Event (View.Tree, Event);
         Item_From_Iter (View, Filter_Iter => Filter_Iter, It => It);
         View.Tree.Get_Selection.Select_Iter (Filter_Iter);
      else
         Filter_Iter := Null_Iter;
         Item_From_Iter (View, Filter_Iter => Filter_Iter, It => It);
      end if;

      if It.Info.Cmd /= Null_Unbounded_String then
         Set_Area_Information
           (Context     => Context,
            Text        => "`" & To_String (It.Info.Cmd) & "`");
      elsif It.Info.Varname /= Null_Unbounded_String then
         Set_Area_Information
           (Context     => Context,
            Text        =>
              View.Tree.Filter.Get_String (Filter_Iter, Column_Full_Name));
      end if;

      return Context;
   end Build_Context;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference)
   is
      pragma Unreferenced (Self);
      View : constant GVD_Variable_View :=
        Variable_MDI_Views.Retrieve_View (Kernel);
   begin
      if View /= null then
         Set_Font_And_Colors (View.Tree, Fixed_Font => False, Pref => Pref);
         if Pref = null
           or else Pref = Preference (Show_Types)
         then
            View.Update;
         end if;
      end if;
   end Execute;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (View    : not null access GVD_Variable_View_Record;
      Menu    : not null access Gtk.Menu.Gtk_Menu_Record'Class) is
   begin
      Append_Menu (Menu, View.Kernel, Show_Types);
   end Create_Menu;

   --------------------
   -- Create_Toolbar --
   --------------------

   overriding procedure Create_Toolbar
     (Self    : not null access GVD_Variable_View_Record;
      Toolbar : not null access Gtk.Toolbar.Gtk_Toolbar_Record'Class) is
   begin
      Self.Build_Filter
        (Toolbar,
         Hist_Prefix => "debugger-variables",
         Tooltip     => -"Filter the contents of the Variables view",
         Placeholder => -"filter",
         Options     =>
           Has_Regexp or Has_Negate or Has_Whole_Word or Has_Fuzzy);
   end Create_Toolbar;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
     (Self   : not null access Variable_Tree_View_Record;
      Iter   : Gtk_Tree_Iter) return Boolean is
   begin
      return Self.Pattern = null
        or else Self.Pattern.Start
          (Self.Model.Get_String (Iter, Column_Descr)) /= GPS.Search.No_Match;
   end Is_Visible;

   --------------------
   -- Filter_Changed --
   --------------------

   overriding procedure Filter_Changed
     (Self    : not null access GVD_Variable_View_Record;
      Pattern : in out Search_Pattern_Access) is
   begin
      GPS.Search.Free (Self.Tree.Pattern);
      Self.Tree.Pattern := Pattern;
      Self.Tree.Refilter;  --  Recompute visibility of rows
   end Filter_Changed;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Printable_Var_Filter    : Action_Filter;
      Debugger_Stopped_Filter : Action_Filter;
   begin
      Variable_Views.Register_Module (Kernel);
      Variable_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open debugger variables window",
         Description => -"Open the Variables view for the debugger");

      Debugger_Command_Action_Hook.Add (new On_Command);

      Debugger_Stopped_Filter := Kernel.Lookup_Filter ("Debugger stopped");
      Printable_Var_Filter := Debugger_Stopped_Filter
        and Kernel.Lookup_Filter ("Debugger printable variable");

      Register_Action
        (Kernel, "debug tree display variable",
         Command => new Tree_Display_Command,
         Description =>
           -"Display the value of the variable in the Variables view",
         Filter      => Printable_Var_Filter,
         Category    => -"Debug");
      Register_Contextual_Menu
        (Kernel,
         Label       => -"Debug/Display in Variables view",
         Action      => "debug tree display variable");

      Register_Action
        (Kernel, "debug tree display local variables",
         Command => new Tree_Local_Vars_Command,
         Description =>
           -"Display the local variables in the Variables view",
         Filter      => Debugger_Stopped_Filter,
         Icon_Name   => "gps-debugger-local-vars-symbolic",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug tree display arguments",
         Command     => new Tree_Arguments_Command,
         Description =>
           -("Display the arguments of the current subprogram in the"
             & "  Variables view"),
         Filter      => Debugger_Stopped_Filter,
         Icon_Name   => "gps-debugger-arguments-symbolic",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug tree display registers",
         Command     => new Tree_Registers_Command,
         Description =>
           -"Display the contents of registers in the Variables view",
         Filter      => Debugger_Stopped_Filter,
         Icon_Name   => "gps-debugger-registers-symbolic",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug tree display expression",
         Command     => new Tree_Expression_Command,
         Description =>
           -"Display the value of any expression in the Variables view",
         Filter      => Debugger_Stopped_Filter,
         Icon_Name   => "gps-add-symbolic",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug tree undisplay",
         Command     => new Tree_Undisplay_Command,
         Description =>
           -"Remove the display of a variable in the Variables view",
         Filter      => Debugger_Stopped_Filter,
         Icon_Name   => "gps-remove-symbolic",
         Category    => -"Debug");

      Register_Action
        (Kernel, "debug tree clear",
         Command     => new Tree_Clear_Command,
         Description =>
           -"Remove the display of all variables in the Variables view",
         Filter      => Debugger_Stopped_Filter,
         Icon_Name   => "gps-clear-symbolic",
         Category    => -"Debug");

      Show_Types := Kernel.Get_Preferences.Create_Invisible_Pref
        ("debugger-variables-show-types", True, Label => -"Show types");
   end Register_Module;

end GVD.Variables;
