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

with GNATCOLL.Traces;             use GNATCOLL.Traces;

with VSS.Strings.Conversions;

with Glib;                        use Glib;
with Glib.Object;

with Gdk.RGBA;

with Gtk.Box;                     use Gtk.Box;
with Gtk.Cell_Renderer_Text;      use Gtk.Cell_Renderer_Text;
with Gtk.Enums;                   use Gtk.Enums;
with Gtk.Menu_Item;
with Gtk.Scrolled_Window;         use Gtk.Scrolled_Window;
with Gtk.Tree_Model;              use Gtk.Tree_Model;
with Gtk.Tree_View_Column;        use Gtk.Tree_View_Column;

with Commands.Interactive;        use Commands, Commands.Interactive;

with GPS.Kernel.Actions;
with GPS.Kernel.Hooks;            use GPS.Kernel.Hooks;
with GPS.Kernel.Preferences;      use GPS.Kernel.Preferences;
with GPS.Kernel.Properties;

with DAP.Modules.Preferences;
with DAP.Requests;
with DAP.Clients.Stack_Trace;     use DAP.Clients.Stack_Trace;
with DAP.Views.Registers.Scopes;

with Default_Preferences;         use Default_Preferences;
with String_Utils;                use String_Utils;
with GUI_Utils;                   use GUI_Utils;

with DAP.Views.Registers.Variables;
use DAP.Views.Registers.Variables;
with DAP.Views.Registers.SetExpression;
use DAP.Views.Registers.SetExpression;
with DAP.Views.Registers.SetVariable;
use DAP.Views.Registers.SetVariable;

package body DAP.Views.Registers is

   Me : constant Trace_Handle := Create ("GPS.DAP.Registers", On);

   type On_Pref_Changed is new Preferences_Hooks_Function with record
      View : Registers_View;
   end record;
   overriding procedure Execute
     (Self   : On_Pref_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      Pref   : Preference);
   --  Called when the preferences have changed, to refresh the editor
   --  appropriately.

   type Add_All_Registers_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Add_All_Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Add all the available registers in the view.

   type Add_Registers_Command is new Interactive_Command with null record;
   overriding function Execute
     (Command : access Add_Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Open a dialog to let the user choose the visible registers

   type Remove_Selected_Registers_Command is
     new Interactive_Command with null record;
   overriding function Execute
     (Command : access Remove_Selected_Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type;
   --  Remove the selected registers

   procedure On_Edit
     (Self     : access Glib.Object.GObject_Record'Class;
      Path     : UTF8_String;
      New_Text : UTF8_String);
   --  Edit a register's value callback

   function Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint;
   --  Smart sort for the registers' names

   ----------------
   -- Initialize --
   ----------------

   function Initialize
     (Self : access Registers_View_Record'Class) return Gtk_Widget
   is
      Scrolled : Gtk_Scrolled_Window;

      Column_Types : constant GType_Array :=
        (Name_Column           => GType_String,
         Raw_Column            => GType_String,
         Type_Column           => GType_String,
         FG_Color_Column       => Gdk.RGBA.Get_Type,
         BG_Name_Color_Column  => Gdk.RGBA.Get_Type,
         BG_Value_Color_Column => Gdk.RGBA.Get_Type,
         Editable_Column       => GType_Boolean,
         Id_Column             => GType_Int);

      Col        : Gtk_Tree_View_Column;
      Render     : Gtk_Cell_Renderer_Text;
      Col_Number : Gint with Unreferenced;

      procedure Create (Column : Glib.Gint; Allowed : Boolean; Name : String);
      --  Create column for registers values

      ------------
      -- Create --
      ------------

      procedure Create
        (Column : Glib.Gint; Allowed : Boolean; Name : String) is
      begin
         Gtk_New (Col);
         Col_Number := Self.Tree.Append_Column (Col);
         Col.Set_Title (Name);
         Col.Set_Resizable (True);
         Col.Set_Reorderable (True);
         Col.Set_Clickable (True);
         Gtk_New (Render);
         Col.Pack_Start (Render, False);
         Col.Add_Attribute (Render, "text", Column);
         Col.Add_Attribute (Render, "foreground-rgba", FG_Color_Column);
         Col.Add_Attribute (Render, "background-rgba", BG_Value_Color_Column);
         Col.Add_Attribute (Render, "editable", Editable_Column);
         Col.Set_Visible (Allowed);
         Render.On_Edited (On_Edit'Access, Self, True);
      end Create;

   begin
      Initialize_Vbox (Self, Homogeneous => False);

      Gtk_New (Scrolled);
      Scrolled.Set_Policy (Policy_Automatic, Policy_Automatic);
      Self.Pack_Start (Scrolled, Expand => True, Fill => True);

      Gtk_New (Self.Model, Column_Types);
      Gtk_New (Self.Tree,  Self.Model);
      Set_Name (Self.Tree, "Registers Tree");  --  For testsuite

      Self.Tree.Get_Selection.Set_Mode (Selection_Multiple);
      Self.Tree.Set_Search_Column (Name_Column);
      Self.Tree.Set_Grid_Lines (Grid_Lines_Both);
      Add (Scrolled, Self.Tree);

      Gtk_New (Col);
      Col_Number := Self.Tree.Append_Column (Col);
      Col.Set_Title ("Name");
      Col.Set_Resizable (True);
      Col.Set_Sort_Column_Id (Name_Column);
      Col.Set_Clickable (True);
      Gtk_New (Render);
      Col.Pack_Start (Render, False);
      Col.Add_Attribute (Render, "text", Name_Column);
      Col.Add_Attribute (Render, "background-rgba", BG_Name_Color_Column);

      Create (Raw_Column, True, "Raw");

      Create
        (Type_Column,
         DAP.Modules.Preferences.Registers_Type.Get_Pref,
         "Type");

      Self.Model.Set_Sort_Func (Name_Column, Sort_Func'Access);
      Self.Model.Set_Sort_Column_Id (Name_Column, Sort_Descending);

      Self.Modify_Font (Default_Style.Get_Pref_Font);

      Preferences_Changed_Hook.Add
        (Obj   =>
            new On_Pref_Changed'
           (Hook_Function with View => Registers_View (Self)),
         Watch => Self);

      return Gtk_Widget (Self.Tree);
   end Initialize;

   ----------
   -- Load --
   ----------

   overriding procedure Load
     (Self  : in out Registers_Property_Record;
      Value : GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;

      Values : constant JSON_Array := Value.Get ("value");
   begin
      Trace (Me, "Loading variable view from JSON, has items ?"
             &  Boolean'Image (Length (Values) > 0));

      for Index in 1 .. Length (Values) loop
         declare
            V : constant JSON_Value := Get (Values, Index);
         begin
            Self.Items.Include (String'(V.Get ("name")));
         end;
      end loop;
   end Load;

   ---------------
   -- On_Attach --
   ---------------

   overriding procedure On_Attach
     (Self   : not null access Registers_View_Record;
      Client : not null access DAP.Clients.DAP_Client'Class)
   is
      Found    : Boolean;
      Property : Registers_Property_Record;
   begin
      if Client /= null
        and then DAP.Modules.Preferences.Preserve_State_On_Exit.Get_Pref
      then
         Get_Property
           (Property,
            Client.Get_Executable,
            Name  => "dap_debugger_registers",
            Found => Found);
         if Found then
            Self.Registers := Property.Items;
            Self.Old_Values.Clear;
            Self.Update;
         end if;
      end if;
   end On_Attach;

   -------------
   -- On_Edit --
   -------------

   procedure On_Edit
     (Self     : access Glib.Object.GObject_Record'Class;
      Path     : UTF8_String;
      New_Text : UTF8_String)
   is
      use type DAP.Clients.DAP_Client_Access;

      Widget : constant Registers_View := Registers_View (Self);
      Client : constant DAP.Clients.DAP_Client_Access := Get_Client (Widget);
   begin
      if Client = null
        or else not Client.Is_Ready_For_Command
      then
         return;
      end if;

      if Client.Get_Capabilities.Is_Set
        and then Client.Get_Capabilities.Value.supportsSetExpression
      then
         declare
            Req : DAP.Views.Registers.SetExpression.
              Set_Expression_Request_Access :=
                new DAP.Views.Registers.SetExpression.
                  Set_Expression_Request (Widget.Kernel);
         begin
            Req.Parameters.arguments.expression :=
              VSS.Strings.Conversions.To_Virtual_String
                ("$" & Widget.Model.Get_String
                   (Widget.Model.Get_Iter_From_String (Path),
                    Name_Column));
            Req.Parameters.arguments.value :=
              VSS.Strings.Conversions.To_Virtual_String (New_Text);
            Req.Parameters.arguments.frameId :=
              Client.Get_Stack_Trace.Get_Current_Frame_Id;

            Client.Enqueue (DAP.Requests.DAP_Request_Access (Req));
         end;

      elsif Client.Get_Capabilities.Is_Set
        and then Client.Get_Capabilities.Value.supportsSetVariable
      then
         declare
            Req : DAP.Views.Registers.SetVariable.
              Set_Variable_Request_Access :=
                new DAP.Views.Registers.SetVariable.
                  Set_Variable_Request (Widget.Kernel);
         begin
            Req.Parameters.arguments.variablesReference :=
              Integer
                (Widget.Model.Get_Int
                   (Widget.Model.Get_Iter_From_String (Path),
                    Id_Column));
            Req.Parameters.arguments.name :=
              VSS.Strings.Conversions.To_Virtual_String
                (Widget.Model.Get_String
                   (Widget.Model.Get_Iter_From_String (Path),
                    Name_Column));
            Req.Parameters.arguments.value :=
              VSS.Strings.Conversions.To_Virtual_String (New_Text);

            Client.Enqueue (DAP.Requests.DAP_Request_Access (Req));
         end;

      else
         Widget.Kernel.Get_Messages_Window.Insert_Text
           ("Editing is not supported");
      end if;
   end On_Edit;

   ---------------------------
   -- On_Process_Terminated --
   ---------------------------

   overriding procedure On_Process_Terminated
     (View : not null access Registers_View_Record) is
   begin
      View.Old_Values.Clear;
   end On_Process_Terminated;

   -----------------------
   -- On_Status_Changed --
   -----------------------

   overriding procedure On_Status_Changed
     (Self   : not null access Registers_View_Record;
      Status : GPS.Debuggers.Debugger_State)
   is
      use GPS.Debuggers;
   begin
      if Status = Debug_Available then
         Self.Update;
      else
         Self.Registers_Id := 0;
      end if;
   end On_Status_Changed;

   -----------------
   -- Create_Menu --
   -----------------

   overriding procedure Create_Menu
     (Self : not null access Registers_View_Record;
      Menu : not null access Gtk.Menu.Gtk_Menu_Record'Class)
   is
      K : constant Kernel_Handle := Self.Kernel;
   begin
      Append_Menu (Menu, K, DAP.Modules.Preferences.Registers_Type);
      Menu.Append (Gtk.Menu_Item.Gtk_Menu_Item_New);
   end Create_Menu;

   ---------------
   -- On_Detach --
   ---------------

   overriding procedure On_Detach
     (Self   : not null access Registers_View_Record;
      Client : not null access DAP.Clients.DAP_Client'Class)
   is
      function Deep_Copy
        (Registers : Registers_Set.Set) return Registers_Set.Set;

      ---------------
      -- Deep_Copy --
      ---------------

      function Deep_Copy
        (Registers : Registers_Set.Set) return Registers_Set.Set
      is
         Result : Registers_Set.Set;
      begin
         for Name of Registers loop
            Result.Include (Name);
         end loop;

         return Result;
      end Deep_Copy;

   begin
      if Client /= null
        and then DAP.Modules.Preferences.Preserve_State_On_Exit.Get_Pref
      then
         GPS.Kernel.Properties.Set_Property
           (Kernel     => Self.Kernel,
            File       => Client.Get_Executable,
            Name       => "dap_debugger_registers",
            Property   =>
               new Registers_Property_Record'
                 (Items => Deep_Copy (Self.Registers)),
            Persistent => True);
      end if;
   end On_Detach;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Add_All_Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);

      View : constant Registers_View :=
        Registers_View
          (Registers_MDI_Views.Get_Or_Create_View
             (Get_Kernel (Context.Context)));

   begin
      View.Send_Request (Select_All_Names);
      View.Locked := True;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Add_Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);

      View : constant Registers_View :=
        Registers_View
          (Registers_MDI_Views.Get_Or_Create_View
             (Get_Kernel (Context.Context)));

   begin
      View.Send_Request (Select_Names);
      View.Locked := True;

      return Commands.Success;
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Remove_Selected_Registers_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
      pragma Unreferenced (Command);
      View   : constant Registers_View :=
        Registers_View
          (Registers_MDI_Views.Get_Or_Create_View
             (Get_Kernel (Context.Context)));
      List   : Gtk_Tree_Path_List.Glist;
      G_Iter : Gtk_Tree_Path_List.Glist;
      Path   : Gtk_Tree_Path;
      Model  : Gtk_Tree_Model;

      use Gtk_Tree_Path_List;
   begin
      pragma Assert (View /= null);
      View.Locked := True;
      View.Tree.Get_Selection.Get_Selected_Rows (Model, List);

      if Model /= Null_Gtk_Tree_Model and then List /= Null_List then
         --  The children must be modified before their fathers
         G_Iter := Gtk_Tree_Path_List.Last (List);

         while G_Iter /= Gtk_Tree_Path_List.Null_List loop
            Path := Gtk_Tree_Path (Gtk_Tree_Path_List.Get_Data (G_Iter));

            if Path /= Null_Gtk_Tree_Path then
               declare
                  Name : constant String :=
                    View.Model.Get_String
                      (View.Model.Get_Iter (Path), Name_Column);
               begin
                  View.Registers.Delete (Name);
               end;
            end if;

            G_Iter := Gtk_Tree_Path_List.Prev (G_Iter);
         end loop;
      end if;

      Free_Path_List (List);
      View.Locked := False;
      View.Update;
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
      use Default_Preferences;

      pragma Unreferenced (Kernel);
      Changed : Boolean := False;
   begin
      if Pref = null then
         Update (Self.View);
         return;

      elsif Pref = Preference (DAP.Modules.Preferences.Registers_Type) then
         Self.View.Tree.Get_Column (Type_Column).Set_Visible
           (DAP.Modules.Preferences.Registers_Type.Get_Pref);
         Changed := True;
      end if;

      if Changed then
         Self.View.Old_Values.Clear;
         Self.View.Resize := True;
         Update (Self.View);
      end if;
   end Execute;

   ------------------
   -- Send_Request --
   ------------------

   procedure Send_Request
     (Self : not null access Registers_View_Record;
      Kind : Command_Kind)
   is
      use type DAP.Clients.DAP_Client_Access;
      Client : constant DAP.Clients.DAP_Client_Access := Get_Client (Self);
   begin
      if Self.Locked then
         return;
      end if;

      if Client = null then
         Self.Model.Clear;
         return;
      end if;

      if Self.Registers_Id = 0 then
         declare
            Req : DAP.Views.Registers.Scopes.Scopes_Request_Access;
         begin
            Req := new DAP.Views.Registers.Scopes.Scopes_Request (Self.Kernel);

            Req.Kind := Kind;
            Req.Parameters.arguments.frameId :=
              Client.Get_Stack_Trace.Get_Current_Frame_Id;
            Get_Client (Self).Enqueue (DAP.Requests.DAP_Request_Access (Req));
         end;

      else
         declare
            Req : Variables_Request_Access :=
              new Variables_Request (Self.Kernel);
         begin
            Req.Kind := Kind;
            Req.Parameters.arguments.variablesReference :=
              Self.Registers_Id;
            Get_Client (Self).Enqueue (DAP.Requests.DAP_Request_Access (Req));
         end;
      end if;
   end Send_Request;

   ----------
   -- Save --
   ----------

   overriding procedure Save
     (Self  : access Registers_Property_Record;
      Value : in out GNATCOLL.JSON.JSON_Value)
   is
      use GNATCOLL.JSON;

      Values : JSON_Array;
   begin
      Trace (Me, "Saving registers view to JSON, has items ?"
             & Self.Items.Length'Img);

      for Item of Self.Items loop
         declare
            Register_Value : constant JSON_Value := Create_Object;
         begin
            Register_Value.Set_Field ("name", Item);
            Append (Values, Register_Value);
         end;
      end loop;
      Value.Set_Field ("value", Values);
   end Save;

   ---------------
   -- Sort_Func --
   ---------------

   function Sort_Func
     (Model : Gtk_Tree_Model;
      A     : Gtk.Tree_Model.Gtk_Tree_Iter;
      B     : Gtk.Tree_Model.Gtk_Tree_Iter) return Gint
   is
      S_A : constant String := Get_String (Model, A, Name_Column);
      S_B : constant String := Get_String (Model, B, Name_Column);
   begin
      if Smart_Sort (S_A, S_B) then
         return 1;
      else
         return -1;
      end if;
   end Sort_Func;

   ------------
   -- Update --
   ------------

   procedure Update (Client : not null access DAP.Clients.DAP_Client'Class) is
      View : constant Registers_View := Registers_View
        (Registers_MDI_Views.Retrieve_View (Client.Kernel));
   begin
      if View /= null then
         View.Update;
      end if;
   end Update;

   ------------
   -- Update --
   ------------

   overriding procedure Update
     (Self : not null access Registers_View_Record) is
   begin
      Self.Send_Request (Update_Registers);
   end Update;

   ---------------------
   -- Register_Module --
   ---------------------

   procedure Register_Module
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class)
   is
      Filter : Action_Filter;
   begin
      Filter := Kernel.Lookup_Filter ("Debugger stopped");

      Simple_Views.Register_Module (Kernel);
      Simple_Views.Register_Open_View_Action
        (Kernel,
         Action_Name => "open registers view",
         Description => "Open the Registers view for the debugger",
         Filter      => Filter);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "registers add all",
         Command     => new Add_All_Registers_Command,
         Description => "Add all registers",
         Icon_Name   => "gps-add-symbolic",
         Category    => "Debug",
         Filter      => Filter);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "registers add dialog",
         Command     => new Add_Registers_Command,
         Description => "Open a dialog to select the registers",
         Icon_Name   => "gps-add-symbolic",
         Category    => "Debug",
         Filter      => Filter);

      GPS.Kernel.Actions.Register_Action
        (Kernel, "registers delete selected",
         Command     => new Remove_Selected_Registers_Command,
         Description => "Remove the selected registers",
         Icon_Name   => "gps-remove-symbolic",
         Category    => "Debug",
         Filter      => Filter);
   end Register_Module;

end DAP.Views.Registers;
