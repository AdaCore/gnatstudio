------------------------------------------------------------------------------
--                               GNAT Studio                                --
--                                                                          --
--                        Copyright (C) 2021, AdaCore                       --
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
with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Gtk.Label;     use Gtk.Label;
with Gtk.Menu;      use Gtk.Menu;
with Gtk.Menu_Item; use Gtk.Menu_Item;

with GNATCOLL.Projects;
with GNATCOLL.Traces;    use GNATCOLL.Traces;

with Commands.Interactive;         use Commands.Interactive;

with GPS.Kernel.Actions;           use GPS.Kernel.Actions;
with GPS.Kernel.Contexts;          use GPS.Kernel.Contexts;
with GPS.Kernel.Hooks;             use GPS.Kernel.Hooks;
with GPS.Kernel.Messages;          use GPS.Kernel.Messages;
with GPS.Kernel.Modules.UI;        use GPS.Kernel.Modules.UI;
with GPS.Kernel.Task_Manager;      use GPS.Kernel.Task_Manager;

with GPS.Editors.Line_Information; use GPS.Editors.Line_Information;

package body Refactoring.Code_Actions is

   Me : constant Trace_Handle := Create ("Refactoring.Code_Actions");

   Code_Action_Name : constant String := "Show Code Actions";

   --  By design, there should be only one "Code Action" message, at the
   --  place of the cursor. We guarantee in this module that there is only
   --  one such message. This message always belongs to Category below.
   Category : Unbounded_String;

   type Code_Action_Record is record
      Command : Command_Access;
      Markup  : Unbounded_String;
   end record;

   package Code_Action_Vectors is new Ada.Containers.Vectors
     (Positive, Code_Action_Record);

   type Code_Action_Message is new
     GPS.Kernel.Messages.Primary_Abstract_Message with
   record
      Actions : Code_Action_Vectors.Vector;
      Title   : Unbounded_String;
   end record;
   type Code_Action_Message_Access is access all Code_Action_Message'Class;

   overriding function Get_Text
     (Self : not null access constant Code_Action_Message)
      return Unbounded_String is (To_Unbounded_String ("code action"));

   overriding procedure Finalize (Self : not null access Code_Action_Message);

   function Get_Or_Create_Current_Code_Action_Message
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type;
      Create : Boolean) return Code_Action_Message_Access;
   --  Get the current message, suitable for the given location. Create it if
   --  needed. If a message exists but at another location, remove it.

   type Code_Action_Command is new Root_Command with record
      Kernel : Kernel_Handle;
   end record;
   overriding function Execute
     (Command : access Code_Action_Command) return Command_Return_Type;

   type On_Location_Changed is new File_Location_Hooks_Function with
     null record;
   overriding procedure Execute
     (Self   : On_Location_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line, Column : Integer;
      Project      : GNATCOLL.Projects.Project_Type);
   --  Clear code action messages in response to the location changing

   type Code_Action_Menu_Item_Record is new Gtk_Menu_Item_Record with record
      Kernel   : Kernel_Handle;
      Position : Positive;
   end record;
   type Code_Action_Menu_Item is access all Code_Action_Menu_Item_Record'Class;

   procedure On_Menu_Item_Activated (Self : access Gtk_Menu_Item_Record'Class);
   --  Called when a menu item has been activated

   function Popup_Code_Actions_Menu
     (Kernel : Kernel_Handle) return Command_Return_Type;
   --  Pop up the local menu showing the list of code actions that can
   --  be performed. Return Failure if none was available.

   type Activate_Code_Actions_Menu_Command is new Interactive_Command with
     null record;
   overriding function Execute
     (Command : access Activate_Code_Actions_Menu_Command;
      Context : Interactive_Command_Context)
      return Commands.Command_Return_Type;

   ----------------------------
   -- On_Menu_Item_Activated --
   ----------------------------

   procedure On_Menu_Item_Activated
     (Self : access Gtk_Menu_Item_Record'Class)
   is
      Item    : constant Code_Action_Menu_Item := Code_Action_Menu_Item (Self);
      M       : Message_Access;
      Message : Code_Action_Message_Access;
      Context : constant Selection_Context :=
        Item.Kernel.Get_Current_Context;

      Action  : Code_Action_Record;
      Command : Command_Access;
   begin
      --  Check for the presence of a code action relevant to the current
      --  context.

      if not Has_File_Information (Context) then
         return;
      end if;

      M := Item.Kernel.Get_Messages_Container.Get_First_Message
        (File_Information (Context), Category);

      if M = null then
         return;
      end if;

      Message := Code_Action_Message_Access (M);

      if Message.Actions.Last_Index < Item.Position then
         return;
      end if;

      --  We reached this point: we can proceed with executing the code action

      Action := Message.Actions.Element (Item.Position);

      --  Memory management: we are about to transfer the ownership of the
      --  command to the Task_Manager: make sure we don't free it when we
      --  free the message as well.
      Command := Action.Command;
      Action.Command := null;
      Message.Actions.Replace_Element (Item.Position, Action);

      Launch_Background_Command
        (Item.Kernel, Command,
         Active            => False,
         Show_Bar          => True,
         Queue_Id          => "Code Action",
         Block_Exit        => False,
         Start_Immediately => True);

   exception
      when E : others =>
         Trace (Me, E);
   end On_Menu_Item_Activated;

   -------------
   -- Execute --
   -------------

   overriding procedure Execute
     (Self   : On_Location_Changed;
      Kernel : not null access Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line, Column : Integer;
      Project      : GNATCOLL.Projects.Project_Type)
   is
      Ignored : Code_Action_Message_Access;
   begin
      Ignored := Get_Or_Create_Current_Code_Action_Message
        (Kernel => Kernel,
         File   => File,
         Line   => Editable_Line_Type (Line),
         Column => Visible_Column_Type (Column),
         Create => False);
   exception
      when E : others =>
         Trace (Me, E);
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Activate_Code_Actions_Menu_Command;
      Context : Interactive_Command_Context) return Command_Return_Type
   is
   begin
      return Popup_Code_Actions_Menu (Get_Kernel (Context.Context));
   end Execute;

   -------------
   -- Execute --
   -------------

   overriding function Execute
     (Command : access Code_Action_Command) return Command_Return_Type
   is
   begin
      return Popup_Code_Actions_Menu (Command.Kernel);
   end Execute;

   -----------------------------
   -- Popup_Code_Actions_Menu --
   -----------------------------

   function Popup_Code_Actions_Menu
     (Kernel : Kernel_Handle) return Command_Return_Type
   is
      M       : Message_Access;
      Message : Code_Action_Message_Access;
      Context : constant Selection_Context := Kernel.Get_Current_Context;

      Menu    : Gtk_Menu;
      Item    : Code_Action_Menu_Item;
      Label   : Gtk_Label;
      Pos     : Positive := 1;
   begin
      --  Check for the presence of a code action relevant to the current
      --  context.

      if not Has_File_Information (Context) then
         return Failure;
      end if;

      M := Kernel.Get_Messages_Container.Get_First_Message
        (File_Information (Context), Category);

      if M = null then
         return Failure;
      end if;

      Message := Code_Action_Message_Access (M);

      if Message.Actions.Is_Empty then
         return Failure;
      end if;

      --  If we reach this stage, we can pop a menu with the various
      --  choices.

      Gtk_New (Menu);
      Menu.Set_Name ("gnatstudio_code_actions_menu");

      for Action of Message.Actions loop
         Item := new Code_Action_Menu_Item_Record;
         Initialize_With_Label (Item, "");
         Item.Position := Pos;
         Item.Kernel := Kernel;
         Item.On_Activate (On_Menu_Item_Activated'Access);
         Label := Gtk_Label (Item.Get_Child);
         Label.Set_Markup (To_String (Action.Markup));
         Menu.Append (Item);
         Pos := Pos + 1;
      end loop;

      Show_All (Menu);
      Menu.Set_Can_Focus (True);
      Popup_Custom_Contextual_Menu (Menu, Kernel);

      return Success;
   exception
      when E : others =>
         Trace (Me, E);
         return Failure;
   end Popup_Code_Actions_Menu;

   -----------------------------------------------
   -- Get_Or_Create_Current_Code_Action_Message --
   -----------------------------------------------

   function Get_Or_Create_Current_Code_Action_Message
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File   : Virtual_File;
      Line   : Editable_Line_Type;
      Column : Visible_Column_Type;
      Create : Boolean) return Code_Action_Message_Access
   is
      use type Visible_Column_Type;
      Message : Message_Access;
      Action  : Action_Item;
   begin
      Message := Kernel.Get_Messages_Container.Get_First_Message
        (File, Category);

      if Message /= null then
         --  We've already got a message. It might be suitable: check that
         --  it's on the right line.
         if Message.Get_Line = Natural (Line)
           and then Message.Get_Column = Column
         then
            return Code_Action_Message_Access (Message);
         end if;

         --  If we reach this, the message is not suitable and should be
         --  deleted.
         Kernel.Get_Messages_Container.Remove_Category
           (To_String (Category), Empty_Message_Flags);
      end if;

      --  If we reach this, there is no message in the Category: create our
      --  message now if requested

      if not Create then
         return null;
      end if;

      Message := new Code_Action_Message'
        (Primary_Abstract_Message with
           Actions => Code_Action_Vectors.Empty_Vector,
         Title => To_Unbounded_String (""));

      GPS.Kernel.Messages.Initialize
        (Self                     => Message,
         Container                => Kernel.Get_Messages_Container,
         Category                 => To_String (Category),
         File                     => File,
         Line                     => Natural (Line),
         Column                   => 1,
         Importance               => Unspecified,
         Actual_Line              => (Integer (Line)),
         Actual_Column            => 1,
         Flags                    => Sides_Only,
         Allow_Auto_Jump_To_First => False);

      Action := new Line_Information_Record;

      --  Compute a nice tooltip
      declare
         Binding : constant String := Kernel.Get_Shortcut (Code_Action_Name);
      begin
         if Binding = "" then
            Action.Tooltip_Text := To_Unbounded_String ("Apply Code Action");
         else
            Action.Tooltip_Text := To_Unbounded_String
              ("Apply Code Action    <b>" & Binding & "</b>");
         end if;
      end;

      Action.Image := To_Unbounded_String ("gps-codefix");
      Action.Associated_Command := new Code_Action_Command'
        (Root_Command with Kernel => Kernel_Handle (Kernel));

      Message.Set_Action (Action);

      return Code_Action_Message_Access (Message);
   end Get_Or_Create_Current_Code_Action_Message;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize
     (Self : not null access Code_Action_Message)
   is
      Parent : constant access Abstract_Message := Message_Access (Self);
   begin
      for A of Self.Actions loop
         Unref (A.Command);
      end loop;

      --  Call Finalize from the super class
      Finalize (Parent);
   end Finalize;

   ---------------------
   -- Add_Code_Action --
   ---------------------

   procedure Add_Code_Action
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class;
      File    : Virtual_File;
      Line    : Editable_Line_Type;
      Column  : Visible_Column_Type;
      Markup  : String;
      Command : Command_Access)
   is
      Message : constant Code_Action_Message_Access
        := Get_Or_Create_Current_Code_Action_Message
          (Kernel => Kernel,
           File   => File,
           Line   => Line,
           Column => Column,
           Create => True);
      Action  : Code_Action_Record;
   begin
      --  Defensive programming, shouldn't happen
      if Message = null then
         return;
      end if;

      Action.Command := Command;
      Action.Markup  := To_Unbounded_String (Markup);

      Message.Actions.Append (Action);
   end Add_Code_Action;

   ----------------------
   -- Register_Actions --
   ----------------------

   procedure Register_Actions
     (Kernel : access GPS.Kernel.Kernel_Handle_Record'Class) is
   begin
      Category := To_Unbounded_String ("_internal_code_actions");

      Location_Changed_Hook.Add_Debounce (new On_Location_Changed);

      Register_Action
        (Kernel,
         Code_Action_Name,
         new Activate_Code_Actions_Menu_Command,
         "Pop-up a menu with Code Actions applicable" &
           " to the current editor location");
   end Register_Actions;

end Refactoring.Code_Actions;
