------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2001-2019, AdaCore                     --
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

with Ada.Characters.Handling; use Ada.Characters.Handling;
with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;

with Commands;                use Commands;
with Gdk.Event;               use Gdk.Event;
with Gtk.Label;               use Gtk.Label;
with Glib.Convert;            use Glib.Convert;
with GNAT.Strings;            use GNAT.Strings;
with GNATCOLL.Traces;         use GNATCOLL.Traces;
with GNATCOLL.VFS;            use GNATCOLL.VFS;
with GPS.Intl;                use GPS.Intl;
with GPS.Kernel.MDI;          use GPS.Kernel.MDI;
with GPS.Kernel.Modules.UI;   use GPS.Kernel.Modules.UI;
with GPS.Kernel.Task_Manager; use GPS.Kernel.Task_Manager;
with Gtk.Box;                 use Gtk.Box;
with Gtk.Widget;              use Gtk.Widget;
with Gtk.Size_Group;          use Gtk.Size_Group;
with Pango.Layout;            use Pango.Layout;
with String_Utils;            use String_Utils;
with Learn;                   use Learn;

package body GPS.Kernel.Actions is
   Me : constant Trace_Handle := Create ("GPS.KERNEL.ACTIONS");

   use Actions_Htable.String_Hash_Table;

   Future            : constant String := ASCII.LF &
     (-("Future references to this action will execute the last"
        & " definition encountered"));
   Overrides_Builtin : constant String :=
     -"Action overrides a builtin action" & ASCII.LF;

   procedure Launch_Foreground_Command
     (Kernel          : not null access Kernel_Handle_Record'Class;
      Command         : not null access Root_Command'Class);
   --  Executes a command, blocking the whole GPS interface while doing so.
   --  It is recommended instead to use Launch_Background_Command, but this one
   --  is sometimes used in user's python scripts.

   ----------------------------
   -- Actions Learn Provider --
   ----------------------------

   type Actions_Learn_Provider_Type is new Learn_Provider_Type with record
      Kernel : Kernel_Handle;
   end record;

   overriding function Get_Name
     (Self : not null access Actions_Learn_Provider_Type) return String
   is
     ("Actions");

   Provider : access Actions_Learn_Provider_Type;

   Actions_Size_Group : Gtk_Size_Group;

   ------------------------
   -- Actions Learn Item --
   ------------------------

   type Action_Learn_Item_Type is new Learn_Item_Type with record
      Action_Name : Unbounded_String;
   end record;

   overriding function Get_ID
     (Self : not null access Action_Learn_Item_Type) return String
   is
     (To_String (Self.Action_Name));

   overriding function Get_Widget
     (Self : not null access Action_Learn_Item_Type) return Gtk_Widget;

   overriding function Get_Help
     (Self : not null access Action_Learn_Item_Type) return String;
   --  Return the associated action's help

   overriding function Is_Visible
     (Self        : not null access Action_Learn_Item_Type;
      Context     : Selection_Context;
      Filter_Text : String) return Boolean;
   --  Return True if the associated action is visible in the given context

   overriding procedure On_Double_Click
     (Self    : not null access Action_Learn_Item_Type;
      Context : Selection_Context);
   --  Execute the action associated with the given learn item when the user
   --  double-clicks on it.

   ----------------
   -- Get_Widget --
   ----------------

   overriding function Get_Widget
     (Self : not null access Action_Learn_Item_Type) return Gtk_Widget
   is
      Action_Name    : constant String := To_String (Self.Action_Name);
      Action         : constant Action_Record_Access := Lookup_Action
        (Kernel => Provider.Kernel,
         Name   => Action_Name);
      Action_Hbox    : Gtk_Hbox;
      Name_Label     : Gtk_Label;
      Shortcut_Label : Gtk_Label;
   begin
      if Action = null then
         return null;
      end if;

      Gtk_New_Hbox (Action_Hbox, Homogeneous => False);

      Gtk_New (Name_Label,  Get_Label (Action));
      Action_Hbox.Pack_Start (Name_Label, Expand => False);

      if Actions_Size_Group = null then
         Gtk_New (Actions_Size_Group);
      end if;

      Actions_Size_Group.Add_Widget (Name_Label);
      Name_Label.Set_Alignment (0.0, 0.5);
      Name_Label.Set_Ellipsize (Ellipsize_Middle);

      Gtk_New
        (Shortcut_Label,
         Provider.Kernel.Get_Shortcut
           (Action          => Action_Name,
            Use_Markup      => True,
            Return_Multiple => True));
      Shortcut_Label.Set_Use_Markup (True);
      Action_Hbox.Pack_Start
        (Shortcut_Label,
         Expand => True,
         Fill   => False);

      return Gtk_Widget (Action_Hbox);
   end Get_Widget;

   --------------
   -- Get_Help --
   --------------

   overriding function Get_Help
     (Self : not null access Action_Learn_Item_Type) return String
   is
      Action : constant Action_Record_Access := Lookup_Action
        (Provider.Kernel, To_String (Self.Action_Name));
   begin
      if Action = null then
         return "";
      else
         return Get_Full_Description (Action);
      end if;
   end Get_Help;

   ----------------
   -- Is_Visible --
   ----------------

   overriding function Is_Visible
        (Self        : not null access Action_Learn_Item_Type;
         Context     : Selection_Context;
         Filter_Text : String) return Boolean
   is
      pragma Unreferenced (Filter_Text);
      Action : constant Action_Record_Access := Lookup_Action
        (Provider.Kernel, To_String (Self.Action_Name));
   begin
      return Filter_Matches (Action, Context);
   end Is_Visible;

   ---------------------
   -- On_Double_Click --
   ---------------------

   overriding procedure On_Double_Click
     (Self    : not null access Action_Learn_Item_Type;
      Context : Selection_Context)
   is
      Success : Boolean with Unreferenced;
   begin
      Success := Execute_Action
        (Provider.Kernel,
         Action  => To_String (Self.Action_Name),
         Context => Context);
   end On_Double_Click;

   ----------
   -- Free --
   ----------

   procedure Free (Action : in out Action_Record_Access) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Action_Record, Action_Record_Access);
   begin
      --  In the past, we did not free the command explictly, since menus might
      --  have referenced directly, But since now they also keep the name of
      --  the action, it is safe to free the command

      Commands.Unref (Command_Access (Action.Command));

      --  Do not free the Action.Filter, which will be taken care of when the
      --  kernel itself is destroyed. This means that filters always have a
      --  lifespan equal to that of GPS

      Free (Action.Category);
      Free (Action.Description);
      Free (Action.Name);
      Free (Action.Icon_Name);
      Unchecked_Free (Action);
   end Free;

   ---------------------
   -- Register_Action --
   ---------------------

   procedure Register_Action
     (Kernel       : access Kernel_Handle_Record'Class;
      Name         : String;
      Command      : access Commands.Interactive.Interactive_Command'Class;
      Description  : String := "";
      Filter       : Action_Filter := null;
      Category     : String := "General";
      Icon_Name    : String := "";
      For_Learning : Boolean := False;
      Shortcut_Active_For_View : Ada.Tags.Tag := Ada.Tags.No_Tag)
   is
      Old            : constant Action_Record_Access :=
        Lookup_Action (Kernel, Name);
      Overridden      : Boolean := False;
      Cat            : GNAT.Strings.String_Access;
      Action         : Action_Record_Access;
      Stock          : GNAT.Strings.String_Access;
      Cmd            : access Interactive_Command'Class;
      Status_Changed : Boolean := False;
   begin
      --  Initialize the kernel actions table.
      if Kernel.Actions = null then
         Kernel.Actions := new Actions_Htable_Record;
      end if;

      if Old /= null then
         if Name (Name'First) = '/' then
            --  This is a menu: do not display a message about the overriding,
            --  since it is legitimate to want to recreate or redefine a menu,
            --  for instance in the Build module.

            null;
         else
            Insert (Kernel, '"' & Name & """: " & Overrides_Builtin
                    & Future, Mode => Error);
            Status_Changed := True;
         end if;

         Overridden := True;
      end if;

      if Category /= "" then
         Cat := new String'(Category);
      end if;

      if Icon_Name /= "" then
         Stock := new String'(Icon_Name);
      end if;

      --  Handle memory management for the filter

      if Filter /= null then
         Register_Filter (Kernel, Filter, Name => "");
      end if;

      if Command /= null then
         --  ??? The use of Unrestricted_Access is ugly, but it allows nicer
         --  user code :
         --   * users can extend the Interactive_Command type in package bodies
         --   * and still call Register_Action ( new My_Command);
         --     without using a temporary variable to store the allocated
         --     command.
         Cmd := Command.all'Unrestricted_Access;
      end if;

      --  Create the action

      Action := new Action_Record'
        (Cmd,
         Filter,
         new String'(Description),
         Name                         => new String'(Name),
         Modified                     => False,
         Category                     => Cat,
         Overridden                   => Overridden,
         Disabled                     => False,
         Icon_Name                    => Stock,
         Shortcut_Active_For_View     => Shortcut_Active_For_View);

      Set (Actions_Htable_Access (Kernel.Actions).Table,
           To_Lower (Name), Action);

      if Status_Changed then
         Action_Status_Changed (Kernel, Name);
      end if;

      if For_Learning then
         declare
            Item : constant access Action_Learn_Item_Type :=
              new Action_Learn_Item_Type;
         begin
            Item.Action_Name := To_Unbounded_String (Name);
            Initialize (Item, Group_Name => Category);
            Provider.Add_Item
              (Item        => Item,
               ID          => Name);
         end;
      end if;
   end Register_Action;

   -----------------------
   -- Unregister_Action --
   -----------------------

   procedure Unregister_Action
     (Kernel       : access Kernel_Handle_Record'Class;
      Name         : String;
      Remove_Menus_And_Toolbars : Boolean := True)
   is
      A : Action_Record_Access;
   begin
      loop
         A := Get
           (Actions_Htable_Access (Kernel.Actions).Table, To_Lower (Name));
         exit when A = null;

         Remove (Actions_Htable_Access (Kernel.Actions).Table,
                 To_Lower (Name));
      end loop;

      if Remove_Menus_And_Toolbars then
         Remove_UI_For_Action (Kernel, Name);
      end if;

      Provider.Delete_Item (Name);

      --  Unregister the gtk+ action, too.
      Kernel.Get_Application.Remove_Action (Name);
   end Unregister_Action;

   -----------
   -- Start --
   -----------

   function Start (Kernel : access Kernel_Handle_Record'Class)
      return Action_Iterator
   is
      Iter : Action_Iterator;
   begin
      Get_First (Actions_Htable_Access (Kernel.Actions).Table, Iter.Iterator);
      return Iter;
   end Start;

   ----------
   -- Next --
   ----------

   procedure Next
     (Kernel : access Kernel_Handle_Record'Class;
      Iter   : in out Action_Iterator) is
   begin
      Get_Next (Actions_Htable_Access (Kernel.Actions).Table, Iter.Iterator);
   end Next;

   ---------
   -- Get --
   ---------

   function Get (Iter : Action_Iterator) return String is
   begin
      return Get_Key (Iter.Iterator);
   end Get;

   ---------
   -- Get --
   ---------

   function Get (Iter : Action_Iterator) return Action_Record_Access is
   begin
      return Get_Element (Iter.Iterator);
   end Get;

   -------------------
   -- Lookup_Action --
   -------------------

   function Lookup_Action
     (Kernel : access Kernel_Handle_Record'Class;
      Name   : String) return Action_Record_Access
   is
      Action  : Action_Record_Access;
   begin
      if Kernel.Actions = null then
         return null;
      else
         Action := Get (Actions_Htable_Access (Kernel.Actions).Table, Name);

         if Action = null
           or else Action.Disabled
           or else Action.Command = null   --  Not sure when this happens ?
         then
            return null;
         else
            return Action;
         end if;

      end if;
   end Lookup_Action;

   -------------------------
   -- Set_Action_Disabled --
   -------------------------

   procedure Set_Action_Disabled
     (Kernel   : not null access Kernel_Handle_Record'Class;
      Name     : String;
      Disabled : Boolean)
   is
      Action  : Action_Record_Access;
   begin
      if Kernel.Actions /= null then
         Action := Get (Actions_Htable_Access (Kernel.Actions).Table, Name);
         if Action /= null then
            Action.Disabled := Disabled;
            Action_Status_Changed (Kernel, Name);
         end if;
      end if;
   end Set_Action_Disabled;

   -----------
   -- Reset --
   -----------

   overriding procedure Reset (X : access Actions_Htable_Record) is
   begin
      --  Free the actions and their filters
      Reset (X.Table);
   end Reset;

   --------------------
   -- Filter_Matches --
   --------------------

   function Filter_Matches
     (Self    : access Action_Record;
      Context : Selection_Context) return Boolean
   is
   begin
      return Self /= null
        and then not Self.Disabled
        and then
          (Context = No_Context or else Filter_Matches (Self.Filter, Context));
   end Filter_Matches;

   -----------
   -- "and" --
   -----------

   function "and"
     (Action : access Action_Record;
      Filter : access Action_Filter_Record'Class)
      return access Action_Filter_Record'Class
   is
   begin
      if Action = null then
         return Filter;
      elsif Filter = null then
         return Action.Filter;
      else
         return Action.Filter and Filter;
      end if;
   end "and";

   ----------------------
   -- Get_Filter_Error --
   ----------------------

   function Get_Filter_Error
     (Self : access Action_Record) return Unbounded_String
   is
   begin
      return Get_Error_Message (Self.Filter);
   end Get_Filter_Error;

   -----------------
   -- Get_Command --
   -----------------

   function Get_Command
     (Self    : not null access Action_Record)
      return not null access Interactive_Command'Class is
   begin
      return Self.Command;
   end Get_Command;

   -------------------------------
   -- Launch_Foreground_Command --
   -------------------------------

   procedure Launch_Foreground_Command
     (Kernel          : not null access Kernel_Handle_Record'Class;
      Command         : not null access Root_Command'Class)
   is
      Result  : Command_Return_Type;
      C : Command_Access;
   begin
      loop
         begin
            --  ??? Not elegant. We should refactor commands so that Execute
            --  takes a context as parameter, and Interactive_Context is just
            --  a child of Context (MB20-044)

            if Command.all in Interactive_Command'Class then
               --  We want to make sure that the context provides access to
               --  the kernel.
               Result := Interactive_Command_Access (Command).Execute
                 (Create_Null_Context (New_Context (Kernel => Kernel)));
            else
               Result := Command.Execute;
            end if;

         exception
            when E : others =>
               Trace (Me, E);
               Result := Failure;
         end;

         exit when Result = Success or Result = Failure;
      end loop;

      C := Command_Access (Command);
      Unref (C);
   end Launch_Foreground_Command;

   --------------------
   -- Execute_Action --
   --------------------

   function Execute_Action
     (Kernel      : not null access Kernel_Handle_Record'Class;
      Action      : String;
      Context     : Selection_Context := No_Context;
      Event       : Gdk.Event.Gdk_Event := null;
      Repeat      : Positive := 1;
      Args        : access String_List := null;
      Synchronous : Boolean := False;
      Show_Bar    : Boolean := False;
      Via_Menu    : Boolean := False;
      Block_Exit  : Boolean := False;
      Error_Msg_In_Console : Boolean := True) return Boolean
   is
      Child : GPS_MDI_Child;
      --  The child that currently has the focus

      procedure Undo_Group (Start : Boolean);
      --  Start or end an undo group

      ----------------
      -- Undo_Group --
      ----------------

      procedure Undo_Group (Start : Boolean) is
         C : MDI_Child;
      begin
         if Start then
            if Repeat >= 2 then
               C := Get_Focus_Child (Get_MDI (Kernel));
               if C /= null
                  and then C.all in GPS_MDI_Child_Record'Class
               then
                  Child := GPS_MDI_Child (C);
               end if;

               if Child /= null then
                  Start_Group (Get_Command_Queue (Child));
               end if;
            end if;

         elsif Child /= null then
            End_Group (Get_Command_Queue (Child));
         end if;
      end Undo_Group;

      Act : constant Action_Record_Access := Lookup_Action (Kernel, Action);
      C : Selection_Context := Context;
      Custom : Command_Access;
      Args_In_Out : String_List_Access := String_List_Access (Args);
   begin
      if Act = null then
         Free (Args_In_Out);
         if Action (Action'First) = '/' then
            declare
               B : constant Block_Trace_Handle :=
                  Create (Me, "Execute menu action " & Action);
            begin
               GPS.Kernel.Modules.UI.Execute_Menu
                  (Kernel_Handle (Kernel), Action);
            end;
            return True;
         else
            Insert (Kernel, -"Action not defined : " & Action);
            return False;
         end if;
      end if;

      if Context = No_Context then
         C := Get_Current_Context (Kernel);
      end if;

      if Filter_Matches (Act, C) then
         declare
            B : constant Block_Trace_Handle :=
               Create (Me, "Execute action " & Action & Repeat'Img
                  & " times synchronous=" & Synchronous'Img);
         begin
            --  For background commands, we do not use undo groups, since there
            --  might be several such commands running in parallel anyway.
            if Synchronous then
               Undo_Group (Start => True);
            end if;

            for R in 1 .. Repeat loop
               Custom := Create_Proxy
                  (Act.Command,
                   (Event       => Event,
                    Context     => C,
                    Synchronous => Synchronous,
                    Dir         => No_File,
                    Args        => Args_In_Out,
                    Via_Menu    =>
                      Via_Menu or else
                      (Event /= null and then
                        (Get_Event_Type (Event) = Button_Press or else
                         Get_Event_Type (Event) = Button_Release or else
                         Get_Event_Type (Event) = Key_Press or else
                         Get_Event_Type (Event) = Key_Release)),
                    Label       => new String'(Action),
                   Repeat_Count => R,
                    Remaining_Repeat => Repeat - R));

               if Synchronous then
                  Launch_Foreground_Command (Kernel, Custom);
               else
                  Launch_Background_Command
                     (Kernel,
                      Custom,
                      Block_Exit      => Block_Exit,
                      Active          => True,  --  immediately if possible
                      Show_Bar        => Show_Bar,
                      Queue_Id        => "");
               end if;
            end loop;

            if Synchronous then
               Undo_Group (Start => False);
            end if;
         end;

         return True;

      else
         declare
            M : constant Unbounded_String := Get_Filter_Error (Act);
            Msg : constant String :=
               "Could not execute """ & Action & """"
               & (if M = "" then "" else ": " & To_String (M));
         begin
            Trace (Me, Msg);
            if Error_Msg_In_Console then
               Insert (Kernel, Msg);
            end if;
         end;

         Free (Args_In_Out);
         return False;
      end if;
   end Execute_Action;

   -------------------
   -- Get_Icon_Name --
   -------------------

   function Get_Icon_Name (Self : access Action_Record) return String is
   begin
      if Self.Icon_Name = null then
         return "";
      else
         return Self.Icon_Name.all;
      end if;
   end Get_Icon_Name;

   ----------------------------
   -- Is_Key_Shortcut_Active --
   ----------------------------

   function Is_Key_Shortcut_Active
     (Self  : access Action_Record;
      Child : access MDI_Child_Record'Class;
      Key   : Gdk_Key_Type;
      Modif : Gdk_Modifier_Type)
      return Boolean
   is
      pragma Unreferenced (Key);
      use Ada.Tags;
   begin
      if Self = null then
         --  The action was not created for this Key: a warning will be
         --  displayed when trying to execute the action.
         return True;
      end if;

      --  If no child if focused, if there is no key modifier, or if the key
      --  shortcut is active from all the views, return True.

      if Modif /= 0
        or else Self.Shortcut_Active_For_View = Ada.Tags.No_Tag
      then
         return True;
      end if;

      --  Otherwise, check if the type of the view being focused is within
      --  the Shortcut_Active_For_View tag's hierarchy.

      declare
         Child_Tag : constant Ada.Tags.Tag := (if Child /= null then
                                                  Child.all'Tag
                                               else
                                                  Ada.Tags.No_Tag);
         Parent_Tag : Ada.Tags.Tag;
      begin
         if Child_Tag = Ada.Tags.No_Tag then
            return False;
         end if;

         Parent_Tag := Ada.Tags.Parent_Tag (Child_Tag);

         while Parent_Tag /= Ada.Tags.No_Tag loop
            if Parent_Tag = Self.Shortcut_Active_For_View then
               return True;
            end if;

            Parent_Tag := Ada.Tags.Parent_Tag (Parent_Tag);
         end loop;
      end;

      return False;
   end Is_Key_Shortcut_Active;

   --------------------------
   -- Get_Full_Description --
   --------------------------

   function Get_Full_Description
     (Action           : not null access Action_Record;
      Kernel           : access Kernel_Handle_Record'Class := null;
      Use_Markup       : Boolean := True;
      Include_Name     : Boolean := True;
      Include_Category : Boolean := True;
      Include_Menus    : Boolean := True)
     return String
   is
      use Ada.Strings.Unbounded;

      function Tag (T : String) return String is
        (if Use_Markup then "<b>" & T & "</b>" else T);
      --  Utility function, to highlight a bit of text

      function Escape (T : String) return String is
         (if Use_Markup then Escape_Text (T) else T);
      --  Utility function, to escape a bit of text

      Shortcut : constant String :=
        (if Kernel = null
         then ""
         else Kernel.Get_Shortcut
           (Action          => Action.Name.all,
            Use_Markup      => Use_Markup,
            Return_Multiple => True));
      Menus : Unbounded_String :=
        (if Include_Menus then Menu_List_For_Action (Action.Name.all)
         else Null_Unbounded_String);

   begin
      if Menus /= Null_Unbounded_String then
         Menus := Tag ("Menu: ") & To_Unbounded_String
           (Escape (To_String (Menus)));
      end if;

      return
        Escape (Action.Description.all)
        & ASCII.LF & ASCII.LF
        & (if Include_Name then Tag ("Action: ")
           & Escape (Action.Name.all) & ASCII.LF else "")
        & (if Include_Category then Tag ("Category: ")
           & Escape
             ((if Action.Category = null then "" else Action.Category.all))
           & ASCII.LF
           else "")
        & (if Shortcut = "" then ""
           else Tag ("Shortcut: ") & Shortcut & ASCII.LF)
        & (To_String (Menus));
   end Get_Full_Description;

   ------------------
   -- Get_Category --
   ------------------

   function Get_Category
     (Action : not null access Action_Record) return String
   is
   begin
      if Action.Category = null then
         return "";
      else
         return Action.Category.all;
      end if;
   end Get_Category;

   ----------------
   -- Has_Filter --
   ----------------

   function Has_Filter
      (Self : not null access Action_Record) return Boolean is
   begin
      return Self.Filter /= null;
   end Has_Filter;

   --------------
   -- Get_Name --
   --------------

   function Get_Name (Self : not null access Action_Record) return String is
   begin
      return Self.Name.all;
   end Get_Name;

   ---------------
   -- Get_Label --
   ---------------

   function Get_Label (Self : not null access Action_Record) return String is
      Name : constant String := Get_Name (Self);
   begin
      if Name'Length > 1 then
         return To_Upper (Name (Name'First))
           & To_Lower (Name (Name'First + 1 .. Name'Last));
      else
         return To_Upper (Name);
      end if;
   end Get_Label;

   -------------------------------------
   -- Register_Actions_Learn_Provider --
   -------------------------------------

   procedure Register_Actions_Learn_Provider
     (Kernel : not null access Kernel_Handle_Record'Class) is
   begin
      Provider := new Actions_Learn_Provider_Type'
        (Learn_Provider_Type with
           Kernel => Kernel_Handle (Kernel));
      Learn.Register_Provider (Provider);
   end Register_Actions_Learn_Provider;

end GPS.Kernel.Actions;
