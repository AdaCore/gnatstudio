-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                   Copyright (C) 2003 - 2005                       --
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

with Ada.Unchecked_Deallocation;

with GNAT.OS_Lib;  use GNAT.OS_Lib;

with Glib.Xml_Int; use Glib.Xml_Int;
with Gdk.Event;    use Gdk.Event;
with Gtk.Label;    use Gtk.Label;
with GPS.Intl;     use GPS.Intl;

package body Commands.Interactive is

   type Internal_Component_Iterator is new Component_Iterator_Record with
      record
         At_End : Boolean := False;
      end record;

   function Get
     (Iter : access Internal_Component_Iterator) return Command_Component;
   procedure Next (Iter : access Internal_Component_Iterator);
   --  See docs from inherited subprograms

   type Internal_Component_Record is new Command_Component_Record
      with null record;

   function Component_Editor
     (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Component : access Internal_Component_Record)
      return Gtk.Widget.Gtk_Widget;
   procedure Update_From_Editor
     (Component : access Internal_Component_Record;
      Editor    : access Gtk.Widget.Gtk_Widget_Record'Class);
   function Get_Name
     (Component : access Internal_Component_Record) return String;
   procedure To_XML
     (Component   : access Internal_Component_Record;
      Action_Node : Glib.Xml_Int.Node_Ptr);
   --  See docs for inherited subprograms

   Internal_Component : aliased Internal_Component_Record;
   --  Used for all internal components, ie that can't be edited graphically

   ----------------
   -- On_Failure --
   ----------------

   function On_Failure
     (Iter : access Component_Iterator_Record) return Component_Iterator
   is
      pragma Unreferenced (Iter);
   begin
      return null;
   end On_Failure;

   ----------
   -- Free --
   ----------

   procedure Free (Iter : in out Component_Iterator_Record) is
      pragma Unreferenced (Iter);
   begin
      null;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (Iter : in out Component_Iterator) is
      procedure Unchecked_Free is new Ada.Unchecked_Deallocation
        (Component_Iterator_Record'Class, Component_Iterator);
   begin
      Free (Iter.all);
      Unchecked_Free (Iter);
   end Free;

   ----------------------
   -- Component_Editor --
   ----------------------

   function Component_Editor
     (Kernel    : access GPS.Kernel.Kernel_Handle_Record'Class;
      Component : access Internal_Component_Record)
      return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Kernel, Component);
      Label : Gtk_Label;
   begin
      Gtk_New (Label, -"Built-in action");
      return Gtk.Widget.Gtk_Widget (Label);
   end Component_Editor;

   ------------------------
   -- Update_From_Editor --
   ------------------------

   procedure Update_From_Editor
     (Component : access Internal_Component_Record;
      Editor    : access Gtk.Widget.Gtk_Widget_Record'Class)
   is
      pragma Unreferenced (Component, Editor);
   begin
      null;
   end Update_From_Editor;

   -----------
   -- Start --
   -----------

   function Start
     (Command : access Interactive_Command) return Component_Iterator
   is
      pragma Unreferenced (Command);
   begin
      return new Internal_Component_Iterator'
        (Component_Iterator_Record with At_End => False);
   end Start;

   ---------
   -- Get --
   ---------

   function Get
     (Iter : access Internal_Component_Iterator) return Command_Component is
   begin
      if Iter.At_End then
         return null;
      else
         return Internal_Component'Access;
      end if;
   end Get;

   ----------
   -- Next --
   ----------

   procedure Next (Iter : access Internal_Component_Iterator) is
   begin
      Iter.At_End := True;
   end Next;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Interactive_Command) return Command_Return_Type is
   begin
      return Execute (Interactive_Command_Access (Command), Null_Context);
   end Execute;

   ------------------------------------
   -- Launch_Synchronous_Interactive --
   ------------------------------------

   procedure Launch_Synchronous_Interactive
     (Command : access Interactive_Command'Class;
      Context : Interactive_Command_Context;
      Wait    : Duration := 0.0)
   is
      function Execute_Command
        (Command : Command_Access) return Command_Return_Type;

      ---------------------
      -- Execute_Command --
      ---------------------

      function Execute_Command
        (Command : Command_Access) return Command_Return_Type is
      begin
         return Execute
           (Interactive_Command_Access (Command), Context);
      end Execute_Command;

      procedure Internal is new Launch_Synchronous_Generic (Execute_Command);

   begin
      Internal (Command_Access (Command), Wait);
   end Launch_Synchronous_Interactive;

   ------------------
   -- Create_Proxy --
   ------------------

   function Create_Proxy
     (Command : access Interactive_Command'Class;
      Context : Interactive_Command_Context) return Command_Access
   is
      C : constant Interactive_Command_Proxy_Access :=
            new Interactive_Command_Proxy;
   begin
      C.Command := Interactive_Command_Access (Command);
      C.Context := Context;
      return Command_Access (C);
   end Create_Proxy;

   -------------
   -- Execute --
   -------------

   function Execute
     (Command : access Interactive_Command_Proxy) return Command_Return_Type
   is
   begin
      return Execute (Command.Command, Command.Context);
   end Execute;

   ----------
   -- Name --
   ----------

   function Name (Command : access Interactive_Command_Proxy) return String is
   begin
      if Command.Context.Label = null then
         return -"Interactive command";
      else
         return Command.Context.Label.all;
      end if;
   end Name;

   ----------
   -- Free --
   ----------

   procedure Free (X : in out Interactive_Command_Proxy) is
   begin
      Free (X.Context);
   end Free;

   procedure Free (X : in out Interactive_Command_Context) is
   begin
      GPS.Kernel.Unref (X.Context);

      if X.Args /= null then
         for J in X.Args'Range loop
            Free (X.Args (J));
         end loop;

         Free (X.Args);
      end if;

      Free (X.Dir);
      Free (X.Label);
   end Free;

   --------------------
   -- Command_Editor --
   --------------------

   function Command_Editor
     (Command : access Interactive_Command) return Gtk.Widget.Gtk_Widget
   is
      pragma Unreferenced (Command);
   begin
      return null;
   end Command_Editor;

   ------------------------
   -- Update_From_Editor --
   ------------------------

   procedure Update_From_Editor
     (Command : access Interactive_Command;
      Editor  : Gtk.Widget.Gtk_Widget)
   is
      pragma Unreferenced (Command, Editor);
   begin
      null;
   end Update_From_Editor;

   --------------
   -- Get_Name --
   --------------

   function Get_Name
     (Component : access Internal_Component_Record) return String
   is
      pragma Unreferenced (Component);
   begin
      return -"Built-in command";
   end Get_Name;

   ------------
   -- To_XML --
   ------------

   procedure To_XML
     (Component   : access Internal_Component_Record;
      Action_Node : Glib.Xml_Int.Node_Ptr)
   is
      pragma Unreferenced (Component);
      C : constant Node_Ptr := new Node;
   begin
      C.Tag := new String'("builtin");
      Add_Child (Action_Node, C);
   end To_XML;

   ------------
   -- To_XML --
   ------------

   procedure To_XML
     (Command     : access Interactive_Command;
      Action_Node : Glib.Xml_Int.Node_Ptr)
   is
      pragma Unreferenced (Command);
   begin
      Set_Attribute (Action_Node, "built-in", "true");
   end To_XML;

   --------------
   -- Progress --
   --------------

   function Progress
     (Command : access Interactive_Command_Proxy) return Progress_Record is
   begin
      return Progress (Command.Command);
   end Progress;

   ------------------
   -- Set_Progress --
   ------------------

   procedure Set_Progress
     (Command  : access Interactive_Command_Proxy;
      Progress : Progress_Record) is
   begin
      Set_Progress (Command.Command, Progress);
   end Set_Progress;

   ----------
   -- Undo --
   ----------

   function Undo (Command : access Interactive_Command_Proxy) return Boolean is
   begin
      return Undo (Command.Command);
   end Undo;

   ---------------
   -- Interrupt --
   ---------------

   procedure Interrupt (Command : in out Interactive_Command_Proxy) is
   begin
      Interrupt (Command.Command.all);
   end Interrupt;

end Commands.Interactive;
