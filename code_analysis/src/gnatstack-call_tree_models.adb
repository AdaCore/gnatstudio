------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2010-2018, AdaCore                     --
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

with Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with System.Address_To_Access_Conversions;

with Glib.Object;
with Gtk.Tree_Model;         use Gtk.Tree_Model;
with Gtk.Tree_Model.Utils;

package body GNATStack.Call_Tree_Models is

   use type Glib.Gint;
   use type Gtk.Tree_Model.Gtk_Tree_Iter;

   procedure Populate (Node : not null Node_Access);
   --  Populates children nodes of the tree.

   function Create_Iter
     (Self : not null access Call_Tree_Model_Record'Class;
      Node : not null Node_Access) return Gtk.Tree_Model.Gtk_Tree_Iter;
   --  Creates iterator for the specified node.

   function Node_At
     (Self : not null access Call_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return not null Node_Access;
   --  Returns node at the spcified position.

   procedure On_Destroy
     (Ada_Object : System.Address;
      C_Object   : System.Address);
   pragma Convention (C, On_Destroy);
   --  Release memory.

   package Node_Conversions is
     new System.Address_To_Access_Conversions (Node_Record);

   package Call_Tree_Model_Conversions is
     new System.Address_To_Access_Conversions (Call_Tree_Model_Record'Class);

   --------------
   -- Children --
   --------------

   overriding function Children
     (Self   : access Call_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Node : constant Node_Access := Node_At (Self, Parent);

   begin
      if not Node.Populated then
         Populate (Node);
      end if;

      if not Node.Children.Is_Empty then
         return Create_Iter (Self, Node.Children.First_Element);

      else
         return Gtk.Tree_Model.Null_Iter;
      end if;
   end Children;

   -----------------
   -- Create_Iter --
   -----------------

   function Create_Iter
     (Self : not null access Call_Tree_Model_Record'Class;
      Node : not null Node_Access) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      pragma Assert (Node /= Self.Root'Access);

   begin
      return
        Gtk.Tree_Model.Utils.Init_Tree_Iter
          (Self.Stamp,
           Node_Conversions.To_Address
             (Node_Conversions.Object_Pointer (Node)));
   end Create_Iter;

   ---------------------
   -- Get_Column_Type --
   ---------------------

   overriding function Get_Column_Type
     (Self  : access Call_Tree_Model_Record;
      Index : Glib.Gint) return Glib.GType
   is
      pragma Unreferenced (Self);

   begin
      case Index is
         when 0 =>
            return Glib.GType_String;

         when others =>
            raise Program_Error;
      end case;
   end Get_Column_Type;

   --------------
   -- Get_Iter --
   --------------

   overriding function Get_Iter
     (Self : access Call_Tree_Model_Record;
      Path : Gtk.Tree_Model.Gtk_Tree_Path) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Indices : constant Glib.Gint_Array := Gtk.Tree_Model.Get_Indices (Path);
      Node    : Node_Access := Self.Root'Access;
      Index   : Positive;

   begin
      for J in Indices'Range loop
         Index := Positive (Indices (J) + 1);

         if not Node.Populated then
            Populate (Node);
         end if;

         if Index > Natural (Node.Children.Length) then
            return Gtk.Tree_Model.Null_Iter;
         end if;

         Node := Node.Children.Element (Index);
      end loop;

      return Create_Iter (Self, Node);
   end Get_Iter;

   -------------------
   -- Get_N_Columns --
   -------------------

   overriding function Get_N_Columns
     (Self : access Call_Tree_Model_Record) return Glib.Gint
   is
      pragma Unreferenced (Self);

   begin
      return 1;
   end Get_N_Columns;

   --------------
   -- Get_Path --
   --------------

   overriding function Get_Path
     (Self : access Call_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Path
   is
      Path : Gtk.Tree_Model.Gtk_Tree_Path;
      Node : Node_Access := Node_At (Self, Iter);

   begin
      if Iter = Gtk.Tree_Model.Null_Iter then
         return Null_Gtk_Tree_Path;
      end if;

      Gtk.Tree_Model.Gtk_New (Path);

      while Node /= Self.Root'Access loop
         Gtk.Tree_Model.Prepend_Index
           (Path, Glib.Gint (Node.Parent.Children.Find_Index (Node) - 1));

         Node := Node.Parent;
      end loop;

      return Path;
   end Get_Path;

   ---------------
   -- Get_Value --
   ---------------

   overriding procedure Get_Value
     (Self   : access Call_Tree_Model_Record;
      Iter   : Gtk.Tree_Model.Gtk_Tree_Iter;
      Column : Glib.Gint;
      Value  : out Glib.Values.GValue)
   is
      Node : constant Node_Access := Node_At (Self, Iter);

   begin
      case Column is
         when 0 =>
            Glib.Values.Init (Value, Glib.GType_String);
            Glib.Values.Set_String
              (Value,
               Ada.Strings.Unbounded.To_String
                 (Node.Subprogram.Identifier.Prefix_Name));

         when others =>
            raise Program_Error;
      end case;
   end Get_Value;

   -------------
   -- Gtk_New --
   -------------

   procedure Gtk_New
     (Item       : out Call_Tree_Model;
      Subprogram : not null GNATStack.Data_Model.Subprogram_Information_Access)
   is
   begin
      Item := new Call_Tree_Model_Record;
      Initialize (Item, Subprogram);
   end Gtk_New;

   ---------------
   -- Has_Child --
   ---------------

   overriding function Has_Child
     (Self : access Call_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return Boolean
   is
      Node : constant Node_Access := Node_At (Self, Iter);

   begin
      if not Node.Populated then
         Populate (Node);
      end if;

      return not Node.Children.Is_Empty;
   end Has_Child;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Self       : not null access Call_Tree_Model_Record'Class;
      Subprogram : not null GNATStack.Data_Model.Subprogram_Information_Access)
   is
      Child : Node_Access;

   begin
      Gtkada.Abstract_Tree_Model.Initialize (Self);

      Child :=
        new Node_Record'
          (Parent     => Self.Root'Access,
           Subprogram => Subprogram,
           others     => <>);
      Self.Root.Children.Append (Child);

      Glib.Object.Weak_Ref
        (Self,
         On_Destroy'Access,
         Call_Tree_Model_Conversions.To_Address
           (Call_Tree_Model_Conversions.Object_Pointer (Self)));
   end Initialize;

   ----------------
   -- N_Children --
   ----------------

   overriding function N_Children
     (Self : access Call_Tree_Model_Record;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter := Gtk.Tree_Model.Null_Iter)
      return Glib.Gint
   is
      Node : constant Node_Access := Node_At (Self, Iter);

   begin
      if not Node.Populated then
         Populate (Node);
      end if;

      return Glib.Gint (Node.Children.Length);
   end N_Children;

   ----------
   -- Next --
   ----------

   overriding procedure Next
     (Self : access Call_Tree_Model_Record;
      Iter : in out Gtk.Tree_Model.Gtk_Tree_Iter)
   is
      Node     : constant Node_Access := Node_At (Self, Iter);
      Position : Node_Vectors.Cursor := Node.Parent.Children.Find (Node);

   begin
      if Iter = Gtk.Tree_Model.Null_Iter then
         null;

      else
         Node_Vectors.Next (Position);

         if Node_Vectors.Has_Element (Position) then
            Iter := Create_Iter (Self, Node_Vectors.Element (Position));

         else
            Iter := Gtk.Tree_Model.Null_Iter;
         end if;
      end if;
   end Next;

   -------------
   -- Node_At --
   -------------

   function Node_At
     (Self : not null access Call_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter) return not null Node_Access is
   begin
      if Iter = Gtk.Tree_Model.Null_Iter then
         return Self.Root'Access;

      else
         if Gtk.Tree_Model.Utils.Get_Stamp (Iter) /= Self.Stamp then
            raise Program_Error;
         end if;

         return
           Node_Access
             (Node_Conversions.To_Pointer
                  (Gtk.Tree_Model.Utils.Get_User_Data_1 (Iter)));
      end if;
   end Node_At;

   ---------------
   -- Nth_Child --
   ---------------

   overriding function Nth_Child
     (Self   : access Call_Tree_Model_Record;
      Parent : Gtk.Tree_Model.Gtk_Tree_Iter;
      N      : Glib.Gint) return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Node  : constant Node_Access := Node_At (Self, Parent);
      Index : constant Positive := Positive (N + 1);

   begin
      if not Node.Populated then
         Populate (Node);
      end if;

      if Index > Natural (Node.Children.Length) then
         return Gtk.Tree_Model.Null_Iter;

      else
         return Create_Iter (Self, Node.Children.Element (Index));
      end if;
   end Nth_Child;

   ----------------
   -- On_Destroy --
   ----------------

   procedure On_Destroy
     (Ada_Object : System.Address;
      C_Object   : System.Address)
   is
      pragma Unreferenced (C_Object);

      procedure Deallocate_Children (Node : not null Node_Access);
      --  Deallocates children nodes.

      -------------------------
      -- Deallocate_Children --
      -------------------------

      procedure Deallocate_Children (Node : not null Node_Access) is

         procedure Free is
           new Ada.Unchecked_Deallocation (Node_Record, Node_Access);

      begin
         for J in 1 .. Natural (Node.Children.Length) loop
            declare
               Child : Node_Access := Node.Children.Element (J);

            begin
               Deallocate_Children (Child);
               Free (Child);
            end;
         end loop;
      end Deallocate_Children;

      Self : constant not null Call_Tree_Model :=
        Call_Tree_Model (Call_Tree_Model_Conversions.To_Pointer (Ada_Object));

   begin
      Deallocate_Children (Self.Root'Access);
   end On_Destroy;

   ------------
   -- Parent --
   ------------

   overriding function Parent
     (Self  : access Call_Tree_Model_Record;
      Child : Gtk.Tree_Model.Gtk_Tree_Iter)
      return Gtk.Tree_Model.Gtk_Tree_Iter
   is
      Node : constant Node_Access := Node_At (Self, Child);

   begin
      if Child = Gtk.Tree_Model.Null_Iter then
         return Gtk.Tree_Model.Null_Iter;

      else
         return Create_Iter (Self, Node.Parent);
      end if;
   end Parent;

   --------------
   -- Populate --
   --------------

   procedure Populate (Node : not null Node_Access) is

      procedure Create_Node
        (Position : GNATStack.Data_Model.Subprogram_Information_Sets.Cursor);
      --  Creates node for the specified subprogram.

      -----------------
      -- Create_Node --
      -----------------

      procedure Create_Node
        (Position : GNATStack.Data_Model.Subprogram_Information_Sets.Cursor)
      is
         use GNATStack.Data_Model.Subprogram_Information_Sets;

         Child : constant Node_Access :=
           new Node_Record'
             (Parent     => Node,
              Subprogram => Element (Position),
              others     => <>);

      begin
         Node.Children.Append (Child);
      end Create_Node;

   begin
      Node.Subprogram.Calls.Iterate (Create_Node'Access);
      Node.Populated := True;
   end Populate;

   -------------------
   -- Subprogram_At --
   -------------------

   function Subprogram_At
     (Self : not null access Call_Tree_Model_Record'Class;
      Iter : Gtk.Tree_Model.Gtk_Tree_Iter)
      return GNATStack.Data_Model.Subprogram_Information_Access is
   begin
      return Node_At (Self, Iter).Subprogram;
   end Subprogram_At;

end GNATStack.Call_Tree_Models;
