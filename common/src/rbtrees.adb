-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                      Copyright (C) 2003-2005                      --
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
-- a copy of the GNU General Public License along with this library; --
-- if not,  write to the  Free Software Foundation, Inc.,  59 Temple --
-- Place - Suite 330, Boston, MA 02111-1307, USA.                    --
-----------------------------------------------------------------------

with Ada.Unchecked_Deallocation;

package body Rbtrees is

   type Red_Black is (Red, Black);

   type Node is record
      Color : Red_Black;
      Value : Item;
      Parent, Left, Right : Rbtree_Iterator;

      Size : Natural;
      --  size in the number of nodes below, and including Node.  Not needed
      --  except for the subprograms Index, Get_Nth and Length
   end record;

   procedure Unchecked_Free is new Ada.Unchecked_Deallocation
     (Node, Rbtree_Iterator);

   procedure Binary_Tree_Insert (Tree : in out Rbtree; N : Rbtree_Iterator);
   pragma Inline (Binary_Tree_Insert);
   --  The standard binary tree handling. This function doesn't preserve the
   --  properties of the red-black tree

   procedure Left_Rotate (Tree : in out Rbtree; X : Rbtree_Iterator);
   pragma Inline (Left_Rotate);
   --  Operate a left-rotation on a tree node
   --      X                   Y
   --     / \                 / \
   --    A   Y        =>     X   C
   --       / \             / \
   --      B   C           A   B

   procedure Right_Rotate (Tree : in out Rbtree; X : Rbtree_Iterator);
   pragma Inline (Right_Rotate);
   --  The opposite of Left_Rotate

   procedure Delete_Fixup (Tree : in out Rbtree; N : Rbtree_Iterator);
   pragma Inline (Delete_Fixup);
   --  Make sure the red-black tree rules are still valid after a removal

   ------------------------
   -- Binary_Tree_Insert --
   ------------------------

   procedure Binary_Tree_Insert (Tree : in out Rbtree; N : Rbtree_Iterator) is
   begin
      if Tree.Root = null then
         Tree.Root := N;
      else
         declare
            X : Rbtree_Iterator := Tree.Root;
         begin
            loop
               X.Size := X.Size + 1;
               if N.Value < X.Value then
                  if X.Left = null then
                     N.Parent := X;
                     X.Left := N;
                     return;
                  else
                     X := X.Left;
                  end if;
               else
                  if X.Right = null then
                     N.Parent := X;
                     X.Right := N;
                     return;
                  else
                     X := X.Right;
                  end if;
               end if;
            end loop;
         end;
      end if;
   end Binary_Tree_Insert;

   -----------------
   -- Left_Rotate --
   -----------------

   procedure Left_Rotate (Tree : in out Rbtree; X : Rbtree_Iterator) is
      Y : constant Rbtree_Iterator := X.Right;
   begin
      Y.Size := X.Size;

      X.Right := Y.Left;
      if Y.Left /= null then
         Y.Left.Parent := X;
         X.Size := 1 + X.Right.Size;
      else
         X.Size := 1;
      end if;

      Y.Parent := X.Parent;

      if X.Parent = null then
         Tree.Root := Y;
      elsif X = X.Parent.Left then
         X.Parent.Left := Y;
      else
         X.Parent.Right := Y;
      end if;

      Y.Left   := X;
      X.Parent := Y;

      if X.Left /= null then
         X.Size := X.Size + X.Left.Size;
      end if;
   end Left_Rotate;

   ------------------
   -- Right_Rotate --
   ------------------

   procedure Right_Rotate (Tree : in out Rbtree; X : Rbtree_Iterator) is
      Y : constant Rbtree_Iterator := X.Left;
   begin
      Y.Size := X.Size;

      X.Left := Y.Right;
      if Y.Right /= null then
         Y.Right.Parent := X;
         X.Size := 1 + X.Left.Size;
      else
         X.Size := 1;
      end if;

      Y.Parent := X.Parent;

      if X.Parent = null then
         Tree.Root := Y;
      elsif X = X.Parent.Right then
         X.Parent.Right := Y;
      else
         X.Parent.Left := Y;
      end if;

      Y.Right  := X;
      X.Parent := Y;

      if X.Right /= null then
         X.Size := X.Size + X.Right.Size;
      end if;
   end Right_Rotate;

   ------------
   -- Insert --
   ------------

   procedure Insert (Tree : in out Rbtree; Value : Item) is
      N : Rbtree_Iterator := new Node'
        (Color  => Red,
         Value  => Value,
         Size   => 1,
         Parent => null,
         Left   => null,
         Right  => null);
      Y : Rbtree_Iterator;
   begin
      Binary_Tree_Insert (Tree, N);

      while N /= Tree.Root and then N.Parent.Color = Red loop
         if N.Parent = N.Parent.Parent.Left then
            Y := N.Parent.Parent.Right;
            if Y /= null and then Y.Color = Red then
               N.Parent.Color := Black;
               Y.Color        := Black;
               N              := N.Parent.Parent;
               N.Color        := Red;

            else
               if N = N.Parent.Right then
                  N := N.Parent;
                  Left_Rotate (Tree, N);
               end if;
               N.Parent.Color        := Black;
               N.Parent.Parent.Color := Red;
               Right_Rotate (Tree, N.Parent.Parent);
            end if;

         else
            Y := N.Parent.Parent.Left;
            if Y /= null and then Y.Color = Red then
               N.Parent.Color := Black;
               Y.Color        := Black;
               N              := N.Parent.Parent;
               N.Color        := Red;

            else
               if N = N.Parent.Left then
                  N := N.Parent;
                  Right_Rotate (Tree, N);
               end if;
               N.Parent.Color        := Black;
               N.Parent.Parent.Color := Red;
               Left_Rotate (Tree, N.Parent.Parent);
            end if;
         end if;
      end loop;

      Tree.Root.Color := Black;
   end Insert;

   -------------
   -- Minimum --
   -------------

   function Minimum (Tree : Rbtree) return Rbtree_Iterator is
      Y : Rbtree_Iterator := Tree.Root;
   begin
      if Y /= null then
         while Y.Left /= null loop
            Y := Y.Left;
         end loop;
      end if;
      return Y;
   end Minimum;

   -------------
   -- Maximum --
   -------------

   function Maximum (Tree : Rbtree) return Rbtree_Iterator is
      Y : Rbtree_Iterator := Tree.Root;
   begin
      if Y /= null then
         while Y.Right /= null loop
            Y := Y.Right;
         end loop;
      end if;
      return Y;
   end Maximum;

   ------------
   -- Remove --
   ------------

   procedure Remove (Tree : in out Rbtree; Iter : Rbtree_Iterator) is
      Z : constant Rbtree_Iterator := Iter;
      Y, X : Rbtree_Iterator;
      Nil : aliased Node;
   begin
      Y := Iter.Parent;
      while Y /= null loop
         begin
            Y.Size := Y.Size - 1;
         end;
         Y := Y.Parent;
      end loop;

      if Z.Left = null or else Z.Right = null then
         Y := Z;
      else
         Y := Next (Z);
      end if;

      if Y.Left /= null then
         X := Y.Left;
      else
         X := Y.Right;
      end if;

      if Y.Parent = null then
         Tree.Root := X;
      elsif Y = Y.Parent.Left then
         Y.Parent.Left := X;
      else
         Y.Parent.Right := X;
      end if;

      if X = null then
         Nil.Color := Black;
         Nil.Parent := Y.Parent;
         X := Nil'Unchecked_Access;
      else
         X.Parent := Y.Parent;
      end if;

      if Y /= Z then
         Z.Value := Y.Value;
      end if;

      if Tree.Root /= null and then Y.Color = Black then
         Delete_Fixup (Tree, X);
      end if;

      Free (Y.Value);
      Unchecked_Free (Y);
   end Remove;

   ------------------
   -- Delete_Fixup --
   ------------------

   procedure Delete_Fixup (Tree : in out Rbtree; N : Rbtree_Iterator) is
      X : Rbtree_Iterator := N;
      W : Rbtree_Iterator;
   begin
      while X /= Tree.Root
        and then (X = null or else X.Color = Black)
      loop
         --  First case

         if X = null or else X = X.Parent.Left then
            W := X.Parent.Right;

            if W /= null and then W.Color = Red then
               W.Color := Black;
               X.Parent.Color := Red;
               Left_Rotate (Tree, X.Parent);
               W := X.Parent.Right;
            end if;

            if W = null then
               X := X.Parent;

            elsif (W.Left = null or else W.Left.Color = Black)
              and then (W.Right = null or else W.Right.Color = Black)
            then
               W.Color := Red;
               X := X.Parent;

            else
               if W.Right = null or else W.Right.Color = Black then
                  if W.Left /= null then
                     W.Left.Color := Black;
                  end if;

                  W.Color := Red;
                  Right_Rotate (Tree, W);
                  W := X.Parent.Right;
               end if;

               W.Color := X.Parent.Color;
               X.Parent.Color := Black;
               if W.Right /= null then
                  W.Right.Color := Black;
               end if;
               Left_Rotate (Tree, X.Parent);
               X := Tree.Root;
            end if;

            --  Second case

         else
            W := X.Parent.Left;

            if W /= null and then W.Color = Red then
               W.Color := Black;
               X.Parent.Color := Red;
               Right_Rotate (Tree, X.Parent);
               W := X.Parent.Left;
            end if;

            if W = null then
               X := X.Parent;

            elsif (W.Right = null or else W.Right.Color = Black)
              and then (W.Left = null or else W.Left.Color = Black)
            then
               W.Color := Red;
               X := X.Parent;
            else
               if W.Left = null or else W.Left.Color = Black then
                  if W.Right /= null then
                     W.Right.Color := Black;
                  end if;

                  W.Color := Red;
                  Left_Rotate (Tree, W);
                  W := X.Parent.Left;
               end if;

               W.Color := X.Parent.Color;
               X.Parent.Color := Black;
               if W.Left /= null then
                  W.Left.Color := Black;
               end if;
               Right_Rotate (Tree, X.Parent);
               X := Tree.Root;
            end if;
         end if;
      end loop;

      X.Color := Black;
   end Delete_Fixup;

   ----------
   -- Next --
   ----------

   function Next (Iter : Rbtree_Iterator) return Rbtree_Iterator is
   begin
      if Iter.Right /= null then
         declare
            X : Rbtree_Iterator := Iter.Right;
         begin
            while X.Left /= null loop
               X := X.Left;
            end loop;
            return X;
         end;

      else
         declare
            Y : Rbtree_Iterator := Iter.Parent;
            X : Rbtree_Iterator := Iter;
         begin
            while Y /= null and then X = Y.Right loop
               X := Y;
               Y := Y.Parent;
            end loop;
            return Y;
         end;
      end if;
   end Next;

   --------------
   -- Previous --
   --------------

   function Previous (Iter : Rbtree_Iterator) return Rbtree_Iterator is
   begin
      if Iter.Left /= null then
         declare
            X : Rbtree_Iterator := Iter.Left;
         begin
            while X.Right /= null loop
               X := X.Right;
            end loop;
            return X;
         end;

      else
         declare
            Y : Rbtree_Iterator := Iter.Parent;
            X : Rbtree_Iterator := Iter;
         begin
            while Y /= null and then X = Y.Left loop
               X := Y;
               Y := Y.Parent;
            end loop;
            return Y;
         end;
      end if;
   end Previous;

   ---------
   -- Get --
   ---------

   function Get (Iter : Rbtree_Iterator) return Item is
   begin
      return Iter.Value;
   end Get;

   ----------
   -- Find --
   ----------

   function Find (Tree : Rbtree; Value : Item) return Rbtree_Iterator is
      X : Rbtree_Iterator := Tree.Root;
   begin
      while X /= null loop
         if X.Value < Value then
            X := X.Right;
         elsif Value < X.Value then
            X := X.Left;
         else
            return X;
         end if;
      end loop;
      return null;
   end Find;

   -----------
   -- Index --
   -----------

   function Index (Iter : Rbtree_Iterator) return Natural is
      Result : Natural := 1;
      Y : Rbtree_Iterator := Iter;
   begin
      if Iter.Left /= null then
         Result := Result + Iter.Left.Size;
      end if;

      while Y.Parent /= null loop
         if Y = Y.Parent.Right then
            if Y.Parent.Left /= null then
               Result := Result + Y.Parent.Left.Size + 1;
            else
               Result := Result + 1;
            end if;
         end if;
         Y := Y.Parent;
      end loop;

      return Result;
   end Index;

   ------------
   -- Length --
   ------------

   function Length (Tree : Rbtree) return Natural is
   begin
      if Tree.Root = null then
         return 0;
      else
         return Tree.Root.Size;
      end if;
   end Length;

   -------------
   -- Get_Nth --
   -------------

   function Get_Nth (Tree : Rbtree; Nth : Positive)
      return Rbtree_Iterator
   is
      X : Rbtree_Iterator := Tree.Root;
      R : Natural;
      N : Natural := Nth;
   begin
      if Nth > Length (Tree) then
         return null;
      end if;

      loop
         if X.Left = null then
            R := 1;
         else
            R := X.Left.Size + 1;
         end if;

         exit when R = N;

         if N < R then
            X := X.Left;
         else
            N := N - R;
            X := X.Right;
         end if;
      end loop;

      return X;
   end Get_Nth;

   -----------
   -- Clear --
   -----------

   procedure Clear (Tree : in out Rbtree) is
      X : Rbtree_Iterator := Tree.Root;
   begin
      while X /= null loop
         if X.Left /= null then
            X := X.Left;
         elsif X.Right /= null then
            X := X.Right;
         else
            Free (X.Value);
            X := X.Parent;

            if X = null then
               Unchecked_Free (Tree.Root);
            elsif X.Left /= null then
               Unchecked_Free (X.Left);
            else
               Unchecked_Free (X.Right);
            end if;
         end if;
      end loop;
   end Clear;

   --------------
   -- Is_Empty --
   --------------

   function Is_Empty (Tree : Rbtree) return Boolean is
   begin
      return Tree.Root = null;
   end Is_Empty;
end Rbtrees;

