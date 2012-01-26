------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                     Copyright (C) 2004-2012, AdaCore                     --
--                                                                          --
--  This library is free software;  you can redistribute it and/or modify   --
--  it under terms of the  GNU General Public License  as published by the  --
--  Free Software  Foundation;  either version 3,  or (at your  option) any --
--  later version. This library is distributed in the hope that it will be  --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                    --
--                                                                          --
--  As a special exception under Section 7 of GPL version 3, you are        --
--  granted additional permissions described in the GCC Runtime Library     --
--  Exception, version 3.1, as published by the Free Software Foundation.   --
--                                                                          --
--  You should have received a copy of the GNU General Public License and   --
--  a copy of the GCC Runtime Library Exception along with this program;    --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see   --
--  <http://www.gnu.org/licenses/>.                                         --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

with Ada.Text_IO;

separate (Templates_Parser)

package body Definitions is

   -----------
   -- Clone --
   -----------

   function Clone (D : Tree) return Tree is
   begin
      return new Def'(D.all);
   end Clone;

   -----------
   -- Parse --
   -----------

   function Parse (Line : String) return Tree is
      --  Format to parse: <name> = <ref|value>['|'<value>]
      K, L  : Natural;
      Name  : Unbounded_String;
      Value : Unbounded_String;
      Ref   : Natural;
   begin
      K := Strings.Fixed.Index (Line, "=");

      if K = 0 then
         raise Internal_Error
           with "SET wrong definition, missing name or value";
      end if;

      Name := To_Unbounded_String
        (Fixed.Trim (Line (Line'First .. K - 1), Both));

      --  Check if we have a single value

      declare
         Data : constant String :=
                  Fixed.Trim (Line (K + 1 .. Line'Last), Both);
      begin
         L := Fixed.Index (Data, "|");

         if L = 0 then
            --  Single data, this can be a ref or a value
            if Data (Data'First) = '$' then
               Ref := Positive'Value (Data (Data'First + 1 .. Data'Last));
               return new Def'
                 (Name, (Definitions.Ref, Null_Unbounded_String, Ref));

            else
               Value := To_Unbounded_String
                 (No_Quote (Fixed.Trim (Data (L + 1 .. Data'Last), Left)));
               return new Def'(Name, (Definitions.Const, Value, 1));
            end if;

         else
            --  Multiple data, the first one must be a ref, the second a value
            if Data (Data'First) /= '$' then
               raise Internal_Error
                  with "SET, reference expected found a value";
            end if;

            Ref   := Positive'Value (Data (Data'First + 1 .. L - 1));
            Value := To_Unbounded_String
              (No_Quote (Fixed.Trim (Data (L + 1 .. Data'Last), Left)));
            return new Def'
              (Name, (Definitions.Ref_Default, Value, Ref));
         end if;
      end;
   end Parse;

   ----------------
   -- Print_Tree --
   ----------------

   procedure Print_Tree (D : Tree) is
      N : constant Node := D.N;
   begin
      Text_IO.Put (To_String (D.Name) & " = ");

      case D.N.Kind is
         when Const       =>
            Text_IO.Put (Quote (To_String (N.Value)));
         when Ref         =>
            Text_IO.Put ('$' & Utils.Image (N.Ref));
         when Ref_Default =>
            Text_IO.Put
              ('$' & Utils.Image (N.Ref)
               & " | " & Quote (To_String (N.Value)));
      end case;
   end Print_Tree;

   -------------
   -- Release --
   -------------

   procedure Release (D : in out Tree) is
      procedure Unchecked_Free is new Unchecked_Deallocation (Def, Tree);
   begin
      Unchecked_Free (D);
   end Release;

end Definitions;
