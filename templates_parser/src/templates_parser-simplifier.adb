------------------------------------------------------------------------------
--                             Templates Parser                             --
--                                                                          --
--                       Copyright (C) 2010, AdaCore                        --
--                                                                          --
--  This library is free software; you can redistribute it and/or modify    --
--  it under the terms of the GNU General Public License as published by    --
--  the Free Software Foundation; either version 2 of the License, or (at   --
--  your option) any later version.                                         --
--                                                                          --
--  This library is distributed in the hope that it will be useful, but     --
--  WITHOUT ANY WARRANTY; without even the implied warranty of              --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU       --
--  General Public License for more details.                                --
--                                                                          --
--  You should have received a copy of the GNU General Public License       --
--  along with this library; if not, write to the Free Software Foundation, --
--  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.          --
--                                                                          --
--  As a special exception, if other files instantiate generics from this   --
--  unit, or you link this unit with other files to produce an executable,  --
--  this  unit  does not  by itself cause  the resulting executable to be   --
--  covered by the GNU General Public License. This exception does not      --
--  however invalidate any other reasons why the executable file  might be  --
--  covered by the  GNU Public License.                                     --
------------------------------------------------------------------------------

separate (Templates_Parser)

package body Simplifier is

   procedure Run (T : in out Tree) is

      procedure Rewrite (T : in out Data.Tree);
      --  Optimize data tree T

      procedure Link_End (T, To : Tree; Link : out Tree);
      --  Link To at end of tree T, Link is the pointer to the last node

      --------------
      -- Link_End --
      --------------

      procedure Link_End (T, To : Tree; Link : out Tree) is
         N : Tree := T;
      begin
         while N.Next /= null loop
            N := N.Next;
         end loop;
         N.Next := To;
         Link := N;
      end Link_End;

      -------------
      -- Rewrite --
      -------------

      procedure Rewrite (T : in out Data.Tree) is
         use type Data.Tree;
         use type Data.NKind;
         use type Nkind;
         D, Prev : Data.Tree;
         Moved   : Boolean := False;
         Old     : Data.Tree;
      begin
         D := T;

         while D /= null loop
            case D.Kind is
               when Data.Text =>
                  if Prev = null
                    and then T.Kind = Data.Text
                    and then D /= T
                  then
                     Append (T.Value, D.Value);

                     Old := T.Next;
                     T.Next := D.Next;
                     D := D.Next;
                     Moved := True;

                     Data.Release (Old, Single => True);

                  elsif Prev /= null and then Prev.Kind = Data.Text then
                     Append (Prev.Value, D.Value);

                     Old := Prev.Next;
                     Prev.Next := D.Next;
                     D := D.Next;
                     Moved := True;

                     Data.Release (Old, Single => True);
                  end if;

               when Data.Var =>
                  --  Rewrite also the macro if any

                  if D.Var.Is_Macro then
                     Run (D.Var.Def);

                     --  Check if we have a single resulting TEXT node

                     if D.Var.Def /= null
                       and then D.Var.Def.Kind = Text
                       and then D.Var.Def.Text.Kind = Data.Text
                       and then D.Var.Def.Text.Next = null
                       and then D.Var.Def.Next = null
                     then
                        declare
                           C : aliased Filter.Filter_Context (P_Size => 0);
                        begin
                           if Prev = null then
                              --  First node is a variable (line starting with
                              --  a tag variable), replace it by the
                              --  corresponding text node.
                              Old := T;
                              T := new Data.Node'
                                (Kind  => Data.Text,
                                 Next  => D.Var.Def.Text.Next,
                                 Value => To_Unbounded_String
                                   (Data.Translate
                                      (D.Var,
                                       To_String (D.Var.Def.Text.Value),
                                       C'Access)));

                              D := D.Next;
                              Moved := True;

                              Data.Release (Old, Single => True);

                           elsif Prev.Kind = Data.Text then
                              --  First node was a text, merge the values
                              Append (Prev.Value, D.Var.Def.Text.Value);

                              Old := Prev.Next;
                              Prev.Next := D.Next;
                              D := D.Next;
                              Moved := True;

                              Data.Release (Old, Single => True);
                           end if;
                        end;
                     end if;
                  end if;
            end case;

            if not Moved then
               Prev := D;
               D := D.Next;
            else
               Moved := False;
            end if;
         end loop;
      end Rewrite;

      N       : Tree := T;
      Prev, P : Tree;
      Moved   : Boolean := False;

   begin
      T := N;

      while N /= null loop
         case N.Kind is
            when Text =>
               Rewrite (N.Text);

            when Table_Stmt =>
               Run (N.Blocks);

            when Section_Block =>
               Run (N.Common);
               Run (N.Sections);

            when Section_Stmt =>
               Run (N.N_Section);

            when If_Stmt =>
               declare
                  V : constant String := Expr.Analyze (N.Cond);
               begin
                  if V = Expr.Unknown then
                     Run (N.N_True);
                     Run (N.N_False);

                  elsif (Expr.Is_True (V) and then N.N_True = null)
                    or else (not Expr.Is_True (V) and then N.N_False = null)
                  then
                     --  The corresponding branch does not exists, skip IF
                     Expr.Release (N.Cond);
                     Release (N.N_True);
                     Release (N.N_False);

                     declare
                        Old : Tree;
                     begin
                        if Prev = null then
                           Old := T;
                           T := T.Next;
                        else
                           Old := Prev.Next;
                           Prev.Next := N.Next;
                        end if;

                        N := N.Next;
                        Free (Old);
                     end;

                     Moved := True;

                  elsif Expr.Is_True (V) then
                     Expr.Release (N.Cond);
                     Release (N.N_False);

                     Run (N.N_True);

                     if Prev = null then
                        Link_End (N.N_True, T.Next, Link => P);
                        Free (T);
                        T := N.N_True;
                     else
                        Link_End (N.N_True, N.Next, Link => P);
                        Free (Prev.Next);
                        Prev.Next := N.N_True;
                     end if;

                     Prev := P;
                     N := N.Next;
                     Moved := True;

                  else
                     Expr.Release (N.Cond);
                     Release (N.N_True);

                     Run (N.N_False);

                     if Prev = null then
                        Link_End (N.N_False, T.Next, Link => P);
                        Free (T);
                        T := N.N_False;
                     else
                        Link_End (N.N_False, N.Next, Link => P);
                        Free (Prev.Next);
                        Prev.Next := N.N_False;
                     end if;

                     Prev := P;
                     N := N.Next;
                     Moved := True;
                  end if;
               end;

            when others =>
               null;
         end case;

         if Moved then
            Moved := False;
         else
            Prev := N;
            N := N.Next;
         end if;
      end loop;
   end Run;

end Simplifier;
