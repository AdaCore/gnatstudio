-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2007-2008, AdaCore                 --
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

with Ada.Strings.Unbounded;      use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
with GNAT.Regpat;                use GNAT.Regpat;

with Docgen2_Backend;            use Docgen2_Backend;
with Docgen2.Scripts;            use Docgen2.Scripts;
with Docgen2.Utils;              use Docgen2.Utils;
with String_Utils;               use String_Utils;

package body Docgen2.Tags is

--     procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Ptr);

   XML_Regpat : constant Pattern_Matcher :=
                  Compile (" *<([/]?) *([^ </>]+) *([^<>]*)>", Single_Line);

   function To_Unbounded_String
     (Docgen      : Docgen_Object;
      N           : Node_Ptr;
      Entity_Name : String;
      Href        : String) return Unbounded_String;

   function Strip_Blanks
     (S : Unbounded_String;
      Keep_Formatting : Boolean) return String;

   procedure Parse_Buffer
     (S    : String;
      N    : in out Node_Ptr;
      Opts : Docgen_Options);

   procedure Free_Node (N : in out Node_Ptr);
   --  Free a xml node tree

   ------------------
   -- Strip_Blanks --
   ------------------

   function Strip_Blanks
     (S : Unbounded_String;
      Keep_Formatting : Boolean) return String
   is
      Str : constant String := To_String (S);
      Res : Unbounded_String;
      Idx : Natural;
      Nxt : Natural;

   begin
      Idx := Str'First;
      Skip_Blanks (Str, Idx);

      if Idx > Str'Last then
         return "";
      end if;

      loop
         Nxt := Line_End (Str, Idx);

         if not Keep_Formatting
           and then Is_Blank_Line (Str (Idx .. Nxt), Idx)
         then
            Res := Res & ASCII.LF;

         else
            if not Keep_Formatting then
               Skip_Blanks (Str, Idx);
            end if;

            Res := Res & Str (Idx .. Nxt);
         end if;

         if Keep_Formatting then
            Res := Res & ASCII.LF;

         elsif not Is_Blank (Str (Nxt))
           and then Next_Line (Str, Idx) /= Str'Last
         then
            Res := Res & ' ';
         end if;

         Idx := Next_Line (Str, Idx);
         exit when Idx = Str'Last;
      end loop;

      return To_String (Res);
   end Strip_Blanks;

   -------------------------
   -- To_Unbounded_String --
   -------------------------

   function To_Unbounded_String
     (Docgen      : Docgen_Object;
      N           : Node_Ptr;
      Entity_Name : String;
      Href        : String) return Unbounded_String
   is
      Str     : Ada.Strings.Unbounded.Unbounded_String;
      Node    : Node_Ptr := N;
      Backend : Docgen2_Backend.Backend_Handle renames Get_Backend (Docgen);

   begin
      if Node = null then
         return Null_Unbounded_String;
      end if;

      if Node.Tag = Null_Unbounded_String
        and then Node.Children = null
      then
         return To_Unbounded_String
           (Backend.Filter (Strip_Blanks (Node.Value, True)));
      end if;

      while Node /= null loop
         declare
            Val_Str      : constant String :=
                             Backend.Filter
                               (Strip_Blanks
                                  (Node.Value,
                                   Node.Tag = Null_Unbounded_String));
            Val_U_Str    : Unbounded_String;
            Children_Str : constant Unbounded_String :=
                             To_Unbounded_String
                               (Docgen, Node.Children, Entity_Name, Href);
            Idx          : Natural;
            Nxt          : Natural;
         begin

            if Node.Tag /= Null_Unbounded_String then
               if Next_Line (Val_Str, Val_Str'First) < Val_Str'Last then
                  Idx := Val_Str'First;
                  Val_U_Str := Null_Unbounded_String;

                  while Idx < Val_Str'Last loop
                     Nxt := Line_End (Val_Str, Idx);
                     Append
                       (Val_U_Str,
                        Backend.Gen_Paragraph (Val_Str (Idx .. Nxt)));
                     Idx := Next_Line (Val_Str, Idx);
                  end loop;

               else
                  Val_U_Str := To_Unbounded_String (Val_Str);
               end if;

               declare
                  Res : constant String :=
                          On_Custom_Tag
                            (Docgen,
                             To_String (Node.Tag),
                             To_String (Node.Attributes),
                             To_String (Val_U_Str),
                             Entity_Name,
                             Href);
               begin
                  if Res'Length > 0 then
                     --  The way for a script not to print anything is to
                     --  return a single space character. In this case, do
                     --  not append it to the result.
                     if Res'Length > 1 or else Res /= " " then
                        Append (Str, Res);
                     end if;
                  else
                     Append
                       (Str,
                        Backend.Gen_User_Tag
                          (To_String (Node.Tag),
                           To_String (Node.Attributes),
                           To_String (Val_U_Str)));
                  end if;
               end;
            end if;

            if Length (Children_Str) > 0 then
               Str := Str & Children_Str;
            end if;

            --  For unidentified tags, we generate the extra text after
            --  children text, if any
            if Node.Tag = Null_Unbounded_String then
               Idx := Val_Str'First;
               while Idx < Val_Str'Last loop
                  Nxt := Line_End (Val_Str, Idx);
                  Str := Str & Backend.Gen_Paragraph (Val_Str (Idx .. Nxt));
                  Idx := Next_Line (Val_Str, Idx);
               end loop;
            end if;
         end;

         Node := Node.Next;
      end loop;

      return Str;
   end To_Unbounded_String;

   ---------------
   -- To_String --
   ---------------

   function To_String (Comment : Comment_Type) return String
   is
   begin
      return To_String (Comment.Block);
   end To_String;

   ------------------
   -- Parse_Buffer --
   ------------------

   procedure Parse_Buffer
     (S    : String;
      N    : in out Node_Ptr;
      Opts : Docgen_Options)
   is
      Matches     : Match_Array (0 .. 3);
      Tag         : Unbounded_String;
      New_Node    : Node;
      Closing_Tag : Boolean;
      Stand_Alone : Boolean;
   begin
      Match (XML_Regpat, S, Matches);

      --  No xml case
      if Matches (0) = No_Match then
         --  Regular string. Let's append it to the current node value
         N.Value := N.Value & S;

         return;
      end if;

      --  Append characters to the last opened tag.
      if Matches (0).First > S'First then
         N.Value := N.Value & S (S'First .. Matches (0).First - 1);
      end if;

      Tag := To_Unbounded_String (S (Matches (2).First .. Matches (2).Last));

      --  Treat closing tags.
      if S (Matches (1).First .. Matches (1).Last) = "/" then
         Closing_Tag := True;
      else
         Closing_Tag := False;
      end if;

      --  If we found an unexpected xml tag, then treat it like raw text
      if not Is_Custom_Tag (To_String (Tag))
        or else (Closing_Tag and then Tag /= N.Tag)
      then
         N.Value := N.Value & S (Matches (0).First .. Matches (0).Last);

      --  Treat closing tags.
      elsif Closing_Tag then
         N := N.Parent;

      --  Opening tag
      else
         New_Node :=
           (Tag        => Tag,
            Value      => Null_Unbounded_String,
            Attributes => Null_Unbounded_String,
            Next       => null,
            Children   => null,
            Parent     => N);

         if N.Children = null then
            N.Children := new Node'(New_Node);
            N := N.Children;
         else
            N := N.Children;
            while N.Next /= null loop
               N := N.Next;
            end loop;

            N.Next := new Node'(New_Node);
            N := N.Next;
         end if;

         --  See if the tag finishes by '/>'
         Stand_Alone := False;

         if Matches (3).First >= Matches (3).Last
           and then S (Matches (3).Last) = '/'
         then
            Stand_Alone := True;
            Matches (3).Last := Matches (3).Last - 1;
         end if;

         --  Now initialize the attributes field
         if Matches (3).First <= Matches (3).Last then
            N.Attributes :=
              To_Unbounded_String (S (Matches (3).First .. Matches (3).Last));
         end if;

         --  Set N to parent if the tag is stand-alone
         if Stand_Alone then
            N := N.Parent;
         end if;
      end if;

      if Matches (0).Last < S'Last then
         Parse_Buffer (S (Matches (0).Last + 1 .. S'Last), N, Opts);
      end if;
   end Parse_Buffer;

   ----------------------
   -- Add_Comment_Line --
   ----------------------

   procedure Add_Comment_Line
     (Sloc_Start : Source_Location;
      Sloc_Stop  : Source_Location;
      Comment    : String;
      Force_New  : Boolean;
      List       : in out Comments_List.Vector)
   is
      Elem   : Comment_Access;
   begin
      if not Force_New
        and then not List.Is_Empty
        and then List.Last_Element.Sloc_Stop.Line + 1 >= Sloc_Start.Line
      then
         Elem := List.Last_Element;
         Elem.Block := Elem.Block & Comment;
         Elem.Sloc_Stop := Sloc_Stop;

      else
         Elem := new Comment_Type'
           (Block      => To_Unbounded_String (Comment),
            Sloc_Start => Sloc_Start,
            Sloc_Stop  => Sloc_Stop,
            Analysed   => False);
         List.Append (Elem);
      end if;
   end Add_Comment_Line;

   ----------------------
   -- Analyse_Comments --
   ----------------------

   procedure Analyse_Comment
     (Comment     : in out Comment_Type;
      Docgen      : Docgen_Object;
      File        : Source_File;
      Entity_Name : String;
      Href        : String)
   is
      N : Node_Ptr;
   begin
      if Comment.Analysed then
         return;
      end if;

      N := new Node'
        (Tag        => Null_Unbounded_String,
         Value      => Null_Unbounded_String,
         Attributes => Null_Unbounded_String,
         Next       => null,
         Children   => null,
         Parent     => null);

      --  Translate the block into an xml tree
      Parse_Buffer (To_String (Comment.Block), N, Get_Options (Docgen));

      while N.Parent /= null loop
         Warning
           (Get_Kernel (Docgen), File, Comment.Sloc_Stop,
            "Tag " & To_String (N.Tag) &
            " is not closed");
         N := N.Parent;
      end loop;

      --  And translate the xml tree back to a block, using appropriate
      --  user tag transformations
      Comment.Block := To_Unbounded_String
        (Docgen, N, Entity_Name, Href);
      Free_Node (N);
      --  Set comment.analyzed so that this transformation is not done twice.
      Comment.Analysed := True;
   end Analyse_Comment;

   ---------------
   -- Free_Node --
   ---------------

   procedure Free_Node (N : in out Node_Ptr) is
      procedure Internal is new Ada.Unchecked_Deallocation (Node, Node_Ptr);
      Child : Node_Ptr;
      Next  : Node_Ptr;
   begin
      Child := N.Children;

      while Child /= null loop
         Next := Child.Next;
         Free_Node (Child);
         Child := Next;
      end loop;

      Internal (N);
      N := null;
   end Free_Node;

   ----------
   -- Free --
   ----------

   procedure Free (Comment : in out Comment_Type) is
   begin
      Comment.Block := Null_Unbounded_String;
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (List : in out Comments_List.Vector) is
      procedure Internal is new Ada.Unchecked_Deallocation
        (Comment_Type, Comment_Access);

      Comment : Comment_Access;
   begin
      for J in reverse List.First_Index .. List.Last_Index loop
         Comment := List.Element (J);
         Free (Comment.all);
         Internal (Comment);
         List.Delete (J);
      end loop;
   end Free;

   --------------
   -- Find_Doc --
   --------------

   function Find_Doc
     (Sloc_Start : Source_Location;
      Sloc_End   : Source_Location;
      Comments   : Comments_List.Vector;
      File_Doc   : Boolean := False) return Comment_Type
   is
   begin
      for J in Comments.First_Index .. Comments.Last_Index loop
         if File_Doc
           and then Comments.Element (J).Sloc_Start.Line < Sloc_Start.Line
           and then
             (J = Comments.Last_Index
              or else Comments.Element (J + 1).Sloc_Start.Line >
                Sloc_Start.Line)
         then
            return Comments.Element (J).all;

         elsif Sloc_Start.Line - 1 = Comments.Element (J).Sloc_Stop.Line
           or else Sloc_End.Line + 1 = Comments.Element (J).Sloc_Start.Line
         then
            --  ??? Should not return twice a comment. Mark it as used ?
            return Comments.Element (J).all;
         end if;

         exit when Comments.Element (J).Sloc_Start > Sloc_End;
      end loop;

      return No_Comment;
   end Find_Doc;

   ------------
   -- Ignore --
   ------------

   function Ignore
     (Loc      : Source_Location;
      Comments : Comments_List.Vector) return Boolean
   is
--        Ignore_State : Boolean := False;
--        Elem         : User_Tag;
      pragma Unreferenced (Loc, Comments);

   begin
      return False;
--        for J in Context.User_Tags.First_Index ..
--          Context.User_Tags.Last_Index
--        loop
--           Elem := Context.User_Tags.Element (J);
--
--           if Location (Elem).Line > Loc.Line then
--              return Ignore_State;
--           end if;
--
--           if Kind (Elem) = Tag_Kind_Doc_Ignore then
--              Ignore_State := Is_Opening_Tag (Elem);
--           end if;
--        end loop;
--
--        return Ignore_State;
   end Ignore;

end Docgen2.Tags;
