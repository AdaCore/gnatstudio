-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2007-2010, AdaCore                 --
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
      Href        : String;
      Keep_Layout : Boolean := False) return Unbounded_String;

--     function Strip_Blanks (S : Unbounded_String) return String;

   procedure Parse_Buffer
     (S    : String;
      N    : in out Node_Ptr;
      Opts : Docgen_Options);

   procedure Free_Node (N : in out Node_Ptr);
   --  Free a xml node tree

   -------------------------
   -- To_Unbounded_String --
   -------------------------

   function To_Unbounded_String
     (Docgen      : Docgen_Object;
      N           : Node_Ptr;
      Entity_Name : String;
      Href        : String;
      Keep_Layout : Boolean := False) return Unbounded_String
   is
      Node    : Node_Ptr;
      Backend : Docgen2_Backend.Backend_Handle renames
                      Get_Backend (Docgen);

      function Is_Intended_LF (Str : String; Idx : Natural) return Boolean;
      --  True if the line starting at Idx seems intended (e.g. is a list item
      --  or a paragraph start.

      function Is_Intended_LF (Str : String; Idx : Natural) return Boolean is
      begin
         if Idx + 2 <= Str'Last
           and then (Str (Idx .. Idx + 1) = "- "
                     or else Str (Idx .. Idx + 1) = "* ")
         then
            --  List item
            return True;
         end if;

         if Str (Idx) in 'A' .. 'Z' then
            for J in reverse Str'First .. Idx - 1 loop
               if Str (J) = '.' then
                  --  A new sentence starting with a new-line: suppose this is
                  --  intentional
                  return True;

               elsif not Is_Blank (Str (J)) then
                  return False;

               end if;
            end loop;
         end if;

         return False;
      end Is_Intended_LF;

   begin
      if N = null then
         return Null_Unbounded_String;
      end if;

      if N.Kind = Text_Node then
         return To_Unbounded_String (Backend.Filter (To_String (N.Value)));

      elsif N.Kind = Root_Node then
         declare
            Ret : Unbounded_String;
         begin
            Node := N.Children;

            while Node /= null loop
               Append
                 (Ret, To_Unbounded_String (Docgen, Node, Entity_Name, Href));
               Node := Node.Next;
            end loop;

            return Ret;
         end;

      else
         declare
            C_Val  : Unbounded_String;
            Val    : Unbounded_String;
            Keep_L : Boolean := Keep_Layout;
         begin

            if To_String (N.Tag) = "code" then
               Keep_L := True;
            end if;

            Node := N.Children;

            while Node /= null loop
               Append
                 (C_Val,
                  To_Unbounded_String
                    (Docgen, Node, Entity_Name, Href, Keep_L));
               Node := Node.Next;
            end loop;

            Node := N;

            declare
               S       : constant String := To_String (C_Val);
               Idx     : Natural;
               P_First : Natural := S'First;

            begin
               Idx := S'First;

               while Idx < S'Last loop
                  if Is_Intended_LF (S, Idx) then
                     Append (Val,
                             Backend.Gen_Paragraph (S (P_First .. Idx - 1)));
                     P_First := Idx;
                  end if;

                  Idx := Next_Line (S, Idx);
               end loop;

               if P_First > S'First then
                  Append (Val, Backend.Gen_Paragraph (S (P_First .. S'Last)));
               else
                  Append (Val, S);
               end if;

               declare
                  Res : constant String :=
                          On_Custom_Tag
                            (Docgen,
                             To_String (Node.Tag),
                             To_String (Node.Attributes),
                             To_String (Val),
                             Entity_Name,
                             Href);
               begin
                  if Res'Length > 0 then
                     --  The way for a script not to print anything is to
                     --  return a single space character. In this case, do
                     --  not append it to the result.
                     if Res'Length > 1 or else Res /= " " then
                        return To_Unbounded_String (Res);
                     else
                        return Null_Unbounded_String;
                     end if;

                  else
                     return To_Unbounded_String
                       (Backend.Gen_User_Tag
                          (To_String (Node.Tag),
                           To_String (Node.Attributes),
                           To_String (Val)));
                  end if;
               end;
            end;
         end;
      end if;
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
      Matches      : Match_Array (0 .. 3);
      Tag          : Unbounded_String;
      New_Node     : Node_Ptr;
      Closing_Tag  : Boolean;
      Stand_Alone  : Boolean;

      procedure Append_Txt (S : String);
      --  Append text to N

      procedure Append_Txt (S : String) is
         New_Node : Node_Ptr;
      begin
         if N.Last_Child /= null and then N.Last_Child.Kind = Text_Node then
            Append (N.Last_Child.Value, S);

         else
            New_Node := new Node'
              (Kind       => Text_Node,
               Value      => To_Unbounded_String (S),
               Parent     => N,
               Children   => null,
               Last_Child => null,
               Next       => null);

            if N.Last_Child = null then
               N.Children := New_Node;
            else
               N.Last_Child.Next := New_Node;
            end if;

            N.Last_Child := New_Node;
         end if;
      end Append_Txt;

   begin
      Match (XML_Regpat, S, Matches);

      --  No xml case
      if Matches (0) = No_Match then
         --  Regular string. Let's append it to the current node value
         Append_Txt (S);

         return;
      end if;

      --  Append characters to the last opened tag.
      if Matches (0).First > S'First then
         Append_Txt (S (S'First .. Matches (0).First - 1));
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
        or else (Closing_Tag
                 and then (N.Kind /= Element_Node or else Tag /= N.Tag))
      then
         Append_Txt (S (Matches (0).First .. Matches (0).Last));

      --  Treat closing tags.
      elsif Closing_Tag then
         N := N.Parent;

      --  Opening tag
      else
         New_Node := new Node'
           (Kind       => Element_Node,
            Tag        => Tag,
            Attributes => Null_Unbounded_String,
            Next       => null,
            Children   => null,
            Last_Child => null,
            Parent     => N);

         if N.Children = null then
            N.Children := New_Node;
         else
            N.Last_Child.Next := New_Node;
         end if;

         N.Last_Child := New_Node;

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
            New_Node.Attributes :=
              To_Unbounded_String (S (Matches (3).First .. Matches (3).Last));
         end if;

         --  Set N to the newly created node if the tag is not stand-alone
         if not Stand_Alone then
            N := New_Node;
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
        (Kind       => Root_Node,
         Next       => null,
         Children   => null,
         Last_Child => null,
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
