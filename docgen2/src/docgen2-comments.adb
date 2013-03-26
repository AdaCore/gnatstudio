------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2007-2013, AdaCore                     --
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

with Ada.Unchecked_Deallocation;
with GNAT.Regpat;                use GNAT.Regpat;

with Docgen2_Backend;            use Docgen2_Backend;
with Docgen2.Scripts;            use Docgen2.Scripts;
with Docgen2.Utils;              use Docgen2.Utils;
with String_Utils;               use String_Utils;
with GNATCOLL.Utils;             use GNATCOLL.Utils;
with GNATCOLL.VFS;               use GNATCOLL.VFS;
with GNATCOLL.Xref;

package body Docgen2.Comments is

   -----------
   -- Local --
   -----------

   --  Local package containing routines routines shared by both versions

   package Local is

      procedure Append_Txt (N : Node_Ptr; S : String);
      --  Accumulate consecutive comments in the same node

      procedure Free_Node (N : in out Node_Ptr);
      --  Free a xml node tree

      function To_Unbounded_String
        (Docgen      : Docgen_Object;
         N           : Node_Ptr;
         Entity_Name : String;
         Href        : String;
         Keep_Layout : Boolean := False) return Unbounded_String;
      --  Translate the xml tree back to a block, using appropriate user tag
      --  transformations

   end Local;

   ---------------
   -- Version_2 --
   ---------------

   --  Local package containing the behavior required for Docgen version 2

   package Version_2 is

      procedure Parse_Buffer
        (Comment     : in out Comment_Type;
         Docgen      : Docgen_Object;
         File        : Virtual_File;
         Entity_Name : String;
         Href        : String);
      --  Translate the block into an xml tree

   end Version_2;

   ---------------------
   -- Analyse_Comment --
   ---------------------

   procedure Analyse_Comment
     (Comment     : in out Comment_Type;
      Docgen      : Docgen_Object;
      File        : Virtual_File;
      Entity_Name : String;
      Href        : String)
   is
   begin
      if Comment.Analysed then
         return;
      end if;

      Version_2.Parse_Buffer (Comment, Docgen, File, Entity_Name, Href);

      Comment.Analysed := True;
   end Analyse_Comment;

   ----------
   -- Free --
   ----------

   procedure Free (Comment : in out Comment_Type) is
   begin
      Comment.Block := Null_Unbounded_String;
   end Free;

   ---------------
   -- To_String --
   ---------------

   function To_String (Comment : Comment_Type) return String is
   begin
      return To_String (Comment.Block);
   end To_String;

   package body Local is

      ----------------
      -- Append_Txt --
      ----------------

      procedure Append_Txt (N : Node_Ptr; S : String) is
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
         --  True if the line starting at Idx seems intended (e.g. is a list
         --  item or a paragraph start.

         function Is_Intended_LF (Str : String; Idx : Natural) return Boolean
         is
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
                     --  A new sentence starting with a new-line: suppose this
                     --  is intentional
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
                  Append (Ret,
                    To_Unbounded_String (Docgen, Node, Entity_Name, Href));
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
                     Append (Val,
                       Backend.Gen_Paragraph (S (P_First .. S'Last)));
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

   end Local;

   ---------------
   -- Version_2 --
   ---------------

   package body Version_2 is
      XML_Regpat : constant Pattern_Matcher :=
        Compile (" *<([/]?) *([^ </>]+) *([^<>]*)>", Single_Line);

      ------------------
      -- Parse_Buffer --
      ------------------

      procedure Parse_Buffer
        (Comment     : in out Comment_Type;
         Docgen      : Docgen_Object;
         File        : Virtual_File;
         Entity_Name : String;
         Href        : String) is

         procedure Parse
           (S    : String;
            N    : in out Node_Ptr;
            Opts : Docgen_Options);

         procedure Parse
           (S    : String;
            N    : in out Node_Ptr;
            Opts : Docgen_Options)
         is
            Matches      : Match_Array (0 .. 3);
            Tag          : Unbounded_String;
            New_Node     : Node_Ptr;
            Closing_Tag  : Boolean;
            Stand_Alone  : Boolean;

         begin
            Match (XML_Regpat, S, Matches);

            --  No xml case
            if Matches (0) = No_Match then
               --  Regular string. Let's append it to the current node value
               Local.Append_Txt (N, S);

               return;
            end if;

            --  Append characters to the last opened tag.
            if Matches (0).First > S'First then
               Local.Append_Txt (N, S (S'First .. Matches (0).First - 1));
            end if;

            Tag :=
              To_Unbounded_String (S (Matches (2).First .. Matches (2).Last));

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
               Local.Append_Txt (N, S (Matches (0).First .. Matches (0).Last));

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
                    To_Unbounded_String
                      (S (Matches (3).First .. Matches (3).Last));
               end if;

               --  Set N to the newly created node if the tag is not
               --  stand-alone
               if not Stand_Alone then
                  N := New_Node;
               end if;
            end if;

            if Matches (0).Last < S'Last then
               Parse (S (Matches (0).Last + 1 .. S'Last), N, Opts);
            end if;
         end Parse;

         --  Local variables

         S    : constant String := To_String (Comment.Block);
         Opts : constant Docgen_Options := Get_Options (Docgen);
         N    : Node_Ptr;
      begin
         if S = "" then
            return;
         end if;

         N := new Node'
           (Kind       => Root_Node,
            Next       => null,
            Children   => null,
            Last_Child => null,
            Parent     => null);

         Parse (S, N, Opts);

         --  Check unclosed XML tags

         while N.Parent /= null loop
            Warning
              (Get_Kernel (Docgen),
               (File => File,
                Line => Comment.Sloc_Stop.Line,
                Column =>
                  GNATCOLL.Xref.Visible_Column (Comment.Sloc_Stop.Column)),
               "Tag " & To_String (N.Tag) &
                 " is not closed");

            N := N.Parent;
         end loop;

         --  Translate the xml tree back to a block, using appropriate
         --  user tag transformations

         Comment.Block :=
           Local.To_Unbounded_String (Docgen, N, Entity_Name, Href);

         Local.Free_Node (N);
      end Parse_Buffer;

   end Version_2;

end Docgen2.Comments;
