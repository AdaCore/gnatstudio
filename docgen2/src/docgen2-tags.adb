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
with Docgen2.Hooks;              use Docgen2.Hooks;
with Docgen2.Utils;              use Docgen2.Utils;
with GPS.Kernel;                 use GPS.Kernel;
with String_Utils;               use String_Utils;

package body Docgen2.Tags is

--     procedure Free is new Ada.Unchecked_Deallocation (Node, Node_Ptr);

   XML_Regpat : constant Pattern_Matcher :=
                  Compile (" *<([/]?) *([^ </>]+) *([^<>]*)>", Single_Line);

   function Is_Tag
     (S       : Unbounded_String;
      Options : Docgen_Options) return Boolean;
   --  Return true if S is a user-defined tag

   function Deep_Copy (Comment : Comment_Type) return Comment_Type;
   --  Performs a deep copy of comment

   function To_Unbounded_String
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Backend         : access Backend_Record'Class;
      Entity_Name     : String;
      Href            : String;
      N               : Node_Ptr;
      Keep_Formatting : Boolean) return Unbounded_String;

   function Strip_Blanks
     (S : Unbounded_String;
      Keep_Formatting : Boolean) return String;

   procedure Parse_Buffer
     (S    : String;
      N    : in out Node_Ptr;
      Opts : Docgen_Options);

   ------------
   -- Is_Tag --
   ------------

   function Is_Tag
     (S       : Unbounded_String;
      Options : Docgen_Options) return Boolean
   is
   begin
      for J in
        Options.User_Tags.First_Index .. Options.User_Tags.Last_Index
      loop
         if S = Options.User_Tags.Element (J) then
            return True;
         end if;
      end loop;

      return False;
   end Is_Tag;

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

         if Is_Blank_Line (Str (Idx .. Nxt), Idx) then
            Res := Res & ASCII.LF;
         else
            if not Keep_Formatting then
               Skip_Blanks (Str, Idx);
            end if;

            Res := Res & Str (Idx .. Nxt);

            if not Is_Blank (Str (Nxt)) then
               Res := Res & ' ';
            end if;
         end if;

         if Keep_Formatting then
            Res := Res & ASCII.LF;
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
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Backend         : access Backend_Record'Class;
      Entity_Name     : String;
      Href            : String;
      N               : Node_Ptr;
      Keep_Formatting : Boolean) return Unbounded_String
   is
      Str   : Ada.Strings.Unbounded.Unbounded_String;
      Node  : Node_Ptr := N;

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
                                   Keep_Formatting
                                   and then Node.Tag = Null_Unbounded_String));
            Val_U_Str    : Unbounded_String;
            Children_Str : constant Unbounded_String :=
                             To_Unbounded_String
                               (Kernel, Backend, Entity_Name, Href,
                                Node.Children, Keep_Formatting);
            Idx          : Natural;
            Nxt          : Natural;
         begin

            if Node.Tag /= Null_Unbounded_String then
               declare
                  Res : constant String :=
                          User_Tag_Action
                            (Kernel,
                             To_String (Node.Tag),
                             To_String (Node.Attributes),
                             Val_Str,
                             Entity_Name,
                             Href);
               begin
                  if Res'Length = 0 then
                     Idx := Val_Str'First;
                     Val_U_Str := Null_Unbounded_String;
                     while Idx < Val_Str'Last loop
                        Nxt := Line_End (Val_Str, Idx);
                        Append
                          (Val_U_Str,
                           Backend.Gen_Paragraph (Val_Str (Idx .. Nxt)));
                        Idx := Next_Line (Val_Str, Idx);
                     end loop;

                     Append
                       (Str,
                        Backend.Gen_User_Tag
                          (To_String (Node.Tag),
                           To_String (Node.Attributes),
                           To_String (Val_U_Str)));
                  else
                     Append (Str, Res);
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

   function To_String
     (Kernel          : access GPS.Kernel.Kernel_Handle_Record'Class;
      Backend         : access Backend_Record'Class;
      Comment         : access Comment_Type;
      Entity_Name     : String;
      Href            : String;
      Keep_Formatting : Boolean) return String
   is
   begin
      if not Comment.Analysed then
         Comment.Block := To_Unbounded_String
           (Kernel, Backend, Entity_Name, Href, Comment.N, Keep_Formatting);
         Comment.Analysed := True;
      end if;

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
      if not Is_Tag (Tag, Opts)
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
            N          => null,
            Analysed   => False);
         List.Append (Elem);
      end if;
   end Add_Comment_Line;

   ----------------------
   -- Analyse_Comments --
   ----------------------

   procedure Analyse_Comments
     (Kernel  : access GPS.Kernel.Kernel_Handle_Record'Class;
      Options : Docgen_Options;
      File    : Source_File;
      List    : in out Comments_List.Vector)
   is
      Elem   : Comment_Access;

   begin
      if List.Is_Empty then
         return;
      end if;

      for Index in List.First_Index .. List.Last_Index loop
         Elem := List.Element (Index);
         Elem.N := new Node'
           (Tag        => Null_Unbounded_String,
            Value      => Null_Unbounded_String,
            Attributes => Null_Unbounded_String,
            Next       => null,
            Children   => null,
            Parent     => null);
         Parse_Buffer
           (To_String (Elem.Block),
            Elem.N,
            Options);

         while Elem.N.Parent /= null loop
            Warning
              (Kernel, File, Elem.Sloc_Stop,
               "Tag " & To_String (Elem.N.Tag) &
               " is not closed");
            Elem.N := Elem.N.Parent;
         end loop;
      end loop;
   end Analyse_Comments;

   ---------------
   -- Deep_Copy --
   ---------------

   function Deep_Copy (Comment : Comment_Type) return Comment_Type
   is
      Ret : Comment_Type := Comment;
      function Deep_Copy (N : Node_Ptr) return Node_Ptr;

      function Deep_Copy (N : Node_Ptr) return Node_Ptr is
         Ret    : Node_Ptr;
         Dest   : Node_Ptr;
         Source : Node_Ptr;
      begin
         if N = null then
            return null;
         end if;

         Ret := new Node'(N.all);
         Dest := Ret;
         Dest.Children := Deep_Copy (N.Children);

         Source := N.Next;

         while Source /= null loop
            Dest.Next := new Node'(Source.all);
            Dest := Dest.Next;
            Dest.Children := Deep_Copy (Source.Children);

            Source := Source.Next;
         end loop;

         return Ret;
      end Deep_Copy;
   begin
      Ret.N := Deep_Copy (Comment.N);
      return Ret;
   end Deep_Copy;

   ----------
   -- Free --
   ----------

   procedure Free (Comment : in out Comment_Type) is
      procedure Free_Node (N : in out Node_Ptr);
      --  Free a xml node tree

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
      end Free_Node;

   begin
      Free_Node (Comment.N);
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
      Index : Natural;
   begin

      Index := Comments.First_Index;

      while Index <= Comments.Last_Index loop
         if File_Doc
           and then Comments.Element (Index).Sloc_Start.Line > Sloc_Start.Line
         then
            if Index > Comments.First_Index then
               return Deep_Copy (Comments.Element (Index - 1).all);
            else
               return No_Comment;
            end if;

         elsif Sloc_Start.Line - 1 = Comments.Element (Index).Sloc_Stop.Line
           or else Sloc_End.Line + 1 = Comments.Element (Index).Sloc_Start.Line
         then
            --  ??? Should not return twice a comment. Mark it as used ?
            return Deep_Copy (Comments.Element (Index).all);
         end if;

         exit when Comments.Element (Index).Sloc_Start > Sloc_End;

         Index := Index + 1;
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
