------------------------------------------------------------------------------
--                                  G P S                                   --
--                                                                          --
--                     Copyright (C) 2009-2018, AdaCore                     --
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

with Ada.IO_Exceptions;
with Ada.Strings.Fixed;
with Ada.Strings.Unbounded;           use Ada.Strings.Unbounded;
with Ada.Strings.Wide_Wide_Unbounded; use Ada.Strings.Wide_Wide_Unbounded;

with GNAT.Decode_UTF8_String;         use GNAT.Decode_UTF8_String;
with GNAT.Encode_UTF8_String;         use GNAT.Encode_UTF8_String;
with GNAT.Strings;                    use GNAT.Strings;
with GNAT.UTF_32;                     use GNAT.UTF_32;

with GNATCOLL.Traces;       use GNATCOLL.Traces;

package body XML_Utils is

   Me : constant Trace_Handle := Create ("GPS.COMMON.XML_Utils");

   procedure Skip_Blanks (Buf : String; Index : in out Natural);
   --  Skip blanks, LF and CR, starting at Index. Index is updated to the
   --  new position (first non blank or EOF)

   function Get_Node (Buf : String; Index : access Natural) return Node_Ptr;
   --  The main parse routine. Starting at Index.all, Index.all is updated
   --  on return. Return the node starting at Buf (Index.all) which will
   --  also contain all the children and subchildren.

   procedure Get_Buf
     (Buf        : String;
      Index      : in out Natural;
      Terminator : Character;
      S          : out String_Ptr;
      Skip_Quote : Boolean := False);
   --  On return, S will contain the String starting at Buf (Index) and
   --  terminating before the first 'Terminator' character. Index will also
   --  point to the next non blank character.
   --  The special XML '&' characters are translated appropriately in S.
   --  S is set to null if Terminator wasn't found in Buf.
   --  If Skip_Quote then ignore Terminator in substrings quoted with ' and "

   procedure Extract_Attrib
     (Tag        : in out String_Ptr;
      Attributes : out String_Ptr;
      Empty_Node : out Boolean);
   --  Extract the attributes as a string, if the tag contains blanks ' '
   --  On return, Tag is unchanged and Attributes contains the string
   --  If the last character in Tag is '/' then the node is empty and
   --  Empty_Node is set to True.

   procedure Get_Next_Word
     (Buf   : String;
      Index : in out Natural;
      Word  : out String_Ptr);
   --  extract the next textual word from Buf and return it.
   --  return null if no word left.
   --  The special XML '&' characters are translated appropriately in S.

   function Translate (S : String) return String;
   --  Translate S by replacing the XML '&' special characters by the
   --  actual ASCII character.
   --  This function currently handles:
   --   - &quot;
   --   - &gt;
   --   - &lt;
   --   - &amp;
   --   - &apos;

   -----------------
   -- Skip_Blanks --
   -----------------

   procedure Skip_Blanks (Buf : String; Index : in out Natural) is
   begin
      while Index < Buf'Last and then
        (Buf (Index) = ' '  or else Buf (Index) = ASCII.LF
          or else Buf (Index) = ASCII.HT
          or else Buf (Index) = ASCII.CR)
      loop
         Index := Index + 1;
      end loop;
   end Skip_Blanks;

   -------------
   -- Get_Buf --
   -------------

   procedure Get_Buf
     (Buf        : String;
      Index      : in out Natural;
      Terminator : Character;
      S          : out String_Ptr;
      Skip_Quote : Boolean := False)
   is
      Start : constant Natural := Index;

   begin
      while Index <= Buf'Last and then Buf (Index) /= Terminator loop
         if Skip_Quote and then Buf (Index) in '"' | ''' then
            declare
               Quote : constant Character := Buf (Index);
            begin
               Index := Index + 1;

               while Index <= Buf'Last and then Buf (Index) /= Quote loop
                  Index := Index + 1;
               end loop;
            end;
         end if;

         Index := Index + 1;
      end loop;

      if Index > Buf'Last then
         S := null;

      else
         S := new String'(Translate (Buf (Start .. Index - 1)));
         Index := Index + 1;

         if Index < Buf'Last then
            Skip_Blanks (Buf, Index);
         end if;
      end if;
   end Get_Buf;

   --------------------
   -- Extract_Attrib --
   --------------------

   procedure Extract_Attrib
     (Tag        : in out String_Ptr;
      Attributes : out String_Ptr;
      Empty_Node : out Boolean)
   is
      Index             : Natural := Tag'First;
      Index_Last_Of_Tag : Natural;
      S                 : String_Ptr;

   begin
      --  First decide if the node is empty

      if Tag (Tag'Last) = '/' then
         Empty_Node := True;
      else
         Empty_Node := False;
      end if;

      while Index <= Tag'Last
        and then not
          (Tag (Index) = ' '  or else Tag (Index) = ASCII.LF
           or else Tag (Index) = ASCII.HT
           or else Tag (Index) = ASCII.CR
           or else Tag (Index) = '/')
      loop
         Index := Index + 1;
      end loop;

      Index_Last_Of_Tag := Index - 1;
      Skip_Blanks (Tag.all, Index);

      if Index <= Tag'Last then
         if Empty_Node then
            Attributes := new String'(Tag (Index .. Tag'Last - 1));
         else
            Attributes := new String'(Tag (Index .. Tag'Last));
         end if;

         S := new String'(Tag (Tag'First .. Index_Last_Of_Tag));
         Free (Tag);
         Tag := S;
      end if;
   end Extract_Attrib;

   -------------------
   -- Get_Next_Word --
   -------------------

   procedure Get_Next_Word
     (Buf   : String;
      Index : in out Natural;
      Word  : out String_Ptr)
   is
      Terminator : Character := ' ';
   begin
      Skip_Blanks (Buf, Index);

      if Buf (Index) = ''' or Buf (Index) = '"' then
         --  If the word starts with a quotation mark, then read until
         --  the closing mark

         Terminator := Buf (Index);
         Index := Index + 1;
         Get_Buf (Buf, Index, Terminator, Word);

      else
         --  For a normal word, scan up to either a blank, or a '='

         declare
            Start_Index : constant Natural := Index;
         begin
            while Index <= Buf'Last
              and then Buf (Index) /= ' '
              and then Buf (Index) /= '='
            loop
               Index := Index + 1;
            end loop;

            Word := new String'(Translate (Buf (Start_Index .. Index - 1)));
         end;
      end if;

      if Index < Buf'Last then
         Skip_Blanks (Buf, Index);
      end if;
   end Get_Next_Word;

   ---------------
   -- Translate --
   ---------------

   function Translate (S : String) return String is
      Str       : String (1 .. S'Length);
      Start, J  : Positive;
      Index     : Positive := S'First;
      In_String : Boolean  := False;

   begin
      if S'Length = 0 then
         return S;
      else
         J := Str'First;

         loop
            if In_String or else S (Index) /= '&' then
               Str (J) := S (Index);
            else
               Index := Index + 1;
               Start := Index;

               while S (Index) /= ';' loop
                  Index := Index + 1;
                  pragma Assert (Index <= S'Last);
               end loop;

               if S (Start) = '#' then
                  declare
                     N : Natural;
                  begin
                     N := Natural'Value (S (Start + 1 .. Index - 1));
                     Str (J) := Character'Val (N);
                  exception
                     when Constraint_Error =>
                        Str (J) := '?';
                  end;
               elsif S (Start .. Index - 1) = "quot" then
                  Str (J) := '"';
               elsif S (Start .. Index - 1) = "gt" then
                  Str (J) := '>';
               elsif S (Start .. Index - 1) = "lt" then
                  Str (J) := '<';
               elsif S (Start .. Index - 1) = "amp" then
                  Str (J) := '&';
               elsif S (Start .. Index - 1) = "apos" then
                  Str (J) := ''';
               end if;
            end if;

            exit when Index = S'Last;

            if S (Index) = '"' then
               In_String := not In_String;
            end if;

            Index := Index + 1;
            J     := J + 1;
         end loop;

         return Str (1 .. J);
      end if;
   end Translate;

   -------------------
   -- Get_Attribute --
   -------------------

   function Get_Attribute
     (N              : Node_Ptr;
      Attribute_Name : UTF8_String;
      Default        : UTF8_String := "") return UTF8_String
   is
      Index      : Natural;
      Key, Value : String_Ptr;

   begin
      if N = null or else N.Attributes = null then
         return Default;
      end if;

      Index := N.Attributes'First;
      while Index < N.Attributes'Last loop
         Get_Next_Word (N.Attributes.all, Index, Key);
         Get_Buf (N.Attributes.all, Index, '=', Value);
         Free (Value);
         Get_Next_Word (N.Attributes.all, Index, Value);

         if Attribute_Name = Key.all then
            exit;
         else
            Free (Key);
            Free (Value);
         end if;
      end loop;

      Free (Key);

      if Value = null then
         return Default;
      else
         declare
            V : constant String := Value.all;
         begin
            Free (Value);
            return V;
         end;
      end if;
   end Get_Attribute;

   -------------------
   -- Set_Attribute --
   -------------------

   procedure Set_Attribute
     (N : Node_Ptr; Attribute_Name, Attribute_Value : UTF8_String)
   is
      Index, Tmp : Natural;
      Key, Value : String_Ptr;
      Atts       : String_Ptr;
      Str        : constant String :=
        Attribute_Name & "=""" & Protect (Attribute_Value) & """ ";

   begin
      if N.Attributes /= null then
         Index := N.Attributes'First;
         --  First remove any definition of the attribute in the current list
         while Index < N.Attributes'Last loop
            Tmp := Index;
            Get_Next_Word (N.Attributes.all, Index, Key);
            Get_Buf (N.Attributes.all, Index, '=', Value);
            Free (Value);

            Get_Next_Word (N.Attributes.all, Index, Value);
            Free (Value);

            if Attribute_Name = Key.all then
               Atts := new String'
                 (Str
                  & N.Attributes (N.Attributes'First .. Tmp - 1)
                  & N.Attributes (Index .. N.Attributes'Last));
               Free (N.Attributes);
               N.Attributes := Atts;
               Free (Key);
               return;
            end if;

            Free (Key);
         end loop;

         Atts := new String'(Str & N.Attributes.all);
         Free (N.Attributes);
         N.Attributes := Atts;

      else
         N.Attributes := new String'(Str);
      end if;
   end Set_Attribute;

   ---------------
   -- Add_Child --
   ---------------

   procedure Add_Child
     (N : Node_Ptr; Child : Node_Ptr; Append : Boolean := False)
   is
      Tmp : Node_Ptr;
   begin
      if Append then
         if N.Child = null then
            N.Child := Child;
         else
            Tmp := N.Child;
            while Tmp.Next /= null loop
               Tmp := Tmp.Next;
            end loop;
            Tmp.Next := Child;
         end if;

      else
         Child.Next := N.Child;
         N.Child := Child;
      end if;
      Child.Parent := N;
   end Add_Child;

   --------------
   -- Get_Node --
   --------------

   function Get_Node (Buf : String; Index : access Natural) return Node_Ptr is
      N          : constant Node_Ptr := new Node;
      Q          : Node_Ptr;
      S          : String_Ptr;
      Empty_Node : Boolean;
      Last_Child : Node_Ptr;

   begin
      pragma Assert (Buf (Index.all) = '<');

      Index.all := Index.all + 1;
      Get_Buf (Buf, Index.all, '>', N.Tag, Skip_Quote => True);

      --  Check to see whether it is a comment, PI, !DOCTYPE, or the like:

      if N.Tag (N.Tag'First) = '!' or N.Tag (N.Tag'First) = '?' then
         return Get_Node (Buf, Index);
      else
         --  Here we have to deal with the attributes of the form
         --  <tag attrib='xyyzy'>

         Extract_Attrib (N.Tag, N.Attributes, Empty_Node);

         --  it is possible to have a child-less node that has the form
         --  <tag /> or <tag attrib='xyyzy'/>

         if Empty_Node then
            N.Value := new String'("");
         else
            if Buf (Index.all) = '<' then
               if Buf (Index.all + 1) = '/' then
                  --  No value contained on this node

                  N.Value := new String'("");
                  Index.all := Index.all + 1;

               else
                  --  Parse the children

                  Add_Child (N, Get_Node (Buf, Index));
                  Last_Child := N.Child;
                  pragma Assert (Buf (Index.all) = '<');

                  while Buf (Index.all + 1) /= '/' loop
                     Q := Last_Child;
                     Q.Next := Get_Node (Buf, Index);
                     Q.Next.Parent := N;
                     Last_Child := Q.Next;
                     pragma Assert (Buf (Index.all) = '<');
                  end loop;

                  Index.all := Index.all + 1;
               end if;

            else
               --  Get the value of this node

               Get_Buf (Buf, Index.all, '<', N.Value);
            end if;

            pragma Assert (Buf (Index.all) = '/');
            Index.all := Index.all + 1;
            Get_Buf (Buf, Index.all, '>', S);
            pragma Assert (N.Tag.all = S.all);
            Free (S);
         end if;

         return N;
      end if;
   exception
      when others =>
         return null;
   end Get_Node;

   -------------
   -- Protect --
   -------------

   function Protect (S : String; Ignore_LF : Boolean := False) return String is
      R      : Unbounded_Wide_Wide_String;
      Wide   : Wide_Wide_String (1 .. S'Length);
      Length : Natural;
   begin
      Decode_Wide_Wide_String (S, Wide, Length);

      for J in 1 .. Length loop
         case Wide (J) is
            when '<' =>
               Append (R, "&lt;");
            when '>' =>
               Append (R, "&gt;");
            when '&' =>
               Append (R, "&amp;");
            when ''' =>
               Append (R, "&apos;");
            when '"' =>
               Append (R, "&quot;");
            when others =>
               declare
                  C_Val : Natural;
               begin
                  C_Val := Wide_Wide_Character'Pos (Wide (J));
                  if Get_Category (UTF_32'Val (C_Val)) = Cc
                    and then
                      (not Ignore_LF or else C_Val /= Character'Pos (ASCII.LF))
                  then
                     declare
                        Img : constant Wide_Wide_String :=
                                Natural'Wide_Wide_Image (C_Val);
                     begin
                        Append
                          (R, "&#" & Img (Img'First + 1 .. Img'Last) & ";");
                     end;

                  else
                     Append (R, Wide (J));
                  end if;
               end;

         end case;
      end loop;

      return Encode_Wide_Wide_String (To_Wide_Wide_String (R));
   end Protect;

   -----------
   -- Print --
   -----------

   procedure Print (N : Node_Ptr; File : Virtual_File) is
      Success : Boolean;
      pragma Unreferenced (Success);
   begin
      Print (N, File, Success);
   end Print;

   -----------
   -- Print --
   -----------

   procedure Print
     (N       : Node_Ptr;
      File    : Virtual_File;
      Success : out Boolean;
      Style   : String := "")
   is
      Writable : Writable_File;

      procedure Do_Indent (Indent : Natural);
      --  Print a string made of Indent blank characters

      procedure Print_String (S : String);
      --  Print S to File, after replacing the special '<', '>',
      --  '"', '&' and ''' characters.

      procedure Print_Node (N : Node_Ptr; Indent : Natural);
      --  Write a node and its children to File

      procedure Put (S : String);
      --  Write S to File

      procedure Put_Line (S : String);
      --  Write S & LF to File

      ---------
      -- Put --
      ---------

      procedure Put (S : String) is
      begin
         Write (Writable, S);
      end Put;

      --------------
      -- Put_Line --
      --------------

      procedure Put_Line (S : String) is
      begin
         Put (S & ASCII.LF);
      end Put_Line;

      ---------------
      -- Do_Indent --
      ---------------

      procedure Do_Indent (Indent : Natural) is
      begin
         Put ((1 .. Indent => ' '));
      end Do_Indent;

      ------------------
      -- Print_String --
      ------------------

      procedure Print_String (S : String) is
      begin
         for J in S'Range loop
            case S (J) is
               when '<' => Put ("&lt;");
               when '>' => Put ("&gt;");
               when '&' => Put ("&amp;");
               when ''' => Put ("&apos;");
               when '"' => Put ("&quot;");
               when ASCII.NUL .. Character'Val (9)
                  | Character'Val (11) .. Character'Val (31) =>
                  declare
                     Img : constant String :=
                       Integer'Image (Character'Pos (S (J)));
                  begin
                     Put ("&#" & Img (Img'First + 1 .. Img'Last) & ";");
                  end;
               when others => Put ((1 => S (J)));
            end case;
         end loop;
      end Print_String;

      ----------------
      -- Print_Node --
      ----------------

      procedure Print_Node (N : Node_Ptr; Indent : Natural) is
         P : Node_Ptr;
      begin
         Do_Indent (Indent);
         Put ("<" & N.Tag.all);

         if N.Attributes /= null then
            Put (" " & N.Attributes.all);
         end if;

         if N.Child /= null then
            Put_Line (">");
            P := N.Child;
            while P /= null loop
               Print_Node (P, Indent + 2);
               P := P.Next;
            end loop;

            Do_Indent (Indent);
            Put_Line ("</" & N.Tag.all & ">");

         elsif N.Value = null
           or else N.Value.all = ""
         then
            --  The following handles the difference between what you got
            --  when you parsed <tag/> vs. <tag />.
            if N.Tag (N.Tag'Last) = '/' then
               Put_Line (">");
            else
               Put_Line (" />");
            end if;
         else
            Put (">");
            Print_String (N.Value.all);
            Put_Line ("</" & N.Tag.all & ">");
         end if;
      end Print_Node;

   begin
      if File = GNATCOLL.VFS.No_File then
         Success := False;
         return;
      end if;

      Writable := Write_File (File);

      Put_Line ("<?xml version=""1.0""?>");

      if Style /= "" then
         Put_Line ("<?xml-stylesheet href='" & Style & "' type='text/xsl'?>");
      end if;

      Print_Node (N, 0);

      begin
         Close (Writable);
         Success := True;

      exception
         when Ada.IO_Exceptions.Use_Error =>
            --  Call to Close raises exception when file can't be saved.

            Success := False;
      end;
   end Print;

   -----------
   -- Parse --
   -----------

   function Parse (File : Virtual_File) return Node_Ptr is
      Buf    : GNAT.Strings.String_Access;
      Result : Node_Ptr;

   begin
      Buf := Read_File (File);

      if Buf = null then
         return null;
      end if;

      Result := Parse_Buffer (Buf.all);
      Free (Buf);
      return Result;
   end Parse;

   ------------------
   -- Parse_Buffer --
   ------------------

   function Parse_Buffer (Buffer : UTF8_String) return Node_Ptr is
      Index         : aliased Natural := 2;
      XML_Version   : String_Ptr;
      Encoding      : Integer;
      Encoding_Last : Integer;
      Result        : Node_Ptr;
   begin
      Get_Buf (Buffer, Index, '>', XML_Version);
      if XML_Version = null then
         return null;
      else
         --  Check the encoding specified for that file
         Encoding := Ada.Strings.Fixed.Index (XML_Version.all, "encoding");

         if Encoding /= 0 then
            while Encoding <= XML_Version'Last
              and then XML_Version (Encoding) /= '"'
            loop
               Encoding := Encoding + 1;
            end loop;

            Encoding := Encoding + 1;
            Encoding_Last := Encoding + 1;

            while Encoding_Last <= XML_Version'Last
              and then XML_Version (Encoding_Last) /= '"'
            loop
               Encoding_Last := Encoding_Last + 1;
            end loop;

            if Encoding_Last <= XML_Version'Last then
               --  In GPS, we assume all XML files are encoded in UTF_8
               if XML_Version (Encoding .. Encoding_Last - 1) /= "UTF-8" then
                  Trace (Me, "XML file is not UTF-8");
               end if;
            else
               Result := Get_Node (Buffer, Index'Unchecked_Access);
            end if;
         else
            Result := Get_Node (Buffer, Index'Unchecked_Access);
         end if;

         Free (XML_Version);
         return Result;
      end if;
   end Parse_Buffer;

   --------------
   -- Find_Tag --
   --------------

   function Find_Tag (N : Node_Ptr; Tag : UTF8_String) return Node_Ptr is
      P : Node_Ptr := N;

   begin
      while P /= null loop
         if P.Tag.all = Tag then
            return P;
         end if;

         P := P.Next;
      end loop;

      return null;
   end Find_Tag;

   -----------------------------
   -- Find_Tag_With_Attribute --
   -----------------------------

   function Find_Tag_With_Attribute
     (N     : Node_Ptr;
      Tag   : UTF8_String;
      Key   : UTF8_String;
      Value : UTF8_String := "") return Node_Ptr
   is
      P : Node_Ptr := N;
   begin
      while P /= null loop
         if P.Tag.all = Tag then
            declare
               The_Value : constant String := Get_Attribute (P, Key);
            begin
               if The_Value /= "" then
                  if Value = "" or The_Value = Value then
                     --  if Value is not given when calling the
                     --  the function only the Key need to match
                     return P;
                  end if;
               end if;
            end;
         end if;
         P := P.Next;
      end loop;

      return null;
   end Find_Tag_With_Attribute;

   ---------------
   -- Get_Field --
   ---------------

   function Get_Field (N : Node_Ptr; Field : UTF8_String) return String_Ptr is
      P : constant Node_Ptr := Find_Tag (N.Child, Field);

   begin
      if P /= null then
         return P.Value;
      else
         return null;
      end if;
   end Get_Field;

   ----------
   -- Free --
   ----------

   procedure Free (N : in out Node_Ptr) is

      procedure Free_Node (N : in out Node_Ptr);
      --  Free the memory for a node, but doesn't remove it from its parent

      procedure Unchecked_Free is
        new Ada.Unchecked_Deallocation (Node, Node_Ptr);

      ---------------
      -- Free_Node --
      ---------------

      procedure Free_Node (N : in out Node_Ptr) is
         Child : Node_Ptr := N.Child;
         Next  : Node_Ptr;

      begin
         Free (N.Tag);
         Free (N.Attributes);
         Free (N.Value);

         --  Free all the children
         while Child /= null loop
            Next := Child.Next;
            Free_Node (Child);
            Child := Next;
         end loop;

         Unchecked_Free (N);
      end Free_Node;

      Child    : Node_Ptr;
      Previous : Node_Ptr;

   begin
      if N = null then
         return;
      end if;

      if N.Parent /= null then
         Child := N.Parent.Child;

         --  Remove the node from its parent
         while Child /= null and then Child /= N loop
            Previous := Child;
            Child := Child.Next;
         end loop;

         if Child = N then
            if Previous = null then
               N.Parent.Child := N.Next;
            else
               Previous.Next := N.Next;
            end if;
         end if;
      end if;

      --  Free the memory occupied by the node
      Free_Node (N);
   end Free;

   ---------------
   -- Deep_Copy --
   ---------------

   function Deep_Copy (N : Node_Ptr) return Node_Ptr is

      function Deep_Copy_Internal
        (N : Node_Ptr; Parent : Node_Ptr := null) return Node_Ptr;
      --  Internal version of Deep_Copy. Returns a deep copy of N, whose
      --  parent should be Parent.

      ------------------------
      -- Deep_Copy_Internal --
      ------------------------

      function Deep_Copy_Internal
        (N : Node_Ptr; Parent : Node_Ptr := null) return Node_Ptr
      is
         Attr    : String_Ptr;
         Value   : String_Ptr;

         New_N   : Node_Ptr;
         Child   : Node_Ptr;
         N_Child : Node_Ptr;
      begin
         if N = null then
            return null;

         else
            if N.Attributes /= null then
               Attr := new String'(N.Attributes.all);
            end if;

            if N.Value /= null then
               Value := new String'(N.Value.all);
            end if;

            --  Do not clone Next: For the initial node, we should not clone
            --  the next nodes, only its children. And for children this is
            --  done by Deep_Copy_Internal on the parent

            New_N := new Node'
              (Tag => new String'(N.Tag.all),
               Attributes => Attr,
               Value => Value,
               Parent => Parent,
               Child => null,
               Next => null,
               Specific_Data => N.Specific_Data);

            --  Clone each child

            Child := N.Child;
            while Child /= null loop
               if N_Child = null then
                  New_N.Child := Deep_Copy_Internal (Child, Parent => New_N);
                  N_Child := New_N.Child;
               else
                  N_Child.Next := Deep_Copy_Internal (Child, Parent => New_N);
                  N_Child := N_Child.Next;
               end if;
               Child := Child.Next;
            end loop;

            return New_N;
         end if;
      end Deep_Copy_Internal;

   begin
      return Deep_Copy_Internal (N);
   end Deep_Copy;

   --------------
   -- Is_Equal --
   --------------

   function Is_Equal (Node1, Node2 : Node_Ptr) return Boolean is
   begin
      if Node1 = null then
         if Node2 /= null then
            return False;
         else
            return True;
         end if;
      elsif Node2 = null then
         return False;
      end if;

      if Node1.Tag = null then
         if Node2.Tag /= null then
            return False;
         end if;
      elsif Node2.Tag = null then
         return False;
      elsif Node1.Tag.all /= Node2.Tag.all then
         return False;
      end if;

      if Node1.Attributes = null then
         if Node2.Attributes /= null then
            return False;
         end if;
      elsif Node2.Attributes = null then
         return False;
      elsif Node1.Attributes.all /= Node2.Attributes.all then
         return False;
      end if;

      if Node1.Value = null then
         if Node2.Value /= null then
            return False;
         end if;
      elsif Node2.Value = null then
         return False;
      elsif Node1.Value.all /= Node2.Value.all then
         return False;
      end if;

      if Node1.Child = null then
         if Node2.Child /= null then
            return False;
         end if;
      elsif Node2.Child = null then
         return False;
      elsif not Is_Equal (Node1.Child, Node2.Child) then
         return False;
      end if;

      if Node1.Next = null then
         if Node2.Next /= null then
            return False;
         end if;
      elsif Node2.Next = null then
         return False;
      elsif not Is_Equal (Node1.Next, Node2.Next) then
         return False;
      end if;
      return True;
   end Is_Equal;

   -----------------------------
   -- String_To_Encoded_ASCII --
   -----------------------------

   function String_To_Encoded_ASCII (S : String) return String is
      U : Unbounded_String;
   begin
      for J in S'Range loop
         if S (J) = '#'
           or else Character'Pos (S (J)) >= 127
         then
            declare
               Img : constant String :=
                       Natural'Image (Character'Pos (S (J)));
            begin
               Append (U, "#" & Img (Img'First + 1 .. Img'Last) & ";");
            end;
         else
            Append (U, S (J));
         end if;
      end loop;

      return To_String (U);
   end String_To_Encoded_ASCII;

   -----------------------------
   -- Encoded_ASCII_To_String --
   -----------------------------

   function Encoded_ASCII_To_String (S : String) return String is
      U     : Unbounded_String;
      Index : Natural := S'First;
      Next  : Natural;
   begin
      while Index <= S'Last loop
         if S (Index) = '#' then
            Next := Index + 1;
            loop
               if Next > S'Last then
                  Trace (Me, "XML error: '#' not corresponding to ';'");
                  return To_String (U);
               end if;

               exit when S (Next) = ';';

               Next := Next + 1;
            end loop;

            declare
               N : Natural;
            begin
               N := Natural'Value (S (Index + 1 .. Next - 1));
               Append (U, Character'Val (N));
            exception
               when Constraint_Error =>
                  Trace
                    (Me, "XML error: could not read number in '#' entity");
                  return To_String (U);
            end;

            Index := Next + 1;
         else
            Append (U, S (Index));
            Index := Index + 1;
         end if;
      end loop;

      return To_String (U);
   end Encoded_ASCII_To_String;

   --------------------
   -- Add_File_Child --
   --------------------

   procedure Add_File_Child
     (N              : Node_Ptr;
      Tag            : UTF8_String;
      File           : Virtual_File;
      Use_VFS_Prefix : Boolean := True)
   is
      Child : Node_Ptr;

   begin
      Child := new Node;

      if Use_VFS_Prefix then
         Child.Tag := new UTF8_String'("vfs_" & Tag);
      else
         Child.Tag := new UTF8_String'(Tag);
      end if;
      Child.Value := new UTF8_String'
        (String_To_Encoded_ASCII (+Full_Name (File)));

      declare
         Host : constant String := Get_Host (File);
      begin
         if Host /= Local_Host then
            Set_Attribute (Child, "server", Host);
         end if;
      end;

      Add_Child (N, Child);
   end Add_File_Child;

   --------------------
   -- Get_File_Child --
   --------------------

   function Get_File_Child
     (N              : Node_Ptr;
      Tag            : UTF8_String;
      Host           : String := "";
      Use_VFS_Prefix : Boolean := True) return Virtual_File
   is
      Child : Node_Ptr;

   begin
      Child := N.Child;

      if Child /= null then
         if Use_VFS_Prefix then
            Child := Find_Tag (Child, "vfs_" & Tag);
         else
            Child := Find_Tag (Child, Tag);
         end if;
      end if;

      if Child = null then
         --  Revert to trying to find in an attribute named Tag: this might
         --  be the case when trying to parse previous XML file formats.

         declare
            S : constant String := Get_Attribute (N, Tag, "");
         begin
            if S /= "" then
               if Host /= "" then
                  --  Return the remote file immediately: they are always saved
                  --  as full paths.
                  return Create (+S, Host);
               else
                  return Create (+S);
               end if;
            end if;
         end;

         --  Another way to falling back to a previous scheme.
         --  (this is in the case of the desktop only)

         Child := Find_Tag (N.Child, "File");

         if Child = null then
            return No_File;
         end if;
      end if;

      declare
         Value     : constant String :=
                       Encoded_ASCII_To_String (Child.Value.all);
         Host_Attr : constant String :=
                       Get_Attribute (Child, "server", Host);
      begin
         if Value = "" then
            return No_File;
         end if;

         if Host_Attr = "" or else Host_Attr = Local_Host then
            return Create (+Value);
         else
            return Create (+Value, Host_Attr);
         end if;
      end;
   end Get_File_Child;

end XML_Utils;
