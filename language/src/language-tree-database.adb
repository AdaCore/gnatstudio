-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                  Copyright (C) 2006-2010, AdaCore                 --
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

with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;

with Glib.Convert; use Glib.Convert;

with Language.Documentation; use Language.Documentation;
with Language.Unknown;       use Language.Unknown;

with System;            use System;
with String_Utils; use String_Utils;

package body Language.Tree.Database is

   procedure Internal_Update_Contents
     (File : Structured_File_Access; Is_New_File : Boolean);
   --  Same as Update_Contents, but takes into account differences depending on
   --  the fact that the file is a brand new one, or an existing one.

   procedure Free
     (This : in out Construct_Db_Data_Access;
      Db   : access Construct_Database);
   --  Removes all indexes contained in This from the database, and the
   --  deallocates it.

   procedure Unchecked_Free is new Standard.Ada.Unchecked_Deallocation
     (Construct_Db_Data_Array, Construct_Db_Data_Access);

   procedure Free (File : in out Structured_File_Access);

   ------------------------------
   -- Get_Last_Relevant_Entity --
   ------------------------------

   function Get_Last_Relevant_Construct
     (Tree : Construct_Tree; Offset : Natural)
      return Construct_Tree_Iterator
   is
      Last_Relevant_Construct : Construct_Tree_Iterator :=
        Null_Construct_Tree_Iterator;
      It                      : Construct_Tree_Iterator;
   begin

      for J in reverse 1 .. Tree.Contents'Last loop
         if Tree.Contents (J).Construct.Sloc_Start.Index <= Offset then
            Last_Relevant_Construct := (Tree.Contents (J)'Access, J);
            It := Last_Relevant_Construct;

            while It /= Null_Construct_Tree_Iterator loop
               --  If we found the enclosing construct, nothing more to get.

               if Get_Construct (It).Sloc_End.Index >= Offset then
                  exit;
               end if;

               --  If the iterator is not anymore on the same scope, we have
               --  jumped in an enclosing scope, and therefore the last
               --  construct found is in fact unreacheable. It is the actual
               --  one.

               if Get_Parent_Scope (Tree, It)
                 /= Get_Parent_Scope (Tree, Last_Relevant_Construct)
               then
                  Last_Relevant_Construct := It;
               end if;

               It := Prev (Tree, It, Jump_Over);
            end loop;

            exit;
         end if;
      end loop;

      return Last_Relevant_Construct;
   end Get_Last_Relevant_Construct;

   --------------------
   -- Get_Name_Index --
   --------------------

   overriding function Get_Name_Index
     (Lang      : access Tree_Language;
      Construct : Simple_Construct_Information) return String
   is
      pragma Unreferenced (Lang);
   begin
      return Construct.Name.all;
   end Get_Name_Index;

   -----------------------
   -- Get_Documentation --
   -----------------------

   function Get_Documentation
     (Lang   : access Tree_Language;
      Entity : Entity_Access) return String
   is
      Tree                 : constant Construct_Tree :=
        Get_Tree (Get_File (Entity));
      Buffer               : constant GNAT.Strings.String_Access :=
        Get_Buffer (Get_File (Entity));
      Node : constant Construct_Tree_Iterator :=
        To_Construct_Tree_Iterator (Entity);

      Beginning, Current   : Natural;
      Result               : Unbounded_String;

      Type_Start, Type_End : Source_Location;
      Success              : Boolean;
      Language             : constant Language_Access :=
        Get_Language (Tree_Language'Class (Lang.all)'Access);
      Add_New_Line         : Boolean := False;
   begin
      Get_Documentation_Before
        (Context       => Get_Language_Context (Language).all,
         Buffer        => Buffer.all,
         Decl_Index    => Get_Construct (Node).Sloc_Start.Index,
         Comment_Start => Beginning,
         Comment_End   => Current);

      if Beginning = 0 then
         Get_Documentation_After
           (Context       => Get_Language_Context (Language).all,
            Buffer        => Buffer.all,
            Decl_Index    => Get_Construct (Node).Sloc_End.Index,
            Comment_Start => Beginning,
            Comment_End   => Current);
      end if;

      if Beginning /= 0 then
         Append
           (Result,
            Escape_Text
              (Comment_Block
                 (Language,
                  Buffer (Beginning .. Current),
                  Comment => False,
                  Clean   => True)));

         Add_New_Line := True;
      end if;

      if Get_Construct (Node).Category in Subprogram_Category then
         declare
            Sub_Iter               : Construct_Tree_Iterator :=
              Next (Tree, Node, Jump_Into);
            Has_Parameter          : Boolean := False;
            Biggest_Parameter_Name : Integer := 0;
         begin
            while Is_Parent_Scope (Node, Sub_Iter) loop
               if Get_Construct (Sub_Iter).Category = Cat_Parameter then
                  Add_New_Line := True;

                  if Get_Construct (Sub_Iter).Name'Length >
                    Biggest_Parameter_Name
                  then
                     Biggest_Parameter_Name :=
                       Get_Construct (Sub_Iter).Name'Length;
                  end if;
               end if;

               Sub_Iter := Next (Tree, Sub_Iter, Jump_Over);
            end loop;

            Sub_Iter := Next (Tree, Node, Jump_Into);

            while Is_Parent_Scope (Node, Sub_Iter) loop
               if Get_Construct (Sub_Iter).Category = Cat_Parameter then
                  if not Has_Parameter then
                     if Add_New_Line then
                        Append (Result, ASCII.LF & ASCII.LF);
                     end if;

                     Append
                       (Result, "<b>Parameters:</b>");
                     Has_Parameter := True;
                     Add_New_Line := True;
                  end if;

                  Append (Result, ASCII.LF);

                  Get_Referenced_Entity
                    (Language,
                     Buffer.all,
                     Get_Construct (Sub_Iter).all,
                     Type_Start,
                     Type_End,
                     Success);

                  Append
                    (Result, Escape_Text (Get_Construct (Sub_Iter).Name.all));

                  for J in Get_Construct (Sub_Iter).Name'Length + 1
                    .. Biggest_Parameter_Name
                  loop
                     Append (Result, " ");
                  end loop;

                  if Success then
                     Append
                       (Result,
                        " : " & Escape_Text
                          (Buffer (Type_Start.Index .. Type_End.Index)));
                  else
                     Append (Result, " : ???");
                  end if;
               end if;

               Sub_Iter := Next (Tree, Sub_Iter, Jump_Over);
            end loop;
         end;

         Get_Referenced_Entity
           (Language,
            Buffer.all,
            Get_Construct (Node).all,
            Type_Start,
            Type_End,
            Success);

         if Success then
            if Add_New_Line then
               Append (Result, ASCII.LF & ASCII.LF);
            end if;

            Append
              (Result,
               "<b>Return:</b>"
               & ASCII.LF
               & Escape_Text (Buffer (Type_Start.Index .. Type_End.Index)));
         end if;

      elsif Get_Construct (Node).Category in Data_Category then
         declare
            Var_Start, Var_End : Source_Location;
         begin
            Get_Referenced_Entity
              (Language,
               Buffer.all,
               Get_Construct (Node).all,
               Var_Start,
               Var_End,
               Success);

            if Success then
               if Add_New_Line then
                  Append (Result, ASCII.LF & ASCII.LF);
               end if;

               Append
                 (Result,
                  "<b>Type: </b>"
                  & Escape_Text (Buffer (Var_Start.Index .. Var_End.Index)));
            end if;
         end;
      end if;

      return To_String (Result);
   end Get_Documentation;

   -----------------
   -- Get_Profile --
   -----------------

   function Get_Profile
     (Lang     : access Tree_Language;
      Entity   : Entity_Access;
      Max_Size : Natural) return String
   is
      Tree                 : constant Construct_Tree :=
        Get_Tree (Get_File (Entity));
      Buffer               : constant GNAT.Strings.String_Access :=
        Get_Buffer (Get_File (Entity));
      Node : constant Construct_Tree_Iterator :=
        To_Construct_Tree_Iterator (Entity);

      Result : String (1 .. Max_Size);
      Result_Index : Integer := 0;
      Language             : constant Language_Access :=
        Get_Language (Tree_Language'Class (Lang.all)'Access);
      Type_Start, Type_End : Source_Location;
      Success : Boolean;

      function Append (Str : String) return Boolean;

      function Append (Str : String) return Boolean is
      begin
         if Result_Index + Str'Length <= Result'Last then
            Result
              (Result_Index + 1 .. Result_Index + Str'Length) := Str;

            Result_Index := Result_Index + Str'Length;

            return True;
         else
            if Result_Index + 1 > Result'Last then
               return False;
            else
               Result
                 (Result_Index + 1 .. Result'Last) := Str
                 (Str'First .. Str'First + (Result'Last - Result_Index - 1));

               return False;
            end if;
         end if;
      end Append;

   begin
      if Get_Construct (Node).Category in Subprogram_Category then
         declare
            Sub_Iter               : Construct_Tree_Iterator :=
              Next (Tree, Node, Jump_Into);
            Has_Parameter          : Boolean := False;
         begin
            while Is_Parent_Scope (Node, Sub_Iter) loop
               if Get_Construct (Sub_Iter).Category = Cat_Parameter then
                  if not Has_Parameter then
                     if not Append ("(") then
                        return Result;
                     end if;

                     Has_Parameter := True;
                  else
                     if not Append ("; ") then
                        return Result;
                     end if;
                  end if;

                  Get_Referenced_Entity
                    (Language,
                     Buffer.all,
                     Get_Construct (Sub_Iter).all,
                     Type_Start,
                     Type_End,
                     Success);

                  if not Append
                    (Escape_Text (Get_Construct (Sub_Iter).Name.all))
                  then
                     return Result;
                  end if;

                  if Success then
                     if not Append
                       (" : " & Escape_Text
                          (Buffer (Type_Start.Index .. Type_End.Index)))
                     then
                        return Result;
                     end if;
                  else
                     if not Append (" : ???") then
                        return Result;
                     end if;
                  end if;
               end if;

               Sub_Iter := Next (Tree, Sub_Iter, Jump_Over);
            end loop;

            if Has_Parameter then
               if not Append (")") then
                  return Result;
               end if;
            end if;
         end;

         Get_Referenced_Entity
           (Language,
            Buffer.all,
            Get_Construct (Node).all,
            Type_Start,
            Type_End,
            Success);

         if Success then
            if not Append
              (" return "
               & Escape_Text (Buffer (Type_Start.Index .. Type_End.Index)))
            then
               return Result;
            end if;
         end if;

         return Result (1 .. Result_Index);
      else
         return "";
      end if;
   end Get_Profile;

   ---------------------
   -- Get_Declaration --
   ---------------------

   function Get_Declaration
     (Lang   : access Tree_Language;
      Entity : Entity_Access) return Entity_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Entity;
   end Get_Declaration;

   ----------
   -- Diff --
   ----------

   overriding procedure Diff
     (Lang               : access Tree_Language;
      Old_Tree, New_Tree : Construct_Tree;
      Callback           : Diff_Callback)
   is
      pragma Unreferenced (Lang);
   begin
      for J in Old_Tree.Contents'Range loop
         Callback
           ((Old_Tree.Contents (J)'Access, J),
            Null_Construct_Tree_Iterator,
            Removed);
      end loop;

      for J in New_Tree.Contents'Range loop
         Callback
           (Null_Construct_Tree_Iterator,
            (New_Tree.Contents (J)'Access, J),
            Added);
      end loop;
   end Diff;

   ------------------
   -- Get_Language --
   ------------------

   overriding function Get_Language
     (Tree : access Unknown_Tree_Language) return Language_Access
   is
      pragma Unreferenced (Tree);
   begin
      return Unknown_Lang;
   end Get_Language;

   ----------------------
   -- Find_Declaration --
   ----------------------

   function Find_Declaration
     (Lang     : access Tree_Language;
      File     : Structured_File_Access;
      Line     : Integer;
      Column   : String_Index_Type) return Entity_Access
   is
      pragma Unreferenced (Lang, File, Line, Column);
   begin
      return Null_Entity_Access;
   end Find_Declaration;

   --------------------
   -- Find_Next_Part --
   --------------------

   function Find_Next_Part
     (Lang   : access Tree_Language;
      Entity : Entity_Access) return Entity_Access
   is
      pragma Unreferenced (Lang);
   begin
      return Entity;
   end Find_Next_Part;

   ----------------
   -- Get_Buffer --
   ----------------

   overriding function Get_Buffer
     (Provider : access File_Buffer_Provider;
      File     : GNATCOLL.VFS.Virtual_File) return GNAT.Strings.String_Access
   is
      pragma Unreferenced (Provider);
   begin
      return Read_File (File);
   end Get_Buffer;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Buffer_Provider_Access) is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (Buffer_Provider'Class, Buffer_Provider_Access);
   begin
      Internal (This);
   end Free;

   ---------
   -- Ref --
   ---------

   procedure Ref (File : Structured_File_Access) is
   begin
      File.Ref := File.Ref + 1;
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (File : Structured_File_Access) is
   begin
      if File /= null then
         File.Ref := File.Ref + 1;
      end if;
   end Unref;

   ------------------------------
   -- Is_Externally_Referenced --
   ------------------------------

   function Is_Externally_Referenced
     (File : Structured_File_Access) return Boolean
   is
   begin
      return File.Ref > 0;
   end Is_Externally_Referenced;

   -----------
   -- Free --
   -----------

   procedure Free (File : in out Structured_File_Access) is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (Structured_File, Structured_File_Access);
   begin
      Free_Annotations (File.Tree);
      Free (File.Tree);
      Free (File.Db_Data_Tree, File.Db);
      Free (File.Line_Starts);
      Free (File.Cache_Buffer);

      Internal (File);
   end Free;

   --------------
   -- Get_Tree --
   --------------

   function Get_Tree (File : Structured_File_Access) return Construct_Tree is
   begin
      if File /= null then
         return File.Tree;
      else
         return Null_Construct_Tree;
      end if;
   end Get_Tree;

   ----------------
   -- Get_Buffer --
   ----------------

   Empty_String : aliased String := "";

   function Get_Buffer
     (File : Structured_File_Access) return GNAT.Strings.String_Access
   is
   begin
      if File = null then
         return Empty_String'Access;
      elsif File.Cache_Buffer = null then
         File.Cache_Buffer := Get_Buffer (File.Db.Provider, File.File);
      end if;

      return File.Cache_Buffer;
   end Get_Buffer;

   -------------------
   -- Get_File_Path --
   -------------------

   function Get_File_Path
     (File : Structured_File_Access) return Virtual_File is
   begin
      if File = null then
         return No_File;
      else
         return File.File;
      end if;
   end Get_File_Path;

   ------------------------
   -- Get_Offset_Of_Line --
   ------------------------

   function Get_Offset_Of_Line
     (File : Structured_File_Access; Line : Integer)
      return String_Index_Type
   is
   begin
      if File.Line_Starts = null then
         declare
            Lines       : Line_Start_Indexes_Access :=
              new Line_Start_Indexes (1 .. 1000);
            Buffer      : constant
              GNAT.Strings.String_Access := Get_Buffer (File);
            Lines_Index : Integer := 2;
            Tmp_Lines   : Line_Start_Indexes_Access;
         begin
            Lines (1) := 1;

            for J in Buffer'Range loop
               if Buffer (J) = ASCII.LF then
                  if Lines_Index > Lines'Last then
                     Tmp_Lines := Lines;
                     Lines := new Line_Start_Indexes (1 .. Lines'Last * 2);
                     Lines (1 .. Tmp_Lines'Last) := Tmp_Lines.all;
                     Free (Tmp_Lines);
                  end if;

                  Lines (Lines_Index) := String_Index_Type (J) + 1;
                  Lines_Index := Lines_Index + 1;
               end if;
            end loop;

            File.Line_Starts := new Line_Start_Indexes'
              (Lines (1 .. Lines_Index - 1));
            Free (Lines);
         end;
      end if;

      return File.Line_Starts (Line);
   end Get_Offset_Of_Line;

   -----------------------
   -- To_Visible_Column --
   -----------------------

   function To_Visible_Column
     (File         : Structured_File_Access;
      Line        : Integer;
      Line_Offset : String_Index_Type) return Visible_Column_Type
   is
      Tab_Width : constant := 8;

      Str : constant GNAT.Strings.String_Access := Get_Buffer (File);
      Current_Index : String_Index_Type  := Get_Offset_Of_Line (File, Line);
      Current_Col   : Visible_Column_Type := 1;
   begin
      loop
         exit when Current_Index - Get_Offset_Of_Line (File, Line) + 1
           >= Line_Offset;

         if Natural (Current_Index) <= Str'Last
           and then Str (Natural (Current_Index)) = ASCII.HT
         then
            Current_Col := Current_Col + Visible_Column_Type (Tab_Width) -
              ((Current_Col - 1) mod Visible_Column_Type (Tab_Width));
         else
            Current_Col := Current_Col + 1;
         end if;

         Current_Index := Current_Index + 1;
      end loop;

      return Current_Col;
   end To_Visible_Column;

   --------------------------
   -- To_Line_String_Index --
   --------------------------

   function To_Line_String_Index
     (File   : Structured_File_Access;
      Line   : Integer;
      Column : Visible_Column_Type) return String_Index_Type
   is
      Current_Index : String_Index_Type := Get_Offset_Of_Line (File, Line);
   begin
      Skip_To_Column (Buffer  => Get_Buffer (File).all,
                      Columns => Integer (Column),
                      Index   => Natural (Current_Index));

      return String_Index_Type
        (Current_Index - Get_Offset_Of_Line (File, Line)) + 1;
   end To_Line_String_Index;

   ---------------------
   -- To_String_Index --
   ---------------------

   function To_String_Index
     (File   : Structured_File_Access;
      Line   : Integer;
      Column : Visible_Column_Type) return String_Index_Type is
   begin
      return Get_Offset_Of_Line (File, Line)
        + To_Line_String_Index (File, Line, Column) - 1;
   end To_String_Index;

   --------------------
   -- To_Line_Column --
   --------------------

   procedure To_Line_Column
     (File                 : Structured_File_Access;
      Absolute_Byte_Offset : String_Index_Type;
      Line                 : out Integer;
      Column               : out Visible_Column_Type)
   is
      --  Dummy initialization, to force recomputation of file offsets if
      --  needed.
      First_Line : String_Index_Type := Get_Offset_Of_Line (File, 1);
      pragma Unreferenced (First_Line);

      Index_In_Line : String_Index_Type;
   begin
      Line := -1;

      for J in File.Line_Starts'Range loop
         if File.Line_Starts (J) > Absolute_Byte_Offset then
            Line := J - 1;

            exit;
         end if;
      end loop;

      if Line = -1 then
         Line := File.Line_Starts'Last;
      end if;

      Index_In_Line := Absolute_Byte_Offset -
        Get_Offset_Of_Line (File, Line) + 1;

      Column := To_Visible_Column (File, Line, Index_In_Line);
   end To_Line_Column;

   ------------------
   -- Get_Language --
   ------------------

   function Get_Tree_Language
     (File : Structured_File_Access) return Tree_Language_Access is
   begin
      return File.Tree_Lang;
   end Get_Tree_Language;

   ---------------------
   -- Update_Contents --
   ---------------------

   procedure Update_Contents (File : Structured_File_Access) is
   begin
      Internal_Update_Contents (File, False);
   end Update_Contents;

   ------------------------------
   -- Internal_Update_Contents --
   ------------------------------

   procedure Internal_Update_Contents
     (File : Structured_File_Access; Is_New_File : Boolean)
   is
      Buffer     : GNAT.Strings.String_Access;
      Constructs : aliased Construct_List;

      Current_Update_Kind : Update_Kind;

      New_Tree         : Construct_Tree;
      New_Db_Data_Tree : Construct_Db_Data_Access;

      Old_Tree : Construct_Tree := null;

      procedure Add_New_Construct_If_Needed (It : Construct_Tree_Iterator);

      procedure Diff_Callback
        (Old_Obj, New_Obj : Construct_Tree_Iterator; Kind : Diff_Kind);

      ---------------------------------
      -- Add_New_Construct_If_Needed --
      ---------------------------------

      procedure Add_New_Construct_If_Needed (It : Construct_Tree_Iterator) is
         Data      : Trie_Additional_Data;
         Construct : constant access Simple_Construct_Information :=
                       Get_Construct (It);
      begin
         --  We add only named constructs in the database, and we dismiss some
         --  categories.

         if Construct.Name = null
           or else Construct.Category = Cat_Parameter
           or else Construct.Category = Cat_Field
           or else Construct.Category = Cat_With
           or else Construct.Category = Cat_Use
         then
            return;
         end if;

         Data.File := File;

         Construct_Db_Trie.Insert
           (File.Db.Entities_Db'Access,
            It,
            Data,
            File.Tree_Lang,
            New_Db_Data_Tree (It.Index));
      end Add_New_Construct_If_Needed;

      -------------------
      -- Diff_Callback --
      -------------------

      procedure Diff_Callback
        (Old_Obj, New_Obj : Construct_Tree_Iterator; Kind : Diff_Kind) is
      begin
         case Kind is
            when Removed =>
               Current_Update_Kind := Structural_Change;

               Delete
                 (File.Db.Entities_Db'Access,
                  File.Db_Data_Tree (Old_Obj.Index));

               Construct_Annotations_Pckg.Free
                 (Get_Annotation_Container (File.Tree, Old_Obj).all);

            when Added =>
               Current_Update_Kind := Structural_Change;
               Add_New_Construct_If_Needed (New_Obj);

            when Preserved =>
               if Get_Construct (Old_Obj).Attributes /=
                 Get_Construct (New_Obj).Attributes
                 or else Get_Construct (Old_Obj).Is_Declaration /=
                 Get_Construct (New_Obj).Is_Declaration
                 or else Get_Construct (Old_Obj).Visibility /=
                 Get_Construct (Old_Obj).Visibility
               then
                  Current_Update_Kind := Structural_Change;
               end if;

               New_Db_Data_Tree (New_Obj.Index) :=
                 File.Db_Data_Tree (Old_Obj.Index);

               --  Copy the annotation from the old obj to the new obj

               New_Tree.Contents (New_Obj.Index).Annotations :=
                 File.Tree.Contents (Old_Obj.Index).Annotations;

               --  If we're in a structural change, there may have been object
               --  inserted / removed before this construct, so we have to
               --  update the index and the persistent entity.

               if Current_Update_Kind = Structural_Change then
                  --  Update the construct wrapper if the construct is stored
                  --  in the database.

                  if New_Db_Data_Tree (New_Obj.Index) /=
                    Construct_Db_Trie.Null_Construct_Trie_Index
                  then
                     Replace
                       (File.Db.Entities_Db'Access,
                        New_Db_Data_Tree (New_Obj.Index),
                        New_Obj,
                        (File => File));
                  end if;

                  --  Update the persistent annotation if any

                  declare
                     use Construct_Annotations_Pckg;

                     Annotations : constant access Annotation_Container :=
                       Get_Annotation_Container (New_Tree, New_Obj);
                     Persistent_Annotation : Annotation (Other_Kind);
                  begin
                     if Is_Set
                       (Annotations.all, File.Db.Persistent_Entity_Key)
                     then
                        Get_Annotation
                          (Annotations.all,
                           File.Db.Persistent_Entity_Key,
                           Persistent_Annotation);

                        Entity_Persistent_Annotation
                          (Persistent_Annotation.Other_Val.all).Info.Index :=
                          New_Obj.Index;
                     end if;
                  end;
               end if;
         end case;
      end Diff_Callback;

   begin
      --  If update are temporary disabled, mark it and return.

      if File.Lock_Depth > 0 then
         if File.Lock_Kind = Defer_Updates then
            File.Update_Locked := True;
         end if;

         return;
      end if;

      --  Phase 1 : analyze the new tree

      Buffer := Get_Buffer (File.Db.Provider, File.File);

      --  ??? We are assuming that Buffer is encoded in UTF8, is this the case?
      Parse_Constructs (File.Lang, Buffer.all, Constructs);
      New_Tree := To_Construct_Tree (Constructs'Access, True);

      Analyze_Referenced_Identifiers
        (Buffer.all, File.Lang, File.Db, New_Tree);
      Analyze_Constructs_Identifiers (File.Db, New_Tree);
      New_Db_Data_Tree := new Construct_Db_Data_Array
        (1 .. New_Tree.Contents'Length);
      New_Db_Data_Tree.all :=
        (others => Construct_Db_Trie.Null_Construct_Trie_Index);

      --  Phase 2 : replace previous content by the new one

      if Is_New_File then
         Current_Update_Kind := Full_Change;

         declare
            It : Construct_Tree_Iterator := First (New_Tree);
         begin
            while It /= Null_Construct_Tree_Iterator loop
               Add_New_Construct_If_Needed (It);

               It := Next (New_Tree, It, Jump_Into);
            end loop;
         end;
      else
         Current_Update_Kind := Minor_Change;
         New_Tree.Annotations := File.Tree.Annotations;

         Diff
           (File.Tree_Lang,
            File.Tree,
            New_Tree,
            Diff_Callback'Unrestricted_Access);

         File.Tree.Annotations :=
           Tree_Annotations_Pckg.Null_Annotation_Container;

         Free (File.Cache_Buffer);
         Unchecked_Free (File.Db_Data_Tree);
         Free (File.Line_Starts);

         Old_Tree := File.Tree;
      end if;

      Free (Constructs);
      Free (Buffer);

      File.Tree := New_Tree;
      File.Db_Data_Tree := New_Db_Data_Tree;

      --  Notify database assistants

      declare
         Cur : Database_Listeners.Cursor;
      begin
         Cur := First (File.Db.Listeners);

         while Cur /= Database_Listeners.No_Element loop
            File_Updated (Element (Cur), File, Old_Tree, Current_Update_Kind);
            Cur := Next (Cur);
         end loop;
      end;

      Free (Old_Tree);
   end Internal_Update_Contents;

   ----------
   -- Lock --
   ----------

   function Lock_Updates
     (File : Structured_File_Access;
      Kind : Lock_Kind_Type := Defer_Updates) return Update_Lock
   is
      Last_Lock_Kind : constant Lock_Kind_Type := File.Lock_Kind;
   begin
      if File /= null then
         File.Lock_Depth := File.Lock_Depth + 1;
         File.Lock_Kind := Kind;
      end if;

      return
        (Limited_Controlled with
         File_Locked => File,
         Last_Lock_Kind => Last_Lock_Kind);
   end Lock_Updates;

   ------------
   -- Unlock --
   ------------

   procedure Unlock (This : in out Update_Lock) is
   begin
      if This.File_Locked /= null then
         This.File_Locked.Lock_Depth := This.File_Locked.Lock_Depth - 1;

         if This.File_Locked.Lock_Depth = 0
           and then This.File_Locked.Update_Locked
         then
            Update_Contents (This.File_Locked);
            This.File_Locked.Update_Locked := False;
         end if;

         --  Revert to the previous lock kind
         This.File_Locked.Lock_Kind := This.Last_Lock_Kind;
         This.File_Locked := null;
      end if;
   end Unlock;

   --------------
   -- Finalize --
   --------------

   overriding procedure Finalize (This : in out Update_Lock) is
   begin
      Unlock (This);
   end Finalize;

   ---------
   -- "=" --
   ---------

   overriding function "=" (Left, Right : Structured_File) return Boolean is
   begin
      return Left.File = Right.File;
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Structured_File_Access) return Boolean is
   begin
      return Left.File < Right.File;
   end "<";

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Construct_Database_Access) is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (Construct_Database, Construct_Database_Access);
   begin
      Destroy (This);
      Internal (This);
   end Free;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (Db         : Construct_Database_Access;
      Provider   : Buffer_Provider_Access;
      Lg_Handler : Abstract_Language_Handler)
   is
   begin
      Db.Tree_Registry := Tree_Annotations_Pckg.Create_Annotation_Key_Registry;
      Db.Construct_Registry :=
        Construct_Annotations_Pckg.Create_Annotation_Key_Registry;
      Construct_Annotations_Pckg.Get_Annotation_Key
        (Db.Construct_Registry, Db.Persistent_Entity_Key);
      Db.Provider := Provider;
      Db.Null_Structured_File.Db := Db;
      Db.Lg_Handler := Lg_Handler;
   end Initialize;

   -------------------
   -- Get_Or_Create --
   -------------------

   function Get_Or_Create
     (Db        : Construct_Database_Access;
      File      : Virtual_File) return Structured_File_Access
   is
      Lang      : Language_Access;
      Tree_Lang : Tree_Language_Access;
   begin
      if Contains (Db.Files_Db, File) then
         return Element (Db.Files_Db, File);
      else
         if not Is_Regular_File (File) then
            return Db.Null_Structured_File'Access;
         end if;

         Lang := Db.Lg_Handler.Get_Language_From_File (File);

         if Lang = Unknown_Lang then
            --  Files that are not yet associated with a language may be later
            --  on, e.g. after project initialization. So we don't want to
            --  force the association between a structured file and an unknown
            --  language. There will be no information to analyze anyway.
            return Db.Null_Structured_File'Access;
         end if;

         Tree_Lang := Db.Lg_Handler.Get_Tree_Language_From_File (File);

         declare
            New_File  : constant Structured_File_Access := new Structured_File;
         begin
            New_File.File := File;
            New_File.Lang := Lang;
            New_File.Tree_Lang := Tree_Lang;
            New_File.Db := Db;

            Insert (Db.Files_Db, File, New_File);
            Insert (Db.Sorted_Files_Db, New_File);

            Internal_Update_Contents (New_File, True);

            return New_File;
         end;
      end if;
   end Get_Or_Create;

   -----------------
   -- Remove_File --
   -----------------

   procedure Remove_File
     (Db        : Construct_Database_Access;
      File      : Virtual_File)
   is
      S_File : Structured_File_Access;
   begin
      if Contains (Db.Files_Db, File) then
         S_File := Element (Db.Files_Db, File);

         if Is_Externally_Referenced (S_File) then
            --  In this case, the file is still used somewhere else so we can't
            --  remove it. Typically, it's used by the outline.

            return;
         end if;

         --  Notify database assistants

         declare
            Cur : Database_Listeners.Cursor;
         begin
            Cur := First (Db.Listeners);

            while Cur /= Database_Listeners.No_Element loop
               File_Updated
                 (Element (Cur), S_File, Get_Tree (S_File), Removed);
               Cur := Next (Cur);
            end loop;
         end;

         --  Perform the actual deletion

         Db.Files_Db.Delete (File);
         Db.Sorted_Files_Db.Delete (S_File);

         Free (S_File);
      end if;
   end Remove_File;

   ---------------------
   -- Update_Contents --
   ---------------------

   procedure Update_Contents
     (Db : access Construct_Database; File : Virtual_File) is
   begin
      if Contains (Db.Files_Db, File) then
         Update_Contents (Element (Db.Files_Db, File));
      end if;
   end Update_Contents;

   -----------
   -- Clear --
   -----------

   procedure Clear (Db : access Construct_Database) is
      C : File_Map.Cursor := First (Db.Files_Db);
      Garbage : Structured_File_Access;
   begin
      declare
         Cur : Database_Listeners.Cursor;
      begin
         Cur := First (Db.Listeners);

         while Cur /= Database_Listeners.No_Element loop
            Before_Clear_Db (Element (Cur), Db);
            Cur := Next (Cur);
         end loop;
      end;

      while C /= File_Map.No_Element loop
         Garbage := Element (C);
         Free (Garbage);

         C := Next (C);
      end loop;

      Clear (Db.Entities_Db'Access);
      Clear (Db.Files_Db);
      Clear (Db.Sorted_Files_Db);
   end Clear;

   ----------
   -- Free --
   ----------

   procedure Destroy (Db : access Construct_Database) is
      Assistant_Cur : Assistant_Map.Cursor;
      Assistant     : Database_Assistant_Access;

      procedure Unchecked_Free_Assistant is new
        Standard.Ada.Unchecked_Deallocation
          (Database_Assistant'Class, Database_Assistant_Access);
      procedure Unchecked_Free is new Standard.Ada.Unchecked_Deallocation
        (Buffer_Provider'Class, Buffer_Provider_Access);
   begin
      Clear (Db);

      Assistant_Cur := First (Db.Assistants);

      while Assistant_Cur /= Assistant_Map.No_Element loop
         Assistant := Element (Assistant_Cur);

         Free (Assistant.all);
         Unchecked_Free_Assistant (Assistant);
         Assistant_Cur := Next (Assistant_Cur);
      end loop;

      Tree_Annotations_Pckg.Free (Db.Tree_Registry);
      Construct_Annotations_Pckg.Free (Db.Construct_Registry);

      Unchecked_Free (Db.Provider);
   end Destroy;

   ----------
   -- Free --
   ----------

   procedure Free
     (This : in out Construct_Db_Data_Access;
      Db    : access Construct_Database)
   is
   begin
      for J in This'Range loop
         Delete (Db.Entities_Db'Access, This (J));
      end loop;

      Unchecked_Free (This);
   end Free;

   -----------
   -- Start --
   -----------

   function Start
     (Db : access Construct_Database; Prefix : String; Is_Partial : Boolean)
      return Construct_Db_Iterator
   is
      It : Construct_Db_Iterator;
   begin
      It.It := Start (Db.Entities_Db'Access, Prefix, Is_Partial);

      return It;
   end Start;

   -------------------
   -- Get_Construct --
   -------------------

   function Get_Construct
     (It : Construct_Db_Iterator) return Construct_Tree_Iterator
   is
   begin
      return Get_Construct_It (It.It);
   end Get_Construct;

   --------------------
   -- Get_Current_Id --
   --------------------

   function Get_Current_Id (It : Construct_Db_Iterator) return String is
   begin
      return Get_Index (It.It);
   end Get_Current_Id;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (It : Construct_Db_Iterator) return Structured_File_Access is
   begin
      return Get_Additional_Data (It.It).File;
   end Get_File;

   ---------
   -- Get --
   ---------

   function Get (It : Construct_Db_Iterator) return Entity_Access is
   begin
      return
        (File => Get_Additional_Data (It.It).File,
         It   => Get_Construct_It (It.It));
   end Get;

   ----------
   -- Next --
   ----------

   procedure Next (It : in out Construct_Db_Iterator) is
   begin
      Next (It.It);
   end Next;

   ------------
   -- At_End --
   ------------

   function At_End (It : Construct_Db_Iterator) return Boolean is
   begin
      return At_End (It.It);
   end At_End;

   --------------
   -- Is_Valid --
   --------------

   function Is_Valid (It : Construct_Db_Iterator) return Boolean is
   begin
      return Is_Valid (It.It);
   end Is_Valid;

   ----------
   -- Free --
   ----------

   procedure Free (It : in out Construct_Db_Iterator) is
   begin
      Free (It.It);
   end Free;

   --------------------------------------
   -- Get_Tree_Annotation_Key_Registry --
   --------------------------------------

   function Get_Tree_Annotation_Key_Registry
     (Db : Construct_Database_Access)
      return access Tree_Annotations_Pckg.Annotation_Key_Registry is
   begin
      return Db.Tree_Registry'Access;
   end Get_Tree_Annotation_Key_Registry;

   -------------------------------------------
   -- Get_Construct_Annotation_Key_Registry --
   -------------------------------------------

   function Get_Construct_Annotation_Key_Registry
     (Db : Construct_Database_Access)
      return access Construct_Annotations_Pckg.Annotation_Key_Registry
   is
   begin
      return Db.Construct_Registry'Access;
   end Get_Construct_Annotation_Key_Registry;

   ---------------
   -- Get_Files --
   ---------------

   function Start_File_Search
     (Db : Construct_Database) return File_Set.Cursor
   is
   begin
      return First (Db.Sorted_Files_Db);
   end Start_File_Search;

   ----------------------
   -- To_Entity_Access --
   ----------------------

   function To_Entity_Access
     (File       : Structured_File_Access;
      Construct  : Construct_Tree_Iterator) return Entity_Access
   is
      Result : Entity_Access;
   begin
      if Construct = Null_Construct_Tree_Iterator then
         return Null_Entity_Access;
      else
         pragma Assert (Construct.Index in File.Tree.Contents'Range);

         Result.File := File;
         Result.It := Construct;

         return Result;
      end if;
   end To_Entity_Access;

   --------------------------------
   -- To_Construct_Tree_Iterator --
   --------------------------------

   function To_Construct_Tree_Iterator
     (Entity : Entity_Access) return Construct_Tree_Iterator
   is
   begin
      return Entity.It;
   end To_Construct_Tree_Iterator;

   --------------
   -- Get_File --
   --------------

   function Get_File (Entity : Entity_Access) return Structured_File_Access is
   begin
      return Entity.File;
   end Get_File;

   -------------------
   -- Get_Construct --
   -------------------

   function Get_Construct
     (Entity : Entity_Access) return access Simple_Construct_Information is
   begin
      return Get_Construct (Entity.It);
   end Get_Construct;

   ---------------
   -- To_String --
   ---------------

   function To_String (Entity : Entity_Access) return String is
   begin
      if Entity = Null_Entity_Access then
         return "[null]";
      else
         return To_String
           (Entity.It) & " from " & String (Full_Name (Entity.File.File).all);
      end if;
   end To_String;

   --------------
   -- Contains --
   --------------

   function Contains
     (Scope : Entity_Access; Entity : Entity_Access) return Boolean
   is
   begin
      if Scope.File = Entity.File then
         declare
            Tree : constant Construct_Tree := Get_Tree (Scope.File);
         begin
            return Encloses (Tree, Scope.It, Entity.It);
         end;
      else
         return False;
      end if;
   end Contains;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Entity_Array_Access) is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (Entity_Array, Entity_Array_Access);
   begin
      Internal (This);
   end Free;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Entity_Access) return Boolean is
   begin
      --  Since file comparison is very expensive, it has to be tried first

      return Left.It < Right.It
        or else (Left.It = Right.It
          and then Left.File < Right.File);
   end "<";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Entity_Access) return Boolean is
   begin
      --  Since file comparison is very expensive, it has to be tried first

      return Left.It = Right.It and then Left.File = Right.File;
   end "=";

   ----------------------
   -- To_Entity_Access --
   ----------------------

   function To_Entity_Access
     (Entity : Entity_Persistent_Access) return Entity_Access is
   begin
      if not Exists (Entity) then
         return Null_Entity_Access;
      else
         return To_Entity_Access
           (Entity.File,
            (Get_Tree (Entity.File).Contents (Entity.Index)'Access,
             Entity.Index));
      end if;
   end To_Entity_Access;

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Entity_Persistent_Access) return Boolean is
   begin
      if Left = null then
         return False;
      elsif Right = null then
         return True;
      else
         if Left.all.File = Right.all.File then
            if Left.all.Index = Right.all.Index then
               return Left.all'Address < Right.all'Address;
            else
               return Left.all.Index < Right.all.Index;
            end if;
         else
            return Left.all.File < Right.all.File;
         end if;
      end if;
   end "<";

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Entity_Persistent_Array_Access) is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (Entity_Persistent_Array, Entity_Persistent_Array_Access);
   begin
      Internal (This);
   end Free;

   ---------------------------------
   -- To_Entity_Persistent_Access --
   ---------------------------------

   function To_Entity_Persistent_Access
     (Entity : Entity_Access) return Entity_Persistent_Access
   is
      use Construct_Annotations_Pckg;

      Db : Construct_Database_Access;

      It          : constant Construct_Tree_Iterator :=
        To_Construct_Tree_Iterator (Entity);
      Annotations : access Annotation_Container;

      Persistent_Annotation : Annotation (Other_Kind);
   begin
      if It = Null_Construct_Tree_Iterator then
         return Null_Entity_Persistent_Access;
      end if;

      Db := Get_Database (Get_File (Entity));
      Annotations := Get_Annotation_Container
        (Get_Tree (Get_File (Entity)), It);

      if Is_Set (Annotations.all, Db.Persistent_Entity_Key) then
         Get_Annotation
           (Annotations.all, Db.Persistent_Entity_Key, Persistent_Annotation);
      else
         Persistent_Annotation.Other_Val := new Entity_Persistent_Annotation'
           (Info => new Entity_Persistent_Info'
              (Exists => True,
               File   => Get_File (Entity),
               Index  => To_Construct_Tree_Iterator (Entity).Index,
               Refs   => 0));

         Set_Annotation
           (Annotations.all, Db.Persistent_Entity_Key, Persistent_Annotation);
      end if;

      return Entity_Persistent_Annotation
        (Persistent_Annotation.Other_Val.all).Info;
   end To_Entity_Persistent_Access;

   -------------------
   -- Get_Construct --
   -------------------

   function Get_Construct
     (Entity : Entity_Persistent_Access) return Simple_Construct_Information is
   begin
      return Get_Tree (Entity.File).
        Contents (Natural (Entity.Index)).Construct;
   end Get_Construct;

   ---------
   -- Ref --
   ---------

   procedure Ref (Entity : in out Entity_Persistent_Access) is
   begin
      if Entity /= Null_Entity_Persistent_Access then
         Entity.Refs := Entity.Refs + 1;
      end if;
   end Ref;

   -----------
   -- Unref --
   -----------

   procedure Unref (Entity : in out Entity_Persistent_Access) is
      procedure Free is new Standard.Ada.Unchecked_Deallocation
        (Entity_Persistent_Info, Entity_Persistent_Access);
   begin
      if Entity /= Null_Entity_Persistent_Access then
         Entity.Refs := Entity.Refs - 1;

         if Entity.Refs = 0 and then not Entity.Exists then
            Free (Entity);
         end if;

         Entity := Null_Entity_Persistent_Access;
      end if;
   end Unref;

   -------------
   -- Is_Null --
   -------------

   function Exists (Entity : Entity_Persistent_Access) return Boolean is
   begin
      return Entity /= null and then Entity.Exists;
   end Exists;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (Entity : Entity_Persistent_Access) return Structured_File_Access is
   begin
      return Entity.File;
   end Get_File;

   ---------------------------
   -- Add_Database_Listener --
   ---------------------------

   procedure Add_Database_Listener
     (Db       : Construct_Database_Access;
      Listener : Database_Listener_Access)
   is
   begin
      Db.Listeners.Append (Listener);
   end Add_Database_Listener;

   ------------------------------
   -- Remove_Database_Listener --
   ------------------------------

   procedure Remove_Database_Listener
     (Db       : Construct_Database_Access;
      Listener : Database_Listener_Access)
   is
      It : Database_Listeners.Cursor := First (Db.Listeners);
   begin
      while It /= Database_Listeners.No_Element loop
         if Element (It) = Listener then
            Db.Listeners.Delete (It);

            return;
         end if;

         It := Next (It);
      end loop;
   end Remove_Database_Listener;

   ------------------------
   -- Register_Assistant --
   ------------------------

   procedure Register_Assistant
     (Db        : Construct_Database_Access;
      Name      : String;
      Assistant : Database_Assistant_Access)
   is
   begin
      Insert (Db.Assistants, Name, Assistant);
      Append (Db.Listeners, Database_Listener_Access (Assistant));
   end Register_Assistant;

   -------------------
   -- Get_Assistant --
   -------------------

   function Get_Assistant
     (Db : Construct_Database_Access; Name : String)
      return Database_Assistant_Access
   is
   begin
      return Element (Db.Assistants, Name);
   end Get_Assistant;

   ------------------
   -- Get_Database --
   ------------------

   function Get_Database
     (File : Structured_File_Access) return Construct_Database_Access is
   begin
      return Construct_Database_Access (File.Db);
   end Get_Database;

   ----------
   -- Free --
   ----------

   overriding procedure Free
     (Obj : in out Entity_Persistent_Annotation)
   is
      procedure Internal is new Standard.Ada.Unchecked_Deallocation
        (Entity_Persistent_Info, Entity_Persistent_Access);
   begin
      Obj.Info.Exists := False;

      if Obj.Info.Refs = 0 then
         Internal (Obj.Info);
      end if;
   end Free;

   --------------------
   -- Get_Identifier --
   --------------------

   overriding function Get_Identifier
     (Manager : access Construct_Database; Name : String)
      return Distinct_Identifier
   is
   begin
      return Distinct_Identifier
        (Get_Name_Index (Manager.Entities_Db'Access, Name));
   end Get_Identifier;

   --------------------
   -- Get_Identifier --
   --------------------

   function Get_Identifier
     (Entity : Entity_Access) return Distinct_Identifier
   is
   begin
      return Entity.It.Node.Id;
   end Get_Identifier;

   ------------------------------
   -- Analyze_File_Differences --
   ------------------------------

   procedure Analyze_File_Differences
     (Db                         : Construct_Database_Access;
      New_Set                    : File_Array;
      Removed_Files, Added_Files : out File_Array_Access)
   is
      Local_Removed : File_Array (1 .. Integer (Db.Files_Db.Length));
      Local_Added : File_Array (1 .. New_Set'Length);

      Removed_Index : Integer := 1;
      Added_Index   : Integer := 1;

      Cur : File_Map.Cursor;

      Contained    : Virtual_File;

      New_File_Map : File_Map.Map;
   begin
      --  Computes files removed in the new set

      for J in New_Set'Range loop
         if not New_File_Map.Contains (New_Set (J)) then
            --  It's possible to see duplicate files in the input list, in
            --  particular in case of links. In this case, we just ignore
            --  these duplicate. Otherwise, the file is inserted in the map.

            New_File_Map.Insert (New_Set (J), null);
         end if;
      end loop;

      Cur := Db.Files_Db.First;

      while Cur /= File_Map.No_Element loop
         Contained := File_Map.Key (Cur);

         if not New_File_Map.Contains (Contained) then
            Local_Removed (Removed_Index) := Contained;
            Removed_Index := Removed_Index + 1;
         end if;

         Cur := File_Map.Next (Cur);
      end loop;

      --  Computes files added in the new set

      for J in New_Set'Range loop
         if not Db.Files_Db.Contains (New_Set (J)) then
            Local_Added (Added_Index) := New_Set (J);
            Added_Index := Added_Index + 1;
         end if;
      end loop;

      --  Build the result

      Removed_Files := new File_Array'
        (Local_Removed (1 .. Removed_Index - 1));
      Added_Files := new File_Array'
        (Local_Added (1 .. Added_Index - 1));
   end Analyze_File_Differences;

   ---------
   -- "=" --
   ---------

   overriding function "="
     (Left, Right : Structured_File_Access) return Boolean
   is
      type Tmp_Acc is access all Structured_File;
   begin
      --  The null definition include both cases where the actual value of the
      --  pointer is null, and cases where the file path is unknown. The latter
      --  may have been returned by a Get_Or_Create.

      return
        ((Tmp_Acc (Left) = null or else Left.File = No_File)
         and then (Tmp_Acc (Right) = null or else Right.File = No_File))
        or else Tmp_Acc (Left) = Tmp_Acc (Right);
   end "=";

end Language.Tree.Database;
