-----------------------------------------------------------------------
--                               G P S                               --
--                                                                   --
--                        Copyright (C) 2002                         --
--                            ACT-Europe                             --
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

with Ada.Exceptions;        use Ada.Exceptions;
with Ada.Unchecked_Deallocation;

with GNAT.Regpat;           use GNAT.Regpat;
with GNAT.Case_Util;        use GNAT.Case_Util;

with Basic_Types;           use Basic_Types;
with String_Utils;          use String_Utils;
with Language;              use Language;
with Language.Unknown;      use Language.Unknown;

with Codefix.Merge_Utils; use Codefix.Merge_Utils;

package body Codefix.Text_Manager is

   ------------------
   -- Compare_Last --
   ------------------

   function Compare_Last (Str_1, Str_2 : String) return Boolean is
      Str_1_Lower : String := Str_1;
      Str_2_Lower : String := Str_2;
   begin
      To_Lower (Str_1_Lower);
      To_Lower (Str_2_Lower);

      if Str_1'Length < Str_2'Length then
         return Str_1_Lower = Str_2_Lower
           (Str_2'Last - Str_1'Length + 1 .. Str_2'Last);
      else
         return Str_2_Lower = Str_1_Lower
           (Str_1'Last - Str_2'Length + 1 .. Str_1'Last);
      end if;
   end Compare_Last;

   --------------
   -- Is_Blank --
   --------------

   function Is_Blank (Str : String) return Boolean is
   begin
      for J in Str'Range loop
         if not Is_Blank (Str (J)) then
            return False;
         end if;
      end loop;

      return True;
   end Is_Blank;

   --------------
   -- Is_Blank --
   --------------

   function Is_Blank (Char : Character) return Boolean is
   begin
      return Char = ' ' or else Char = ASCII.HT;
   end Is_Blank;

   -------------------
   -- Is_In_Comment --
   -------------------

   --  ??? Check if -- are in a string
   function Is_In_Comment (Str : String; J : Natural) return Boolean is
      Index_Found : Natural := Str'First;
   begin
      Skip_To_String (Str, Index_Found, "--");
      return J > Index_Found;
   end Is_In_Comment;

   ---------------
   -- Normalize --
   ---------------

   function Normalize (Str : Basic_Types.String_Access) return String is
   begin
      if Str /= null then
         return Reduce (Str.all);
      else
         return "";
      end if;
   end Normalize;

   -------------------------
   -- Without_Last_Blanks --
   -------------------------

   function Without_Last_Blanks (Str : String) return String is
   begin
      for J in reverse Str'Range loop
         if not Is_Blank (Str (J)) then
            return Str (Str'First .. J);
         end if;
      end loop;

      return "";
   end Without_Last_Blanks;

   ------------------
   -- Is_Separator --
   ------------------

   function Is_Separator (Char : Character) return Boolean is
   begin
      if Is_Blank (Char) then
         return True;
      end if;

      case Char is
         when '.' | ',' | ';' => return True;
         when others => return False;
      end case;
   end Is_Separator;


   ----------------------------------------------------------------------------
   --  type Text_Cursor
   ----------------------------------------------------------------------------

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Text_Cursor) return Boolean is
   begin
      return Left.Line < Right.Line
        or else (Left.Line = Right.Line and then Left.Col < Right.Col);
   end "<";

   ---------
   -- ">" --
   ---------

   function ">" (Left, Right : Text_Cursor'Class) return Boolean is
   begin
      return not (Left < Right) and then Left /= Right;
   end ">";

   ----------
   -- "<=" --
   ----------

   function "<=" (Left, Right : Text_Cursor'Class) return Boolean is
   begin
      return not (Left > Right);
   end "<=";

   ----------
   -- ">=" --
   ----------

   function ">=" (Left, Right : Text_Cursor'Class) return Boolean is
   begin
      return not (Left < Right);
   end ">=";

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : File_Cursor) return Boolean is
   begin

      if Left.File_Name /= null xor Right.File_Name /= null then
         return False;
      else
         return Left.Line = Right.Line
           and then Left.Col = Right.Col
           and then ((Left.File_Name = null and then Right.File_Name = null)
                     or else (Left.File_Name.all = Right.File_Name.all));
      end if;

   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : File_Cursor) return Boolean is
   begin
      return Left.File_Name.all < Right.File_Name.all
        or else (Left.File_Name.all = Right.File_Name.all
                 and then Text_Cursor (Left) < Text_Cursor (Right));
   end "<";

   ------------
   -- Assign --
   ------------

   procedure Assign
     (This : in out File_Cursor'Class; Source : File_Cursor'Class) is
   begin
      This.Col := Source.Col;
      This.Line := Source.Line;
      Assign (This.File_Name, Source.File_Name);
   end Assign;

   ----------------------------------------------------------------------------
   --  type Text_Navigator
   ----------------------------------------------------------------------------

   ---------------
   -- Free_Data --
   ---------------

   procedure Free_Data (This : in out Mark_Abstr'Class) is
   begin
      Free (This);
   end Free_Data;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ptr_Mark) is
      procedure Free_Pool is new
        Ada.Unchecked_Deallocation (Mark_Abstr'Class, Ptr_Mark);
   begin
      if This /= null then
         Free (This.all);
      end if;

      Free_Pool (This);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Text_Navigator_Abstr) is
   begin
      Free (This.Files.all);
      Free (This.Files);
   end Free;

   ----------------
   -- Initialize --
   ----------------

   procedure Initialize
     (This : Text_Navigator_Abstr;
      File : in out Text_Interface'Class)
   is
      pragma Unreferenced (This, File);
   begin
      null;
   end Initialize;

   ------------------
   -- Get_New_Mark --
   ------------------

   function Get_New_Mark
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class) return Mark_Abstr'Class
   is
      Real_Cursor : File_Cursor := File_Cursor (Cursor);
   begin
      if Cursor.Line = 0 then
         Real_Cursor.Line := 1;
      end if;

      declare
         Res : Mark_Abstr'Class := Get_New_Mark
          (Get_File (Current_Text, Real_Cursor.File_Name.all).all,
            Text_Cursor (Real_Cursor));
      begin
         Assign (Res.File_Name, Cursor.File_Name);

         if Cursor.Line = 0 then
            Res.Is_First_Line := True;
         else
            Res.Is_First_Line := False;
         end if;

         return Res;
      end;
   end Get_New_Mark;

   ------------------------
   -- Get_Current_Cursor --
   ------------------------

   function Get_Current_Cursor
     (Current_Text : Text_Navigator_Abstr'Class;
      Mark         : Mark_Abstr'Class) return File_Cursor'Class
   is
      Cursor : File_Cursor;
   begin
      Cursor := File_Cursor (Get_Current_Cursor
        (Get_File (Current_Text, Mark.File_Name.all).all, Mark));

      if Mark.Is_First_Line then
         Cursor.Line := 0;
      end if;

      return Cursor;
   end Get_Current_Cursor;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Current_Text           : Text_Navigator_Abstr;
      Cursor                 : File_Cursor'Class;
      Position               : Relative_Position := Specified;
      Category_1, Category_2 : Language_Category := Cat_Unknown)
     return Construct_Information is
   begin
      return Get_Unit
        (Get_File (Current_Text, Cursor.File_Name.all).all,
         Text_Cursor (Cursor),
         Position,
         Category_1,
         Category_2);
   end Get_Unit;

   -----------------
   -- Search_Body --
   -----------------

   function Search_Body
     (Current_Text : Text_Navigator_Abstr;
      File_Name    : String;
      Spec         : Construct_Information) return Construct_Information is
   begin
      return Search_Body (Get_File (Current_Text, File_Name).all, Spec);
   end Search_Body;

   ---------
   -- Get --
   ---------

   function Get
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class;
      Len    : Natural) return String is
   begin
      return Get
        (Get_File (This, Cursor.File_Name.all).all, Text_Cursor (Cursor), Len);
   end Get;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class) return String is
   begin
      return Get_Line
        (Get_File (This, Cursor.File_Name.all).all,
         Text_Cursor (Cursor));
   end Get_Line;

   ---------------
   -- Read_File --
   ---------------

   function Read_File
     (This : Text_Navigator_Abstr;
      File_Name : String) return Dynamic_String is
   begin
      return Read_File (Get_File (This, File_Name).all);
   end Read_File;

   --------------
   -- Get_File --
   --------------

   function Get_File
     (This : Text_Navigator_Abstr'Class;
      Name : String) return Ptr_Text
   is
      Iterator    : Text_List.List_Node := First (This.Files.all);
      New_Text    : Ptr_Text;
   begin
      while Iterator /= Text_List.Null_Node loop
         if Get_File_Name (Data (Iterator).all) = Name then
            return Data (Iterator);
         end if;

         Iterator := Next (Iterator);
      end loop;

      New_Text := New_Text_Interface (This);

      Initialize (This, New_Text.all);

      Append (This.Files.all, New_Text);

      New_Text.File_Name := new String'(Name);
      Initialize (New_Text.all, Name);

      return New_Text;
   end Get_File;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (This      : in out Text_Navigator_Abstr;
      Cursor    : File_Cursor'Class;
      Len       : Natural;
      New_Value : String) is
   begin
      Replace
        (Get_File (This, Cursor.File_Name.all).all,
         Text_Cursor (Cursor),
         Len,
         New_Value);
   end Replace;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (This        : in out Text_Navigator_Abstr;
      Cursor      : File_Cursor'Class;
      New_Line    : String) is
   begin
      Add_Line
        (Get_File (This, Cursor.File_Name.all).all,
         Text_Cursor (Cursor),
         New_Line);
   end Add_Line;

   -----------------
   -- Delete_Line --
   -----------------

   procedure Delete_Line
     (This : in out Text_Navigator_Abstr;
      Cursor : File_Cursor'Class) is
   begin
      Delete_Line
        (Get_File (This, Cursor.File_Name.all).all,
         Text_Cursor (Cursor));
   end Delete_Line;

   ----------------
   -- Get_Entity --
   ----------------

   procedure Get_Entity
     (This : in out Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor : File_Cursor'Class)
   is
      Unit_Info, Body_Info : Construct_Information;
      Line_Cursor          : File_Cursor := File_Cursor (Cursor);
   begin
      Line_Cursor.Col := 1;

      Unit_Info := Get_Unit (Current_Text, Cursor);

      if Unit_Info.Is_Declaration then
         Body_Info := Search_Body
           (Current_Text,
            Cursor.File_Name.all,
            Unit_Info);
         for J in Body_Info.Sloc_Start.Line .. Body_Info.Sloc_End.Line loop
            Line_Cursor.Line := J;
            Get_Line (Current_Text, Line_Cursor, This);
         end loop;
      end if;

      for J in Unit_Info.Sloc_Start.Line .. Unit_Info.Sloc_End.Line loop
         Line_Cursor.Line := J;
         Get_Line (Current_Text, Line_Cursor, This);
      end loop;
   end Get_Entity;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length
     (This   : Text_Navigator_Abstr;
      Cursor : File_Cursor'Class) return Natural is
   begin
      return Line_Length
        (Get_File (This, Cursor.File_Name.all).all,
         File_Cursor (Cursor));
   end Line_Length;

   ------------
   -- Commit --
   ------------

   procedure Commit (This : Text_Navigator_Abstr) is
      Iterator : Text_List.List_Node := First (This.Files.all);
   begin
      while Iterator /= Text_List.Null_Node loop
         Commit (Data (Iterator).all);
         Iterator := Next (Iterator);
      end loop;
   end Commit;

   -------------------
   -- Search_String --
   -------------------

   function Search_String
     (This           : Text_Navigator_Abstr'Class;
      Cursor         : File_Cursor'Class;
      Searched       : String;
      Escape_Manager : Escape_Str_Manager'Class;
      Step           : Step_Way := Normal_Step)
     return File_Cursor'Class is
   begin
      return Search_String
        (Get_File (This, Cursor.File_Name.all).all,
         File_Cursor (Cursor),
         Searched,
         Escape_Manager,
         Step);
   end Search_String;

   function Search_Unit
     (This      : Text_Navigator_Abstr'Class;
      File_Name : String;
      Category  : Language_Category;
      Name      : String := "") return Construct_Information is
   begin
      return Search_Unit (Get_File (This, File_Name).all, Category, Name);
   end Search_Unit;

   --------------
   -- Line_Max --
   --------------

   function Line_Max
     (This      : Text_Navigator_Abstr'Class;
      File_Name : String) return Natural is
   begin
      return Line_Max (Get_File (This, File_Name).all);
   end Line_Max;

   ----------------------------
   -- Get_Extended_Unit_Name --
   ----------------------------

   function Get_Extended_Unit_Name
     (This     : Text_Navigator_Abstr'Class;
      Cursor   : File_Cursor'Class;
      Category : Language_Category := Cat_Unknown)
     return String is
   begin
      return Get_Extended_Unit_Name
        (Get_File (This, Cursor.File_Name.all).all,
         Cursor,
         Category);
   end Get_Extended_Unit_Name;

   ---------------------
   -- Get_Right_Paren --
   ---------------------

   function Get_Right_Paren
     (Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : File_Cursor'Class)
     return File_Cursor'Class is

      Result      : File_Cursor;
      Line_Cursor : File_Cursor := File_Cursor (Cursor);

   begin
      Line_Cursor.Col := 1;

      Result :=
        (Text_Cursor (Get_Right_Paren
                        (Get_File (Current_Text, Cursor.File_Name.all).all,
                         Cursor,
                         Get_Line (Current_Text, Line_Cursor)))
         with File_Name => Clone (Cursor.File_Name));

      return Result;
   end Get_Right_Paren;

   ----------------
   -- Get_Entity --
   ----------------

   procedure Get_Entity
     (Current_Text         : Text_Navigator_Abstr'Class;
      Cursor               : File_Cursor'Class;
      Spec_Begin, Spec_End : out File_Cursor'Class;
      Body_Begin, Body_End : out File_Cursor'Class)
   is
      Unit_Info, Body_Info : Construct_Information;
   begin
      Unit_Info := Get_Unit (Current_Text, Cursor);

      if Unit_Info.Is_Declaration then
         Body_Info := Search_Body
           (Current_Text,
            Cursor.File_Name.all,
            Unit_Info);

         Assign (Body_Begin.File_Name, Cursor.File_Name);
         Assign (Body_End.File_Name, Cursor.File_Name);
         Assign (Spec_Begin.File_Name, Cursor.File_Name);
         Assign (Spec_End.File_Name, Cursor.File_Name);

         Body_Begin.Col := Body_Info.Sloc_Start.Column;
         Body_Begin.Line := Body_Info.Sloc_Start.Line;
         Body_End.Col := Body_Info.Sloc_End.Column;
         Body_End.Line := Body_Info.Sloc_End.Line;

         Spec_Begin.Col := Unit_Info.Sloc_Start.Column;
         Spec_Begin.Line := Unit_Info.Sloc_Start.Line;
         Spec_End.Col := Unit_Info.Sloc_End.Column;
         Spec_End.Line := Unit_Info.Sloc_End.Line;

      else
         Body_Begin.Col := Unit_Info.Sloc_Start.Column;
         Body_Begin.Line := Unit_Info.Sloc_Start.Line;
         Body_End.Col := Unit_Info.Sloc_End.Column;
         Body_End.Line := Unit_Info.Sloc_End.Line;

         Assign (Body_Begin.File_Name, Cursor.File_Name);
         Assign (Body_End.File_Name, Cursor.File_Name);

         Assign (Spec_Begin, Null_File_Cursor);
         Assign (Spec_End, Null_File_Cursor);
      end if;

   end Get_Entity;

   ---------------
   -- Next_Word --
   ---------------

   procedure Next_Word
     (This   : Text_Navigator_Abstr'Class;
      Cursor : in out File_Cursor'Class;
      Word   : out Dynamic_String) is
   begin
      Next_Word
        (Get_File (This, Cursor.File_Name.all).all,
         Cursor,
         Word);
   end Next_Word;

   -------------------
   -- Get_Structure --
   -------------------

   function Get_Structure
     (This      : Text_Navigator_Abstr'Class;
      File_Name : String) return Construct_List_Access is
   begin
      return Get_Structure (Get_File (This, File_Name).all);
   end Get_Structure;

   ----------------
   -- Update_All --
   ----------------

   procedure Update_All
     (This : Text_Navigator_Abstr'Class)
   is
      Node : Text_List.List_Node := First (This.Files.all);
   begin
      while Node /= Text_List.Null_Node loop
         Constrain_Update (Data (Node).all);
         Node := Next (Node);
      end loop;
   end Update_All;

   ----------------------------------------------------------------------------
   --  type Text_Interface
   ----------------------------------------------------------------------------

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Text_Interface) is
   begin
      Free (This.Structure.all);
      Free (This.Structure);
      Free (This.File_Name);
   end Free;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Ptr_Text) is
      procedure Delete is new
        Ada.Unchecked_Deallocation (Text_Interface'Class, Ptr_Text);
   begin
      Free (This.all);
      Delete (This);
   end Free;

   --------------
   -- Get_Unit --
   --------------

   function Get_Unit
     (Current_Text           : Text_Interface;
      Cursor                 : Text_Cursor'Class;
      Position               : Relative_Position := Specified;
      Category_1, Category_2 : Language_Category := Cat_Unknown)
     return Construct_Information
   is

      function Nearer return Boolean;
      --  Return True when Current_Info is nearer Cursor than Info_Saved.

      Current_Info : Construct_Access;
      Info_Saved   : Construct_Access;

      function Nearer return Boolean is
         D_Current_Line, D_Current_Col : Integer;
         D_Old_Line, D_Old_Col : Integer;
      begin
         D_Current_Line := Current_Info.Sloc_Start.Line - Cursor.Line;
         D_Current_Col := Current_Info.Sloc_Start.Column - Cursor.Col;

         case Position is
            when Before =>
               if D_Current_Line > 0
                 or else (D_Current_Line = 0 and then D_Current_Col > 0)
               then
                  return False;
               end if;
            when After =>
               if D_Current_Line < 0
                 or else (D_Current_Line = 0 and then D_Current_Col < 0)
               then
                  return False;
               end if;
            when others =>
               return False;
         end case;

         if Info_Saved = null then
            return True;
         end if;

         D_Old_Line := Info_Saved.Sloc_Start.Line - Cursor.Line;
         D_Old_Col := Info_Saved.Sloc_Start.Column - Cursor.Col;

         return (abs D_Old_Line) > (abs D_Current_Line) or else
           (D_Old_Line = D_Current_Line and then
              (abs D_Old_Col) > (abs D_Current_Col));

      end Nearer;

      --  begin of Get_Unit

   begin
      Current_Info := Get_Structure (Current_Text).First;

      while Current_Info /= null loop

         if Category_1 = Cat_Unknown
           or else Current_Info.Category = Category_1
           or else Current_Info.Category = Category_2
         then
            if (Current_Info.Sloc_Start.Line = Cursor.Line
                and then Current_Info.Sloc_Start.Column = Cursor.Col)
              or else (Current_Info.Sloc_Entity.Line = Cursor.Line
                       and then Current_Info.Sloc_Entity.Column = Cursor.Col)
            then
               return Current_Info.all;
            elsif Position /= Specified and then Nearer then
               Info_Saved := Current_Info;
            end if;
         end if;

         Current_Info := Current_Info.Next;
      end loop;

      if Info_Saved /= null then
         return Info_Saved.all;
      end if;

      Raise_Exception
        (Codefix_Panic'Identity,
         "Cursor given is not at the beginning of a unit.");
   end Get_Unit;

   -----------------
   -- Search_Body --
   -----------------

   --  Assertion : The last unit can never be the unit looked for
   function Search_Body
     (Current_Text : Text_Interface;
      Spec         : Construct_Information) return Construct_Information
   is
      procedure Seeker (Stop : Source_Location; Is_First : Boolean := False);
      --  Recursivly scan a token list in order to found the declaration what
      --  correpond with a body. Stop is the beginning of the current block.

      type Scope_Node;

      type Ptr_Scope_Node is access all Scope_Node;

      procedure Free (This : in out Ptr_Scope_Node);

      package Scope_Lists is new Generic_List (Ptr_Scope_Node);
      use Scope_Lists;

      type Scope_Node is record
         Scope_List : Scope_Lists.List;
         Result     : Construct_Access;
         Name       : Dynamic_String;
         Root       : Ptr_Scope_Node;
      end record;

      function Get_Node (Container : Ptr_Scope_Node; Name : String)
        return Ptr_Scope_Node;

      procedure Add_Node (Container : Ptr_Scope_Node; Object : Ptr_Scope_Node);

      function Get_Node (Container : Ptr_Scope_Node; Name : String)
        return Ptr_Scope_Node is
         Current_Node : Scope_Lists.List_Node := First (Container.Scope_List);
      begin
         while Current_Node /= Scope_Lists.Null_Node loop
            if Data (Current_Node).Name.all = Name then
               return Data (Current_Node);
            end if;

            Current_Node := Next (Current_Node);
         end loop;

         raise Codefix_Panic;
      end Get_Node;

      procedure Add_Node
        (Container : Ptr_Scope_Node; Object : Ptr_Scope_Node) is
      begin
         Append (Container.Scope_List, Object);
         Object.Root := Container;
      end Add_Node;

      procedure Free (This : in out Ptr_Scope_Node) is
         procedure Free_Pool is
           new Ada.Unchecked_Deallocation (Scope_Node, Ptr_Scope_Node);
      begin
         Free (This.Scope_List);
         Free (This.Name);
         Free_Pool (This);
      end Free;

      Current_Info  : Construct_Access;
      Found         : Boolean := False;
      Result        : Construct_Access;
      Current_Scope : Ptr_Scope_Node;
      First_Scope   : Ptr_Scope_Node;

      ------------
      -- Seeker --
      ------------

      --  ??? Solve eventuals memory leaks linked to intra-packages

      procedure Seeker (Stop : Source_Location; Is_First : Boolean := False) is
         Current_Result : Construct_Access;
         New_Sloc       : Source_Location;
         This_Scope     : Ptr_Scope_Node := null;
      begin

         if not Is_First then

            if Current_Info.Is_Declaration
              and then (Current_Info.Category = Cat_Package
                     or else Current_Info.Category = Cat_Protected)
            then
               Current_Scope := Get_Node
                 (Current_Scope, Current_Info.Name.all);
               This_Scope := Current_Scope; --  ???  Is it usefull ?
               Current_Result := Current_Scope.Result;
               Result := Current_Result;
            end if;

            if not Current_Info.Is_Declaration
              and then (Current_Info.Category = Cat_Package
                     or else Current_Info.Category = Cat_Protected)
            then
               This_Scope := new Scope_Node;
               Assign (This_Scope.Name, Current_Info.Name.all);
               Add_Node (Current_Scope, This_Scope);
               Current_Scope := This_Scope;
            end if;

            Current_Info := Current_Info.Prev;
         end if;

         while Current_Info /= null loop

            if Current_Info.Category not in Construct_Category then

               --  Is it the end of the scope ?
               if Current_Info.Sloc_End.Line < Stop.Line
                 or else (Current_Info.Sloc_End.Line = Stop.Line
                          and then Current_Info.Sloc_End.Column < Stop.Column)
               then
                  if This_Scope /= null then
                     Current_Scope := This_Scope.Root;
                  end if;

                  return;
               end if;

               --  Does this body have the rigth profile ?
               if not Current_Info.Is_Declaration and then
                 Current_Info.Name.all = Spec.Name.all
                 and then Normalize (Current_Info.Profile) =
                   Normalize (Spec.Profile)
               then
                  Result := Current_Info;
                  if This_Scope /= null then
                     This_Scope.Result := Result; --  ???  or current_result ?
                  end if;
               end if;

               --  Is it the beginning of a scope ?
               if (not Current_Info.Is_Declaration and then
                     Current_Info.Category in Enclosing_Entity_Category)
                 or else Current_Info.Category = Cat_Protected
                 or else Current_Info.Category = Cat_Package
               then

                  Current_Result := Result;
                  New_Sloc := Current_Info.Sloc_Start;

                  Seeker (New_Sloc);

                  if This_Scope /= null then
                     Current_Scope := This_Scope;
                  end if;

                  if Found then
                     return;
                  else
                     Result := Current_Result;
                  end if;

                  --  Is this spec the right one ?
               elsif Current_Info.Is_Declaration and then
                 Current_Info.Name.all = Spec.Name.all and then
                 Current_Info.Sloc_Start = Spec.Sloc_Start and then
                 Normalize (Current_Info.Profile) = Normalize (Spec.Profile)
               then
                  Found := True;
                  return;
               else
                  Current_Info := Current_Info.Prev;
               end if;

            else

               Current_Info := Current_Info.Prev;

            end if;

         end loop;
      end Seeker;

   begin
      Current_Info := Get_Structure (Current_Text).Last;
      First_Scope := new Scope_Node;
      Current_Scope := First_Scope;
      Seeker (Current_Info.Sloc_Start, True);
      Free (First_Scope);
      return Result.all;
   end Search_Body;

   -------------------
   -- Get_File_Name --
   -------------------

   function Get_File_Name (This : Text_Interface) return String is
   begin
      return This.File_Name.all;
   end Get_File_Name;

   -----------------
   -- Line_Length --
   -----------------

   function Line_Length
     (This   : Text_Interface'Class;
      Cursor : Text_Cursor'Class) return Natural is
   begin
      return Get_Line (This, Cursor)'Length;
   end Line_Length;

   -------------------
   -- Search_String --
   -------------------

   function Search_String
     (This           : Text_Interface'Class;
      Cursor         : Text_Cursor'Class;
      Searched       : String;
      Escape_Manager : Escape_Str_Manager'Class;
      Step           : Step_Way := Normal_Step)
     return File_Cursor'Class is

      Last, Increment    : Integer;
      Ext_Red            : Extract_Line;
      New_Cursor, Result : File_Cursor;
      Line_Cursor        : File_Cursor;
      Old_Col            : Integer;

   begin
      New_Cursor := File_Cursor (Cursor);
      Line_Cursor := New_Cursor;
      Line_Cursor.Col := 1;

      case Step is
         when Normal_Step =>
            Last := Line_Max (This);
            Increment := 1;
            Get_Line (This, Line_Cursor, Ext_Red);
         when Reverse_Step =>
            Last := 1;
            Increment := -1;
            Old_Col := New_Cursor.Col;
            New_Cursor.Col := 1;
            Get (This, Line_Cursor, Old_Col, Ext_Red);
            New_Cursor.Col := Old_Col;
      end case;


      loop
         Result := File_Cursor (Search_String
                                  (Ext_Red,
                                   New_Cursor,
                                   Searched,
                                   Escape_Manager,
                                   Step));

         if Result /= Null_File_Cursor then
            Result := Clone (Result);
            Free (Ext_Red);
            return Result;
         end if;

         exit when New_Cursor.Line = Last;

         New_Cursor.Col := 1;
         New_Cursor.Line := New_Cursor.Line + Increment;
         Free (Ext_Red);
         Get_Line (This, New_Cursor, Ext_Red);

         if Step = Reverse_Step then
            New_Cursor.Col := 0;
         end if;

      end loop;

      Free (Ext_Red);
      return Null_File_Cursor;
   end Search_String;

   -----------------
   -- Search_Unit --
   -----------------

   function Search_Unit
     (This     : Text_Interface'Class;
      Category : Language_Category;
      Name     : String := "") return Construct_Information is

      Current_Info : Construct_Access;

   begin

      Current_Info := Get_Structure (This).First;

      while Current_Info /= null loop
         if Current_Info.Category = Category
           and then
             (Name = "" or else Compare_Last (Current_Info.Name.all, Name))
         then
            return Current_Info.all;
         end if;
         Current_Info := Current_Info.Next;
      end loop;

      return (Category        => Cat_Unknown,
              Name            => null,
              Profile         => null,
              Sloc_Start      => (0, 0, 0),
              Sloc_Entity     => (0, 0, 0),
              Sloc_End        => (0, 0, 0),
              Is_Declaration  => False,
              Prev            => null,
              Next            => null);

   end Search_Unit;

   ----------------------------
   -- Get_Extended_Unit_Name --
   ----------------------------

   function Get_Extended_Unit_Name
     (This     : Text_Interface'Class;
      Cursor   : Text_Cursor'Class;
      Category : Language_Category := Cat_Unknown)
     return String is

      Unit_Info    : constant Construct_Information := Get_Unit
        (This, Cursor, After, Category_1 => Category);
      --  ??? Is 'after' a good idea ?

      Result_Name  : Dynamic_String;
      Current_Info : Construct_Access;
      Found        : Boolean := False;

      procedure Seeker (Stop : Source_Location);
      --  Initialize Result_Name wirth the extended prefix of unit.

      ------------
      -- Seeker --
      ------------

      procedure Seeker (Stop : Source_Location) is
         New_Sloc     : Source_Location;
         Current_Name : Dynamic_String;
      begin
         Assign (Current_Name, Current_Info.Name.all);

         Current_Info := Current_Info.Prev;

         while Current_Info /= null loop

            if Current_Info.Category not in Construct_Category then

               --  Is it the end of the scope ?
               if Current_Info.Sloc_End.Line < Stop.Line
                 or else (Current_Info.Sloc_End.Line = Stop.Line
                          and then Current_Info.Sloc_End.Column < Stop.Column)
               then
                  Free (Current_Name);
                  return;
               end if;


               --  Is this unit the right one ?

               if Current_Info.Is_Declaration = Unit_Info.Is_Declaration
                 and then
                   ((Current_Info.Name = null and then Unit_Info.Name = null)
                    or else (Current_Info.Name /= null
                             and then Unit_Info.Name /= null
                             and then Current_Info.Name.all =
                               Unit_Info.Name.all))
                 and then Current_Info.Sloc_Start = Unit_Info.Sloc_Start
                 and then Normalize (Current_Info.Profile) =
                   Normalize (Unit_Info.Profile)
               then
                  Result_Name := Current_Name;
                  Found := True;
                  return;

               --  Is it the beginning of a scope ?
               elsif (not Current_Info.Is_Declaration and then
                      Current_Info.Category in Enclosing_Entity_Category)
                 or else Current_Info.Category = Cat_Protected
                 or else Current_Info.Category = Cat_Package
               then

                  New_Sloc := Current_Info.Sloc_Start;

                  Seeker (New_Sloc);

                  if Found then
                     Assign
                       (Result_Name,
                        Current_Name.all & "." & Result_Name.all);
                     Free (Current_Name);
                     return;
                  end if;
               else
                  Current_Info := Current_Info.Prev;
               end if;

            else
               Current_Info := Current_Info.Prev;
            end if;

         end loop;
      end Seeker;


   begin
      Current_Info := Get_Structure (This).Last;
      Seeker (Current_Info.Sloc_Start);

      declare
         Result_Stack : constant String := Result_Name.all;
      begin
         Free (Result_Name);
         return Result_Stack;
      end;
   end Get_Extended_Unit_Name;

   ---------------------
   -- Get_Right_Paren --
   ---------------------

   function Get_Right_Paren
     (This         : Text_Interface'Class;
      Cursor       : Text_Cursor'Class;
      Current_Line : String) return Text_Cursor'Class
   is
      Local_Cursor : Text_Cursor := Text_Cursor (Cursor);
      Local_Line   : Dynamic_String := new String'(Current_Line);
   begin

      loop
         if Local_Cursor.Col > Current_Line'Last then
            Local_Cursor.Col := 1;
            Local_Cursor.Line := Local_Cursor.Line + 1;
            Assign (Local_Line, Get_Line (This, Local_Cursor));
         end if;

         case Local_Line (Local_Cursor.Col) is
            when '(' =>
               Local_Cursor.Col := Local_Cursor.Col + 1;

               declare
                  Stack_Str : constant String := Local_Line.all;
               begin
                  Free (Local_Line);
                  return Get_Right_Paren
                    (This, Local_Cursor, Stack_Str);
               end;
            when ')' =>
               Free (Local_Line);
               return Local_Cursor;
            when others =>
               Local_Cursor.Col := Local_Cursor.Col + 1;
         end case;
      end loop;

      raise Codefix_Panic;
   end Get_Right_Paren;

   ---------------
   -- Next_Word --
   ---------------

   procedure Next_Word
     (This   : Text_Interface'Class;
      Cursor : in out Text_Cursor'Class;
      Word   : out Dynamic_String)
   is
      Current_Line : Dynamic_String;
      Line_Cursor  : Text_Cursor := Text_Cursor (Cursor);
      Begin_Word   : Natural;
   begin
      Line_Cursor.Col := 1;
      Assign (Current_Line, Get_Line (This, Line_Cursor));

      while Is_Blank (Current_Line (Cursor.Col .. Current_Line'Last)) loop
         Cursor.Col := 1;
         Cursor.Line := Cursor.Line + 1;
         Assign (Current_Line, Get_Line (This, Cursor));
      end loop;

      while Is_Blank (Current_Line (Cursor.Col)) loop
         Cursor.Col := Cursor.Col + 1;
      end loop;

      Begin_Word := Cursor.Col;

      while Cursor.Col < Current_Line'Last
        and then not Is_Blank (Current_Line (Cursor.Col))
      loop
         Cursor.Col := Cursor.Col + 1;
      end loop;

      if Is_Blank (Current_Line (Cursor.Col)) then
         Word := new String'(Current_Line (Begin_Word .. Cursor.Col - 1));
      else
         Word := new String'(Current_Line (Begin_Word .. Cursor.Col));
      end if;

      if Cursor.Col = Current_Line'Last then
         Cursor.Col := 1;
         Cursor.Line := Cursor.Line + 1;
      else
         Cursor.Col := Cursor.Col + 1;
      end if;
   end Next_Word;

   -------------------
   -- Get_Structure --
   -------------------

   function Get_Structure
     (This : Text_Interface'Class) return Construct_List_Access
   is
      procedure Display_Constructs;
      pragma Unreferenced (Display_Constructs);
      --  Debug procedure used to display the contents of New_Text

      -----------------------
      -- Display_Contructs --
      -----------------------

      procedure Display_Constructs is
         Current : Construct_Access := This.Structure.First;
      begin
         while Current /= null loop
            if Current.Name /= null then
               Put (Current.Name.all & ": ");
            end if;

            Put (Current.Category'Img);
            Put (", [" & Current.Sloc_Start.Line'Img & ", ");
            Put (Current.Sloc_Start.Column'Img);
            Put ("]-[");
            Put (Current.Sloc_End.Line'Img & ", ");
            Put (Current.Sloc_End.Column'Img);
            Put ("] (");

            if Current.Is_Declaration then
               Put ("spec");
            else
               Put ("body");
            end if;

            Put (")");
            New_Line;
            Current := Current.Next;
         end loop;
      end Display_Constructs;

      New_Buffer  : Extended_Line_Buffer;
      Ignore      : Natural;
      Buffer      : Dynamic_String;
   begin
      if not This.Structure_Up_To_Date.all then
         Buffer := Read_File (This);
         Analyze_Ada_Source
           (Buffer => Buffer.all,
            New_Buffer => New_Buffer,
            Indent_Params => Default_Indent_Parameters,
            Reserved_Casing  => Unchanged,
            Ident_Casing => Unchanged,
            Format_Operators => False,
            Indent => False,
            Constructs => This.Structure,
            Current_Indent => Ignore,
            Prev_Indent => Ignore,
            Callback => null);

         --  pragma Debug (Display_Constructs);

         Free (Buffer);
         This.Structure_Up_To_Date.all := True;
      end if;

      return This.Structure;
   end Get_Structure;

   -----------------------
   --  Text_Has_Changed --
   -----------------------

   procedure Text_Has_Changed (This : in out Text_Interface'Class) is
   begin
      This.Structure_Up_To_Date.all := False;
   end Text_Has_Changed;

   ----------------------------------------------------------------------------
   --  type Text_Cursor
   ----------------------------------------------------------------------------

   ----------
   -- Free --
   ----------

   procedure Free (This : in out File_Cursor) is
   begin
      Free (This.File_Name);
   end Free;

   -----------
   -- Clone --
   -----------

   function Clone (This : File_Cursor) return File_Cursor is
      New_Cursor : File_Cursor := This;
   begin
      New_Cursor.File_Name := Clone (This.File_Name);
      return New_Cursor;
   end Clone;

   ----------------------------------------------------------------------------
   --  type Extract_Line
   ----------------------------------------------------------------------------

   ---------
   -- "=" --
   ---------

   function "=" (Left, Right : Extract_Line) return Boolean is
   begin
      return Left.Cursor = Right.Cursor
        and then Left.Context = Right.Context
        and then (Left.Context = Unit_Deleted
                  or else To_String (Left.Content) =
                    To_String (Right.Content));
   end "=";

   ---------
   -- "<" --
   ---------

   function "<" (Left, Right : Ptr_Extract_Line) return Boolean is
   begin
      return Left.Cursor < Right.Cursor;
   end "<";

   ------------
   -- Assign --
   ------------

   procedure Assign (This : in out Extract_Line; Value : Extract_Line) is
   begin
      This.Context := Value.Context;
      Free (This.Cursor);
      This.Cursor := Clone (Value.Cursor);
      Assign (This.Content, Value.Content);
   end Assign;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Extract_Line) is
   begin
      Free (This.Content);
      Free (This.Cursor);
      if This.Next /= null then
         Free (This.Next.all);
         Free (This.Next);
      end if;
   end Free;

   ---------------
   -- Free_Data --
   ---------------

   procedure Free_Data (This : in out Extract_Line) is
   begin
      Free (This.Content);
      Free (This.Cursor);
   end Free_Data;

   -----------------
   -- Get_Context --
   -----------------

   function Get_Context (This : Extract_Line) return Merge_Info is
   begin
      return This.Context;
   end Get_Context;

   -----------------
   -- Set_Context --
   -----------------

   procedure Set_Context (This : in out Extract_Line; Value : Merge_Info) is
   begin
      This.Context := Value;
   end Set_Context;

   ----------
   -- Next --
   ----------

   function Next (This : Ptr_Extract_Line) return Ptr_Extract_Line is
   begin
      return This.Next;
   end Next;

   function Next (This : Extract_Line) return Ptr_Extract_Line is
   begin
      return This.Next;
   end Next;

   ----------------
   -- Get_String --
   ----------------

   function Get_String (This : Extract_Line) return String is
   begin
      return To_String (This.Content);
   end Get_String;

   ----------------
   -- Get_Cursor --
   ----------------

   function Get_Cursor (This : Extract_Line) return File_Cursor'Class is
   begin
      return This.Cursor;
   end Get_Cursor;

   ------------
   -- Length --
   ------------

   function Length (This : Extract_Line) return Natural is
   begin
      if This.Next /= null then
         return Length (This.Next.all) + 1;
      else
         return 1;
      end if;
   end Length;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (This         : in out Extract_Line;
      Current_Text : in out Text_Navigator_Abstr'Class;
      Offset_Line  : in out Integer)
   is

      Cursor : File_Cursor := This.Cursor;

      procedure Commit_Modified_Line;
      --  Commit separately each part of a modified line, in order to conserv
      --  marks.

      --------------------------
      -- Commit_Modified_Line --
      --------------------------

      procedure Commit_Modified_Line is
         It      : Mask_Iterator;
         Len     : Natural;
         Info    : Merge_Info;
         Content : constant String := Get_String (This);
      begin
         Reset (It);
         This.Number_Actions := 0;

         loop
            Get_Next_Area (This.Content, It, Cursor.Col, Len, Info);

            exit when Len = 0;

            case Info is
               when Unit_Modified =>
                  Replace
                    (Current_Text,
                     Cursor,
                     Len,
                     Content (Cursor.Col .. Cursor.Col + Len - 1));
                  This.Number_Actions := This.Number_Actions + 1;
               when Unit_Created =>
                  Replace
                    (Current_Text,
                     Cursor,
                     0,
                     Content (Cursor.Col .. Cursor.Col + Len - 1));
                  This.Number_Actions := This.Number_Actions + 1;
               when Unit_Deleted =>
                  Replace
                    (Current_Text,
                     Cursor,
                     Len,
                     "");
                  This.Number_Actions := This.Number_Actions + 1;
               when Original_Unit =>
                  null;
            end case;
         end loop;
      end Commit_Modified_Line;

   begin

      Cursor.Line := This.Cursor.Line + Offset_Line;

      case This.Context is
         when Unit_Created =>
            Add_Line
              (Current_Text,
               Cursor,
               To_String (This.Content));
            This.Number_Actions := 1;
            Offset_Line := Offset_Line + 1;
         when Unit_Deleted =>
            Delete_Line
              (Current_Text,
               Cursor);
            This.Number_Actions := 1;
            Offset_Line := Offset_Line - 1;
         when Unit_Modified =>
            Commit_Modified_Line;
         when Original_Unit =>
            null;
      end case;
   end Commit;

   -----------
   -- Clone --
   -----------

   function Clone
     (This      : Extract_Line;
      Recursive : Boolean) return Extract_Line
   is
      New_Line : Extract_Line := This;
   begin

      New_Line.Cursor := Clone (New_Line.Cursor);
      New_Line.Content := Clone (New_Line.Content);

      if Recursive and then New_Line.Next /= null then
         New_Line.Next := new Extract_Line'(Clone (New_Line.Next.all, True));
      else
         New_Line.Next := null;
      end if;

      return New_Line;
   end Clone;

   -----------
   -- Clone --
   -----------

   function Clone (This : Extract_Line) return Extract_Line is
   begin
      return Clone (This, False);
   end Clone;

   ---------------------
   -- Get_Word_Length --
   ---------------------

   function Get_Word_Length
     (This   : Extract_Line;
      Col    : Natural;
      Format : String) return Natural
   is
      Matches    : Match_Array (0 .. 1);
      Matcher    : constant Pattern_Matcher := Compile (Format);
      Str_Parsed : constant String := To_String (This.Content);

   begin
      Match (Matcher, Str_Parsed (Col .. Str_Parsed'Last), Matches);

      if Matches (0) = No_Match then
         Raise_Exception (Codefix_Panic'Identity, "pattern '" & Format &
                          "' from col" & Integer'Image (Col) & " in '" &
                            Str_Parsed & "' can't be found");

      else
         return Matches (1).Last - Col + 1;
      end if;
   end Get_Word_Length;

   -------------------
   -- Search_String --
   -------------------

   function Search_String
     (This           : Extract_Line;
      Cursor         : File_Cursor'Class;
      Searched       : String;
      Escape_Manager : Escape_Str_Manager'Class;
      Step           : Step_Way := Normal_Step) return File_Cursor'Class
   is
      Result  : File_Cursor := File_Cursor (Cursor);
      Content : constant String := To_String (This.Content);
   begin
      if Result.Col = 0 then
         Result.Col := To_String (This.Content)'Last;
      end if;

      case Step is
         when Normal_Step =>
            for J in Result.Col .. Content'Last - Searched'Last + 1 loop
               if Content (J .. J + Searched'Last - 1) = Searched
                 and then not Is_In_Escape_Part (Escape_Manager, Content, J)
               then
                  Result.Col := J;
                  return Result;
               end if;
            end loop;

         when Reverse_Step =>
            for J in reverse Content'First ..
              Result.Col - Searched'Last + 1
            loop
               if Content (J .. J + Searched'Last - 1) = Searched
                 and then not Is_In_Escape_Part (Escape_Manager, Content, J)
               then
                  Result.Col := J;
                  return Result;
               end if;
            end loop;
      end case;

      return Null_File_Cursor;
   end Search_String;

   ---------
   -- Get --
   ---------

   procedure Get
     (This        : Text_Interface'Class;
      Cursor      : File_Cursor'Class;
      Len         : Natural;
      Destination : in out Extract_Line) is
   begin
      Destination :=
        (Context         => Original_Unit,
         Cursor          => Clone (Cursor),
         Original_Length => Len,
         Content         => To_Mergable_String (Get (This, Cursor, Len)),
         Next            => null,
         Coloration      => True,
         Number_Actions  => 0);
   end Get;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (This        : Text_Navigator_Abstr'Class;
      Cursor      : File_Cursor'Class;
      Destination : in out Extract_Line) is

      Str : Dynamic_String;

   begin
      Str := new String'(Get_Line (This, Cursor));
      Destination :=
        (Context         => Original_Unit,
         Cursor          => File_Cursor (Cursor),
         Original_Length => Str.all'Last,
         Content         => To_Mergable_String (Str.all),
         Next            => null,
         Coloration      => True,
         Number_Actions  => 0);
      Free (Str);
   end Get_Line;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (This        : Text_Interface'Class;
      Cursor      : File_Cursor'Class;
      Destination : in out Extract_Line)
   is
      Str : Dynamic_String;
   begin
      Str := new String'(Get_Line (This, Cursor));
      Free_Data (Destination);
      Destination :=
        (Context         => Original_Unit,
         Cursor          => Clone (Cursor),
         Original_Length => Str'Last,
         Content         => To_Mergable_String (Str.all),
         Next            => null,
         Coloration      => True,
         Number_Actions  => 0);
      Free (Str);
   end Get_Line;

   ------------------
   -- Get_New_Text --
   ------------------

   function Get_New_Text
     (This : Extract_Line; Detail : Boolean := True) return String
   is
      pragma Unreferenced (Detail);
   begin
      if This.Context /= Unit_Deleted then
         return To_String (This.Content);
      else
         return "";
      end if;
   end Get_New_Text;

   ------------------
   -- Get_Old_Text --
   ------------------

   function Get_Old_Text
     (This         : Extract_Line;
      Current_Text : Text_Navigator_Abstr'Class) return String
   is
      Old_Extract : Extract;
   begin
      case This.Context is
         when Original_Unit | Unit_Modified =>
            --  ??? Simplify call to Get to have only a line instead of an
            --  extract

            Get (Current_Text, This.Cursor, This.Original_Length, Old_Extract);

            declare
               Res : constant String := Get_New_Text
                 (Get_Record (Old_Extract, 1).all,
                  False);
            begin
               Free (Old_Extract);
               return Res;
            end;

         when Unit_Created =>
            null;
         when Unit_Deleted =>
            Get (Current_Text, This.Cursor, This.Original_Length, Old_Extract);

            declare
               Res : constant String := Get_New_Text
                 (Get_Record (Old_Extract, 1).all,
                  False);
            begin
               Free (Old_Extract);
               return Res;
            end;

      end case;
      return "";
   end Get_Old_Text;

   -------------------------
   -- Get_New_Text_Length --
   -------------------------

   function Get_New_Text_Length
     (This      : Extract_Line;
      Recursive : Boolean := False) return Natural is
      Total : Natural := 0;

      Buffer : constant String := Get_New_Text (This);

   begin
      if Recursive and then This.Next /= null then
         Total := Get_New_Text_Length (This.Next.all, True);
      end if;
      return Total + Buffer'Length;
   end Get_New_Text_Length;

   -------------------------
   -- Get_Old_Text_Length --
   -------------------------

   function Get_Old_Text_Length
     (This      : Extract_Line;
      Current_Text : Text_Navigator_Abstr'Class;
      Recursive : Boolean := False) return Natural
   is
      Total  : Natural := 0;
      Buffer : constant String := Get_Old_Text (This, Current_Text);

   begin
      if Recursive and then This.Next /= null then
         Total := Get_Old_Text_Length (This.Next.all, Current_Text, True);
      end if;

      return Total + Buffer'Length;
   end Get_Old_Text_Length;

   -------------------
   -- Extend_Before --
   -------------------

   procedure Extend_Before
     (This          : in out Ptr_Extract_Line;
      Prev          : in out Ptr_Extract_Line;
      Current_Text  : Text_Navigator_Abstr'Class;
      Size          : Natural)
   is
      Line_Cursor            : File_Cursor;
      New_Line, Current_Line : Ptr_Extract_Line;

   begin
      if This = null then
         return;
      end if;

      Line_Cursor := This.Cursor;
      Line_Cursor.Col := 1;
      New_Line := new Extract_Line;
      Current_Line := This;

      if Prev = null then
         for J in 1 .. Size loop
            if Current_Line.Context /= Unit_Created then
               Line_Cursor.Line := Line_Cursor.Line - 1;
            end if;

            exit when Line_Cursor.Line = 0;

            Get_Line (Current_Text, Clone (Line_Cursor), New_Line.all);
            New_Line.Next := Current_Line;
            Current_Line := New_Line;
            New_Line := new Extract_Line;
         end loop;

      else
         for J in 1 .. Size loop
            if Current_Line.Context /= Unit_Created then
               Line_Cursor.Line := Line_Cursor.Line - 1;
            end if;

            exit when Prev.Cursor.File_Name.all =
              Current_Line.Cursor.File_Name.all
              and then (Prev.Cursor.Line + 1 = Current_Line.Cursor.Line
                        or else Prev.Cursor.Line = Current_Line.Cursor.Line);
            exit when Line_Cursor.Line = 0;

            Get_Line (Current_Text, Clone (Line_Cursor), New_Line.all);
            New_Line.Next := Current_Line;
            Prev.Next := New_Line;
            Current_Line := New_Line;
            New_Line := new Extract_Line;
         end loop;

      end if;

      Free (New_Line);
      Extend_Before (This.Next, This, Current_Text, Size);
      This := Current_Line;
   end Extend_Before;

   ------------------
   -- Extend_After --
   ------------------

   procedure Extend_After
     (This          : in out Ptr_Extract_Line;
      Current_Text  : Text_Navigator_Abstr'Class;
      Size          : Natural)
   is
      Line_Cursor            : File_Cursor;
      New_Line, Current_Line : Ptr_Extract_Line;
      End_Of_File            : Natural;

   begin
      if This = null then
         return;
      end if;

      Line_Cursor := This.Cursor;
      Line_Cursor.Col := 1;
      New_Line := new Extract_Line;
      Current_Line := This;
      End_Of_File := Line_Max (Current_Text, Line_Cursor.File_Name.all);

      if This.Next = null then
         for J in 1 .. Size loop
            Line_Cursor.Line := Line_Cursor.Line + 1;

            exit when Line_Cursor.Line > End_Of_File;

            Get_Line (Current_Text, Clone (Line_Cursor), New_Line.all);
            New_Line.Next := Current_Line.Next;
            Current_Line.Next := New_Line;
            Current_Line := New_Line;
            New_Line := new Extract_Line;
         end loop;

      else
         for J in 1 .. Size loop
            exit when Current_Line.Cursor.File_Name.all =
              Current_Line.Next.Cursor.File_Name.all
              and then (Current_Line.Cursor.Line + 1 =
                          Current_Line.Next.Cursor.Line
                        or else Current_Line.Next.Cursor.Line =
                          Current_Line.Cursor.Line);

            Line_Cursor.Line := Line_Cursor.Line + 1;

            exit when Line_Cursor.Line > End_Of_File;

            Get_Line (Current_Text, Clone (Line_Cursor), New_Line.all);
            New_Line.Next := Current_Line.Next;
            Current_Line.Next := New_Line;
            Current_Line := New_Line;
            New_Line := new Extract_Line;
         end loop;
      end if;

      Free (New_Line);
      Extend_After (Current_Line.Next, Current_Text, Size);
   end Extend_After;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (This       : in out Extract_Line;
      Start, Len : Natural;
      New_String : String) is
   begin
      if Len /= New_String'Length
        or else To_String (This.Content) (Start .. Start + Len - 1) /=
          New_String
      then
         This.Context := Unit_Modified;

         Replace (This.Content, Start, Len, New_String);
      end if;
   end Replace;

   --------------------
   -- Replace_To_End --
   --------------------

   procedure Replace_To_End
     (This  : in out Extract_Line;
      Start : Natural;
      Value : String) is
   begin
      --  ??? May be optimize by a function replace_to_end for Mergable_Strings
      Replace (This, Start, To_String (This.Content)'Last, Value);
   end Replace_To_End;

   --------------------
   -- Set_Coloration --
   --------------------

   procedure Set_Coloration (This : in out Extract_Line; Value : Boolean) is
   begin
      This.Coloration := Value;
   end Set_Coloration;

   --------------------
   -- Get_Coloration --
   --------------------

   function Get_Coloration (This : Extract_Line) return Boolean is
   begin
      return This.Coloration;
   end Get_Coloration;

   ------------
   -- Remove --
   ------------

   procedure Remove
     (This, Prev : Ptr_Extract_Line; Container : in out Extract) is
   begin
      if Prev = null then
         Container.First := This.Next;
      else
         Prev.Next := This.Next;
      end if;

      This.Next := null;
   end Remove;

   --------------
   -- Get_Line --
   --------------

   function Get_Line (This : Ptr_Extract_Line; Cursor : File_Cursor'Class)
     return Ptr_Extract_Line is
   begin
      if This = null then
         return null;
      elsif This.Cursor.File_Name.all = Cursor.File_Name.all
        and then This.Cursor.Line = Cursor.Line
      then
         return This;
      elsif This.Next = null then
         return null;
      else
         return Get_Line (This.Next, Cursor);
      end if;
   end Get_Line;

   -----------------
   -- Merge_Lines --
   -----------------

   procedure Merge_Lines
     (Result              : out Extract_Line;
      Object_1, Object_2  : Extract_Line;
      Success             : out Boolean;
      Chronologic_Changes : Boolean) is
   begin
      Merge_String
        (Result.Content,
         Object_1.Content,
         Object_2.Content,
         Success,
         Chronologic_Changes);

      Result.Cursor := Clone (Object_1.Cursor);
      Result.Original_Length := Object_1.Original_Length;
      Result.Coloration := Object_1.Coloration;
   end Merge_Lines;

   function Data (This : Ptr_Extract_Line) return Extract_Line is
   begin
      return This.all;
   end Data;

   function Is_Null (This : Ptr_Extract_Line) return Boolean is
   begin
      return This = null;
   end Is_Null;

   ----------------------------------------------------------------------------
   --  type Extract
   ----------------------------------------------------------------------------

   -----------
   -- Clone --
   -----------

   function Clone (This : Extract) return Extract is
      New_Extract : Extract := This;
   begin
      if New_Extract.First /= null then
         New_Extract.First :=
           new Extract_Line'(Clone (New_Extract.First.all, True));
      end if;

      return New_Extract;
   end Clone;

   ------------
   -- Assign --
   ------------

   procedure Assign (This : in out Extract'Class; Source : Extract'Class) is
   begin
      This.First := new Extract_Line'(Clone (Source.First.all, True));
   end Assign;

   ---------
   -- Get --
   ---------

   procedure Get
     (This        : Text_Navigator_Abstr'Class;
      Cursor      : File_Cursor'Class;
      Len         : Natural;
      Destination : in out Extract) is
   begin
      Add_Element
        (Destination,
         new Extract_Line'
           (Context         => Original_Unit,
            Cursor          => File_Cursor (Clone (Cursor)),
            Original_Length => Len,
            Content         => To_Mergable_String (Get (This, Cursor, Len)),
            Next            => null,
            Coloration      => True,
            Number_Actions  => 0));
   end Get;

   --------------
   -- Get_Line --
   --------------

   procedure Get_Line
     (This        : Text_Navigator_Abstr'Class;
      Cursor      : File_Cursor'Class;
      Destination : in out Extract)
   is
      Str : constant String := Get_Line (This, Cursor);
   begin
      Add_Element
        (Destination,
         new Extract_Line'
           (Context         => Original_Unit,
            Cursor          => Clone (Cursor),
            Original_Length => Str'Length,
            Content         => To_Mergable_String (Str),
            Next            => null,
            Coloration      => True,
            Number_Actions  => 0));
   end Get_Line;

   ----------------
   -- Get_String --
   ----------------

   function Get_String
     (This     : Extract;
      Position : Natural := 1) return String
   is
      Current_Extract : Ptr_Extract_Line := This.First;
   begin
      for J in 1 .. Position - 1 loop
         Current_Extract := Current_Extract.Next;
      end loop;

      return To_String (Current_Extract.Content);
   end Get_String;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (This          : in out Extract;
      Start, Length : Natural;
      Value         : String;
      Line_Number   : Natural := 1)
   is
      Current_Extract : Ptr_Extract_Line := This.First;
   begin
      for J in 1 .. Line_Number - 1 loop
         Current_Extract := Current_Extract.Next;
      end loop;

      Replace (Current_Extract.all, Start, Length, Value);

   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (This   : in out Extract;
      Start  : File_Cursor'Class;
      Length : Natural;
      Value  : String)
   is
      Current_Extract : constant Ptr_Extract_Line := Get_Line (This, Start);
   begin
      Replace (Current_Extract.all, Start.Col, Length, Value);
   end Replace;

   -------------
   -- Replace --
   -------------

   procedure Replace
     (This                      : in out Extract;
      Dest_Start, Dest_Stop     : File_Cursor'Class;
      Source_Start, Source_Stop : File_Cursor'Class;
      Current_Text              : Text_Navigator_Abstr'Class)
   is
      function Get_Next_Source_Line return Dynamic_String;
      --  Retutn the next source line that have to be added to the extract,
      --  addinf Head or Bottom if needed.

      Destination_Line : Ptr_Extract_Line := Get_Line (This, Dest_Start);

      Cursor_Dest      : File_Cursor := File_Cursor (Dest_Start);
      Cursor_Source    : File_Cursor := File_Cursor (Source_Start);
      Source_Line      : Dynamic_String;
      Head, Bottom     : Dynamic_String;
      Last_Line        : Dynamic_String;

      --------------------------
      -- Get_Next_Source_Line --
      --------------------------

      function Get_Next_Source_Line return Dynamic_String is
         Line : Dynamic_String :=
           new String'(Get_Line (Current_Text, Cursor_Source));
      begin
         if Cursor_Source.Line = Source_Stop.Line then
            Assign (Line, Line (1 .. Source_Stop.Col) & Bottom.all);
         end if;

         if Cursor_Source.Line = Source_Start.Line then
            Assign (Line, Head.all & Line (Source_Start.Col .. Line'Last));
         end if;

         Cursor_Source.Line := Cursor_Source.Line + 1;

         return Line;
      end Get_Next_Source_Line;

   begin
      Cursor_Source.Col := 1;

      Assign (Head, Get_String (Get_Line (This, Dest_Start).all)
                (1 .. Dest_Start.Col - 1));

      Assign (Bottom, Get_String (Get_Line (This, Dest_Stop).all));
      Assign (Bottom, Bottom (Dest_Stop.Col + 1 .. Bottom'Last));

      while Cursor_Source.Line <= Source_Stop.Line loop
         Free (Source_Line);
         Source_Line := Get_Next_Source_Line;

         if Destination_Line /= null then
            Replace_To_End
              (Destination_Line.all,
               1,
               Source_Line.all);

            Assign (Last_Line, Get_String (Destination_Line.all));

            if Cursor_Dest.Line < Dest_Stop.Line then
               Cursor_Dest.Line := Cursor_Dest.Line + 1;
               Destination_Line := Get_Line (Destination_Line, Cursor_Dest);
               null;
            else
               Destination_Line := null;
            end if;
         else
            if Last_Line /= null then
               Add_Indented_Line
                 (This,
                  Cursor_Dest,
                  Source_Line.all,
                  Last_Line.all);
            else
               Add_Indented_Line
                 (This,
                  Cursor_Dest,
                  Source_Line.all,
                  Current_Text);
            end if;
         end if;

      end loop;

      if Destination_Line /= null then
         loop
            Destination_Line.Context := Unit_Deleted;
            Cursor_Dest.Line := Cursor_Dest.Line + 1;

            exit when Cursor_Dest.Line > Dest_Stop.Line;

            Destination_Line := Get_Line (Destination_Line, Cursor_Dest);
         end loop;
      end if;

      Free (Source_Line);
      Free (Head);
      Free (Bottom);
      Free (Last_Line);
   end Replace;

   ------------
   -- Commit --
   ------------

   procedure Commit
     (This         : Extract;
      Current_Text : in out Text_Navigator_Abstr'Class)
   is
      Current_Extract : Ptr_Extract_Line := This.First;
      Last_File_Name  : Dynamic_String := Current_Extract.Cursor.File_Name;
      Offset_Line     : Integer := 0;
   begin
      while Current_Extract /= null loop
         if Current_Extract.Cursor.File_Name.all /= Last_File_Name.all then
            Last_File_Name := Current_Extract.Cursor.File_Name;
            Offset_Line := 0;
         end if;

         Commit (Current_Extract.all, Current_Text, Offset_Line);
         Current_Extract := Current_Extract.Next;
      end loop;
   end Commit;

   ----------
   -- Free --
   ----------

   procedure Free (This : in out Extract) is
   begin
      if This.First /= null then
         Free (This.First.all);
         Free (This.First);
      end if;
   end Free;

   ---------------
   -- Free_Data --
   ---------------

   procedure Free_Data (This : in out Extract'Class) is
   begin
      Free (This);
   end Free_Data;

   ------------------
   -- Get_New_Text --
   ------------------

   function Get_New_Text
     (This         : Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Lines_Before : Natural := 0;
      Lines_After  : Natural := 0) return String
   is
      Extended_Extract : Extract;
   begin
      Extended_Extract := Clone (This);
      Extend_Before (Extended_Extract, Current_Text, Lines_Before);
      Extend_After (Extended_Extract, Current_Text, Lines_After);

      declare
         Buffer : constant String := Get_New_Text (Extended_Extract);
      begin
         Free (Extended_Extract);
         return Buffer;
      end;
   end Get_New_Text;

   ------------------
   -- Get_New_Text --
   ------------------

   function Get_New_Text (This : Extract) return String is
      Current_Extract  : Ptr_Extract_Line := This.First;
      Current_Col      : Natural := 1;
      Current_Length   : Natural;
      Buffer : String
        (1 .. Get_New_Text_Length (This) +
           EOL_Str'Length * Get_Number_Lines (This));
   begin
      Current_Extract := This.First;

      while Current_Extract /= null loop
         Current_Length := Get_New_Text_Length (Current_Extract.all)
           + EOL_Str'Length;
         Buffer (Current_Col ..
                   Current_Col + Current_Length - 1) :=
             Get_New_Text (Current_Extract.all) & EOL_Str;
         Current_Col := Current_Col + Current_Length;
         Current_Extract := Current_Extract.Next;
      end loop;

      return Buffer;
   end Get_New_Text;

   ------------------
   -- Get_Old_Text --
   ------------------

   function Get_Old_Text
     (This         : Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Lines_Before : Natural := 0;
      Lines_After  : Natural := 0) return String
   is
      Current_Extract  : Ptr_Extract_Line;
      Extended_Extract : Extract;
      Current_Col      : Natural := 1;
      Current_Length   : Natural;
   begin

      Extended_Extract := Clone (This);
      Extend_Before (Extended_Extract, Current_Text, Lines_Before);
      Extend_After (Extended_Extract, Current_Text, Lines_After);
      Current_Extract := Extended_Extract.First;

      declare
         Buffer : String (1 .. Get_Old_Text_Length
                            (Extended_Extract, Current_Text) +
                            EOL_Str'Length * Get_Number_Lines
                              (Extended_Extract));
      begin
         while Current_Extract /= null loop
            Current_Length := Get_Old_Text_Length
              (Current_Extract.all,
               Current_Text) + EOL_Str'Length;
            Buffer (Current_Col ..
                      Current_Col + Current_Length - 1) :=
                Get_Old_Text (Current_Extract.all, Current_Text) & EOL_Str;
            Current_Col := Current_Col + Current_Length;
            Current_Extract := Current_Extract.Next;
         end loop;

         Free (Extended_Extract);

         return Buffer;
      end;
   end Get_Old_Text;

   -------------------------
   -- Get_New_Text_Length --
   -------------------------

   function Get_New_Text_Length (This : Extract) return Natural is
   begin
      if This.First /= null then
         return Get_New_Text_Length (This.First.all, True);
      else
         return 0;
      end if;
   end Get_New_Text_Length;

   -------------------------
   -- Get_Old_Text_Length --
   -------------------------

   function Get_Old_Text_Length
     (This         : Extract;
      Current_Text : Text_Navigator_Abstr'Class) return Natural is
   begin
      if This.First /= null then
         return Get_Old_Text_Length (This.First.all, Current_Text, True);
      else
         return 0;
      end if;
   end Get_Old_Text_Length;

   --------------
   -- Get_Line --
   --------------

   function Get_Line
     (This : Extract; Position : File_Cursor'Class) return Ptr_Extract_Line
   is
      Current_Extract : Ptr_Extract_Line := This.First;
   begin
      while Current_Extract /= null loop
         if Current_Extract.Cursor.Line = Position.Line
           and Current_Extract.Cursor.File_Name.all = Position.File_Name.all
         then
            return Current_Extract;
         end if;

         Current_Extract := Current_Extract.Next;
      end loop;

      Raise_Exception
        (Codefix_Panic'Identity,
         "line" & Natural'Image (Position.Line) & " of " &
           Position.File_Name.all & " not found in an extract");
   end Get_Line;

   ----------------
   -- Get_Record --
   ----------------

   function Get_Record
     (This : Extract; Number : Natural) return Ptr_Extract_Line
   is
      Current_Extract : Ptr_Extract_Line := This.First;
      Iterator        : Integer;

   begin

      Iterator := 1;
      loop
         if Current_Extract = null then
            Raise_Exception
              (Codefix_Panic'Identity,
               "record" & Natural'Image (Number) & " not found in an extract");
         end if;


         exit when Iterator = Number;

         Iterator := Iterator + 1;
         Current_Extract := Current_Extract.Next;

      end loop;

      return Current_Extract;
   end Get_Record;

   ------------------
   -- Replace_Word --
   ------------------

   procedure Replace_Word
     (This         : in out Extract;
      Cursor       : File_Cursor'Class;
      New_String   : String;
      Old_String   : String;
      Format_Old   : String_Mode := Text_Ascii)
   is
      Word_Length  : Natural;
      Old_Line     : Dynamic_String;
      Current_Line : Ptr_Extract_Line;
   begin
      Current_Line := Get_Line (This, Cursor);
      Assign (Old_Line, To_String (Current_Line.Content));

      case Format_Old is
         when Regular_Expression =>
            Word_Length := Get_Word_Length
              (Current_Line.all, Cursor.Col, Old_String);

         when Text_Ascii =>
            Word_Length := Old_String'Length;

      end case;

      Replace (Current_Line.all, Cursor.Col, Word_Length, New_String);

      Free (Old_Line);

   end Replace_Word;

   ------------------
   -- Replace_Word --
   ------------------

   procedure Replace_Word
     (This         : in out Extract;
      Cursor       : File_Cursor'Class;
      New_String   : String;
      Old_Length   : Natural)
   is
      Current_Line : Ptr_Extract_Line;
   begin
      Current_Line := Get_Line (This, Cursor);
      Replace (Current_Line.all, Cursor.Col, Old_Length, New_String);
   end Replace_Word;

   --------------
   -- Add_Word --
   --------------

   procedure Add_Word
     (This   : in out Extract;
      Cursor : File_Cursor'Class;
      Word   : String)
   is
      Current_Line : Ptr_Extract_Line := Get_Line (This, Cursor);
   begin
      Insert (Current_Line.Content, Cursor.Col, Word);

      Current_Line.Context := Unit_Modified;
   end Add_Word;

   ----------------------
   -- Get_Number_Lines --
   ----------------------

   function Get_Number_Lines (This : Extract) return Natural is
   begin
      if This.First = null then
         return 0;
      else
         return Length (This.First.all);
      end if;
   end Get_Number_Lines;

   --------------
   -- Add_Line --
   --------------

   procedure Add_Line
     (This   : in out Extract;
      Cursor : File_Cursor'Class;
      Text   : String)
   is
      Line_Cursor : File_Cursor := File_Cursor (Clone (Cursor));
   begin
      Line_Cursor.Col := 1;
      Add_Element
        (This, new Extract_Line'
         (Context         => Unit_Created,
          Cursor          => Line_Cursor,
          Original_Length => 0,
          Content         => To_Mergable_String (Text),
          Next            => null,
          Coloration      => True,
          Number_Actions  => 0));
   end Add_Line;

   -----------------------
   -- Add_Indented_Line --
   -----------------------

   procedure Add_Indented_Line
     (This         : in out Extract;
      Cursor       : File_Cursor'Class;
      Text         : String;
      Current_Text : Text_Navigator_Abstr'Class)
   is
      Line_Cursor : File_Cursor := File_Cursor (Cursor);
   begin
      Line_Cursor.Col := 1;

      Add_Indented_Line
        (This,
         Cursor,
         Text,
         Get_Line (Current_Text, Line_Cursor));
   end Add_Indented_Line;


   -----------------------
   -- Add_Indented_Line --
   -----------------------

   procedure Add_Indented_Line
     (This          : in out Extract;
      Cursor        : File_Cursor'Class;
      Text          : String;
      Previous_Line : String)
   is
      Line_Cursor         : File_Cursor := File_Cursor (Clone (Cursor));
      Indent, Next_Indent : Natural;
      First_Char          : Natural := Text'First;
   begin
      while First_Char <= Text'Last and then Is_Blank (Text (First_Char)) loop
         First_Char := First_Char + 1;
      end loop;

      Line_Cursor.Col := 1;

      Next_Indentation (Unknown_Lang, Previous_Line, Indent, Next_Indent);

      Add_Element
        (This, new Extract_Line'
          (Context         => Unit_Created,
           Cursor          => Line_Cursor,
           Original_Length => 0,
           Content         =>
              To_Mergable_String ((1 .. Next_Indent => ' ') &
                                   Text (First_Char .. Text'Last)),
           Next            => null,
           Coloration      => True,
           Number_Actions  => 0));
   end Add_Indented_Line;

   -----------------
   -- Delete_Line --
   -----------------

   procedure Delete_Line
     (This   : in out Extract;
      Cursor : File_Cursor'Class)
   is
      Line : Ptr_Extract_Line;
   begin
      Line := Get_Line (This, Cursor);
      Line.Context := Unit_Deleted;
   end Delete_Line;

   ----------------------
   -- Delete_All_Lines --
   ----------------------

   procedure Delete_All_Lines (This : in out Extract) is
      Line : Ptr_Extract_Line := This.First;
   begin
      while Line /= null loop
         Line.Context := Unit_Deleted;
         Line := Line.Next;
      end loop;
   end Delete_All_Lines;

   -----------------
   -- Add_Element --
   -----------------


   --  Assertion : This and Previous are not null together
   procedure Add_Element
     (This, Previous, Element : Ptr_Extract_Line;
      Container : in out Extract) is
   begin
      if This = null then
         Previous.Next := Element;
      else
         if This.Cursor > Element.Cursor then
            Element.Next := This;

            if Previous /= null then
               Previous.Next := Element;
            else
               Container.First := Element;
            end if;
         elsif This.Cursor = Element.Cursor
           and then (This.Context = Unit_Modified
                     or else This.Context = Original_Unit)
           and then (Element.Context = Unit_Modified
                     or else Element.Context = Original_Unit)
         then
            return;
         else
            Add_Element (This.Next, This, Element, Container);
         end if;
      end if;
   end Add_Element;

   -----------------
   -- Add_Element --
   -----------------

   procedure Add_Element (This : in out Extract; Element : Ptr_Extract_Line) is
   begin
      if This.First = null then
         This.First := Element;
      else
         Add_Element (This.First, null, Element, This);
      end if;
   end Add_Element;

   procedure Add_Element (This : in out Extract; Element : Extract_Line) is
   begin
      Add_Element (This, new Extract_Line'(Element));
   end Add_Element;


   ---------------------
   -- Get_Word_Length --
   ---------------------

   function Get_Word_Length
     (This   : Extract;
      Cursor : File_Cursor'Class;
      Format : String) return Natural is
   begin
      return Get_Word_Length
        (Get_Line (This, Cursor).all,
         Cursor.Col,
         Format);
   end Get_Word_Length;

   -------------------
   -- Search_String --
   -------------------

   function Search_String
     (This           : Extract;
      Searched       : String;
      Escape_Manager : Escape_Str_Manager'Class;
      Cursor         : File_Cursor'Class := Null_File_Cursor;
      Step           : Step_Way := Normal_Step)
     return File_Cursor'Class is

      Current                 : Ptr_Extract_Line;
      Result, Current_Cursor  : File_Cursor := Null_File_Cursor;

   begin

      if File_Cursor (Cursor) /= Null_File_Cursor then
         Current_Cursor := File_Cursor (Cursor);
      else
         if Step = Normal_Step then
            Current_Cursor :=
              File_Cursor (Get_Cursor (Get_First_Line (This).all));
         else
            null; --  ??? Soon programmed
         end if;
      end if;

      Current := Get_Line (This, Current_Cursor);

      loop

         if Current = null then return Null_File_Cursor; end if;

         Result := File_Cursor (Search_String
                                  (Current.all,
                                   Current_Cursor,
                                   Searched,
                                   Escape_Manager,
                                   Step));

         if Result /= Null_File_Cursor then
            Result.Line := Get_Cursor (Current.all).Line;
            return Result;
         end if;

         exit when Current = null;

         case Step is
            when Normal_Step =>
               Current := Current.Next;
               Current_Cursor.Col := 1;
            when Reverse_Step =>
               Current := Previous (This, Current);
               Current_Cursor.Col := 0;
         end case;

      end loop;

      return Null_File_Cursor;

   end Search_String;

   --------------
   -- Previous --
   --------------

   function Previous (Container : Extract; Node : Ptr_Extract_Line)
     return Ptr_Extract_Line is

      Current : Ptr_Extract_Line := Container.First;

   begin
      if Current = Node then return null; end if;

      while Current.Next /= Node loop
         Current := Current.Next;
         if Current = null then
            Raise_Exception
              (Codefix_Panic'Identity,
               "Line unknowm in the specified extract");
         end if;
      end loop;
      return Current;
   end Previous;

   --------------------
   -- Get_First_Line --
   --------------------

   function Get_First_Line (This : Extract) return Ptr_Extract_Line is
   begin
      return This.First;
   end Get_First_Line;

   -------------------
   -- Extend_Before --
   -------------------

   procedure Extend_Before
     (This         : in out Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Size         : Natural) is

      Null_Prev : Ptr_Extract_Line := null;

   begin
      Extend_Before (This.First, Null_Prev, Current_Text, Size);
   end Extend_Before;

   ------------------
   -- Extend_After --
   ------------------

   procedure Extend_After
     (This         : in out Extract;
      Current_Text : Text_Navigator_Abstr'Class;
      Size         : Natural) is
   begin
      Extend_After (This.First, Current_Text, Size);
   end Extend_After;

   ------------
   -- Reduce --
   ------------

   procedure Reduce
     (This                    : in out Extract;
      Size_Before, Size_After : Natural) is

      type Lines_Array is array (Natural range <>) of Ptr_Extract_Line;

      procedure Delete_First;
      procedure Add_Last;

      Current_Line : Ptr_Extract_Line;
      Lines        : Lines_Array (1 .. Size_Before);
      Left_After   : Natural := 0;
      Next_Line    : Ptr_Extract_Line;
      Prev_Line    : Ptr_Extract_Line;

      procedure Delete_First is
      begin
         Remove (Lines (1), Previous (This, Lines (1)), This);
         Free (Lines (1).all);
         Free (Lines (1));
         Lines (1 .. Lines'Last - 1) := Lines (2 .. Lines'Last);
         Lines (Lines'Last) := null;
      end Delete_First;

      procedure Add_Last is
      begin
         for J in Lines'Range loop
            if Lines (J) = null then
               Lines (J) := Current_Line;
               return;
            end if;
         end loop;

         Delete_First;
         Lines (Lines'Last) := Current_Line;
      end Add_Last;

   begin

      Current_Line := Get_First_Line (This);

      while Current_Line /= null loop
         Next_Line := Next (Current_Line.all);

         if Current_Line.Coloration = False
           or else Current_Line.Context = Original_Unit
         then
            if Left_After = 0 then
               if Size_Before = 0 then
                  Remove (Current_Line, Prev_Line, This);
                  Free (Current_Line.all);
                  Free (Current_Line);
               else
                  Prev_Line := Current_Line;
                  Add_Last;
               end if;
            else
               Prev_Line := Current_Line;
               Left_After := Left_After - 1;
            end if;
         else
            Prev_Line := Current_Line;
            Left_After := Size_After;
            Lines := (others => null);
         end if;

         Current_Line := Next_Line;
      end loop;

      if Left_After = 0 then
         for J in Lines'Range loop
            exit when Lines (J) = null;
            Remove (Lines (J), Previous (This, Lines (J)), This);
            Free (Lines (J).all);
            Free (Lines (J));
         end loop;
      end if;

   end Reduce;

   -----------
   -- Erase --
   -----------

   procedure Erase
     (This        : in out Extract;
      Start, Stop : File_Cursor'Class)
   is
      Current_Line : Ptr_Extract_Line;
      Line_Cursor  : File_Cursor := File_Cursor (Start);
   begin
      Line_Cursor.Col := 1;

      if Start.Line = Stop.Line then
         Current_Line := Get_Line (This, Line_Cursor);

         Delete (Current_Line.Content, Start.Col, Stop.Col - Start.Col + 1);

         if Is_Blank (To_String (Current_Line.Content)) then
            Current_Line.Context := Unit_Deleted;
         else
            Current_Line.Context := Unit_Modified;
         end if;
      else
         Current_Line := Get_Line (This, Line_Cursor);
         Delete (Current_Line.Content, Start.Col);

         if Is_Blank (To_String (Current_Line.Content)) then
            Current_Line.Context := Unit_Deleted;
         else
            Current_Line.Context := Unit_Modified;
         end if;

         for J in Start.Line + 1 .. Stop.Line - 1 loop
            Current_Line := Next (Current_Line.all);
            Current_Line.Context := Unit_Deleted;
         end loop;

         Current_Line := Next (Current_Line.all);

         Delete (Current_Line.Content, 1, Stop.Col);

         if Is_Blank (To_String (Current_Line.Content)) then
            Current_Line.Context := Unit_Deleted;
         else
            Current_Line.Context := Unit_Modified;
         end if;
      end if;
   end Erase;

   ---------------------
   -- Get_Files_Names --
   ---------------------

   function Get_Files_Names (This : Extract; Size_Max : Natural := 0)
     return String
   is
      Previous     : Dynamic_String;
      Current_Line : Ptr_Extract_Line := Get_First_Line (This);
      Result       : Dynamic_String;
   begin
      if Current_Line = null then
         return "";
      end if;

      Previous := Current_Line.Cursor.File_Name;
      Assign (Result, Previous);

      while Current_Line /= null loop
         if Previous.all /= Current_Line.Cursor.File_Name.all then
            Assign
              (Result, Result.all & " / " & Current_Line.Cursor.File_Name.all);
            Previous := Current_Line.Cursor.File_Name;
         end if;
         Current_Line := Current_Line.Next;
      end loop;

      declare
         Result_Stack : constant String := Result.all;
      begin
         Free (Result);
         if Size_Max > 0 and then Result_Stack'Length > Size_Max then
            return Result_Stack
              (Result_Stack'First .. Result_Stack'First + Size_Max)
               & "...";
         else
            return Result_Stack;
         end if;
      end;
   end Get_Files_Names;

   ------------------
   -- Get_Nb_Files --
   ------------------

   function Get_Nb_Files (This : Extract) return Natural is
      Previous     : Dynamic_String;
      Current_Line : Ptr_Extract_Line := Get_First_Line (This);
      Total        : Natural := 1;
   begin
      if Current_Line = null then
         return 0;
      end if;

      Previous := Current_Line.Cursor.File_Name;

      while Current_Line /= null loop
         if Previous.all /= Current_Line.Cursor.File_Name.all then
            Total := Total + 1;
            Previous := Current_Line.Cursor.File_Name;
         end if;
         Current_Line := Current_Line.Next;
      end loop;

      return Total;
   end Get_Nb_Files;

   ------------------------
   -- Delete_Empty_Lines --
   ------------------------

   procedure Delete_Empty_Lines (This : in out Extract) is
      Node : Ptr_Extract_Line := Get_First_Line (This);
   begin
      while Node /= null loop
         if Is_Blank (Get_String (Node.all)) then
            Node.Context := Unit_Deleted;
         end if;

         Node := Next (Node);
      end loop;
   end Delete_Empty_Lines;

   ------------------------
   -- Get_Number_Actions --
   ------------------------

   function Get_Number_Actions (This : Extract) return Natural is
      Line  : Ptr_Extract_Line := Get_First_Line (This);
      Total : Natural := 0;
   begin
      while Line /= null loop
         Total := Total + Line.Number_Actions;
         Line := Next (Line);
      end loop;

      return Total;
   end Get_Number_Actions;

   ----------------------------------------------------------------------------
   --  type Text_Command
   ----------------------------------------------------------------------------

   ------------------
   -- Text_Command --
   ------------------

   procedure Free (This : in out Text_Command) is
   begin
      Free (This.Caption);
   end Free;

   procedure Free_Data (This : in out Text_Command'Class) is
   begin
      Free (This);
   end Free_Data;

   procedure Set_Caption
     (This : in out Text_Command'Class;
      Caption : String) is
   begin
      Assign (This.Caption, Caption);
   end Set_Caption;

   function Get_Caption (This : Text_Command'Class) return String is
   begin
      return This.Caption.all;
   end Get_Caption;

   procedure Secured_Execute
     (This         : Text_Command'Class;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class;
      Success      : out Boolean) is
   begin
      Success := True;
      Update_All (Current_Text);
      Execute (This, Current_Text, New_Extract);

   exception
      when Codefix_Panic =>
         Success := False;
   end Secured_Execute;

   -----------------
   -- Word_Cursor --
   -----------------

   procedure Free (This : in out Word_Cursor) is
   begin
      Free (This.String_Match);
      Free (File_Cursor (This));
   end Free;

   function Clone (This : Word_Cursor) return Word_Cursor is
   begin
      return (Clone (File_Cursor (This)) with
              Clone (This.String_Match), This.Mode);
   end Clone;

   procedure Make_Word_Mark
     (Word         : Word_Cursor;
      Current_Text : Text_Navigator_Abstr'Class;
      Mark         : out Word_Mark) is
   begin
      Mark.Mode := Word.Mode;
      Assign (Mark.String_Match, Word.String_Match);
      Mark.Mark_Id := new Mark_Abstr'Class'(Get_New_Mark (Current_Text, Word));
   end Make_Word_Mark;

   procedure Make_Word_Cursor
     (Word         : Word_Mark;
      Current_Text : Text_Navigator_Abstr'Class;
      Cursor       : out Word_Cursor) is
   begin
      Cursor := (File_Cursor
                   (Get_Current_Cursor (Current_Text, Word.Mark_Id.all))
                 with null, Text_Ascii);
      Cursor.Mode := Word.Mode;
      Assign (Cursor.String_Match, Word.String_Match);

   end Make_Word_Cursor;

   procedure Free (This : in out Word_Mark) is
   begin
      Free (This.String_Match);
      Free (This.Mark_Id);
   end Free;

   ---------------------
   -- Remove_Word_Cmd --
   ---------------------

   procedure Initialize
     (This         : in out Remove_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor'Class) is
   begin
      Make_Word_Mark (Word, Current_Text, This.Word);
   end Initialize;

   procedure Free (This : in out Remove_Word_Cmd) is
   begin
      Free (This.Word);
      Free (Text_Command (This));
   end Free;

   procedure Execute
     (This         : Remove_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Line_Cursor : File_Cursor;
      Word        : Word_Cursor;
   begin

      Make_Word_Cursor (This.Word, Current_Text, Word);
      Line_Cursor := File_Cursor (Word);

      Line_Cursor.Col := 1;
      Get_Line (Current_Text, Line_Cursor, New_Extract);

      case Word.Mode is
         when Text_Ascii =>
            Delete
              (Get_First_Line (New_Extract).Content,
               Word.Col,
               Word.String_Match'Length);
         when Regular_Expression =>
            Delete
              (Get_First_Line (New_Extract).Content,
               Word.Col,
               Get_Word_Length
                 (New_Extract, Word, Word.String_Match.all));
      end case;

      Get_First_Line (New_Extract).Context := Unit_Modified;
      Delete_Empty_Lines (New_Extract);

      Free (Word);
   end Execute;

   ---------------------
   -- Insert_Word_Cmd --
   ---------------------

   procedure Initialize
     (This         : in out Insert_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor'Class;
      Add_Spaces   : Boolean := True;
      Position     : Relative_Position := Specified) is
   begin
      This.Add_Spaces := Add_Spaces;
      This.Position := Position;
      Make_Word_Mark (Word, Current_Text, This.Word);
   end Initialize;

   procedure Free (This : in out Insert_Word_Cmd) is
   begin
      Free (This.Word);
      Free (Text_Command (This));
   end Free;

   procedure Execute
     (This         : Insert_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      New_Str      : Dynamic_String;
      Line_Cursor  : File_Cursor;
      Space_Cursor : File_Cursor;
      Word         : Word_Cursor;

   begin
      Make_Word_Cursor (This.Word, Current_Text, Word);
      Line_Cursor := File_Cursor (Word);

      Assign (New_Str, Word.String_Match);
      Line_Cursor.Col := 1;

      if This.Position = Specified then
         Get_Line (Current_Text, Line_Cursor, New_Extract);

         if This.Add_Spaces then
            Space_Cursor := File_Cursor (Word);
            Space_Cursor.Col := Space_Cursor.Col - 1;

            if  Word.Col > 1
              and then not Is_Separator
                (Get (Current_Text, Space_Cursor, 1) (1))
            then
               Assign (New_Str, " " & New_Str.all);
            end if;

            Space_Cursor.Col := Space_Cursor.Col + 1;

            if Word.Col < Line_Length (Current_Text, Line_Cursor)
              and then not Is_Separator
                (Get (Current_Text, Space_Cursor, 1) (1))
            then
               Assign (New_Str, New_Str.all & " ");
            end if;
         end if;

         Add_Word (New_Extract, Word, New_Str.all);
      elsif This.Position = After then
         Add_Line (New_Extract, Line_Cursor, New_Str.all);
      elsif This.Position = Before then
         Line_Cursor.Line := Line_Cursor.Line - 1;
         Add_Line (New_Extract, Line_Cursor, New_Str.all);
      end if;

      Free (New_Str);
      Free (Word);
   end Execute;

   -------------------
   -- Move_Word_Cmd --
   -------------------

   procedure Initialize
     (This         : in out Move_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor'Class;
      New_Position : File_Cursor'Class)
   is
      New_Word : Word_Cursor;
      File     : File_Cursor;
   begin
      File := Clone (File_Cursor (New_Position));
      New_Word := (File with
                   Word.String_Match, Word.Mode);
      Initialize (This.Step_Remove, Current_Text, Word);
      Initialize (This.Step_Insert, Current_Text, New_Word);
   end Initialize;

   procedure Free (This : in out Move_Word_Cmd) is
   begin
      Free (This.Step_Remove);
      Free (This.Step_Insert);
      Free (Text_Command (This));
   end Free;

   procedure Execute
     (This         : Move_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Extract_Remove, Extract_Insert : Extract;
      Success                        : Boolean;
   begin
      Execute (This.Step_Remove, Current_Text, Extract_Remove);
      Execute (This.Step_Insert, Current_Text, Extract_Insert);
      Merge_Extracts
        (New_Extract,
         Extract_Remove,
         Extract_Insert,
         Success,
         False);

      if not Success then
         raise Codefix_Panic;
      end if;

      Free (Extract_Remove);
      Free (Extract_Insert);
   end Execute;

   ----------------------
   -- Replace_Word_Cmd --
   ----------------------

   procedure Initialize
     (This         : in out Replace_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word         : Word_Cursor'Class;
      New_Word     : String) is
   begin
      Make_Word_Mark (Word, Current_Text, This.Mark);
      Assign (This.Str_Expected, New_Word);
   end Initialize;

   procedure Free (This : in out Replace_Word_Cmd) is
   begin
      Free (This.Mark);
      Free (This.Str_Expected);
      Free (Text_Command (This));
   end Free;

   procedure Execute
     (This         : Replace_Word_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Current_Word : Word_Cursor;
      Line_Cursor  : File_Cursor;
   begin
      Make_Word_Cursor (This.Mark, Current_Text, Current_Word);

      Line_Cursor := File_Cursor (Current_Word);
      Line_Cursor.Col := 1;
      Get_Line (Current_Text, Line_Cursor, New_Extract);

      Replace_Word
        (New_Extract,
         Current_Word,
         This.Str_Expected.all,
         Current_Word.String_Match.all,
         Current_Word.Mode);

      Free (Current_Word);
   end Execute;

   ---------------------
   -- Invert_Word_Cmd --
   ---------------------

   procedure Initialize
     (This         : in out Invert_Words_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      Word1, Word2 : Word_Cursor'Class) is
   begin
      Initialize
        (This.Step_Word1, Current_Text, Word1, Word2.String_Match.all);
      Initialize
        (This.Step_Word2, Current_Text, Word2, Word1.String_Match.all);
   end Initialize;

   procedure Free (This : in out Invert_Words_Cmd) is
   begin
      Free (This.Step_Word1);
      Free (This.Step_Word2);
      Free (Text_Command (This));
   end Free;

   procedure Execute
     (This         : Invert_Words_Cmd;
      Current_Text : Text_Navigator_Abstr'Class;
      New_Extract  : out Extract'Class)
   is
      Extract1, Extract2 : Extract;
      Success            : Boolean;
   begin
      Execute (This.Step_Word1, Current_Text, Extract1);
      Execute (This.Step_Word2, Current_Text, Extract2);
      Merge_Extracts
        (New_Extract,
         Extract1,
         Extract2,
         Success,
         False);

      if not Success then
         raise Codefix_Panic;
      end if;

      Free (Extract1);
      Free (Extract2);
   end Execute;

   --------------------
   -- Merge_Extracts --
   --------------------

   procedure Merge_Extracts
     (Result              : out Extract'Class;
      Object_1, Object_2  : Extract'Class;
      Success             : out Boolean;
      Chronologic_Changes : Boolean)
   is

      procedure Merge_Intern is new Generic_Merge
        (Merge_Type     => Extract,
         Merged_Unit    => Extract_Line,
         Merge_Iterator => Ptr_Extract_Line,
         First          => Get_First_Line,
         Get_Merge_Info => Get_Context,
         Set_Merge_Info => Set_Context,
         Append         => Add_Element,
         Merge_Units    => Merge_Lines);

   begin
      Merge_Intern
        (Extract (Result),
         Extract (Object_1), Extract (Object_2),
         Success,
         Chronologic_Changes);
   end Merge_Extracts;

end Codefix.Text_Manager;
