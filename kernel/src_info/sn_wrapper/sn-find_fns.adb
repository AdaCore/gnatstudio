with SN.DB_Structures,
     Ada.Unchecked_Deallocation,
     DB_API;
use  SN.DB_Structures,
     DB_API;

package body SN.Find_Fns is
      procedure Delete is
         new Ada.Unchecked_Deallocation (String, String_Access);

      --  Find functions for Referred by table
      function Find (DB : DB_File;
            Ref_Class : String := Invalid_String;
            Ref_Symbol_Name : String := Invalid_String;
            Ref_Type : String := Invalid_String;
            Class : String := Invalid_String;
            Symbol_Name : String := Invalid_String;
            Sym_Type : Symbol_Type := Undef;
            Access_Type : String := Invalid_String;
            Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return BY_Table is
         P    : Pair_Ptr;
         Tab  : BY_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Ref_Class = Invalid_String then
               Fall := True;
            else
               Len := Len + Ref_Class'Length + 1;
            end if;
         end if;
         if not Fall then
            if Ref_Symbol_Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Ref_Symbol_Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Ref_Type = Invalid_String then
               Fall := True;
            else
               Len := Len + Ref_Type'Length + 1;
            end if;
         end if;
         if not Fall then
            if Class = Invalid_String then
               Fall := True;
            else
               Len := Len + Class'Length + 1;
            end if;
         end if;
         if not Fall then
            if Symbol_Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Symbol_Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Sym_Type = Undef then
               Fall := True;
            else
               --  Symbol type is 3 letters at most
               Len := Len + 3 + 1;
            end if;
         end if;
         if not Fall then
            if Access_Type = Invalid_String then
               Fall := True;
            else
               Len := Len + Access_Type'Length + 1;
            end if;
         end if;
         if not Fall then
            if Position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Ref_Class = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Ref_Class'Length - 1)
                  := Ref_Class;
               Pos := Pos + Ref_Class'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Ref_Symbol_Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Ref_Symbol_Name'Length - 1)
                  := Ref_Symbol_Name;
               Pos := Pos + Ref_Symbol_Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Ref_Type = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Ref_Type'Length - 1)
                  := Ref_Type;
               Pos := Pos + Ref_Type'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Class = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Class'Length - 1)
                  := Class;
               Pos := Pos + Class'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Symbol_Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Symbol_Name'Length - 1)
                  := Symbol_Name;
               Pos := Pos + Symbol_Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Sym_Type = Undef then
               Fall := True;
            else
               To_String (Sym_Type, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Access_Type = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Access_Type'Length - 1)
                  := Access_Type;
               Pos := Pos + Access_Type'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Position = Invalid_Point then
               Fall := True;
            else
               To_String (Position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Classes table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return CL_Table is
         P    : Pair_Ptr;
         Tab  : CL_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Name'Length - 1)
                  := Name;
               Pos := Pos + Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               To_String (Start_position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Common blocks table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return COM_Table is
         P    : Pair_Ptr;
         Tab  : COM_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Name'Length - 1)
                  := Name;
               Pos := Pos + Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               To_String (Start_position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Constants table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return CON_Table is
         P    : Pair_Ptr;
         Tab  : CON_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Name'Length - 1)
                  := Name;
               Pos := Pos + Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               To_String (Start_position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Common value table
      function Find (DB : DB_File;
            Common_Block : String := Invalid_String;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return COV_Table is
         P    : Pair_Ptr;
         Tab  : COV_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Common_Block = Invalid_String then
               Fall := True;
            else
               Len := Len + Common_Block'Length + 1;
            end if;
         end if;
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Common_Block = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Common_Block'Length - 1)
                  := Common_Block;
               Pos := Pos + Common_Block'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Name'Length - 1)
                  := Name;
               Pos := Pos + Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               To_String (Start_position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Enumerations table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return E_Table is
         P    : Pair_Ptr;
         Tab  : E_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Name'Length - 1)
                  := Name;
               Pos := Pos + Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               To_String (Start_position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Enum-constants table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return EC_Table is
         P    : Pair_Ptr;
         Tab  : EC_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Name'Length - 1)
                  := Name;
               Pos := Pos + Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               To_String (Start_position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Project files table
      function Find (DB : DB_File;
            Name : String := Invalid_String)
      return F_Table is
         P    : Pair_Ptr;
         Tab  : F_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Name'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Name'Length - 1)
                  := Name;
               Pos := Pos + Name'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Function table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return FD_Table is
         P    : Pair_Ptr;
         Tab  : FD_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Name'Length - 1)
                  := Name;
               Pos := Pos + Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               To_String (Start_position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Symbols of files table
      function Find (DB : DB_File;
            Filename : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Class : String := Invalid_String;
            Identifier : String := Invalid_String;
            Sym_Type : Symbol_Type := Undef)
      return FIL_Table is
         P    : Pair_Ptr;
         Tab  : FIL_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Class = Invalid_String then
               Fall := True;
            else
               Len := Len + Class'Length + 1;
            end if;
         end if;
         if not Fall then
            if Identifier = Invalid_String then
               Fall := True;
            else
               Len := Len + Identifier'Length + 1;
            end if;
         end if;
         if not Fall then
            if Sym_Type = Undef then
               Fall := True;
            else
               --  Symbol type is 3 letters at most
               Len := Len + 3;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               To_String (Start_position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Class = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Class'Length - 1)
                  := Class;
               Pos := Pos + Class'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Identifier = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Identifier'Length - 1)
                  := Identifier;
               Pos := Pos + Identifier'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Sym_Type = Undef then
               Fall := True;
            else
               To_String (Sym_Type, Key, Pos);
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Friends table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return FR_Table is
         P    : Pair_Ptr;
         Tab  : FR_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Name'Length - 1)
                  := Name;
               Pos := Pos + Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               To_String (Start_position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Functions table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return FU_Table is
         P    : Pair_Ptr;
         Tab  : FU_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Name'Length - 1)
                  := Name;
               Pos := Pos + Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               To_String (Start_position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Variables table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return GV_Table is
         P    : Pair_Ptr;
         Tab  : GV_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Name'Length - 1)
                  := Name;
               Pos := Pos + Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Position = Invalid_Point then
               Fall := True;
            else
               To_String (Position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Inheritances table
      function Find (DB : DB_File;
            Class : String := Invalid_String;
            Base_Class : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return IN_Table is
         P    : Pair_Ptr;
         Tab  : IN_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Class = Invalid_String then
               Fall := True;
            else
               Len := Len + Class'Length + 1;
            end if;
         end if;
         if not Fall then
            if Base_Class = Invalid_String then
               Fall := True;
            else
               Len := Len + Base_Class'Length + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Class = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Class'Length - 1)
                  := Class;
               Pos := Pos + Class'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Base_Class = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Base_Class'Length - 1)
                  := Base_Class;
               Pos := Pos + Base_Class'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               To_String (Start_position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Include table
      function Find (DB : DB_File;
            Included_file : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Include_from_file : String := Invalid_String)
      return IU_Table is
         P    : Pair_Ptr;
         Tab  : IU_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Included_file = Invalid_String then
               Fall := True;
            else
               Len := Len + Included_file'Length + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Include_from_file = Invalid_String then
               Fall := True;
            else
               Len := Len + Include_from_file'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Included_file = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Included_file'Length - 1)
                  := Included_file;
               Pos := Pos + Included_file'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               To_String (Start_position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Include_from_file = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Include_from_file'Length - 1)
                  := Include_from_file;
               Pos := Pos + Include_from_file'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Instance variables table
      function Find (DB : DB_File;
            Class : String := Invalid_String;
            Variable_Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return IV_Table is
         P    : Pair_Ptr;
         Tab  : IV_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Class = Invalid_String then
               Fall := True;
            else
               Len := Len + Class'Length + 1;
            end if;
         end if;
         if not Fall then
            if Variable_Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Variable_Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Class = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Class'Length - 1)
                  := Class;
               Pos := Pos + Class'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Variable_Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Variable_Name'Length - 1)
                  := Variable_Name;
               Pos := Pos + Variable_Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               To_String (Start_position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Local variables table
      function Find (DB : DB_File;
            Function_Name : String := Invalid_String;
            Variable_Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return LV_Table is
         P    : Pair_Ptr;
         Tab  : LV_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Function_Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Function_Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Variable_Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Variable_Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Function_Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Function_Name'Length - 1)
                  := Function_Name;
               Pos := Pos + Function_Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Variable_Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Variable_Name'Length - 1)
                  := Variable_Name;
               Pos := Pos + Variable_Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               To_String (Start_position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Macros table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return MA_Table is
         P    : Pair_Ptr;
         Tab  : MA_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Name'Length - 1)
                  := Name;
               Pos := Pos + Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               To_String (Start_position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Method definitions table
      function Find (DB : DB_File;
            Class : String := Invalid_String;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return MD_Table is
         P    : Pair_Ptr;
         Tab  : MD_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Class = Invalid_String then
               Fall := True;
            else
               Len := Len + Class'Length + 1;
            end if;
         end if;
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Class = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Class'Length - 1)
                  := Class;
               Pos := Pos + Class'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Name'Length - 1)
                  := Name;
               Pos := Pos + Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               To_String (Start_position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Method implementations table
      function Find (DB : DB_File;
            Class : String := Invalid_String;
            Name : String := Invalid_String;
            Start_position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return MI_Table is
         P    : Pair_Ptr;
         Tab  : MI_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Class = Invalid_String then
               Fall := True;
            else
               Len := Len + Class'Length + 1;
            end if;
         end if;
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Class = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Class'Length - 1)
                  := Class;
               Pos := Pos + Class'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Name'Length - 1)
                  := Name;
               Pos := Pos + Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Start_position = Invalid_Point then
               Fall := True;
            else
               To_String (Start_position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Remarks table
      function Find (DB : DB_File;
            Filename : String := Invalid_String;
            Position : Point := Invalid_Point;
            Class : String := Invalid_String;
            Method_or_function : String := Invalid_String)
      return REM_Table is
         P    : Pair_Ptr;
         Tab  : REM_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length + 1;
            end if;
         end if;
         if not Fall then
            if Position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Class = Invalid_String then
               Fall := True;
            else
               Len := Len + Class'Length + 1;
            end if;
         end if;
         if not Fall then
            if Method_or_function = Invalid_String then
               Fall := True;
            else
               Len := Len + Method_or_function'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Position = Invalid_Point then
               Fall := True;
            else
               To_String (Position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Class = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Class'Length - 1)
                  := Class;
               Pos := Pos + Class'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Method_or_function = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Method_or_function'Length - 1)
                  := Method_or_function;
               Pos := Pos + Method_or_function'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Subroutines table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return SU_Table is
         P    : Pair_Ptr;
         Tab  : SU_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Name'Length - 1)
                  := Name;
               Pos := Pos + Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Position = Invalid_Point then
               Fall := True;
            else
               To_String (Position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Typedefs table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return T_Table is
         P    : Pair_Ptr;
         Tab  : T_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Name'Length - 1)
                  := Name;
               Pos := Pos + Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Position = Invalid_Point then
               Fall := True;
            else
               To_String (Position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Refers to table
      function Find (DB : DB_File;
            Class : String := Invalid_String;
            Symbol_Name : String := Invalid_String;
            Sym_Type : Symbol_Type := Undef;
            Ref_Class : String := Invalid_String;
            Ref_Symbol : String := Invalid_String;
            Ref_Type : String := Invalid_String;
            Access_Type : String := Invalid_String;
            Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return TO_Table is
         P    : Pair_Ptr;
         Tab  : TO_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Class = Invalid_String then
               Fall := True;
            else
               Len := Len + Class'Length + 1;
            end if;
         end if;
         if not Fall then
            if Symbol_Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Symbol_Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Sym_Type = Undef then
               Fall := True;
            else
               --  Symbol type is 3 letters at most
               Len := Len + 3 + 1;
            end if;
         end if;
         if not Fall then
            if Ref_Class = Invalid_String then
               Fall := True;
            else
               Len := Len + Ref_Class'Length + 1;
            end if;
         end if;
         if not Fall then
            if Ref_Symbol = Invalid_String then
               Fall := True;
            else
               Len := Len + Ref_Symbol'Length + 1;
            end if;
         end if;
         if not Fall then
            if Ref_Type = Invalid_String then
               Fall := True;
            else
               Len := Len + Ref_Type'Length + 1;
            end if;
         end if;
         if not Fall then
            if Access_Type = Invalid_String then
               Fall := True;
            else
               Len := Len + Access_Type'Length + 1;
            end if;
         end if;
         if not Fall then
            if Position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Class = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Class'Length - 1)
                  := Class;
               Pos := Pos + Class'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Symbol_Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Symbol_Name'Length - 1)
                  := Symbol_Name;
               Pos := Pos + Symbol_Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Sym_Type = Undef then
               Fall := True;
            else
               To_String (Sym_Type, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Ref_Class = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Ref_Class'Length - 1)
                  := Ref_Class;
               Pos := Pos + Ref_Class'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Ref_Symbol = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Ref_Symbol'Length - 1)
                  := Ref_Symbol;
               Pos := Pos + Ref_Symbol'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Ref_Type = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Ref_Type'Length - 1)
                  := Ref_Type;
               Pos := Pos + Ref_Type'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Access_Type = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Access_Type'Length - 1)
                  := Access_Type;
               Pos := Pos + Access_Type'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Position = Invalid_Point then
               Fall := True;
            else
               To_String (Position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;



      --  Find functions for Unions table
      function Find (DB : DB_File;
            Name : String := Invalid_String;
            Position : Point := Invalid_Point;
            Filename : String := Invalid_String)
      return UN_Table is
         P    : Pair_Ptr;
         Tab  : UN_Table;
         Key  : String_Access;
         Fall : Boolean := False;
         Len  : Integer := 0;
         Pos  : Integer := 1;
      begin
         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Len := Len + Name'Length + 1;
            end if;
         end if;
         if not Fall then
            if Position = Invalid_Point then
               Fall := True;
            else
               Len := Len + 11;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Len := Len + Filename'Length;
            end if;
         end if;

         Key := new String (1 .. Len);
         Fall := False;

         if not Fall then
            if Name = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Name'Length - 1)
                  := Name;
               Pos := Pos + Name'Length;
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Position = Invalid_Point then
               Fall := True;
            else
               To_String (Position, Key, Pos);
               Key (Pos) := Field_Sep;
               Pos := Pos + 1;
            end if;
         end if;
         if not Fall then
            if Filename = Invalid_String then
               Fall := True;
            else
               Key (Pos .. Pos + Filename'Length - 1)
                  := Filename;
               Pos := Pos + Filename'Length;
            end if;
         end if;
         Set_Cursor (DB, By_Key, Key.all, False);
         Delete (Key);
         P   := Get_Pair (DB, Next_By_Key);
         if null = P then
            raise Not_Found;
         end if;
         Tab := Parse_Pair (P.all);
         Free (P);
         return Tab;
      end Find;


      procedure To_String (P : in Point; Str : in String_Access;
            Where : in out Integer) is
         Line_Img : String := Integer'Image (P.Line);
         Col_Img  : String := Integer'Image (P.Column);
      begin
         Str (Where .. Where + 9) := "000000.000";
         Str (Where + 5 - Line_Img'Length + 2 .. Where + 5)
            := Line_Img (2 .. Line_Img'Length);
         Where := Where + 7;
         Str (Where + 2 - Col_Img'Length + 2 .. Where + 2)
            := Col_Img (2 .. Col_Img'Length);
         Where := Where + 3;
      end To_String;

      procedure To_String (Sym_Type : in Symbol_Type; Str : in String_Access;
          Where : in out Integer) is
      begin
         case Sym_Type is
            when CL    => Str (Where .. Where + 1) := "cl";
                        Where := Where + 2;
            when COM   => Str (Where .. Where + 2) := "com";
                        Where := Where + 3;
            when COV   => Str (Where .. Where + 2) := "cov";
                        Where := Where + 3;
            when CON   => Str (Where .. Where + 2) := "con";
                        Where := Where + 3;
            when E     => Str (Where .. Where) := "e";
                        Where := Where + 1;
            when EC    => Str (Where .. Where + 1) := "ec";
                        Where := Where + 2;
            when FD    => Str (Where .. Where + 1) := "fd";
                        Where := Where + 2;
            when FR    => Str (Where .. Where + 1) := "fr";
                        Where := Where + 2;
            when FU    => Str (Where .. Where + 1) := "fu";
                        Where := Where + 2;
            when GV    => Str (Where .. Where + 1) := "gv";
                        Where := Where + 1;
            when IV    => Str (Where .. Where + 1) := "iv";
                        Where := Where + 2;
            when LV    => Str (Where .. Where + 1) := "lv";
                        Where := Where + 2;
            when MA    => Str (Where .. Where + 1) := "ma";
                        Where := Where + 2;
            when MD    => Str (Where .. Where + 1) := "md";
                        Where := Where + 2;
            when MI    => Str (Where .. Where + 1) := "mi";
                        Where := Where + 2;
            when SU    => Str (Where .. Where + 1) := "su";
                        Where := Where + 2;
            when T     => Str (Where .. Where) := "t";
                        Where := Where + 1;
            when UN    => Str (Where .. Where + 1) := "un";
                        Where := Where + 2;
            when IU    => Str (Where .. Where + 1) := "iu";
                        Where := Where + 2;
            when others => raise Invalid_Symbol_Type;
         end case;
      end To_String;
end SN.Find_Fns;

