separate (Src_Info.CPP)

--------------------
-- Sym_GV_Handler --
--------------------

procedure Sym_GV_Handler (Sym : FIL_Table)
is
   type Type_To_Object_Array is array (E_Kind) of E_Kind;
   Type_To_Object : Type_To_Object_Array :=
     (Access_Type               => Access_Object,
      Array_Type                => Array_Object,
      Boolean_Type              => Boolean_Object,
      Class_Wide_Type           => Class_Wide_Object,
      Decimal_Fixed_Point_Type  => Decimal_Fixed_Point_Object,
      Enumeration_Type          => Enumeration_Object,
      Modular_Integer_Type      => Modular_Integer_Object,
      Protected_Type            => Protected_Object,
      Record_Type               => Record_Object,
      Ordinary_Fixed_Point_Type => Ordinary_Fixed_Point_Object,
      Signed_Integer_Type       => Signed_Integer_Object,
      String_Type               => String_Object,
      Task_Type                 => Task_Object,
      others                    => Overloaded_Entity);
   Desc       : CType_Description;
   Var        : GV_Table;
   Success    : Boolean;
   tmp_ptr    : E_Declaration_Info_List;
   Attributes : SN_Attributes;
   Scope      : E_Scope := Global_Scope;
begin
   Info ("Sym_GV_Handler: """
         & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
         & """");

   if not Is_Open (SN_Table (GV)) then
      --  GV table does not exist, nothing to do ...
      return;
   end if;

   --  Lookup variable type
   Var := Find (SN_Table (GV), Sym.Buffer
      (Sym.Identifier.First .. Sym.Identifier.Last));
   Type_Name_To_Kind (Var.Buffer
      (Var.Value_Type.First .. Var.Value_Type.Last), Desc, Success);

   if not Success or Type_To_Object (Desc.Kind) = Overloaded_Entity then
      Free (Var);
      return; -- type not found, ignore errors
   end if;

   Attributes := SN_Attributes (Var.Attributes);

   if (Attributes and SN_STATIC) = SN_STATIC then
      Scope := Static_Local;
   end if;

   Insert_Declaration
     (Handler           => LI_Handler (Global_CPP_Handler),
      File              => Global_LI_File,
      Symbol_Name       =>
        Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
      Source_Filename   =>
        Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
      Location          => Sym.Start_Position,
      Kind              => Type_To_Object (Desc.Kind),
      Scope             => Scope,
      Declaration_Info  => tmp_ptr);

   Free (Var);
exception
   when  DB_Error |   -- non-existent table
         Not_Found => -- no such variable
      null;           -- ignore error
end Sym_GV_Handler;
