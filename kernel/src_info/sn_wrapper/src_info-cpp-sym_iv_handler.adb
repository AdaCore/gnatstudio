separate (Src_Info.CPP)

--------------------
-- Sym_IV_Handler --
--------------------

procedure Sym_IV_Handler (Sym : FIL_Table)
is
   Inst_Var        : IV_Table;
   Decl_Info       : E_Declaration_Info_List;
   Success         : Boolean;
   Desc            : CType_Description;
begin
   Info ("Sym_IV_Handler: """
         & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
         & """");

   if not Is_Open (SN_Table (IV)) then
      --  IV table does not exist, nothing to do ...
      return;
   end if;

   --  Lookup instance variable
   Inst_Var := Find
     (SN_Table (IV),
      Sym.Buffer (Sym.Class.First .. Sym.Class.Last),
      Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));

   --  Determine its type
   Type_Name_To_Kind
     (Inst_Var.Buffer
        (Inst_Var.Value_Type.First .. Inst_Var.Value_Type.Last),
      Desc,
      Success);

   if not Success then -- failed to determine type
      --  if the variable belongs to a template, the unknown type
      --  may be template parameter. Check it.
      --  TODO Here we should parse class template arguments and
      --  locate the type in question. Not implemented yet
      Desc.Kind           := Private_Type;
      Desc.IsVolatile     := False;
      Desc.IsConst        := False;
      Desc.Parent_Point   := Invalid_Point;
      Desc.Ancestor_Point := Invalid_Point;
      Desc.Builtin_Name   := null;
      --  Free (Inst_Var);
      --  return;
   end if;

   if Desc.Parent_Point = Invalid_Point then
      Insert_Declaration
        (Handler           => LI_Handler (Global_CPP_Handler),
         File              => Global_LI_File,
         Symbol_Name       =>
           Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Source_Filename   =>
           Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Location          => Sym.Start_Position,
         Kind              => Type_To_Object (Desc.Kind),
         Scope             => Local_Scope,
         Declaration_Info  => Decl_Info);
   else
      Insert_Declaration
        (Handler           => LI_Handler (Global_CPP_Handler),
         File              => Global_LI_File,
         Symbol_Name       =>
           Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Source_Filename   =>
           Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Location          => Sym.Start_Position,
         Kind              => Desc.Kind,
         Scope             => Local_Scope,
         Parent_Location   => Desc.Parent_Point,
         Parent_Filename   => Desc.Parent_Filename.all,
         Declaration_Info  => Decl_Info);

      --  add reference to the type of this field
      Refer_Type
        (Inst_Var.Buffer
           (Inst_Var.Value_Type.First .. Inst_Var.Value_Type.Last),
         Desc.Parent_Point,
         Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Sym.Start_Position);
   end if;

   Free (Desc);
   Free (Inst_Var);
exception
   when  DB_Error |   -- non-existent table
         Not_Found => -- no such variable
      null;           -- ignore error
end Sym_IV_Handler;

