separate (Src_Info.CPP)

--------------------
-- Sym_CL_Handler --
--------------------

procedure Sym_CL_Handler (Sym : FIL_Table)
is
   Decl_Info : E_Declaration_Info_List;
   Desc      : CType_Description;
   Class_Def : CL_Table;
   Success   : Boolean;
begin

   Info ("Sym_CL_Hanlder: """
         & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
         & """");

   Find_Class
     (Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
      Desc,
      Class_Def,
      Success);

   if not Success then
      return;
   end if;

   Insert_Declaration
     (Handler               => LI_Handler (Global_CPP_Handler),
      File                  => Global_LI_File,
      List                  => Global_LI_File_List,
      Symbol_Name           =>
        Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
      Source_Filename       =>
        Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
      Location              => Sym.Start_Position,
      Kind                  => Record_Type,
      Scope                 => Global_Scope,
      End_Of_Scope_Location => Class_Def.End_Position,
      Declaration_Info      => Decl_Info);

   Insert_Reference
     (Declaration_Info      => Decl_Info,
      File                  => Global_LI_File,
      Source_Filename       =>
        Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
      Location              => Class_Def.End_Position,
      Kind                  => End_Of_Spec);

   Free (Desc);
   Free (Class_Def);
end Sym_CL_Handler;
