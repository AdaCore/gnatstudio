separate (Src_Info.CPP)

--------------------
-- Sym_MA_Handler --
--------------------

procedure Sym_MA_Handler (Sym : FIL_Table)
is
   tmp_ptr    : E_Declaration_Info_List;
begin
   Info ("Sym_MA_Handler: """
         & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
         & """");

   Insert_Declaration
     (Handler           => LI_Handler (Global_CPP_Handler),
      File              => Global_LI_File,
      List              => Global_LI_File_List,
      Symbol_Name       =>
        Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
      Source_Filename   =>
        Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
      Location          => Sym.Start_Position,
      Kind              => Unresolved_Entity,
      Scope             => Global_Scope,
      Declaration_Info  => tmp_ptr);

end Sym_MA_Handler;

