separate (Src_Info.CPP)

--------------------
-- Sym_IU_Handler --
--------------------

procedure Sym_IU_Handler (Sym : FIL_Table) is
begin
   Info ("Sym_IU_Handler: """
         & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
         & """");

   Insert_Dependency
     (Handler           => LI_Handler (Global_CPP_Handler),
      File              => Global_LI_File,
      List              => Global_LI_File_List,
      Source_Filename   =>
        Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
      Referred_Filename =>
        Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));

end Sym_IU_Handler;


