separate (Src_Info.CPP)

--------------------
-- Sym_T_Handler --
--------------------

procedure Sym_T_Handler (Sym : FIL_Table)
is
--   Decl_Info : E_Declaration_Info_List;
begin

   Info ("Sym_T_Hanlder: """
         & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
         & """");

--   Insert_Declaration
--     (Handler           => LI_Handler (Global_CPP_Handler),
--      File              => Global_LI_File,
--      Symbol_Name       =>
--        Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
--      Source_Filename   =>
--        Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
--      Location          => Sym.Start_Position,
--      Kind              => Enumeration_Literal,
--      Scope             => Global_Scope,
--      Declaration_Info  => Decl_Info);

end Sym_T_Handler;
