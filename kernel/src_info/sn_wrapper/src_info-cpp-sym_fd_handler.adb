separate (Src_Info.CPP)

--------------------
-- Sym_FD_Handler --
--------------------

procedure Sym_FD_Handler (Sym : FIL_Table)
is
   Target_Kind : E_Kind;
   tmp_ptr : E_Declaration_Info_List;
begin
   Info ("Sym_FD_Hanlder: """
         & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
         & """");

   declare
      FD_Tab : FD_Table;
   begin
      FD_Tab := Find (SN_Table (FD),
          Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));
      if eq (FD_Tab.Buffer (FD_Tab.Return_Type.First
                                 .. FD_Tab.Return_Type.Last), "void") then
         Target_Kind := Non_Generic_Procedure;
      else
         Target_Kind := Non_Generic_Function_Or_Operator;
      end if;
      Free (FD_Tab);
   exception
      when DB_Error | Not_Found =>
         Fail ("unable to find function " &
               Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last));
   end;

   Insert_Declaration
     (Handler           => LI_Handler (Global_CPP_Handler),
      File              => Global_LI_File,
      List              => Global_LI_File_List,
      Symbol_Name       =>
        Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
      Source_Filename   =>
        Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
      Location          => Sym.Start_Position,
      Kind              => Target_Kind,
      Scope             => Global_Scope,
      Declaration_Info  => tmp_ptr);

end Sym_FD_Handler;
