separate (Src_Info.CPP)

--------------------
-- Sym_MA_Handler --
--------------------

procedure Sym_MA_Handler (Sym : FIL_Table)
is
   Macro      : MA_Table;
--   tmp_ptr    : E_Declaration_Info_List;
begin
   Info ("Sym_MA_Handler: """
         & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
         & """");

   if not Is_Open (SN_Table (MA)) then
      --  MA table does not exist, nothing to do ...
      return;
   end if;

   --  Lookup variable type
   Macro := Find (SN_Table (MA), Sym.Buffer
      (Sym.Identifier.First .. Sym.Identifier.Last));

--   Insert_Declaration
--     (Handler           => LI_Handler (Global_CPP_Handler),
--      File              => Global_LI_File,
--      Symbol_Name       =>
--        Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
--      Source_Filename   =>
--        Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
--      Location          => Sym.Start_Position,
--      Kind              => ?,
--      Scope             => Local_Scope,
--      Declaration_Info  => tmp_ptr);

   Free (Macro);
exception
   when  DB_Error |   -- non-existent table
         Not_Found => -- no such variable
      null;           -- ignore error
end Sym_MA_Handler;

