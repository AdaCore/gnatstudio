separate (Src_Info.CPP)

-------------------------
-- Sym_Default_Handler --
-------------------------

--  This is default handler for symbols, which are not registered
--  in Symbols_Handlers.

procedure Sym_Default_Handler
  (Sym : FIL_Table)
is
   --  pragma Unreferenced (Sym);
begin
   Info ("Sym_Default_Hanlder: '"
         & Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last)
         & "' : " & Symbol_Type'Image (Sym.Symbol));
   null;
end Sym_Default_Handler;
