separate (Src_Info.CPP)

--------------------
-- Sym_IV_Handler --
--------------------

procedure Sym_IV_Handler (Sym : FIL_Table)
is
   Inst_Var        : IV_Table;
   Decl_Info       : E_Declaration_Info_List;
   Type_Decl_Info  : E_Declaration_Info_List;
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
      Free (Inst_Var);
      return;
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
      begin
         Type_Decl_Info := Find_Declaration
           (Global_LI_File,
            Inst_Var.Buffer
               (Inst_Var.Value_Type.First .. Inst_Var.Value_Type.Last),
            Desc.Parent_Point);

         Insert_Reference
           (Declaration_Info     => Type_Decl_Info,
            File                 => Global_LI_File,
            Source_Filename      =>
               Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
            Location             => Sym.Start_Position,
            Kind                 => Reference);
      exception
         when Declaration_Not_Found => -- ignore
            null;
      end;
   end if;

   Free (Desc);
   Free (Inst_Var);
exception
   when  DB_Error |   -- non-existent table
         Not_Found => -- no such variable
      null;           -- ignore error
end Sym_IV_Handler;

