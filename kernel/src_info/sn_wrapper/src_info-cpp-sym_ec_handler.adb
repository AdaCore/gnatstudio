separate (Src_Info.CPP)

--------------------
-- Sym_EC_Handler --
--------------------

procedure Sym_EC_Handler (Sym : FIL_Table)
is
   Decl_Info : E_Declaration_Info_List;

   Ec_Id     : String := Sym.Buffer
     (Sym.Identifier.First ..  Sym.Identifier.Last);

   Desc      : CType_Description;
   Has_Enum  : Boolean := False;
begin

   Info ("Sym_EC_Hanlder: """ & Ec_Id & """");

   if Is_Open (SN_Table (EC)) and then Is_Open (SN_Table (E)) then
      declare
         EC_Def : EC_Table := Find (SN_Table (EC), Ec_Id, Sym.Start_Position);
         E_Def  : E_Table;
      begin
         Find_Enum
           (EC_Def.Buffer
              (EC_Def.Enumeration_Name.First ..
               EC_Def.Enumeration_Name.First),
            Desc, E_Def, Has_Enum);
         Free (E_Def);
         Free (EC_Def);
      exception
         when DB_Error | Not_Found => -- ignore
            Free (E_Def);
            Free (EC_Def);
      end;
   end if;

   if Has_Enum
     and then Desc.Parent_Point /= Invalid_Point
   then -- corresponding enumeration found
      Insert_Declaration
        (Handler           => LI_Handler (Global_CPP_Handler),
         File              => Global_LI_File,
         List              => Global_LI_File_List,
         Symbol_Name       =>
           Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Source_Filename   =>
           Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Location          => Sym.Start_Position,
         Kind              => Enumeration_Literal,
         Parent_Location   => Desc.Parent_Point,
         Parent_Filename   => Desc.Parent_Filename.all,
         Scope             => Global_Scope,
         Declaration_Info  => Decl_Info);
   else
      Insert_Declaration
        (Handler           => LI_Handler (Global_CPP_Handler),
         File              => Global_LI_File,
         List              => Global_LI_File_List,
         Symbol_Name       =>
           Sym.Buffer (Sym.Identifier.First .. Sym.Identifier.Last),
         Source_Filename   =>
           Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
         Location          => Sym.Start_Position,
         Kind              => Enumeration_Literal,
         Scope             => Global_Scope,
         Declaration_Info  => Decl_Info);
   end if;

end Sym_EC_Handler;
