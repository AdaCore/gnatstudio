separate (Src_Info.CPP)

--------------------
-- Sym_CL_Handler --
--------------------

procedure Sym_CL_Handler (Sym : FIL_Table)
is
   Decl_Info  : E_Declaration_Info_List;
   Desc       : CType_Description;
   Class_Def  : CL_Table;
   Success    : Boolean;
   P          : Pair_Ptr;
   Super      : IN_Table;
   Super_Def  : CL_Table;
   Super_Desc : CType_Description;
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

   --  Find all the base classes for this one
   Set_Cursor
     (SN_Table (SN_IN),
      By_Key,
      --  Use name from Class_Def for it does not hold <> when
      --  template class is encountered
      Class_Def.Buffer (Class_Def.Name.First .. Class_Def.Name.Last)
         & Field_Sep,
      False);

   loop
      P := Get_Pair (SN_Table (SN_IN), Next_By_Key);
      exit when P = null;
      Super := Parse_Pair (P.all);
      Info ("Found base class: "
         & Super.Buffer (Super.Base_Class.First .. Super.Base_Class.Last));
      --  Lookup base class definition to find its precise location
      Find_Class
       (Super.Buffer (Super.Base_Class.First .. Super.Base_Class.Last),
        Super_Desc,
        Super_Def,
        Success);
      if Success then -- if found, add it to parent list
         Add_Parent
           (Decl_Info,
            Global_LI_File_List,
            Super_Def.Buffer
              (Super_Def.File_Name.First .. Super_Def.File_Name.Last),
            Super_Def.Start_Position);
         Free (Super_Desc);
         Free (Super_Def);
      end if;
      Free (Super);
      Free (P);
   end loop;

   Insert_Reference
     (Declaration_Info      => Decl_Info,
      File                  => Global_LI_File,
      Source_Filename       =>
        Sym.Buffer (Sym.File_Name.First .. Sym.File_Name.Last),
      Location              => Class_Def.End_Position,
      Kind                  => End_Of_Spec);

   Free (Desc);
   Free (Class_Def);
exception
   when DB_Error => -- something went wrong, ignore it
      null;
end Sym_CL_Handler;
