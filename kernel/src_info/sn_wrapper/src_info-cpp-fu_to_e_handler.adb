separate (Src_Info.CPP)

------------------------
--  Fu_To_E_Handler  --
------------------------

procedure Fu_To_E_Handler (Ref : TO_Table)
is
   Ref_Id : constant String := Ref.Buffer
     (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
   Enum_Desc : CType_Description;
   Enum_Def  : E_Table;
   Success    : Boolean;
   Decl_Info  : E_Declaration_Info_List;
begin
   Info ("Fu_To_E_Handler: """ & Ref_Id & """");
   Find_Enum
     (Type_Name      => Ref_Id,
      Desc           => Enum_Desc,
      Enum_Def       => Enum_Def,
      Success        => Success);
   if not Success then
      Warn ("Enum type " & Ref_Id & " is not found in SN DB");
      return;
   end if;
   if Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last) /=
         Enum_Def.Buffer (Enum_Def.File_Name.First ..
                                    Enum_Def.File_Name.Last)
   then
      begin
         Decl_Info :=
           Find_Dependency_Declaration
             (File        => Global_LI_File,
              Symbol_Name => Enum_Def.Buffer
                (Enum_Def.Name.First .. Enum_Def.Name.Last),
              Kind        => Enumeration_Type,
              Location    => Enum_Def.Start_Position,
              Filename    => Enum_Def.Buffer
                (Enum_Def.File_Name.First .. Enum_Def.File_Name.Last));
      exception
         when Declaration_Not_Found =>
            Insert_Dependency_Declaration
              (Handler            => LI_Handler (Global_CPP_Handler),
               File               => Global_LI_File,
               List               => Global_LI_File_List,
               Symbol_Name        => Enum_Def.Buffer
                 (Enum_Def.Name.First .. Enum_Def.Name.Last),
               Referred_Filename  => Enum_Def.Buffer
                 (Enum_Def.File_Name.First .. Enum_Def.File_Name.Last),
               Source_Filename    => Ref.Buffer
                 (Ref.File_Name.First .. Ref.File_Name.Last),
               Location           => Enum_Def.Start_Position,
               Kind               => Enumeration_Type,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
      end;
   else
      begin
         Decl_Info :=
           Find_Declaration
             (File        => Global_LI_File,
              Symbol_Name => Enum_Def.Buffer
                (Enum_Def.Name.First .. Enum_Def.Name.Last),
              Kind        => Enumeration_Type,
              Location    => Enum_Def.Start_Position);
      exception
         when Declaration_Not_Found =>
            Insert_Declaration
              (Handler            => LI_Handler (Global_CPP_Handler),
               File               => Global_LI_File,
               List               => Global_LI_File_List,
               Symbol_Name        => Enum_Def.Buffer
                 (Enum_Def.Name.First .. Enum_Def.Name.Last),
               Source_Filename    => Ref.Buffer
                 (Ref.File_Name.First .. Ref.File_Name.Last),
               Location           => Enum_Def.Start_Position,
               Kind               => Enumeration_Type,
               Scope              => Global_Scope,
               Declaration_Info   => Decl_Info);
      end;
   end if;
   Insert_Reference
     (File              => Global_LI_File,
      Declaration_Info  => Decl_Info,
      Source_Filename   => Ref.Buffer
        (Ref.File_Name.First .. Ref.File_Name.Last),
      Location          => Ref.Position,
      Kind              => Reference);
   Free (Enum_Def);
   Free (Enum_Desc);
end Fu_To_E_Handler;

