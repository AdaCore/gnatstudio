separate (Src_Info.CPP)

------------------------
--  Fu_To_Ec_Handler  --
------------------------

procedure Fu_To_Ec_Handler (Ref : TO_Table) is
   Decl_Info    : E_Declaration_Info_List;
   Enum_Const   : EC_Table;
begin
   Info ("Fu_To_EC_Handler: """
         & Ref.Buffer (Ref.Referred_Symbol_Name.First ..
               Ref.Referred_Symbol_Name.Last)
         & """");

   Enum_Const := Find (SN_Table (EC),
      Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                  Ref.Referred_Symbol_Name.Last));

   --  Find declaration
   if Enum_Const.Buffer
      (Enum_Const.File_Name.First .. Enum_Const.File_Name.Last)
      = Get_LI_Filename (Global_LI_File) then
      begin
         Decl_Info := Find_Declaration
           (File                    => Global_LI_File,
            Symbol_Name             =>
               Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                           Ref.Referred_Symbol_Name.Last),
            Location                => Enum_Const.Start_Position);
      exception
         when Declaration_Not_Found =>
            Fail ("Failed to lookup declaration for enumeration constant "
               & Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                             Ref.Referred_Symbol_Name.Last));
            Free (Enum_Const);
            return;
      end;
   else -- another file
      begin -- Find dependency declaration
         Decl_Info := Find_Dependency_Declaration
           (File                    => Global_LI_File,
            Symbol_Name             =>
               Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                           Ref.Referred_Symbol_Name.Last),
            Filename                =>
               Enum_Const.Buffer (Enum_Const.File_Name.First ..
                                  Enum_Const.File_Name.Last),
            Location                => Enum_Const.Start_Position);
      exception
         when Declaration_Not_Found => -- dep decl does not yet exist
            Insert_Dependency_Declaration
              (Handler           => LI_Handler (Global_CPP_Handler),
               File              => Global_LI_File,
               List              => Global_LI_File_List,
               Symbol_Name       =>
                  Enum_Const.Buffer (Enum_Const.Name.First ..
                                     Enum_Const.Name.Last),
               Source_Filename   =>
                  Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
               Location          => Enum_Const.Start_Position,
               Kind              => Enumeration_Literal,
               Scope             => Global_Scope,
               Referred_Filename =>
                  Enum_Const.Buffer (Enum_Const.File_Name.First ..
                                     Enum_Const.File_Name.Last),
               Declaration_Info  => Decl_Info);
      end;
   end if;
   Free (Enum_Const);

   Insert_Reference
     (Declaration_Info        => Decl_Info,
      File                    => Global_LI_File,
      Source_Filename         =>
         Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
      Location                => Ref.Position,
      Kind                    => Reference);

end Fu_To_Ec_Handler;

