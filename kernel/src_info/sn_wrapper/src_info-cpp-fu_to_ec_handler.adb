separate (Src_Info.CPP)

------------------------
--  Fu_To_Ec_Handler  --
------------------------

procedure Fu_To_Ec_Handler (Ref : TO_Table) is
   Decl_Info    : E_Declaration_Info_List;
   Enum_Const   : EC_Table;
   Ref_Id       : constant String := Ref.Buffer
     (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
begin

   Info ("Fu_To_EC_Handler: '" & Ref_Id & "'");

   Enum_Const := Find (SN_Table (EC), Ref_Id);

   --  Find declaration
   if Enum_Const.Buffer
      (Enum_Const.File_Name.First .. Enum_Const.File_Name.Last)
      = Get_LI_Filename (Global_LI_File) then
      begin
         Decl_Info := Find_Declaration
           (File                    => Global_LI_File,
            Symbol_Name             => Ref_Id,
            Location                => Enum_Const.Start_Position);
      exception
         when Declaration_Not_Found =>
            Warn ("Could not lookup declaration or enumeration constant "
                  & Ref_Id);
            Free (Enum_Const);
            return;
      end;
   else -- another file
      begin -- Find dependency declaration
         Decl_Info := Find_Dependency_Declaration
           (File                    => Global_LI_File,
            Symbol_Name             => Ref_Id,
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
               Symbol_Name       => Ref_Id,
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

exception

   when DB_Error | Not_Found =>
      Warn ("Could not look up enumeration constant " & Ref_Id);

end Fu_To_Ec_Handler;

