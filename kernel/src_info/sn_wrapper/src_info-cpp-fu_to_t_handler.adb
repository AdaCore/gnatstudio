separate (Src_Info.CPP)

------------------------
--  Fu_To_T_Handler  --
------------------------

procedure Fu_To_T_Handler (Ref : TO_Table) is
   Typedef   : T_Table;
   Ref_Id    : constant String := Ref.Buffer
     (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last);
   Decl_Info : E_Declaration_Info_List;
   Desc      : CType_Description;
   Success   : Boolean := False;
begin
   if not Is_Open (SN_Table (T)) then
      --  .ma table does not exist
      return;
   end if;

   Info ("Fu_To_T: " & Ref_Id);

   Typedef := Find (SN_Table (T), Ref_Id);

   if Typedef.Buffer (Typedef.File_Name.First .. Typedef.File_Name.Last)
     = Get_LI_Filename (Global_LI_File)
   then
      begin
         --  look for declaration in current file
         Decl_Info := Find_Declaration
           (File        => Global_LI_File,
            Symbol_Name => Ref_Id,
            Location    => Typedef.Start_Position);

      exception
         when Declaration_Not_Found =>
            Fail ("unable to find declaration for typedef " & Ref_Id);
      end;
   else
      --  look for dependency declaration
      begin
         Decl_Info := Find_Dependency_Declaration
           (File                 => Global_LI_File,
            Symbol_Name          => Ref_Id,
            Filename             => Typedef.Buffer
              (Typedef.File_Name.First .. Typedef.File_Name.Last),
            Location             => Typedef.Start_Position);
      exception
         when Declaration_Not_Found => -- dep decl does not yet exist

            Original_Type (Ref_Id, Desc, Success);

            if not Success then
               Fail ("unable to find type for typedef " & Ref_Id);
               Free (Desc);
               Free (Typedef);
               return;
            end if;

            if Desc.Ancestor_Point = Invalid_Point then
               --  unknown parent
               Insert_Dependency_Declaration
                 (Handler           => LI_Handler (Global_CPP_Handler),
                  File              => Global_LI_File,
                  List              => Global_LI_File_List,
                  Symbol_Name       => Ref_Id,
                  Source_Filename   => Ref.Buffer
                    (Ref.File_Name.First .. Ref.File_Name.Last),
                  Location          => Typedef.Start_Position,
                  Kind              => Desc.Kind,
                  Scope             => Global_Scope,
                  Referred_Filename => Typedef.Buffer
                    (Typedef.File_Name.First .. Typedef.File_Name.Last),
                  Declaration_Info  => Decl_Info);
            elsif Desc.Ancestor_Point = Predefined_Point then
               --  typedef for builin type
               Insert_Dependency_Declaration
                 (Handler           => LI_Handler (Global_CPP_Handler),
                  File              => Global_LI_File,
                  List              => Global_LI_File_List,
                  Symbol_Name       => Ref_Id,
                  Source_Filename   => Ref.Buffer
                    (Ref.File_Name.First .. Ref.File_Name.Last),
                  Location          => Typedef.Start_Position,
                  Parent_Location   => Predefined_Point,
                  Kind              => Desc.Kind,
                  Scope             => Global_Scope,
                  Referred_Filename => Typedef.Buffer
                    (Typedef.File_Name.First .. Typedef.File_Name.Last),
                  Declaration_Info  => Decl_Info);
            else
               --  parent type found
               Insert_Dependency_Declaration
                 (Handler           => LI_Handler (Global_CPP_Handler),
                  File              => Global_LI_File,
                  List              => Global_LI_File_List,
                  Symbol_Name       => Ref_Id,
                  Source_Filename   => Ref.Buffer
                    (Ref.File_Name.First .. Ref.File_Name.Last),
                  Location          => Typedef.Start_Position,
                  Parent_Location   => Desc.Ancestor_Point,
                  Parent_Filename   => Desc.Ancestor_Filename.all,
                  Kind              => Desc.Kind,
                  Scope             => Global_Scope,
                  Referred_Filename => Typedef.Buffer
                    (Typedef.File_Name.First .. Typedef.File_Name.Last),
                  Declaration_Info  => Decl_Info);
            end if;

      end;
   end if;

   Insert_Reference
     (Declaration_Info     => Decl_Info,
      File                 => Global_LI_File,
      Source_Filename      =>
        Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
      Location             => Ref.Position,
      Kind                 => Reference);

   Free (Typedef);

exception
   when DB_Error | Not_Found  =>
      Fail ("unable to find typedef " & Ref_Id);

end Fu_To_T_Handler;

