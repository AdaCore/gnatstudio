separate (Src_Info.CPP)

------------------------
--  Fu_To_Gv_Handler  --
------------------------

procedure Fu_To_Gv_Handler (Ref : TO_Table) is
   Ref_Kind     : Reference_Kind;
   Decl_Info    : E_Declaration_Info_List;
   Var          : GV_Table;
   Desc         : CType_Description;
   Success      : Boolean;
   Scope        : E_Scope := Global_Scope;
   Attributes   : SN_Attributes;
begin
   Info ("Fu_To_GV_Handler: """
         & Ref.Buffer (Ref.Referred_Symbol_Name.First ..
               Ref.Referred_Symbol_Name.Last)
         & """");

   --  we need declaration's location
   Var := Find (SN_Table (GV),
      Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                  Ref.Referred_Symbol_Name.Last));

   --  Find declaration
   if Var.Buffer (Var.File_Name.First .. Var.File_Name.Last)
      = Get_LI_Filename (Global_LI_File) then
      begin
         Decl_Info := Find_Declaration
           (File                    => Global_LI_File,
            Symbol_Name             =>
               Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                           Ref.Referred_Symbol_Name.Last),
            Location                => Var.Start_Position);
      exception
         when Declaration_Not_Found =>
            Fail ("Failed to lookup declaration for global variable "
               & Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                             Ref.Referred_Symbol_Name.Last));
            Free (Var);
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
               Var.Buffer (Var.File_Name.First .. Var.File_Name.Last),
            Location                => Var.Start_Position);
      exception
         when Declaration_Not_Found => -- dep decl does not yet exist
            --  Collect information about the variable:
            --  type, scope, location of type declaration...
            Type_Name_To_Kind
              (Var.Buffer (Var.Value_Type.First .. Var.Value_Type.Last),
               Desc,
               Success);
            if not Success then -- unknown type
               Free (Var);
               return;
            end if;

            Attributes := SN_Attributes (Var.Attributes);

            if (Attributes and SN_STATIC) = SN_STATIC then
               Scope := Static_Local;
            end if;

            if Desc.Parent_Point = Invalid_Point then
               Insert_Dependency_Declaration
                 (Handler           => LI_Handler (Global_CPP_Handler),
                  File              => Global_LI_File,
                  List              => Global_LI_File_List,
                  Symbol_Name       =>
                     Var.Buffer (Var.Name.First .. Var.Name.Last),
                  Source_Filename   =>
                     Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
                  Location          => Var.Start_Position,
                  Kind              => Type_To_Object (Desc.Kind),
                  Scope             => Scope,
                  Referred_Filename =>
                     Var.Buffer (Var.File_Name.First .. Var.File_Name.Last),
                  Declaration_Info  => Decl_Info);
            else
               Insert_Dependency_Declaration
                 (Handler           => LI_Handler (Global_CPP_Handler),
                  File              => Global_LI_File,
                  List              => Global_LI_File_List,
                  Symbol_Name       =>
                     Var.Buffer (Var.Name.First .. Var.Name.Last),
                  Source_Filename   =>
                     Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
                  Location          => Var.Start_Position,
                  Kind              => Type_To_Object (Desc.Kind),
                  Scope             => Scope,
                  Referred_Filename =>
                     Var.Buffer (Var.File_Name.First .. Var.File_Name.Last),
                  Parent_Location   => Desc.Parent_Point,
                  Parent_Filename   => Desc.Parent_Filename.all,
                  Declaration_Info  => Decl_Info);
            end if;
            Free (Desc);
      end;
   end if;
   Free (Var);

   if Ref.Buffer (Ref.Access_Type.First) = 'r' then
      Ref_Kind := Reference;
   else
      Ref_Kind := Modification;
   end if;


   Insert_Reference
     (Declaration_Info        => Decl_Info,
      File                    => Global_LI_File,
      Source_Filename         =>
         Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
      Location                => Ref.Position,
      Kind                    => Ref_Kind);
exception
   when Not_Found  | DB_Error => -- ignore
      Fail ("Global variable " &
            Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                        Ref.Referred_Symbol_Name.Last) &
            " not found");
end Fu_To_Gv_Handler;

