separate (Src_Info.CPP)

------------------------
--  Fu_To_Gv_Handler  --
------------------------

procedure Fu_To_Gv_Handler (Ref : TO_Table) is
   GVar         : GV_Table;
   End_Position : Point := Ref.Position;
   Ref_Kind     : Reference_Kind;
   tmp_ptr      : E_Declaration_Info_List;
   Desc         : CType_Description;
   Success      : Boolean;
   Attributes   : SN_Attributes;
   Scope        : E_Scope := Global_Scope;
begin
   Info ("Fu_To_GV_Handler: """
         & Ref.Buffer (Ref.Referred_Symbol_Name.First ..
               Ref.Referred_Symbol_Name.Last)
         & """");
   GVar := Find (SN_Table (GV), Ref.Buffer
        (Ref.Referred_Symbol_Name.First .. Ref.Referred_Symbol_Name.Last));
   --  FIX ME
   End_Position.Column := End_Position.Column +
      (Ref.Referred_Symbol_Name.Last - Ref.Referred_Symbol_Name.First + 1);
   if Ref.Buffer (Ref.Access_Type.First .. Ref.Access_Type.First) = "r" then
      Ref_Kind := Reference;
   else
      Ref_Kind := Modification;
   end if;

   --  magic with types
   Type_Name_To_Kind (GVar.Buffer
      (GVar.Value_Type.First .. GVar.Value_Type.Last), Desc, Success);
   if not Success or Type_To_Object (Desc.Kind) = Overloaded_Entity then
      Free (GVar);
      Free (Desc);
      return; -- type not found, ignore errors
   end if;

   begin
      tmp_ptr := Find_Dependency_Declaration
        (File                    => Global_LI_File,
         Symbol_Name             => GVar.Buffer
            (GVar.Name.First .. GVar.Name.Last),
         Location                => GVar.Start_Position);
   exception
      when Declaration_Not_Found =>
         tmp_ptr := null;
   end;

   if tmp_ptr = null then
      Attributes := SN_Attributes (GVar.Attributes);
      if (Attributes and SN_STATIC) = SN_STATIC then
         Scope := Static_Local;
      end if;
      if Desc.Parent_Point = Invalid_Point then
         Insert_Declaration
           (Handler           => LI_Handler (Global_CPP_Handler),
            File              => Global_LI_File,
            List              => Global_LI_File_List,
            Symbol_Name       =>
               GVar.Buffer (GVar.Name.First .. GVar.Name.Last),
            Source_Filename   =>
               GVar.Buffer (GVar.File_Name.First .. GVar.File_Name.Last),
            Location          => GVar.Start_Position,
            Kind              => Type_To_Object (Desc.Kind),
            Scope             => Scope,
            Declaration_Info  => tmp_ptr);
      else
         Insert_Declaration
           (Handler           => LI_Handler (Global_CPP_Handler),
            File              => Global_LI_File,
            List              => Global_LI_File_List,
            Symbol_Name       =>
               GVar.Buffer (GVar.Name.First .. GVar.Name.Last),
            Source_Filename   =>
               GVar.Buffer (GVar.File_Name.First .. GVar.File_Name.Last),
            Location          => GVar.Start_Position,
            Kind              => Type_To_Object (Desc.Kind),
            Scope             => Scope,
            Parent_Location   => Desc.Parent_Point,
            Parent_Filename   => Desc.Parent_Filename.all,
            Declaration_Info  => tmp_ptr);
      end if;
   end if;
   Insert_Reference
     (Declaration_Info        => tmp_ptr,
      File                    => Global_LI_File,
      Source_Filename         => Ref.Buffer (Ref.File_Name.First
                                                   .. Ref.File_Name.Last),
      Location                => Ref.Position,
      Kind                    => Ref_Kind);
   Free (GVar);
   Free (Desc);
exception
   when Not_Found  | DB_Error => -- ignore
      Fail ("Global variable " &
            GVar.Buffer (Ref.Referred_Symbol_Name.First ..
                                    Ref.Referred_Symbol_Name.Last) &
            " not found");
   return;
end Fu_To_Gv_Handler;

