separate (Src_Info.CPP)


------------------------
--  Fu_To_Fu_Handler  --
------------------------

procedure Fu_To_Fu_Handler (Ref : TO_Table) is
   P            : Pair_Ptr;
   Fn           : FU_Table;
   List         : E_Reference_List := null;

   procedure Add (Filename : in String; Pos : in Point);
   --  Adds reference to the given location to the 'List'

   procedure Add (Filename : in String; Pos : in Point) is
      Location  : File_Location;
      Decl_Info : E_Declaration_Info_List;
   begin
      if Filename = Get_LI_Filename (Global_LI_File) then
         Location.File.LI := Global_LI_File;
      else --  this function is from another file
         --  we should add a dependency declaration
         Location.File.LI := Get (Global_LI_File_List.Table, Filename);

         if Location.File.LI = No_LI_File then
            --  ??? what should we do here? Call Create_Or_Complete?
            pragma Assert (False, "can't reference non existent file");
            null;
         end if;
         Insert_Dependency_Declaration
           (Handler           => LI_Handler (Global_CPP_Handler),
            File              => Global_LI_File,
            List              => Global_LI_File_List,
            Symbol_Name       =>
               Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                           Ref.Referred_Symbol_Name.Last),
            Referred_Filename => Get_LI_Filename (Location.File.LI),
            Source_Filename   => Filename,
            Location          => Pos,
            Kind              => Non_Generic_Procedure,
            Scope             => Global_Scope,
            Declaration_Info  => Decl_Info);
      end if;
      Location.File.Part            := Unit_Body;
      Location.File.Source_Filename := new String'(Filename);
      Location.Line                 := Pos.Line;
      Location.Column               := Pos.Column;
      List := new E_Reference_Node'((Location, Reference), List);
   end Add;

   Decl_Info    : E_Declaration_Info_List;
   Overloaded   : Boolean := False;
begin
   Info ("Fu_To_Fu_Handler: """
         & Ref.Buffer (Ref.Referred_Symbol_Name.First ..
               Ref.Referred_Symbol_Name.Last)
         & """");

   Set_Cursor
     (SN_Table (FU),
      By_Key,
      Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                  Ref.Referred_Symbol_Name.Last) & Field_Sep,
      False);

   loop
      P := Get_Pair (SN_Table (FU), Next_By_Key);
      exit when P = null;
      Fn := Parse_Pair (P.all);
      Overloaded := List /= null;
      Add (Fn.Buffer (Fn.File_Name.First .. Fn.File_Name.Last),
           Fn.Start_Position);
      Free (Fn);
      Free (P);
   end loop;

   if not Overloaded then
      --  If procedure
      --    defined in the current file => add reference
      --    defined in another file => add dep decl and reference it
      begin
         if List.Value.Location.File.LI = Global_LI_File then
            --  this is a function defined in the current file
            Decl_Info := Find_Declaration
              (Global_LI_File,
               Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                           Ref.Referred_Symbol_Name.Last),
               "",
               (List.Value.Location.Line, List.Value.Location.Column));
         else
            --  this function is defined somewhere else...
            Decl_Info := Find_Dependency_Declaration
              (Global_LI_File,
               Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                           Ref.Referred_Symbol_Name.Last),
               "",
               Get_LI_Filename (List.Value.Location.File.LI),
               (List.Value.Location.Line, List.Value.Location.Column));
         end if;
         Destroy (List);
      exception
         when Declaration_Not_Found =>
            pragma Assert (False, "How did we get here?");
            null;
      end;
   else -- overloaded entity
      Decl_Info := new E_Declaration_Info_Node'
        (Value =>
           (Declaration => No_Declaration,
            References => List),
         Next => Global_LI_File.LI.Body_Info.Declarations);
      Decl_Info.Value.Declaration.Name :=
         new String'(Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                                 Ref.Referred_Symbol_Name.Last));
      Decl_Info.Value.Declaration.Kind := Overloaded_Entity;
      Global_LI_File.LI.Body_Info.Declarations := Decl_Info;
   end if;

   Insert_Reference
     (Decl_Info,
      Global_LI_File,
      Ref.Buffer (Ref.File_Name.First .. Ref.File_Name.Last),
      Ref.Position,
      Reference);
exception
   when Not_Found  | DB_Error => -- ignore
      Fail ("Function " &
            Ref.Buffer (Ref.Referred_Symbol_Name.First ..
                                    Ref.Referred_Symbol_Name.Last) &
            " not found");
   return;
end Fu_To_Fu_Handler;

