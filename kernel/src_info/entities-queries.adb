with VFS;      use VFS;
with Traces;   use Traces;

with Glib.Unicode; use Glib.Unicode;

package body Entities.Queries is
   Me : constant Debug_Handle := Create ("Entities.Assert");

   Num_Columns_Per_Line : constant := 250;
   --  The number of columns in each line, when computing the proximity of a
   --  match. This is an approximate number, for efficiency. Big values mean
   --  that we give advantage to matches on the same line rather than on the
   --  same column.

   use Entities_Tries;
   use Entity_Information_Arrays;
   use Entity_Reference_Arrays;

   procedure Find
     (EL       : Entity_Information_List_Access;
      File     : Source_File;
      Line     : Integer;
      Column   : Integer;
      Distance : in out Integer;
      Closest  : in out Entity_Information);
   --  Check in EL the entities which has a reference as close as possible
   --  to (Line, Column). Distance is the initial closest distance known, and
   --  is changed to reflect the result of the find. It is set to 0 if an
   --  exact match was found.

   procedure Find
     (Source                 : Source_File;
      Normalized_Entity_Name : String;
      Line                   : Integer;
      Column                 : Integer;
      Entity                 : out Entity_Information;
      Status                 : out Find_Decl_Or_Body_Query_Status);
   --  Find the closest entity to (Line, Column) in Source.

   ----------
   -- Find --
   ----------

   procedure Find
     (EL       : Entity_Information_List_Access;
      File     : Source_File;
      Line     : Integer;
      Column   : Integer;
      Distance : in out Integer;
      Closest  : in out Entity_Information)
   is
      Prox : Integer;
      E    : Entity_Information;
      Ref  : Entity_Reference;
   begin
      if EL /= null then
         For_Each_Entity :
         for Ent in Entity_Information_Arrays.First .. Last (EL.all) loop
            E := EL.Table (Ent);

            Prox := abs (E.Declaration.Column - Column) +
               abs (E.Declaration.Line - Line) * Num_Columns_Per_Line;

            if Prox < Distance then
               Closest := E;
               Distance := Prox;
               exit For_Each_Entity when Distance = 0;
            end if;

            for R in Entity_Reference_Arrays.First .. Last (E.References) loop
               Ref := E.References.Table (R);

               if Ref.Location.File = File then
                  Prox := abs (Ref.Location.Column - Column) +
                     abs (Ref.Location.Line - Line) * Num_Columns_Per_Line;

                  if Prox < Distance then
                     Closest := E;
                     Distance := Prox;

                     exit For_Each_Entity when Distance = 0;
                  end if;
               end if;
            end loop;

         end loop For_Each_Entity;
      end if;
   end Find;

   ----------
   -- Find --
   ----------

   procedure Find
     (Source                 : Source_File;
      Normalized_Entity_Name : String;
      Line                   : Integer;
      Column                 : Integer;
      Entity                 : out Entity_Information;
      Status                 : out Find_Decl_Or_Body_Query_Status)
   is
      Distance : Integer := Integer'Last;
      Closest  : Entity_Information;
   begin
      Find
        (Get (Source.Entities, Normalized_Entity_Name), Source, Line, Column,
         Distance, Closest);

      if Distance /= 0 then
         Find (Get (Source.All_Entities, Normalized_Entity_Name),
               Source, Line, Column, Distance, Closest);
      end if;

      if Distance = 0 then
         Status := Success;
         Entity := Closest;
      elsif Distance = Integer'Last then
         Status := Entity_Not_Found;
         Entity := null;
      else
         Status := Fuzzy_Match;
         Entity := Closest;
      end if;
   end Find;

   ----------------------
   -- Find_Declaration --
   ----------------------

   procedure Find_Declaration
     (Db              : Entities_Database;
      File_Name       : VFS.Virtual_File;
      Entity_Name     : String;
      Line            : Positive;
      Column          : Positive;
      Entity          : out Entity_Information;
      Status          : out Find_Decl_Or_Body_Query_Status)
   is
      Handler  : constant LI_Handler := Get_LI_Handler (Db, File_Name);
      Source   : constant Source_File := Get_Source_Info (Handler, File_Name);

   begin
      if Source = null then
         Trace (Me, "No such file registered: " & Full_Name (File_Name).all);
         Status := Entity_Not_Found;
         Entity := null;

      elsif Case_Insensitive_Identifiers (Handler) then
         Update_Xref (Source);
         Find (Source, UTF8_Strdown (Entity_Name), Line, Column,
               Entity, Status);
      else
         Update_Xref (Source);
         Find (Source, Entity_Name, Line, Column,
               Entity, Status);
      end if;
   end Find_Declaration;

   --------------------
   -- Find_Next_Body --
   --------------------

   procedure Find_Next_Body
     (Entity           : Entity_Information;
      Current_Location : File_Location := No_File_Location;
      Location         : out File_Location)
   is
      Ref     : Entity_Reference;
      Best    : Entity_Reference := No_Entity_Reference;
   begin
      Update_Xref (Entity.Declaration.File);

      for R in Entity_Reference_Arrays.First .. Last (Entity.References) loop
         Ref := Entity.References.Table (R);
         if Ref.Kind = Body_Entity
           or else Ref.Kind = Completion_Of_Private_Or_Incomplete_Type
         then
            Best := Ref;
            exit when Current_Location = No_File_Location;
         else
            exit when Best /= No_Entity_Reference
              and then Ref.Location = Current_Location;
         end if;
      end loop;

      Location := Best.Location;
   end Find_Next_Body;

end Entities.Queries;
