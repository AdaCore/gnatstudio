with Except;
with Input;
with Screen_Output;

package body Tokens is

   ----------
   -- Next --
   ----------

   function Next return Token is
   begin
      loop --  until we have read a valid token.

         --  Open block to catch exceptions raised for user errors.

         Read_A_Valid_Token : declare
            Word : String := Input.Next_Word;

         begin
            --  Figure out which kind of token we have from the first
            --  character and delegate the full token recognition to
            --  the Read routine in the appropriate Instruction, Values
            --  or Values.Operations package.

            case Word (Word'First) is

               when '0' .. '9' | '.' =>
                  return Token'(Kind => Val, Val => Values.Read (Word));

               when '+' | '*' | '/' =>
                  return
                    Token'(Kind => Op, Op => Values.Operations.Read (Word));

               when '-' =>
                  if Word'Length > 1 then
                     return Token'(Kind => Val, Val => Values.Read (Word));
                  else
                     return
                       Token'(Kind => Op, Op => Values.Operations.Read (Word));
                  end if;

               when 'a' .. 'z'
                 |  'A' .. 'Z' =>
                  return
                    Token'(Kind => Instr, Instr => Instructions.Read (Word));

               when others =>
                  raise Except.User_Error;
            end case;

         exception
            when Except.User_Error =>
               Screen_Output.Syntax_Error ("Input ignored till end of line.");
               Input.Skip_Line;
         end Read_A_Valid_Token;
      end loop;
   end Next;

   -------------
   -- Process --
   -------------

   procedure Process (T : Token) is
   begin
      case T.Kind is
         when Val =>
            Values.Process (T.Val);

         when Op =>
            Values.Operations.Process (T.Op);

         when Instr =>
            Instructions.Process (T.Instr);
      end case;
   end Process;

end Tokens;

