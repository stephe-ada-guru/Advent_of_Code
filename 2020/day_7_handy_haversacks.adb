--  Abstract :
--
--  Advent of Code day 7
--
--  Copyright (C) 2020 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (GPL);

with Ada.Command_Line;
with Ada.Text_IO; use Ada.Text_IO;
with Day_7_Handy_Haversacks_Actions;
with Day_7_Handy_Haversacks_Main;
with Day_7_Handy_Haversacks_Runtime;
with WisiToken.Parse.LR.Parser_No_Recover;
with WisiToken.Text_IO_Trace;
procedure Day_7_Handy_Haversacks
is
   Verbose : Boolean renames Day_7_Handy_Haversacks_Runtime.Verbose;

   Data   : aliased Day_7_Handy_Haversacks_Runtime.User_Data;
   Parser : WisiToken.Parse.LR.Parser_No_Recover.Parser (Day_7_Handy_Haversacks_Actions.Descriptor'Access);
   Trace  : aliased WisiToken.Text_IO_Trace.Trace;
begin
   Day_7_Handy_Haversacks_Runtime.Verbose := Ada.Command_Line.Argument_Count > 1;

   Day_7_Handy_Haversacks_Main.Create_Parser (Parser, Trace'Unchecked_Access, Data'Unchecked_Access);

   Parser.Lexer.Reset_With_File (Ada.Command_Line.Argument (1));
   Parser.Parse;
   Parser.Execute_Actions;

   if Verbose then
      Put_Line ("colors:");
      for Cur in Data.Colors.Iterate loop
         Put_Line ("'" & Day_7_Handy_Haversacks_Runtime.Color_ID_Maps.Key (Cur) & "'");
      end loop;
   end if;

   declare
      use Day_7_Handy_Haversacks_Runtime;
      Target_Color : constant Color_ID := Color_ID_Maps.Element (Color_ID_Maps.Find (Data.Colors, "shiny gold"));
      Result       : Integer           := 0; -- count of bag colors that can contain at least 1 target color bag
      Last_Result  : Integer           := 0;
      Pass         : Integer           := 1;

      Rules : array (1 .. Data.Max_Color_ID) of Rule; -- Fast random access to rules

      --  We use a closure operation on Rule.Contains_Target, rather than
      --  direct recursion, to avoid overflowing the stack when there are a
      --  large number of rules.
      function Contains_Target (Bag_Color : in Base_Color_ID) return Boolean
      is begin
         if Bag_Color = No_Color then
            return False;
         end if;
         if Bag_Color = Target_Color then
            return True;
         end if;

         return Rules (Bag_Color).Contains_Target;
      end Contains_Target;

   begin
      for Rule of Data.Rules loop
         Rules (Rule.Containing_Color) := Rule;
      end loop;

      loop
         Result := 0;
         for Rule of Rules loop
            if Contains_Target (Rule.Contained_Color_1) or
              Contains_Target (Rule.Contained_Color_2)
            then
               Result := @ + 1;
               Rule.Contains_Target := True;
            end if;
         end loop;

         if Verbose then
            Put_Line ("pass" & Pass'Image & ":" & Result'Image);
         end if;
         exit when Last_Result = Result;
         Last_Result := Result;
         Pass := @ + 1;
      end loop;
      Put_Line ("result" & Result'Image);
   end;

exception
when WisiToken.Syntax_Error =>
   Parser.Put_Errors;
end Day_7_Handy_Haversacks;
