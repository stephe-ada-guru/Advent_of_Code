--  Abstract :
--
--  see spec.
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

with Ada.Text_IO;
with Day_7_Handy_Haversacks_Actions; use Day_7_Handy_Haversacks_Actions;
with SAL;
with WisiToken.Syntax_Trees.LR_Utils;
package body Day_7_Handy_Haversacks_Runtime is

   overriding
   procedure Set_Lexer
     (User_Data           : in out Day_7_Handy_Haversacks_Runtime.User_Data;
      Lexer               : in     WisiToken.Lexer.Handle;
      Line_Begin_Char_Pos : in     WisiToken.Line_Pos_Vector_Access)
   is
      pragma Unreferenced (Line_Begin_Char_Pos);
   begin
      User_Data.Lexer := Lexer;
   end Set_Lexer;

   procedure Add_Rule
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array)
   is
      pragma Unreferenced (Nonterm);

      Data : Day_7_Handy_Haversacks_Runtime.User_Data renames Day_7_Handy_Haversacks_Runtime.User_Data (User_Data);

      function Find_Add_Color (Name : in String) return Color_ID
      is
         use Color_ID_Maps;
         Found : constant Cursor := Data.Colors.Find (Name);
      begin
         if Has_Element (Found) then
            if Verbose then
               Ada.Text_IO.Put_Line ("found color '" & Name & "'");
            end if;
            return Data.Colors (Found);
         else
            if Verbose then
               Ada.Text_IO.Put_Line ("adding color '" & Name & "'");
            end if;
            Data.Max_Color_ID := @ + 1;
            Data.Colors.Insert (Name, Data.Max_Color_ID);
            return Data.Max_Color_ID;
         end if;
      end Find_Add_Color;

      Colors : Color_Lists.List;
   begin
      case To_Token_Enum (Tree.ID (Tokens (4))) is
      when contained_bags_ID =>
         declare
            use WisiToken.Syntax_Trees.LR_Utils;
            Bags : constant Constant_List := Creators.Create_List
              (Tree, Tokens (4), +contained_bags_ID, +contained_bag_ID);
         begin
            for Bag of Bags loop
               Colors.Append (Find_Add_Color (Data.Lexer.Buffer_Text (Tree.Byte_Region (Tree.Child (Bag, 2)))));
            end loop;
         end;
      when NO_ID =>
         null;

      when others =>
         raise SAL.Programmer_Error;
      end case;

      Data.Rules.Append
        ((Containing_Color => Find_Add_Color (Data.Lexer.Buffer_Text (Tree.Byte_Region (Tokens (1)))),
          Contained_Colors => Colors,
          Contains_Target  => False));
   end Add_Rule;

end Day_7_Handy_Haversacks_Runtime;
--  Local Variables:
--  ada-case-strict: nil
--  End:
