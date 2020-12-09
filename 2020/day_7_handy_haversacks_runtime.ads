--  Abstract :
--
--  WisiToken runtime for day 7 grammar
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

with Ada.Containers.Doubly_Linked_Lists;
with Ada.Containers.Indefinite_Ordered_Maps;
with WisiToken.Lexer;
with WisiToken.Syntax_Trees;
package Day_7_Handy_Haversacks_Runtime is

   Verbose : Boolean;

   type Base_Color_ID is new Integer range 0 .. Integer'Last;
   subtype Color_ID is Base_Color_ID range 1 .. Base_Color_ID'Last;
   No_Color : constant Base_Color_ID := 0;

   package Color_ID_Maps is new Ada.Containers.Indefinite_Ordered_Maps
     (Key_Type     => String,
      Element_Type => Color_ID);

   type Contained_Bag is record
      Count : Positive;
      Color : Color_ID;
   end record;

   package Bag_Lists is new Ada.Containers.Doubly_Linked_Lists (Contained_Bag);

   type Rule is record
      Containing_Color : Base_Color_ID := No_Color;
      Contained_Bags   : Bag_Lists.List;
      Contains_Target  : Boolean       := False;
   end record;

   package Rules_Lists is new Ada.Containers.Doubly_Linked_Lists (Rule);

   type User_Data is new WisiToken.Syntax_Trees.User_Data_Type with
   record
      Lexer        : WisiToken.Lexer.Handle;
      Colors       : Color_ID_Maps.Map;
      Max_Color_ID : Base_Color_ID := No_Color;
      Rules        : Rules_Lists.List;
   end record;

   overriding
   procedure Set_Lexer
     (User_Data           : in out Day_7_Handy_Haversacks_Runtime.User_Data;
      Lexer               : in     WisiToken.Lexer.Handle;
      Line_Begin_Char_Pos : in     WisiToken.Line_Pos_Vector_Access);

   --  Grammar actions

   procedure Add_Rule
     (User_Data : in out WisiToken.Syntax_Trees.User_Data_Type'Class;
      Tree      : in out WisiToken.Syntax_Trees.Tree;
      Nonterm   : in     WisiToken.Syntax_Trees.Valid_Node_Access;
      Tokens    : in     WisiToken.Syntax_Trees.Valid_Node_Access_Array);

end Day_7_Handy_Haversacks_Runtime;
