--  Abstract :
--
--  Advent of code 2020 day 1 puzzle 1
--
--  https://adventofcode.com/2020/day/1
--
--  Copyright (C) 2020 Free Software Foundation All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
procedure Day_3_Toboggan_Trajectory
is
   Input_Error : exception;

   Verbose : constant Boolean := Ada.Command_Line.Argument_Count > 1;

   Input_File : File_Type;

   --  We don't use String, because we want index to start at 0
   type Char_Array is array (Natural range <>) of Character;
begin
   Open (Input_File, In_File, Ada.Command_Line.Argument (1));

   declare
      Line_1 : constant String := Get_Line (Input_File);

      subtype String_N is Char_Array (0 .. Line_1'Last - 1);

      package String_Arrays is new Ada.Containers.Vectors
        (Index_Type   => Positive,
         Element_Type => String_N);

      Forrest : String_Arrays.Vector;

      procedure Append (Item : in String)
      is
         Temp : String_N;
      begin
         for I in Item'Range loop
            Temp (I - 1) := Item (I);
         end loop;
         Forrest.Append (Temp, Count => 1);
      end Append;

      type Position is record
         --  Match Emacs column-number-mode display
         Line : Integer; -- top to bottom
         Col  : Integer; -- left to right
      end record;

      function Image (Pos : in Position) return String
      is ("(" & Pos.Line'Image & "," & Pos.Col'Image & ")");

      function "+" (Left, Right : in Position) return Position
      is begin
         return Result : Position :=
           (Line => Left.Line + Right.Line,
            Col  => Left.Col + Right.Col)
         do
            if Result.Col > String_N'Last then
               Result.Col := @ - String_N'Last - 1;
            end if;
         end return;
      end "+";

      function Element (Pos : in Position) return Character
      is begin
         return Forrest (Pos.Line)(Pos.Col);
      exception
      when Constraint_Error =>
         raise Constraint_Error with Image (Pos) & " out of forrest";
      end Element;

      function Count_Trees (Step : in Position) return Integer
      is
         Pos        : Position := (Line => 1, Col => 0);
         Tree_Count : Integer  := 0;
      begin
         loop
            if Verbose then
               Put ("pos: " & Image (Pos));
            end if;

            case Element (Pos) is
            when '.' =>
               if Verbose then
                  Put_Line (" open");
               end if;

            when '#' =>
               Tree_Count := @ + 1;
               if Verbose then
                  Put_Line (" tree");
               end if;

            when others =>
               raise Input_Error;
            end case;

            Pos := Pos + Step;
            exit when Pos.Line > Forrest.Last_Index;
         end loop;

         if Verbose then
            Put_Line ("slope:" & Image (Step) & ", count:" & Tree_Count'Image);
         end if;
         return Tree_Count;
      end Count_Trees;

   begin
      --  Build Forrest
      Append (Line_1);
      loop
         exit when End_Of_File (Input_File);
         Append (Get_Line (Input_File));
      end loop;

      Put_Line
        ("product of tree counts:" & Integer'Image
           (Count_Trees (Step => (Col => 1, Line => 1)) *
              Count_Trees (Step => (Col => 3, Line => 1)) *
              Count_Trees (Step => (Col => 5, Line => 1)) *
              Count_Trees (Step => (Col => 7, Line => 1)) *
              Count_Trees (Step => (Col => 1, Line => 2))));
   end;
end Day_3_Toboggan_Trajectory;
