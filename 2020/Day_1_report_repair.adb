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
procedure Day_1_Report_Repair
is
   package Int_Vectors is new Ada.Containers.Vectors
     (Index_Type   => Natural,
      Element_Type => Integer);

   Input_File : File_Type;

   Data : Int_Vectors.Vector;
begin
   Open (Input_File, In_File, Ada.Command_Line.Argument (1));

   loop
      exit when End_Of_File (Input_File);

      Data.Append (Integer'Value (Get_Line (Input_File)));
   end loop;

   for I in Data.First_Index .. Data.Last_Index loop
      for J in I + 1 .. Data.Last_Index loop
         if Data (I) + Data (J) = 2020 then
            Put_Line
              (I'Image & J'Image & ":" & Integer'Image (Data (I)) & Integer'Image (Data (J)) & ":" &
                 Integer'(Data (I) * Data (J))'Image);
            exit;
         end if;
      end loop;
   end loop;

   for I in Data.First_Index .. Data.Last_Index loop
      for J in I + 1 .. Data.Last_Index loop
         for K in J + 1 .. Data.Last_Index loop
            if Data (I) + Data (J) + Data (K) = 2020 then
               Put_Line
                 (I'Image & J'Image & K'Image &
                    ":" & Integer'Image (Data (I)) & Integer'Image (Data (J)) & Integer'Image (Data (K)) &
                    ":" & Integer'Image (Data (I) * Data (J) * Data (K)));
               exit;
            end if;
         end loop;
      end loop;
   end loop;

end Day_1_Report_Repair;
