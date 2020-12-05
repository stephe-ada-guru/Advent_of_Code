--  Copyright (C) 2020 Stephen Leake All Rights Reserved.
--
--  This library is free software;  you can redistribute it and/or modify it
--  under terms of the  GNU General Public License  as published by the Free
--  Software  Foundation;  either version 3,  or (at your  option) any later
--  version. This library is distributed in the hope that it will be useful,
--  but WITHOUT ANY WARRANTY;  without even the implied warranty of MERCHAN-
--  TABILITY or FITNESS FOR A PARTICULAR PURPOSE.

pragma License (Modified_GPL);

with Ada.Strings.Maps;
with Ada.Command_Line;
with Ada.Containers.Vectors;
with Ada.Text_IO; use Ada.Text_IO;
procedure Day_5_Binary_Boarding
is
   Verbose : constant Boolean := Ada.Command_Line.Argument_Count > 1;

   Input_File  : File_Type;

   Max_ID : Integer := 0;

   package Integer_Vectors is new Ada.Containers.Vectors (Positive, Integer);

   IDs : Integer_Vectors.Vector;

   function To_Binary_String (Item : in String) return String
   is
      use Ada.Strings.Maps;
      Map : constant Character_Mapping := To_Mapping ("FBLR", "0101");
      Result : String (Item'Range);
   begin
         for I in Item'Range loop
            Result (I) := Value (Map, Item (I));
         end loop;
      return "2#" & Result & "#";
   end To_Binary_String;

begin
   Open (Input_File, In_File, Ada.Command_Line.Argument (1));

   Lines :
   loop
      exit Lines when End_Of_File (Input_File);

      declare
         Line : constant String  := Get_Line (Input_File);
         ID   : constant Integer := Integer'Value (To_Binary_String (Line));
      begin
         if Verbose then
            Put_Line (Line & ":" & ID'Image);
         end if;
         IDs.Append (ID);

         if ID > Max_ID then
            Max_ID := ID;
         end if;
      end;
   end loop Lines;

   Put_Line ("seat_count:" & IDs.Last_Index'Image);
   Put_Line ("max_id:" & Max_ID'Image);

   declare
      Present : array (Integer range 1 .. Max_ID) of Boolean := (others => False);
   begin
      for ID of IDs loop
         Present (ID) := True;
      end loop;

      for ID in Present'First + 1 .. Present'Last - 1 loop
         if Present (ID - 1) and (not Present (ID)) and Present (ID + 1) then
            Put_Line ("my seat id:" & ID'Image);
         end if;
      end loop;
   end;
end Day_5_Binary_Boarding;
