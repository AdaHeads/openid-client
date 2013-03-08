-------------------------------------------------------------------------------
--                                                                           --
--                      Copyright (C) 2012-, AdaHeads K/S                    --
--                                                                           --
--  This is free software;  you can redistribute it and/or modify it         --
--  under terms of the  GNU General Public License  as published by the      --
--  Free Software  Foundation;  either version 3,  or (at your  option) any  --
--  later version. This library is distributed in the hope that it will be   --
--  useful, but WITHOUT ANY WARRANTY;  without even the implied warranty of  --
--  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.                     --
--  You should have received a copy of the GNU General Public License and    --
--  a copy of the GCC Runtime Library Exception along with this program;     --
--  see the files COPYING3 and COPYING.RUNTIME respectively.  If not, see    --
--  <http://www.gnu.org/licenses/>.                                          --
--                                                                           --
-------------------------------------------------------------------------------

package Logger is

   procedure Close;
   --  Close the log file.

   procedure Debug
     (Message : in String);
   --  Write a debug Message to log file.

   procedure Error
     (Message : in String);
   --  Write an error Message to log file.

   procedure Info
     (Message : in String);
   --  Write an info Message to log file.

   procedure Open
     (File_Name : in String);
   --  Open the File_Name log file.

   procedure Warning
     (Message : in String);
   --  Write a warning Message to log file.

end Logger;
