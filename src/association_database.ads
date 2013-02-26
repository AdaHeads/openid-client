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

with Security.Openid;

package Association_Database is

   procedure Clean_Up;
   --  TODO: write comment

   function Has
     (Handle : in Security.Openid.Association_Handle)
      return Boolean;
   --  TODO: write comment

   procedure Insert
     (Item : in Security.Openid.Association);
   --  TODO: write comment

   procedure Load
     (File_Name : in String);
   --  TODO: write comment

   function Look_Up
     (Handle : in Security.Openid.Association_Handle)
      return Security.Openid.Association;
   --  TODO: write comment

   procedure Save
     (File_Name : in String);
   --  TODO: write comment

end Association_Database;
