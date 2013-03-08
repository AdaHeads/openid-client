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

with AWS.OpenID.Security;

package AWS.OpenID.Association_Database is

   function Has
     (Handle : in AWS.OpenID.Security.Association_Handle)
      return Boolean;
   --  Return True if Handle is in the associations database.

   procedure Insert
     (Item : in AWS.OpenID.Security.Association);
   --  Insert Item into the associations database.

   procedure Load
     (File_Name : in String);
   --  Load File_Name into the associations database.

   function Look_Up
     (Handle : in AWS.OpenID.Security.Association_Handle)
      return AWS.OpenID.Security.Association;
   --  Reuturn the Handle association.

   procedure Save
     (File_Name : in String);
   --  Save the association database to File_Name

end AWS.OpenID.Association_Database;
