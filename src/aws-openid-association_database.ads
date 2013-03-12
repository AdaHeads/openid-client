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

   procedure Clean_Up;
   --  Remove all stale Associations from the database. Calling Clean_Up is a
   --  manual process. The library does _NOT_ handle this automatically. It is
   --  left to the user of the library to decide when and how often to clean
   --  the associations database.

   type Has_Association_Handle_Type is access function
     (Handle : in AWS.OpenID.Security.Association_Handle)
      return Boolean;

   function Has_Handle
     (Handle : in AWS.OpenID.Security.Association_Handle)
      return Boolean;
   --  Return True if Handle is in the associations database.

   Has : Has_Association_Handle_Type := Has_Handle'Access;

   type Insert_Association_Type is access procedure
     (Item : in AWS.OpenID.Security.Association);

   procedure Insert_Association
     (Item : in AWS.OpenID.Security.Association);
   --  Insert Item into the associations database.

   Insert : Insert_Association_Type := Insert_Association'Access;

   procedure Load
     (File_Name : in String);
   --  Load File_Name into the associations database. Do not call Load if
   --  you've setup your own Has, Insert and Look_Up handlers.

   type Look_Up_Association_Handle_Type is access function
     (Handle : in AWS.OpenID.Security.Association_Handle)
     return AWS.OpenID.Security.Association;

   function Look_Up_Association_Handle
     (Handle : in AWS.OpenID.Security.Association_Handle)
      return AWS.OpenID.Security.Association;
   --  Reuturn the Handle association.

   Look_Up : Look_Up_Association_Handle_Type :=
               Look_Up_Association_Handle'Access;

   procedure Save
     (File_Name : in String);
   --  Save the association database to File_Name. Do not call Save if you've
   --  set your own Has, Insert and Look_Up handlers.

end AWS.OpenID.Association_Database;
