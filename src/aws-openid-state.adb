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

with AWS.OpenID.Association_Database;
with AWS.OpenID.Authentication_Database;

package body AWS.OpenID.State is

   ------------
   --  Load  --
   ------------

   procedure Load
     (File_Name           : in String;
      Suppress_Exceptions : in Boolean := False)
   is
   begin
      Load_Association_Data :
      begin
         AWS.OpenID.Association_Database.Load (File_Name & ".associations");
      exception
         when others =>
            if not Suppress_Exceptions then
               raise;
            end if;
      end Load_Association_Data;

      Load_Authentication_Data :
      begin
         AWS.OpenID.Authentication_Database.Load
           (File_Name & ".authentications");
      exception
         when others =>
            if not Suppress_Exceptions then
               raise;
            end if;
      end Load_Authentication_Data;
   end Load;

   ------------
   --  Save  --
   ------------

   procedure Save
     (File_Name : in String)
   is
   begin
      AWS.OpenID.Association_Database.Save (File_Name & ".associations");
      AWS.OpenID.Authentication_Database.Save (File_Name & ".authentications");
   end Save;

end AWS.OpenID.State;
