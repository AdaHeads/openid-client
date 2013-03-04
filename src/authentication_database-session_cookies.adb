-------------------------------------------------------------------------------                                                                      --
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

with Ada.Exceptions;

with AWS.Session;

with AWS.OpenID.Log;

package body Authentication_Database is

   Session_Key : constant String := "OpenID.Identity";

   -----------------------
   --  Delete_Identity  --
   -----------------------

   procedure Delete_Identity
     (Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data)
   is
      pragma Unreferenced (Response);
   begin
      AWS.Session.Remove (SID => AWS.Status.Session (Request),
                          Key => Session_Key);
   exception
      when E : others =>
         AWS.OpenID.Log.Error
           (Message => "Exception in Authentication_Database.Delete: " &
              Ada.Exceptions.Exception_Name (E));
         raise;
   end Delete_Identity;

   ----------------
   --  Identity  --
   ----------------

   function Identity
     (Request : in AWS.Status.Data)
      return String
   is
   begin
      if AWS.Session.Exist (SID => AWS.Status.Session (Request),
                            Key => Session_Key)
      then
         return AWS.Session.Get (SID => AWS.Status.Session (Request),
                                 Key => Session_Key);
      else
         raise Not_Authenticated;
      end if;
   exception
      when Not_Authenticated =>
         raise;
      when E : others =>
         AWS.OpenID.Log.Error
           (Message => "Exception in Authentication_Database.Identity: " &
              Ada.Exceptions.Exception_Name (E));
         raise;
   end Identity;

   ------------------------
   --  Is_Authenticated  --
   ------------------------

   function Is_Authenticated
     (Request : in AWS.Status.Data)
      return Boolean
   is
   begin
      return AWS.Session.Exist (SID => AWS.Status.Session (Request),
                                Key => Session_Key);
   exception
      when E : others =>
         AWS.OpenID.Log.Error
           (Message => "Exception in Authentication_Database." &
              "Is_Authenticated: " &
              Ada.Exceptions.Exception_Name (E) & " (" &
              Ada.Exceptions.Exception_Information (E) & ")");
         raise;
   end Is_Authenticated;

   ------------
   --  Load  --
   ------------

   procedure Load (File_Name : in String) renames AWS.Session.Load;

   -------------------------
   --  Register_Identity  --
   -------------------------

   procedure Register_Identity
     (Source   : in     AWS.OpenID.Security.Authentication;
      Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data)
   is
      pragma Unreferenced (Response);

      use type AWS.Session.Id;

      ID : constant AWS.Session.Id := AWS.Status.Session (Request);
   begin
      if ID /= AWS.Session.No_Session then
         AWS.Session.Set (SID   => ID,
                          Key   => Session_Key,
                          Value => AWS.OpenID.Security.Identity (Source));
      end if;
   exception
      when Not_Authenticated =>
         raise;
      when E : others =>
         AWS.OpenID.Log.Error
           (Message => "Exception in Authentication_Database." &
                       "Register_Identity: " &
                       Ada.Exceptions.Exception_Name (E) & " (" &
                       Ada.Exceptions.Exception_Information (E) & ")");
         raise;
   end Register_Identity;

   ------------
   --  Save  --
   ------------

   procedure Save (File_Name : in String) renames AWS.Session.Save;

end Authentication_Database;
