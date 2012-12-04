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

with
  Ada.Exceptions;
with
  AWS.Session,
  AWS.OpenID.Log;

package body Authentication_Database is
   Session_Key : constant String := "OpenID.Identity";

   procedure Register_Identity
     (Source   : in     Security.OpenID.Authentication;
      Request  : in     AWS.Status.Data;
      Response : in out AWS.Response.Data) is
      pragma Unreferenced (Response);
      use type AWS.Session.ID;
      ID : AWS.Session.ID := AWS.Status.Session (Request);
   begin
      if ID = AWS.Session.No_Session then
         null;
      else
         AWS.Session.Set (SID   => ID,
                          Key   => Session_Key,
                          Value => Security.OpenID.Identity (Source));
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

   function Is_Authenticated (Request  : in AWS.Status.Data) return Boolean is
      pragma Inline (Is_Authenticated);
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

   function Identity (Request : in AWS.Status.Data) return String is
      pragma Inline (Identity);
   begin
      if AWS.Session.Exist (SID => AWS.Status.Session (Request),
                            Key => Session_Key) then
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

   procedure Delete_Identity (Request  : in     AWS.Status.Data;
                              Response : in out AWS.Response.Data) is
      pragma Inline (Delete_Identity);
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

   procedure Save (File_Name : in     String) renames AWS.Session.Save;

   procedure Load (File_Name : in     String) renames AWS.Session.Load;
end Authentication_Database;
