--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with
  Ada.Exceptions;
with
  AWS.Session,
  Yolk.Log;

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
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Error,
            Message => "Execption in Authentication_Database." &
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
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Error,
            Message => "Execption in Authentication_Database." &
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
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Error,
            Message => "Execption in Authentication_Database.Identity: " &
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
         Yolk.Log.Trace
           (Handle  => Yolk.Log.Error,
            Message => "Execption in Authentication_Database.Delete: " &
                       Ada.Exceptions.Exception_Name (E));
         raise;
   end Delete_Identity;

   procedure Save (File_Name : in     String) renames AWS.Session.Save;

   procedure Load (File_Name : in     String) renames AWS.Session.Load;
end Authentication_Database;
