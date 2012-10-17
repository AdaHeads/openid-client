--  The Beer-Ware License (revision 42)
--
--  Jacob Sparre Andersen <jacob@jacob-sparre.dk> wrote this. As long as you
--  retain this notice you can do whatever you want with this stuff. If we meet
--  some day, and you think this stuff is worth it, you can buy me a beer in
--  return.
--
--  Jacob Sparre Andersen

with
  Association_Database,
  Authentication_Database;

package body AWS.OpenID.State is
   procedure Save (File_Name : in     String) is
   begin
      Association_Database.Save    (File_Name & ".associations");
      Authentication_Database.Save (File_Name & ".authentications");
   end Save;

   procedure Load (File_Name : in     String) is
   begin
      Association_Database.Load    (File_Name & ".associations");
      Authentication_Database.Load (File_Name & ".authentications");
   end Load;
end AWS.OpenID.State;
