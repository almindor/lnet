program ftptest;

{$mode objfpc}{$H+}

uses
  Interfaces, // this includes the LCL widgetset
  Forms
  { add your units here }, Main, sitesunit, dleparsers, lnetvisual, uFeatures;

begin
  Application.Title:='FTP Test case';
  Application.Initialize;
  Application.CreateForm(TMainForm, MainForm);
  Application.CreateForm(TFormFeatures, FormFeatures);
  Application.Run;
end.

