unit uFeatures;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, FileUtil, LResources, Forms, Controls, Graphics, Dialogs,
  StdCtrls;

type

  { TFormFeatures }

  TFormFeatures = class(TForm)
    ListBoxFeatures: TListBox;
  private
    { private declarations }
  public
    { public declarations }
  end; 

var
  FormFeatures: TFormFeatures;

implementation


initialization
  {$I ufeatures.lrs}

end.

