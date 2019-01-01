unit sitesunit;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, StdCtrls,
  ExtCtrls, Buttons, IniFiles, Menus, EditBtn;

type

  TSiteInfo = record
    Site: string;
    Host: string;
    Port: string;
    path: string;
    user: string;
    pass: string;
    Anonymous: Boolean;
    ldir: string;
    Number: Integer;
  end;
  
  { TfrmSites }

  TfrmSites = class(TForm)
    btnAddSite: TBitBtn;
    btnSave: TBitBtn;
    btnClose: TBitBtn;
    btnConnect: TBitBtn;
    Label1: TLabel;
    Label2: TLabel;
    MenuItemSiteDelete: TMenuItem;
    PopupMenuSites: TPopupMenu;
    txtLDir: TDirectoryEdit;
    txtPort: TLabeledEdit;
    txtSite: TLabeledEdit;
    txtPass: TLabeledEdit;
    txtUser: TLabeledEdit;
    txtPath: TLabeledEdit;
    txtHost: TLabeledEdit;
    lbSites: TListBox;
    procedure lbSitesClick(Sender: TObject);
    procedure MenuItemSiteDeleteClick(Sender: TObject);
    procedure btnCloseClick(Sender: TObject);
    procedure btnConnectClick(Sender: TObject);
    procedure btnSaveClick(Sender: TObject);
    procedure FormCloseQuery(Sender: TObject; var CanClose: boolean);
    procedure btnAddSiteClick(Sender: TObject);
    procedure FormShow(Sender: TObject);
    procedure txtSiteChange(Sender: TObject);
  private
    { private declarations }
    FSites: array of TSiteInfo;
    FSiteIndex: Integer;
    function OkToClose: boolean;
    function  SaveChanges: boolean;
    procedure DeleteSite(const i: Integer);
    procedure LoadSites;
    procedure SaveSites;
    procedure UpdateCurrentSite;
    function  CheckInfo: integer;
    procedure Modified(ok: boolean);
    procedure SaveCurrentSite;
  protected
    procedure ChildHandlesCreated; override;
  public
    { public declarations }
    class procedure LoadLastSite;
    class procedure SaveOption(const ASection,AOption,AValue:string);
    class procedure SaveOption(const ASection,AOption:string; AValue:Integer);
  end;

var
  frmSites: TfrmSites;
  Site: TSiteInfo;
  
  function  GetSitePath:string;

implementation

// simple xor encrypt
function EncryptString(const s:string): string;
var
  i: Integer;
begin
  result := '';
  for i:=1 to Length(s) do
    result := result + chr(ord(s[i]) xor 21);
end;

function DecryptString(const s:string): string;
begin
  result := EncryptString(s);
end;

class procedure TfrmSites.LoadLastSite;
var
  Ini: TIniFile;
  i: Integer;
  s: string;
begin
  s := extractFilePath(Application.ExeName)+'sites.ini';
  Ini := TIniFile.Create(s);
  try
    I := Ini.ReadInteger('global','lastsite', -1);
    s := 'site'+IntToStr(i);
    Site.Site := Ini.ReadString(s, 'site', '');
    Site.Host := Ini.ReadString(s, 'host', '');
    Site.Port := Ini.ReadString(s, 'port', '');
    Site.path := Ini.ReadString(s, 'path', '');
    Site.user := Ini.ReadString(s, 'user', '');
    Site.pass := DecryptString(Ini.ReadString(s, 'pass', ''));
    Site.ldir := ini.ReadString(s, 'ldir', '');
    if Site.Site<>'' then
      Site.Number := I
    else
      Site.Number := 0;
  finally
    Ini.Free;
  end;
end;

class procedure TfrmSites.SaveOption(const ASection, AOption, AValue: string);
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(extractFilePath(Application.ExeName)+'sites.ini');
  try
    Ini.WriteString(ASection, AOption, AValue);
  finally
    Ini.Free;
  end;
end;

class procedure TfrmSites.SaveOption(const ASection, AOption: string; AValue: Integer
  );
var
  Ini: TIniFile;
begin
  Ini := TIniFile.Create(extractFilePath(Application.ExeName)+'sites.ini');
  try
    Ini.WriteInteger(ASection, AOption, AValue);
  finally
    Ini.Free;
  end;
end;

function GetSitePath: string;
begin
  if Site.path='' then
    result := '/'
  else
    Result := Site.Path;
end;

{ TfrmSites }

procedure TfrmSites.FormCloseQuery(Sender: TObject; var CanClose: boolean);
begin
  CanClose := OkToClose;
end;

procedure TfrmSites.btnSaveClick(Sender: TObject);
begin
  if SaveChanges then begin
    Modified(False);
    SaveCurrentSite;
  end;
end;

procedure TfrmSites.btnConnectClick(Sender: TObject);
begin
  if CheckInfo<>0 then
    exit;
  SaveCurrentSite;
  ModalResult := mrYes;
end;

procedure TfrmSites.btnCloseClick(Sender: TObject);
begin
  ModalResult := mrCancel;
end;

procedure TfrmSites.MenuItemSiteDeleteClick(Sender: TObject);
var
  i: Integer;
begin
  i := lbSites.ItemIndex;
  
  if (i >= 0) and (i < Length(FSites)) then begin
    lbSites.Items.Delete(i);
    txtSite.Text := '';
    txtPass.Text := '';
    txtHost.Text := '';
    txtPort.Text := '';
    txtUser.Text := '';
    txtldir.Text := '';
    Modified(False);
    DeleteSite(i);
  end;
end;

procedure TfrmSites.lbSitesClick(Sender: TObject);
begin
  UpdateCurrentSite;
end;

procedure TfrmSites.btnAddSiteClick(Sender: TObject);
begin
  if not btnConnect.Enabled then begin
    // Canceling site data
    
    UpdateCurrentSite;
    
  end else begin
    // it's a new site
    FSiteIndex := -1;
    Modified(True);

    // defaults for new site
    txtSite.text := '';
    txtHost.Text := '';
    txtPort.Text := '21';
    txtPath.Text := '/';
    txtUser.Text := 'anonymous';
    txtPass.Text := '';
    txtldir.text := '';
    
    btnSave.Enabled := false;
  end;
end;

procedure TfrmSites.FormShow(Sender: TObject);
begin
  LoadSites;
  UpdateCurrentSite;
end;

procedure TfrmSites.txtSiteChange(Sender: TObject);
begin
  Modified(True);
end;

function TfrmSites.OkToClose: boolean;
var
  res: TModalResult;
begin
  result := True;
  if btnSave.Enabled then begin
    res := QuestionDlg('Warning!', 'There are unsaved changes'^M'What do you want to do?'^M,
      mtWarning, [mrOk, 'Save Changes', mrIgnore, 'Ignore changes', mrCancel, 'Don''t close'], 0);
    if res = mrOk then
      result := SaveChanges else
    if res=mrCancel then
      result := False;
  end;
end;

function TfrmSites.SaveChanges: boolean;
var
  i: Integer;
begin
  // check consistency
  result := CheckInfo = 0;
  if not result then
    exit;

  if (FSiteIndex<0) then begin
  
    i := Length(FSites);
    SetLength(FSites, i + 1);
    
    FSiteIndex := lbSites.Items.Add(txtSite.Text);
    
  end else begin
  
    i := FSiteIndex;
    // check for extreme cases, should not happan but better be sure...
    if (i<0) or (i>=Length(FSites)) then begin
      i := Length(FSites)-1;
      FSiteIndex := lbSites.Items.IndexOf(FSites[i].Site);
    end;
    
  end;
  
  // store site values
  FSites[i].Site := txtSite.Text;
  FSites[i].Host := txtHost.Text;
  FSites[i].Port := txtPort.Text;
  FSites[i].path := txtPath.Text;
  FSites[i].User := txtUser.Text;
  FSites[i].Pass := txtPass.Text;
  FSites[i].ldir := txtldir.Text;
  
  SaveSites;
end;

procedure TfrmSites.DeleteSite(const i: Integer);
var
  j: Integer;
begin
  for j := i to High(FSites) - 1 do
    FSites[j] := FSites[j + 1];
    
  SetLength(FSites, Length(FSites) - 1);
  SaveSites;
end;

procedure TfrmSites.LoadSites;
var
  Ini: TIniFile;
  i,n: Integer;
  s  : string;
begin
  s := extractFilePath(Application.ExeName) + 'sites.ini';
  Ini := TIniFile.Create(s);
  try
    n:= Ini.ReadInteger('global', 'sitecount', 0);
    SetLength(FSites, n);
    lbSites.Clear;
    for i := 1 to n do
    with FSites[i - 1] do begin
      s := 'site'+IntToStr(i);
      Site := Ini.ReadString(s, 'site', s);
      Host := Ini.ReadString(s, 'host', '');
      Port := Ini.ReadString(s, 'port', '');
      Path := Ini.ReadString(s, 'path', '');
      User := Ini.ReadString(s, 'user', '');
      Pass := DecryptString(Ini.ReadString(s, 'pass', ''));
      ldir := ini.ReadString(s, 'ldir', '');
      lbSites.Items.add(Site);
    end;

    // select the current site on list
    if Site.Site<>'' then begin
      FSiteIndex := lbSites.Items.IndexOf(Site.Site);
      lbSites.ItemIndex:=FSiteIndex;
    end;

  finally
    ini.free;
  end;
end;

procedure TfrmSites.SaveSites;
var
  Ini: TIniFile;
  i  : Integer;
  s  : string;
begin
  s := extractFilePath(Application.ExeName) + 'sites.ini';
  Ini := TIniFile.Create(s);
  try
    ini.WriteInteger('global', 'sitecount', Length(Fsites));
    for i := 0 to High(FSites) do begin
      s :='site' + IntToStr(i + 1);
      ini.WriteString(s, 'site', FSites[i].Site);
      ini.WriteString(s, 'host', FSites[i].Host);
      ini.WriteString(s, 'port', FSites[i].Port);
      ini.WriteString(s, 'path', FSites[i].Path);
      ini.WriteString(s, 'user', FSites[i].User);
      ini.WriteString(s, 'pass', EncryptString(FSites[i].Pass));
      ini.WriteString(s, 'ldir', FSites[i].ldir);
    end;
  finally
    ini.free;
  end;
end;

procedure TfrmSites.UpdateCurrentSite;
begin

  if lbSites.Items.Count>0 then begin
    FSiteIndex := lbSites.ItemIndex;
    if FSiteIndex<0 then
      FSiteIndex := 0;
  end else
    FSiteIndex := -1;
    
  lbSites.ItemIndex := FSiteIndex;
  
  if (FSiteIndex>=0) and (FSiteIndex<Length(FSites)) then
  with FSites[FSiteIndex] do begin
    txtSite.Text := Site;
    txtHost.Text := Host;
    txtPort.Text := Port;
    txtPath.Text := Path;
    txtUser.Text := User;
    txtPass.Text := Pass;
    txtldir.Text := ldir;
  end;
  
  Modified(False);
end;

function TfrmSites.CheckInfo: integer;
var
  tmp: Integer;
begin
  result := 0;
  
  with lbSites.Items do begin
  
    if txtSite.text = '' then
      result := 1
    else
    if (FSiteIndex<0) and (IndexOf(txtSite.text)>=0)
    then
      result := 2
    else
    if txtHost.Text = '' then
      result := 3
    else
    if txtPort.Text = '' then
      result := 4
    else
    if txtUser.Text = '' then
      result := 5;
      
    tmp := StrToIntDef(txtPort.Text, -1);
    if tmp < 0 then
      Result := 6
    else
    if (tmp < 1) or (tmp > 65535) then
      result := 7;
      
  end;

  case result of
    1: ShowMessage('Site dosn''t have a name');
    2: ShowMessage('Site already exists');
    3: ShowMessage('Host is blank');
    4: ShowMessage('Port is blank, try 21');
    5: ShowMessage('User is blank');
    6: ShowMessage('Port value is invalid');
    7: ShowMessage('Port value is out of range');
  end;
end;

procedure TfrmSites.Modified(ok: boolean);
begin
  if ok then
    btnAddSite.Caption := 'Cancel'
  else
    btnAddSite.Caption := 'Add Site';
    
  btnSave.Enabled := ok;
  btnConnect.Enabled := not ok;
  lbSites.Enabled := not ok;
end;

procedure TfrmSites.SaveCurrentSite;
begin
  Site.Site := txtSite.Text;
  Site.Host := txtHost.Text;
  Site.Port := txtPort.Text;
  Site.Path := txtPath.text;
  Site.User := txtUser.Text;
  Site.Pass := txtPass.Text;
  Site.LDir := txtLDir.Text;

  // be sure current site is selected
  FSiteIndex := lbSites.Items.IndexOf(Site.Site);
  if FSiteIndex>=0 then
    lbSites.ItemIndex := FSiteIndex;
    
  Site.Number:= lbSites.ItemIndex + 1;
  
  SaveOption('global', 'lastsite', Site.Number);
end;

procedure TfrmSites.ChildHandlesCreated;
begin
  inherited ChildHandlesCreated;
  Modified(False);
end;

initialization

  {$I sitesunit.lrs}

end.

