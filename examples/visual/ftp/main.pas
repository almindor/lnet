unit main;

{$mode objfpc}{$H+}

{$define INCLUDEERRORS}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, LCLType,
  LNetComponents, lNet, lFTP, ComCtrls, ExtCtrls, StdCtrls, Menus, FileCtrl,
  ActnList, Grids;

type
  TParserResult=(
    prOK,        // directory listing entry (DLE) was parsed successful
    prError,     // DLE was recognized but an error occur while parsing
    prNoImp      // there is no parser for this DLE
  );

  TFTPOperation=(
    foNone,     // current operation is uninteresting
    foListing,  // a listing is expected
    foPWD       // pwd is expected
  );

  TTaggedPicture=class(TPicture)
  public
    Tag: Integer;
  end;


type

  { TMainForm }

  TMainForm = class(TForm)
    accSiteManager: TAction;
    accConnect: TAction;
    accDisconnect: TAction;
    ActionList1: TActionList;
    LeftView: TFileListBox;
    MenuItem1: TMenuItem;
    MenuItemFeatures: TMenuItem;
    MenuItemMkdir: TMenuItem;
    MenuSiteManager: TMenuItem;
    Panel1: TPanel;
    PopupDelete: TMenuItem;
    FTP: TLFTPClientComponent;
    PopupLeft: TPopupMenu;
    PopupLInfo: TMenuItem;
    PopupLDelete: TMenuItem;
    PopupLRename: TMenuItem;
    ProgressBar1: TProgressBar;
    PupupGet: TMenuItem;
    MemoText: TMemo;
    PopupRename: TMenuItem;
    PopupRight: TPopupMenu;
    MainMenu: TMainMenu;
    MenuItemFile: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemExit: TMenuItem;
    MenuItemAbout: TMenuItem;
    Splitter1: TSplitter;
    Splitter2: TSplitter;
    SBar: TStatusBar;
    rmtGrid: TStringGrid;
    ToolBar1: TToolBar;
    ToolButton1: TToolButton;
    ToolButton2: TToolButton;
    ToolButton3: TToolButton;
    ToolButton4: TToolButton;
    procedure AboutMenuItemClick(Sender: TObject);
    procedure FTPFailure(aSocket: TLSocket; const aStatus: TLFTPStatus);
    procedure FTPSuccess(aSocket: TLSocket; const aStatus: TLFTPStatus);
    procedure MenuItemFeaturesClick(Sender: TObject);
    procedure MenuItemMkdirClick(Sender: TObject);
    procedure accConnectExecute(Sender: TObject);
    procedure accDisconnectExecute(Sender: TObject);
    procedure accSiteManagerExecute(Sender: TObject);
    procedure DeletePopupClick(Sender: TObject);
    procedure ExitMenuItemClick(Sender: TObject);
    procedure FormDestroy(Sender: TObject);
    procedure FTPConnect(aSocket: TLSocket);
    procedure FTPControl(aSocket: TLSocket);
    procedure FTPError(const msg: string; aSocket: TLSocket);
    procedure FTPReceive(aSocket: TLSocket);
    procedure FTPSent(aSocket: TLSocket; const Bytes: Integer);
    procedure FormClose(Sender: TObject; var CloseAction: TCloseAction);
    procedure FormCreate(Sender: TObject);
    procedure FormKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure IPEditKeyPress(Sender: TObject; var Key: char);
    procedure LDeletePopupClick(Sender: TObject);
    procedure LInfoPopupClick(Sender: TObject);
    procedure LRenamePopupClick(Sender: TObject);
    procedure LeftViewDblClick(Sender: TObject);
    procedure ListPopupClick(Sender: TObject);
    procedure RenamePopupClick(Sender: TObject);
    procedure rmtGridCompareCells(Sender: TObject; Acol, ARow, Bcol,
      BRow: Integer; var Result: integer);
    procedure rmtGridDblClick(Sender: TObject);
    procedure rmtGridDrawCell(Sender: TObject; Col, Row: Integer; aRect: TRect;
      aState: TGridDrawState);
    procedure rmtGridHeaderClick(Sender: TObject; IsColumn: Boolean;
      Index: Integer);
    procedure rmtGridKeyDown(Sender: TObject; var Key: Word; Shift: TShiftState
      );
  private
    FLastN: Integer;
    FList: TStringList;
    FFile: TFileStream;
    FDLSize: Int64;
    FDLDone: Int64;
    //FIcons: array of TIconRec;
    FIcons: TStringList;
    FSpecialIcons: array of TTaggedPicture;
    CreateFilePath: string;
    FDirListing: string;
    procedure DoList(const FileName: string);
    procedure UpdateSite;
    function CurrentName: string;
    function CurrentNameLink: string;
    function CurrentSize: Int64;
    function CurrentIsDirectory: boolean;
    function CurrentIsLink: boolean;
    function CurrentIsError: boolean;
    function GetFileIcon(aName: string): TObject;
    procedure ChangeDirectory(aDir: string);
    procedure Disconnect(ClearLog: boolean);
    procedure LeftViewDrawItem(sender: TWinControl; Index: Integer;
                               ARect: TRect; State: TOwnerDrawState);
    procedure SetLocalDirectory(const ADir: string);
    { private declarations }
  public
    { public declarations }
    procedure RegisterExt(const LazResName,FileExt:string; special: boolean);
  end; 

var
  MainForm: TMainForm;

implementation

uses
  uFeatures, SitesUnit, DLEParsers;

const
  siDirUp = 0;
  siDir   = 1;
  siLink  = 2;
  siFile  = 3;
  siError = 4;

function RevPos(const substr,str:string): integer;
var
  i,j: Integer;
begin
  result := 0;
  j := Length(SubStr);
  if (j>0)and(j<=Length(Str)) then begin
    for i := Length(Str) downto 1 do
      if Str[i]<>SubStr[j] then
        exit
      else begin
        dec(j);
        if j=0 then
          break;
      end;
    result := i;
  end;
end;

function GetParentDirectory(Path: string): string;
var
  i: Integer;
begin
  Path := StringReplace(Path, PathDelim + PathDelim, PathDelim, [rfReplaceAll]);
  if Length(Path) > 1 then
    for i := Length(Path)-1 downto 1 do
      if Path[i] = PathDelim then begin
        Result := Copy(Path, 1, i);
        Exit;
      end;
  Result := Path;
end;

{ TMainForm }

procedure TMainForm.ExitMenuItemClick(Sender: TObject);
begin
  MainForm.Close;
end;

procedure TMainForm.FormDestroy(Sender: TObject);
var
  i,j: Integer;
begin
  for i := 0 to FIcons.Count-1 do
    if FIcons.Objects[i]<>nil then begin
      for j:=0 to FIcons.Count-1 do
        if FIcons.Objects[i]=FIcons.Objects[j] then
          FIcons.Objects[j]:=nil;
      FIcons.Objects[i].Free;
    end;
  FIcons.Free;

  for i := 0 to Length(FSpecialIcons)-1 do
    FSpecialIcons[i].Free;

 SetLength(FSpecialIcons,0);
end;

procedure TMainForm.FTPConnect(aSocket: TLSocket);
var
  aName, Pass: string;
begin
  aName := Site.User;
  if (aName='') and not InputQuery('Name', 'Please type in your username',
    False, aName)
  then
    exit;
  Pass := Site.Pass;
  if (not Site.Anonymous) and (Pass='') then
    Pass := PasswordBox('Password', 'Please type in your password');

  FTP.Authenticate(aName, Pass);
  FTP.Binary := True;
  
  aName := Site.path;
  if aName='/' then
    aName := '';

  FTP.ListFeatures;
  DoList(aName);
  // TODO: ask for current dir here
end;

procedure TMainForm.FTPControl(aSocket: TLSocket);
var
  s: string;
begin
  if FTP.GetMessage(s) > 0 then begin
    MemoText.Lines.Append(s);
    MemoText.SelStart := Length(MemoText.Text);
  end;
end;

procedure TMainForm.FTPError(const msg: string; aSocket: TLSocket);
begin
  MemoText.Append(msg);
  if not FTP.Connected then begin
    // connection has been closed, update gui
    Disconnect(false);
    ToolButton2.Down := True;
  end;
  CreateFilePath := '';
end;

procedure TMainForm.FTPReceive(aSocket: TLSocket);
  procedure FindNames;
  var
    i, nRow: Integer;
    Parser: TDirEntryParser;
  begin
    rmtGrid.BeginUpdate;
    try
      // adds dirup entry
      rmtGrid.RowCount := 2;
      rmtGrid.Cells[1,1] := '..';
      rmtGrid.objects[0,1] := FSpecialIcons[siDirUp];
      
      // adds every item in list
      if FList.Count > 0 then begin

        nRow := rmtGrid.RowCount;
        rmtGrid.RowCount := nRow + FList.Count;

        //FList.SaveToFile('last.txt');
        for i := 0 to FList.Count-1 do begin

          rmtGrid.Objects[0,nRow] := nil; // no special icon index

          Parser := DirParser.Parse(pchar(FList[i]));
          if Assigned(Parser) then begin
            DirParser.PrefParser := Parser;
            
            // default icon index/entry type
            if Parser.IsLink then begin
              rmtGrid.Objects[0,nRow] := FSpecialIcons[siLink];
              //rmtGrid.Objects[2,nRow] := FSpecialIcons[siLink];
              //rmtGrid.Objects[0,nRow] := FSpecialIcons[siFile];
            end else if Parser.IsDir then
              rmtGrid.Objects[0,nRow] := FSpecialIcons[siDir]
            else begin
              rmtGrid.Objects[0,nRow] := GetFileIcon(Parser.EntryName);
              //rmtGrid.Objects[2,nRow] := GetFileIcon(Parser.EntryName);
              //rmtGrid.Objects[0,nRow] := FSpecialIcons[siFile];
            end;

            // text properties
            rmtGrid.Cells[1, nRow] := Parser.EntryName;
            if Parser.IsDir or Parser.IsLink then
              rmtGrid.Cells[2, nRow] := ''
            else
              rmtGrid.Cells[2, nRow] := IntToStr(Parser.EntrySize);
            rmtGrid.Cells[3, nRow] := FormatDateTime(ShortDateFormat+' '+
                                                  ShortTimeFormat,Parser.Date);
            rmtGrid.Cells[4, nRow] := Parser.Attributes;
            if rmtGrid.Columns[5].Visible then
              rmtGrid.Cells[5, nRow] := Parser.LinkName;
          end else begin
            {$IFDEF INCLUDEERRORS}
            rmtGrid.Cells[1, nRow] := FList[i];
            rmtGrid.Objects[0, nRow] := FSpecialIcons[siError];
            {$ELSE}
            rmtGrid.RowCount:=rmtGrid.RowCount-1;
            continue;
            {$ENDIF}
          end;

          if rmtGrid.Objects[0, nRow] = nil then
            rmtGrid.Objects[0, nRow] := FSpecialIcons[siFile];

          Inc(nRow);
        end;
        
        if FList.Count>1 then
          rmtGrid.SortColRow(True, 1, 1, rmtGrid.RowCount-1);

      end;
    finally
      rmtGrid.EndUpdate;
    end;
  end;

var
  s: string;
  i: Integer;
  Buf: array[0..65535] of Byte;
begin
  if FTP.CurrentStatus = fsRetr then begin // getting file, save to file
    i := FTP.GetData(Buf, SizeOf(Buf));
    Inc(FDLDone, i);
    if i > 0 then begin
      if Length(CreateFilePath) > 0 then begin
        FFile := TFileStream.Create(CreateFilePath, fmCreate or fmOpenWrite);
        CreateFilePath := '';
      end;
      FFile.Write(Buf, i);
    end else if not FTP.DataConnection.Connected then begin
      // file download ended
      LeftView.UpdateFileList;
      FreeAndNil(FFile);
      CreateFilePath := '';
      DoList('');
    end;
    ProgressBar1.Position := Round(FDLDone / FDLSize * 100);
  end else begin // getting listing
    s := FTP.GetDataMessage;
    if Length(s) > 0 then
      FDirListing := FDirListing + s
    else begin
      FList.Text := FDirListing;
      FDirListing := '';
      FindNames;
      FList.Clear;
    end;
  end;
end;

procedure TMainForm.FTPSent(aSocket: TLSocket; const Bytes: Integer);
var
  n: Integer;
begin
  if Bytes > 0 then begin
    Inc(FDLDone, Bytes);
    if MemoText.Lines.Count > 0 then begin
      n := Integer(Round(FDLDone / FDLSize * 100));
      if n <> FLastN then begin
        ProgressBar1.Position := n;
        FLastN := n;
      end;
    end;
  end else
    DoList('');
end;

procedure TMainForm.FormClose(Sender: TObject; var CloseAction: TCloseAction);
begin
  MemoText.Lines.SaveToFile('log.txt');
  FList.Free;
  FreeAndNil(FFile);
end;

procedure TMainForm.AboutMenuItemClick(Sender: TObject);
begin
  MessageDlg('lFTP test program copyright (c) 2005-2008 by Ales Katona and Jesus Reyes. All rights deserved :)',
             mtInformation, [mbOK], 0);
end;

procedure TMainForm.FTPFailure(aSocket: TLSocket; const aStatus: TLFTPStatus);
begin
    // TODO: check status of other commands here properly!
end;

procedure TMainForm.FTPSuccess(aSocket: TLSocket; const aStatus: TLFTPStatus);
var
  i: Integer;
  s: string = '';
begin
  case aStatus of
    fsFeat : FormFeatures.ListBoxFeatures.Items.Assign(FTP.FeatureList);
    fsList : FTP.PresentWorkingDirectory;
    fsPWD  : begin
               Site.Path := FTP.PresentWorkingDirectoryString;
               SBar.Panels[3].Text := site.path;
               TFrmSites.SaveOption('site' + IntToStr(Site.Number), 'path', site.path);
             end;
    // TODO: check status of other commands here properly!
  end;
end;

procedure TMainForm.MenuItemFeaturesClick(Sender: TObject);
begin
  FormFeatures.Show;
end;

procedure TMainForm.MenuItemMkdirClick(Sender: TObject);
var
  s: string = '';
begin
  if InputQuery('New directory', 'Please specify directory name', s) then
    if FTP.MakeDirectory(s) then
      DoList('');
end;

procedure TMainForm.accConnectExecute(Sender: TObject);
begin
  if Length(Site.Host) > 0 then begin
    FTP.Connect(Site.Host, Word(StrToInt(Site.Port)));
    ToolButton4.Down := True;
  end else
    MessageDlg('Please add some sites to the site manager', mtInformation, [mbOK], 0);
end;

procedure TMainForm.accDisconnectExecute(Sender: TObject);
begin
  //Disconnect(true);
  Disconnect(false);
end;

procedure TMainForm.accSiteManagerExecute(Sender: TObject);
var
  F: TFrmSites;
  Res: TModalResult;
begin
  F := TFrmSites.Create(Self);
  try
    Res := F.ShowModal;
    if Res<>mrCancel then begin
      UpdateSite;
      if Res=mrYes then
        accConnectExecute(Self);
    end;
  finally
    F.Free;
  end;
end;

procedure TMainForm.DeletePopupClick(Sender: TObject);
begin
  if CurrentIsDirectory then
    FTP.RemoveDirectory(CurrentName)
  else
    FTP.DeleteFile(CurrentName);
  DoList('');
end;

procedure TMainForm.FormCreate(Sender: TObject);
begin
  FDLSize := 1;
  FList := TStringList.Create;
  FFile := nil;
  FIcons := TStringList.Create;

  RegisterExt('ftp_dirup',  '0'{dummy}, true);
  RegisterExt('ftp_dir',    '1'{dummy}, true);
  RegisterExt('ftp_link',   '2'{dummy}, true);
  RegisterExt('ftp_file',   '3'{dummy}, true);
  RegisterExt('ftp_error',  '4'{dummy}, true);

  // register additional icons
  RegisterExt('ftp_archive','.zip,.gz,.rar,.tar,.bz2', false);


  LeftView.Mask := '*';
  CreateFilePath := '';
  TFrmSites.LoadLastSite;
  UpdateSite;
  
  if (Site.Site='') or (Site.ldir='') then
    SetLocalDirectory(ExtractFilePath(ParamStr(0)));
  

  // custom draw file listbox
  LeftView.Style:=lbOwnerDrawFixed;
  LeftView.OnDrawItem:=@LeftViewDrawItem;
end;

procedure TMainForm.FormKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  case Key of
    VK_F5: begin end;
  end;
end;

procedure TMainForm.IPEditKeyPress(Sender: TObject; var Key: char);
begin
  if Byte(Key) in [VK_EXECUTE, VK_RETURN] then
    accConnectExecute(Sender);
end;

procedure TMainForm.LDeletePopupClick(Sender: TObject);
begin
  if FileExists(LeftView.Directory + LeftView.Items[LeftView.ItemIndex]) then begin
    DeleteFile(LeftView.Directory + LeftView.Items[LeftView.ItemIndex]);
    LeftView.UpdateFileList;
  end;
end;

procedure TMainForm.LInfoPopupClick(Sender: TObject);
var
  f: file;
begin
  if FileExists(LeftView.Directory + LeftView.Items[LeftView.ItemIndex]) then begin
    AssignFile(f, LeftView.Directory + LeftView.Items[LeftView.ItemIndex]);
    Reset(f, 1);
    ShowMessage('FileSize: ' + IntToStr(FileSize(f)) + ' bytes');
    CloseFile(f);
  end;
end;

procedure TMainForm.LRenamePopupClick(Sender: TObject);
var
  aName: string = '';
begin
  if FileExists(LeftView.Directory + LeftView.Items[LeftView.ItemIndex]) then begin
    if InputQuery('New Name', 'Please type in new filename', False, aName) then
      RenameFile(LeftView.Directory + LeftView.Items[LeftView.ItemIndex],
                 LeftView.Directory + aName);
      LeftView.UpdateFileList;
  end;
end;

procedure TMainForm.LeftViewDblClick(Sender: TObject);
var
  s: string;
  FF: TFileStream;
begin
  s := LeftView.FileName;

  if DirectoryExists(s) then begin
    SetLocalDirectory(s);
    if (Site.LDir<>S) and (Site.Number>0) then
      TFrmSites.SaveOption('site'+IntToStr(Site.Number),'ldir',s);
    Site.ldir:=S;
  end else if FTP.Connected then begin
    s := StringReplace(LeftView.FileName, PathDelim + PathDelim,
                       PathDelim, [rfReplaceAll]);
    FDLDone := 0;
    FF := TFileStream.Create(s, fmOpenRead);
    FDLSize := FF.Size;
    FF.Free;
    FLastN := 0;
    FTP.Put(s);
  end;
end;

procedure TMainForm.ListPopupClick(Sender: TObject);
begin
  DoList(CurrentName);
end;

procedure TMainForm.RenamePopupClick(Sender: TObject);
var
  aName: string = '';
begin
  if InputQuery('New Name', 'Please type in new filename', False, aName) then begin
    FTP.Rename(CurrentName, aName);
    DoList('');
  end;
end;

procedure TMainForm.rmtGridCompareCells(Sender: TObject; Acol, ARow, Bcol,
  BRow: Integer; var Result: integer);
var
  A,B: Integer;
begin
  A := TTaggedPicture(rmtGrid.Objects[0, ARow]).Tag;
  B := TTaggedPicture(rmtGrid.Objects[0, BRow]).Tag;
  if aCol = 0 then begin
    // by specific type and name
    ACol:=1;
    BCol:=1;
  end else
  if aCol = 1 then begin
    // by generic type and name
    if A>siError then A := siFile;
    if B>siError then B := siFile;
  end;

  result := A-B;
  if result=0 then
    result := CompareText(rmtGrid.Cells[ACol,ARow],rmtGrid.Cells[ACol,BRow]);
end;

procedure TMainForm.rmtGridDblClick(Sender: TObject);
var
  item: string;
  P: TPoint;
begin
  if not FTP.Connected then
    exit;

  P := rmtGrid.ScreenToClient(Mouse.CursorPos);
  if rmtGrid.MouseToGridZone(P.x, P.y) <> gzNormal then
    exit;

  item := CurrentName;
  if CurrentIsDirectory then
    ChangeDirectory(Item)
  else if CurrentIsLink then
    ChangeDirectory(CurrentNameLink)
  else
  if not CurrentIsError then begin
    FDLSize := CurrentSize;
    FDLDone := 0;
    if FDLSize = 0 then
      FDLSize := 1;
    FreeAndNil(FFile);
    CreateFilePath := IncludeTrailingPathDelimiter(LeftView.Directory) + item;
    FTP.Retrieve(item)
  end;
end;

procedure TMainForm.rmtGridDrawCell(Sender: TObject; Col, Row: Integer;
  aRect: TRect; aState: TGridDrawState);
var
  Pic: TPicture;
begin
  if (Row > 0)and(Col = 0) then begin
    Pic := TPicture(rmtGrid.Objects[2, Row]);
    if Pic=nil then
      Pic := TPicture(rmtGrid.Objects[0, Row]);

    rmtGrid.Canvas.Draw(aRect.Left + 2,aRect.Top + 2, Pic.Graphic);
  end else
  if Col<>5 then
    rmtGrid.DefaultDrawCell(Col,Row,aRect,aState);
end;

procedure TMainForm.rmtGridHeaderClick(Sender: TObject; IsColumn: Boolean;
  Index: Integer);
begin
  if Index in [0,1] then
    rmtGrid.SortColRow(true, Index, 1, rmtGrid.RowCount-1);
end;

procedure TMainForm.rmtGridKeyDown(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if key=VK_BACK then begin
    ChangeDirectory('..');
    key := 0;
  end else
  if Key=VK_RETURN then begin
    rmtGridDblClick(Sender);
    Key := 0;
  end;
end;

procedure TMainForm.DoList(const FileName: string);
begin
  FDirListing := '';
  FTP.List(FileName);
end;

procedure TMainForm.UpdateSite;
begin
  if Site.Site='' then
    SBar.Panels[0].Text := '<see sites manager>'
  else begin
    SBar.Panels[0].Text := Site.site;
    if DirectoryExists(Site.ldir) then
      SetLocalDirectory(Site.ldir);
  end;
  SBar.Panels[1].Text := Site.user;
  if Site.Host <> '' then
    SBar.Panels[2].Text := Site.Host+GetSitePath
  else
    SBar.Panels[2].Text := '';
end;

function TMainForm.CurrentName: string;
begin
  result := rmtGrid.Cells[1, rmtGrid.Row];
end;

function TMainForm.CurrentNameLink: string;
begin
  result := rmtGrid.Cells[5, rmtGrid.Row];
end;

function TMainForm.CurrentSize: Int64;
begin
  result := StrToInt64Def(rmtGrid.Cells[2, rmtGrid.Row], 0);
end;

function TMainForm.CurrentIsDirectory: boolean;
begin
  result :=
    (rmtGrid.Objects[0, rmtGrid.Row] = FSpecialIcons[siDirUp]) or
    (rmtGrid.Objects[0, rmtGrid.Row] = FSpecialIcons[siDir]);
end;

function TMainForm.CurrentIsLink: boolean;
begin
  result :=
    (rmtGrid.Objects[0, rmtGrid.Row] = FSpecialIcons[siLink]);
end;

function TMainForm.CurrentIsError: boolean;
begin
  result :=
    (rmtGrid.Objects[0, rmtGrid.Row] = FSpecialIcons[siError]);
end;

function TMainForm.GetFileIcon(aName: string): TObject;
var
  i: Integer;
begin
  i := FIcons.IndexOf(lowercase(ExtractFileExt(aName)));
  if i>=0 then
    result := FICons.Objects[i]
  else
    result := nil;
end;

procedure TMainForm.ChangeDirectory(aDir: string);
begin
  // todo: implement refresh
  // todo: implement quick parent director
  //  WriteLn('Changing directory to ',aDir);
  if aDir='..' then begin
    FTP.ChangeDirectory(aDir);
    DoList('');
  end else begin
    FTP.ChangeDirectory(aDir);
    DoList('');
  end;
end;

procedure TMainForm.Disconnect(ClearLog: boolean);
begin
  FTP.Disconnect;
  rmtGrid.RowCount := 1;
  if ClearLog then
    MemoText.Clear;
end;

procedure TMainForm.LeftViewDrawItem(sender: TWinControl; Index: Integer;
  ARect: TRect; State: TOwnerDrawState);
var
  ts: TTextStyle;
  Pic: TPicture;
begin
  if Index>=0 then begin
    LeftView.Canvas.FillRect(ARect);
    //
    ts := LeftView.canvas.TextStyle;
    ts.Layout:=tlCenter;
    LeftView.Canvas.TextStyle := ts;
    //
    Pic := TPicture(LeftView.Items.Objects[Index]);
    if Pic=nil then
      Pic := FSpecialIcons[siFile];

    LeftView.Canvas.Draw(aRect.Left + 2,aRect.Top + 2, Pic.Graphic);
    ARect.Left := ARect.left + Pic.Graphic.Width + 4;

    LeftView.Canvas.TextRect(ARect,ARect.Left,ARect.Top,LeftView.Items[Index]);
  end;
end;


function CompareFileDir(List: TStringList; Index1, Index2: Integer): Integer;
var
  Tag1,Tag2: Integer;
begin
  Tag1 := TTaggedPicture(List.Objects[Index1]).Tag;
  if Tag1>siError then
    Tag1:=siFile;

  Tag2 := TTaggedPicture(List.Objects[Index2]).Tag;
  if Tag2>siError then
    Tag2:=siFile;

  result := Tag1-Tag2;
  if result=0 then
    result := CompareText(List[Index1],List[Index2]);
end;

procedure TMainForm.SetLocalDirectory(const ADir: string);
var
  i,j: Integer;
  L: TStringList;
  Obj: TObject;
begin

  LeftView.Items.BeginUpdate;
  LeftView.Directory := IncludeTrailingPathDelimiter(ADir);

  // the default sorting doesn't work very well under linux
  // do it "by hand"
  L:=TStringList.Create;
  j:=-1;
  for i:=0 to LeftView.Items.Count-1 do
  with leftView do begin

    if (Items[i]='[.]') then
      continue;
    if (Items[i]='[..]') then begin
      j:=i;
      continue;
    end;

    if (LeftStr(Items[i],1)='[') and
       (Rightstr(Items[i],1)=']') then
      Obj := FSpecialICons[siDir]
    else begin
      Obj := GetFileIcon(Items[i]);
      if Obj=nil then
        Obj := FSpecialIcons[siFile];
    end;

    L.AddObject(Items[i], Obj);
  end;

  L.CustomSort(@CompareFileDir);

  if (j>=0) or
    ((L.Count=0) and DirectoryExists(GetParentDirectory(ADir))) then
    L.InsertObject(0, '[..]', FSpecialIcons[siDirUp]);

  LeftView.Items.Assign(L);

  L.Free;
  LeftView.Items.EndUpdate;
end;

procedure TMainForm.RegisterExt(const LazResName,FileExt: string; special: boolean);
var
  i: Longint;
  Pic: TTaggedPicture;
  Lst: TStringList;
begin
  if LazarusResources.Find(LazResName)=nil then
    exit;

  if special then begin
    // special icons (they had a fixed index)
    Pic := TTaggedPicture.Create;
    Pic.LoadFromLazarusResource(LazResName);
    Pic.Tag:=Length(FSpecialIcons);
    SetLength(FSpecialIcons, Pic.Tag + 1);
    FSpecialIcons[Pic.Tag] := Pic;
  end else begin
    // extension based icons
    Lst := TStringList.Create;
    Lst.CommaText := lowercase(FileExt);
    try
      Pic := nil;
      for i:=0 to Lst.Count-1 do begin
        if FIcons.IndexOf(Lst[i])<0 then begin
          if Pic=nil then begin
            Pic:=TTaggedPicture.Create;
            Pic.LoadFromLazarusResource(LazResName);
            Pic.Tag:=Length(FSpecialIcons) + FIcons.Count;
          end;
          FIcons.AddObject(Lst[i], Pic);
        end;
      end;
    finally
      Lst.Free;
    end;
 end;
end;

initialization
  {$I main.lrs}
  {$I icons.lrs}
  //add additional icons in a new file iconsextra.lrs
  //register an extensiona and icon with function RegisterExt()
  {$I iconsextra.lrs}

end.

