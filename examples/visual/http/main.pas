unit main; 

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, LResources, Forms, Controls, Graphics, Dialogs, lNet, lHTTP,
  lNetComponents, ExtCtrls, StdCtrls, Buttons, Menus, lHTTPUtil;
  
type

  { TMainForm }

  TMainForm = class(TForm)
    ButtonSendRequest: TButton;
    CheckBoxPOST: TCheckBox;
    EditPOST: TEdit;
    EditURL: TEdit;
    HTTPClient: TLHTTPClientComponent;
    LabelPOST: TLabel;
    LabelURI: TLabel;
    SSL: TLSSLSessionComponent;
    MainMenu1: TMainMenu;
    MemoHTML: TMemo;
    MemoStatus: TMemo;
    MenuItemExit: TMenuItem;
    MenuItemAbout: TMenuItem;
    MenuItemHelp: TMenuItem;
    MenuItemFile: TMenuItem;
    MenuPanel: TPanel;
    PanelSep: TPanel;
    procedure ButtonSendRequestClick(Sender: TObject);
    procedure EditPOSTChange(Sender: TObject);
    procedure EditURLKeyPress(Sender: TObject; var Key: char);
    procedure HTTPClientCanWrite(ASocket: TLHTTPClientSocket;
      var OutputEof: TWriteBlockStatus);
    procedure HTTPClientDisconnect(aSocket: TLSocket);
    procedure HTTPClientDoneInput(ASocket: TLHTTPClientSocket);
    procedure HTTPClientError(const msg: string; aSocket: TLSocket);
    function HTTPClientInput(ASocket: TLHTTPClientSocket; ABuffer: pchar;
      ASize: dword): dword;
    procedure HTTPClientProcessHeaders(ASocket: TLHTTPClientSocket);
    procedure MenuItemAboutClick(Sender: TObject);
    procedure MenuItemExitClick(Sender: TObject);
    procedure SSLSSLConnect(aSocket: TLSocket);
  private
    HTTPBuffer: string;
    POSTBuffer: string;
    procedure AppendToMemo(aMemo: TMemo; const aText: string);
    { private declarations }
  public
    { public declarations }
  end; 

var
  MainForm: TMainForm;

implementation

{ TMainForm }

procedure TMainForm.HTTPClientError(const msg: string; aSocket: TLSocket);
begin
  MessageDlg(msg, mtError, [mbOK], 0);
end;

procedure TMainForm.HTTPClientDisconnect(aSocket: TLSocket);
begin
  AppendToMemo(MemoStatus, 'Disconnected.');
end;

procedure TMainForm.ButtonSendRequestClick(Sender: TObject);
var
  URL, aHost, aURI: string;
  aPort: Word;
begin
  HTTPClient.Method := hmGet;
  if CheckBoxPOST.Checked then begin
    POSTBuffer := URLEncode(EditPOST.Text, True); // url-encode the string for in query usage
    HTTPClient.Method := hmPost; // obviously
    HTTPClient.ContentLength := Length(POSTBuffer); // specify POST data size
    HTTPClient.AddExtraHeader('Content-Type: application/x-www-form-urlencoded'); // specify POST data content-type, usual urlencoded this time
  end;

  URL := EditURL.Text;
  if Pos('http', URL) <= 0 then // HTTP[S] is required
    URL := 'http://' + URL;

  HTTPBuffer := '';
  SSL.SSLActive := DecomposeURL(URL, aHost, aURI, aPort);
  HTTPClient.Host := aHost;
  HTTPClient.URI  := aURI;
  HTTPClient.Port := aPort;

  HTTPClient.SendRequest;
end;

procedure TMainForm.EditPOSTChange(Sender: TObject);
begin
  CheckBoxPOST.Checked := True;
end;

procedure TMainForm.EditURLKeyPress(Sender: TObject; var Key: char);
begin
  if Key = #13 then
    ButtonSendRequestClick(Sender);
end;

procedure TMainForm.HTTPClientCanWrite(ASocket: TLHTTPClientSocket;
  var OutputEof: TWriteBlockStatus);
var
  n: Integer;
begin
  if (HTTPClient.Method <> hmPost)
  or (Length(POSTBuffer) = 0) then
    Exit; // nothing to be done

  n := aSocket.SendMessage(POSTBuffer); // try to send the POST data

  if n = Length(POSTBuffer) then begin // if we've sent it all, mark finished
    OutputEof := wsDone;
    POSTBuffer := ''; // for clarity
  end else begin
    OutputEof := wsPendingData; // we've still got pending data
    Delete(POSTBuffer, 1, n); // make sure to "remove sent" from the "buffer"
  end;
end;

procedure TMainForm.HTTPClientDoneInput(ASocket: TLHTTPClientSocket);
begin
  aSocket.Disconnect;
  AppendToMemo(MemoStatus, 'Finished.');
end;

function TMainForm.HTTPClientInput(ASocket: TLHTTPClientSocket; ABuffer: pchar;
  ASize: dword): dword;
var
  oldLength: dword;
begin
  oldLength := Length(HTTPBuffer);
  setlength(HTTPBuffer,oldLength + ASize);
  move(ABuffer^,HTTPBuffer[oldLength + 1], ASize);
  MemoHTML.Text := UTF8Encode(HTTPBuffer);
  MemoHTML.SelStart := Length(HTTPBuffer);
  AppendToMemo(MemoStatus, IntToStr(ASize) + '...');
  Result := aSize; // tell the http buffer we read it all
end;

procedure TMainForm.HTTPClientProcessHeaders(ASocket: TLHTTPClientSocket);
begin
  AppendToMemo(MemoStatus, 'Response: ' + IntToStr(HTTPStatusCodes[ASocket.ResponseStatus]) +
                    ' ' + ASocket.ResponseReason + ', data...');
end;

procedure TMainForm.MenuItemAboutClick(Sender: TObject);
begin
  MessageDlg('Copyright (c) 2006-2008 by Ales Katona and Micha Nelissen. All rights deserved :)',
             mtInformation, [mbOK], 0);
end;

procedure TMainForm.MenuItemExitClick(Sender: TObject);
begin
  Close;
end;

procedure TMainForm.SSLSSLConnect(aSocket: TLSocket);
begin
  MemoStatus.Append('TLS handshake successful');
end;

procedure TMainForm.AppendToMemo(aMemo: TMemo; const aText: string);
begin
  aMemo.Append(aText);
  aMemo.SelStart := Length(aMemo.Text);
end;

initialization
  {$I main.lrs}

end.

