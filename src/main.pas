unit main;

{
  Version         0.1
  Author          Marcus Fernstrom
  Copyright       Marcus Fernstrom, 2018
  License         GPLv3
  GitHub          https://github.com/MFernstrom/jsonhelper
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  jsonparser, fpjson, LCLType, Clipbrd;

type

  { Tjsonhelperform }

  Tjsonhelperform = class(TForm)
    ClearButton: TButton;
    HideButton: TButton;
    QuitButton: TButton;
    InvalidLabel: TLabel;
    StatusLabel: TLabel;
    JsonInputMemo: TMemo;
    JsonOutputMemo: TMemo;
    Splitter1: TSplitter;
    TrayIcon1: TTrayIcon;
    procedure ClearButtonClick(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HideButtonClick(Sender: TObject);
    procedure QuitButtonClick(Sender: TObject);
    procedure JsonInputMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure JsonOutputMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrayIcon1Click(Sender: TObject);
  private

  public

  end;

var
  jsonhelperform: Tjsonhelperform;
  jData : TJSONData;

implementation

{$R *.lfm}

{ Tjsonhelperform }

procedure Tjsonhelperform.ClearButtonClick(Sender: TObject);
begin
  JsonInputMemo.Clear;
  JsonOutputMemo.Clear;
end;

procedure Tjsonhelperform.FormCreate(Sender: TObject);
var
  path: String;
begin
  path := Application.Params[0];
  path := StringReplace(path, 'MacOS/jsonhelper', 'Resources/brackets.ico', []);
  TrayIcon1.Icon.LoadFromFile(path);
end;

procedure Tjsonhelperform.HideButtonClick(Sender: TObject);
begin
  jsonhelperform.visible := false;
  jsonhelperform.Hide;
end;

procedure Tjsonhelperform.QuitButtonClick(Sender: TObject);
begin
  Halt;
end;

procedure Tjsonhelperform.JsonInputMemoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  // Keyup for left-hand memo
  try
    InvalidLabel.Visible:=false;
    StatusLabel.Visible:=false;

    JsonOutputMemo.Text:='';

    jData := GetJSON(JsonInputMemo.Text);
    JsonOutputMemo.Text:=jData.FormatJSON;
  except
    on E: Exception do begin
      InvalidLabel.Visible:=true;
      StatusLabel.Caption:=E.Message;
      StatusLabel.Visible:=true;
    end;
  end;
end;

procedure Tjsonhelperform.JsonOutputMemoKeyUp(Sender: TObject; var Key: Word;
  Shift: TShiftState);
begin
  if (Shift = [ssMeta]) and (Key = VK_C) then begin
    if JsonOutputMemo.SelLength > 0 then
      Clipboard.AsText := JsonOutputMemo.SelText
    else
      Clipboard.AsText := JsonOutputMemo.Text;
  end;
end;

procedure Tjsonhelperform.TrayIcon1Click(Sender: TObject);
begin
  jsonhelperform.visible := true;
  jsonhelperform.Show;
end;

end.

