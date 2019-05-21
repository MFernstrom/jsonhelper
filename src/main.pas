unit main;

{
  Version         0.3.4
  Author          Marcus Fernstrom
  Copyright       Marcus Fernstrom, 2018
  License         GPLv3
  GitHub          https://github.com/MFernstrom/jsonhelper
}

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, StdCtrls, ExtCtrls,
  jsonparser, fpjson, LCLType, Clipbrd, LResources, SynEdit,
  SynHighlighterJScript, SynFacilHighlighter;

type

  { Tjsonhelperform }

  Tjsonhelperform = class(TForm)
    ClearButton: TButton;
    SearchInput: TEdit;
    FontComboBox: TComboBox;
    HideButton: TButton;
    FontLabel: TLabel;
    QuitButton: TButton;
    InvalidLabel: TLabel;
    StatusLabel: TLabel;
    JsonInputMemo: TMemo;
    Splitter1: TSplitter;
    JSONSynEdit: TSynEdit;
    TrayIcon1: TTrayIcon;
    procedure ClearButtonClick(Sender: TObject);
    procedure SearchInputKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure FontComboBoxChange(Sender: TObject);
    procedure FormCreate(Sender: TObject);
    procedure HideButtonClick(Sender: TObject);
    procedure JSONSynEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure QuitButtonClick(Sender: TObject);
    procedure JsonInputMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
    procedure TrayIcon1Click(Sender: TObject);
  private

  public

  end;

var
  jsonhelperform: Tjsonhelperform;
  jData : TJSONData;
  jsonHighlighter : TSynFacilSyn;

implementation

{$R *.lfm}

{ Tjsonhelperform }

procedure Tjsonhelperform.ClearButtonClick(Sender: TObject);
begin
  JsonInputMemo.Clear;
  JSONSynEdit.Clear;
end;

procedure Tjsonhelperform.SearchInputKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if Key <> VK_RETURN then begin
    JSONSynEdit.CaretX:=0;
    JSONSynEdit.CaretY:=0;
  end;
  JSONSynEdit.SearchReplace(SearchInput.Text, '', []);
end;

procedure Tjsonhelperform.FontComboBoxChange(Sender: TObject);
var
  fsize: Integer;
begin
  fsize := StrtoInt(FontComboBox.Items[FontComboBox.ItemIndex]);
  JsonInputMemo.font.Size := fsize;
  JSONSynEdit.font.Size := fsize;
end;

procedure Tjsonhelperform.FormCreate(Sender: TObject);
begin
  TrayIcon1.Icon.LoadFromLazarusResource('brackets');
  jsonHighlighter := TSynFacilSyn.Create(self);
  JSONSynEdit.Highlighter := jsonHighlighter;
  jsonHighlighter.LoadFromFile('C:\Users\Marcus\Documents\jshl.xml');
end;

procedure Tjsonhelperform.HideButtonClick(Sender: TObject);
begin
  jsonhelperform.visible := false;
  jsonhelperform.Hide;
end;

procedure Tjsonhelperform.JSONSynEditKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
begin
  if ((Shift = [ssMeta]) or (Shift = [ssCtrl])) and (Key = VK_C) then begin
    if JSONSynEdit.SelAvail then begin
      Clipboard.AsText := JSONSynEdit.SelText
    end else begin
      Clipboard.AsText := JSONSynEdit.Text;
    end;
  end;
end;

procedure Tjsonhelperform.QuitButtonClick(Sender: TObject);
begin
  Halt;
end;

procedure Tjsonhelperform.JsonInputMemoKeyUp(Sender: TObject; var Key: Word; Shift: TShiftState);
var
  temp: String;
begin
  // Keyup for left-hand memo
  if (Shift = [ssMeta]) and (Key = VK_C) then begin
    if JsonInputMemo.SelLength > 0 then
      Clipboard.AsText := JsonInputMemo.SelText
    else
      Clipboard.AsText := JsonInputMemo.Text;
  end else begin
    try
      InvalidLabel.Visible:=false;
      StatusLabel.Visible:=false;

      JSONSynEdit.Clear;

      temp := Trim(JsonInputMemo.Text);

      jData := GetJSON(temp);

      if length(temp) > 0 then
        JSONSynEdit.Text := jData.FormatJSON;

    except
      on E: Exception do begin
        InvalidLabel.Visible:=true;
        StatusLabel.Caption:=E.Message;
        StatusLabel.Visible:=true;
      end;
    end;
  end;
end;

procedure Tjsonhelperform.TrayIcon1Click(Sender: TObject);
begin
  jsonhelperform.visible := true;
  jsonhelperform.Show;
end;

initialization
  {$I ico.lrs}

end.
