unit uPanel2XML;

interface

uses
  Vcl.Controls, Vcl.ExtCtrls, Vcl.StdCtrls, Vcl.Samples.Spin,
  Generics.Collections, Generics.Defaults,
  Classes, Xml.XMLDoc, Xml.XMLIntf;

type
  TPanel2Xml = class
    private
      FFileName: string;
      FPanel: TPanel;
      FDictMarkers: TDictionary<Integer, string>;

      function GetValueAsString(AControl: TControl): string;
      function AddNodeFromComponent(const Parent: IXMLNode; AControl: TControl): IXMLNode;
    public
      property FileName: string read FFileName write FFileName;
      property PanelComponent: TPanel read FPanel;
//      property DictMarkers read FDictMarkers write FDictMarkers;

      procedure AssignPanel(APanel: TPanel);

      function SaveXml: Boolean;

      destructor Destroy; override;
      constructor Create(APanel: TPanel; ADictMarkers: TDictionary<Integer, string>; AFileName: string=''); overload;
  end;

implementation

uses
  SysUtils;

{ TPanel2XmlManager }

function TPanel2Xml.AddNodeFromComponent(const Parent: IXMLNode; AControl: TControl): IXMLNode;
var
  Ctrl: TControl;
begin
  Result := Parent;
  // TPanel is treated as new section -> recursive node creation
  if AControl is TPanel then begin
    Result := Parent.AddChild(FDictMarkers[AControl.Tag]);
    for var I := 0 to FPanel.ControlCount - 1 do begin
      Ctrl := FPanel.Controls[I];
      AddNodeFromComponent(Result, Ctrl);
    end;
  end else
  // save values of supported components
  if (AControl is TEdit) or
     (AControl is TSpinEdit) then begin
    Result.AddChild(FDictMarkers[AControl.Tag]).Text := GetValueAsString(AControl);
  end;
end;

function TPanel2Xml.GetValueAsString(AControl: TControl): string;
begin
  if AControl is TEdit then begin
    Result := TEdit(AControl).Text;
  end else
  if AControl is TSpinEdit then begin
    Result := FloatToStr(TSpinEdit(AControl).Value);
  end;
end;

procedure TPanel2Xml.AssignPanel(APanel: TPanel);
begin
  FPanel := APanel;
end;

constructor TPanel2Xml.Create(APanel: TPanel; ADictMarkers: TDictionary<Integer, string>; AFileName: string='');
begin
  inherited Create;

  FFileName := AFileName;
  if FFileName = '' then FFileName := 'panel.xml';

  FPanel := APanel;
  FDictMarkers := ADictMarkers;
end;

destructor TPanel2Xml.Destroy;
begin
  inherited;
end;

function TPanel2Xml.SaveXml: Boolean;
var
  Doc: IXMLDocument;
  App, Settings, Presets, Stats, Node, pnlNode: IXMLNode;
  Ctrl: TControl;
  List: TList<TControl>;
begin
  Result := True;
  try
    try
      Doc := NewXMLDocument;
      Doc.Encoding := 'utf-8';
      Doc.Options := [doNodeAutoIndent];

      pnlNode := Doc.AddChild(FDictMarkers[FPanel.Tag]);
//      for var I := 0 to FPanel.ControlCount - 1 do begin
//        Ctrl := FPanel.Controls[I];
//        if Ctrl is TLabel then Continue;
//
//        Node := AddNodeFromComponent(pnlNode, Ctrl);
//      end;


      List := TList<TControl>.Create;
      try
        for var I := 0 to FPanel.ControlCount - 1 do
          List.Add(FPanel.Controls[I]);

        List.Sort(
          TComparer<TControl>.Construct(
            function(const A, B: TControl): Integer
            begin
              Result := TWinControl(A).TabOrder - TWinControl(B).TabOrder;
            end
          )
        );

        for var CtrlTmp in List do begin
//          if Ctrl is TLabel then Continue;

          Node := AddNodeFromComponent(pnlNode, CtrlTmp);
        end;

      finally
        List.Free;
      end;

      Doc.SaveToFile(FFileName);

    except
      Result := False;
    end;
  finally

  end;
end;

end.
