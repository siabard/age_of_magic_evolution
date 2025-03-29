unit xml_reader;
{$mode objfpc}

{

var
  tilemaps: RTilemap;
  tileset: RTileset;
  layer: RLayer;
begin
  tilemaps := ParseTilemap('./resources/test.xml');

  WriteLn('Tilesets ');
  for tileset in tilemaps.FTilesets do
  begin
    DebugTileset(tileset);
  end;

  for layer in tilemaps.FLayers do
  begin
    DebugLayer(layer);
  end;
  tilemaps.FLayers.Free;
  tilemaps.FTilesets.Free;
end.

}

interface

uses
  SysUtils,
  strutils,
  Classes,
  types,
  DOM,
  XMLRead,
  Generics.Collections;

type
  RTileset = record
    image_source: string;
    image_path: string;
    firstgid: integer;
    tilesetname: string;
    tilewidth: integer;
    tileheight: integer;
    columns: integer;
    rows: integer;
  end;

  RLayer = record
    order: integer;
    layer_name: string;
    Data: array of integer;
  end;

  RTilemap = record
    FWidth: integer;
    FHeight: integer;
    FTilesets: specialize TList<RTileset>;
    FLayers: specialize TList<RLayer>;
  end;

procedure DebugTileset(ATileset: RTileset);
function FindAttributeValue(ANode: TDOMNode; AName: string): string;
function FindAttribute(ANode: TDOMNode; AName: string): boolean;
function FindChildNodeAttributeValue(ANode: TDOMNode; AName: string;
  AAttributeName: string): string;
function SplitString(const S, Delimiter: unicodestring): TStringList;
function AnalyzeLayer(IOrder: integer; LayerNode: TDOMNode): RLayer;
procedure DebugLayer(ALayer: RLayer);
function ParseTileset(APath: string): RTileset;
function ParseTilemap(APath: string): RTilemap;
function getTilesetIndex(Tilesets: specialize TList<RTileset>; gid: Integer): integer;

implementation

procedure DebugTileset(ATileset: RTileset);
begin
  WriteLn(' Tileset name : ', ATileset.tilesetname);
  WriteLn(' Tileset Image Source : ', ATileset.image_source);
  WriteLn(' Tileset first gid : ', ATileset.firstgid);
  WriteLn(Format(' Tileset width %4d height %4d',
    [ATileset.tilewidth, Atileset.tileheight]));
end;

{ 해당 Node의 특정 속성값을 반환한다. }
function FindAttributeValue(ANode: TDOMNode; AName: string): string;
var
  i: integer;
begin
  Result := '';
  for I := 0 to ANode.Attributes.Length - 1 do
  begin
    if ANode.Attributes.Item[I].NodeName = unicodestring(AName) then
    begin
      Result := Format('%s', [ANode.Attributes.Item[I].NodeValue]);
      Break;
    end;

  end;
end;

{ 해당 Node 하위 Node 의 특정 속성값이 있는지 반환한다. }
function FindAttribute(ANode: TDOMNode; AName: string): boolean;
var
  i: integer;
begin
  Result := False;
  for I := 0 to ANode.Attributes.Length - 1 do
  begin
    if ANode.Attributes.Item[I].NodeName = unicodestring(AName) then
    begin

      Result := True;
      Break;
    end;

  end;
end;


{ 해당 Node 하위 Node 의 특정 속성값을 반환한다. }
function FindChildNodeAttributeValue(ANode: TDOMNode; AName: string;
  AAttributeName: string): string;
var
  i: integer;
begin
  Result := '';

  for I := 0 to ANode.ChildNodes.Count - 1 do
  begin
    if ANode.ChildNodes.Item[I].NodeName = unicodestring(AName) then
    begin
      Result := FindAttributeValue(ANode.ChildNodes.Item[I], AAttributeName);
      break;
    end;
  end;
end;

{ Tileset 정보를 분석하여 RTileset 을 반환한다. }
function AnalyzeTileset(TilesetNode: TDOMNode): RTileset;
var
  image_source: string;
  firstgid: integer;
  code: integer;
  image_width: integer;
  image_height: integer;
begin
  // image_source
  image_source := FindChildNodeAttributeValue(TilesetNode, 'image', 'source');
  Result.image_source := image_source;

  // tilewidth & tileheight
  Val(FindAttributeValue(TilesetNode, 'tilewidth'), Result.tilewidth, code);
  Val(FindAttributeValue(TilesetNode, 'tileheight'), Result.tileheight, code);
  Val(FindAttributeValue(TilesetNode, 'columns'), Result.columns, code);

  /// image_width, columns
  Val(FindChildNodeAttributeValue(TilesetNode, 'image', 'width'), image_width, code);
  if code = 0 then
  begin
    Result.columns := image_width div Result.tilewidth;
  end
  else
  begin
    Result.columns := 0;
  end;

  /// image_height, rows
  Val(FindChildNodeAttributeValue(TilesetNode, 'image', 'height'), image_height, code);
  if code = 0 then
  begin
    Result.rows := image_height div Result.tileheight;
  end
  else
  begin
    Result.rows := 0;
  end;

  // tilename
  Result.tilesetname := FindAttributeValue(TilesetNode, 'name');

  // firstgid
  Val(FindAttributeValue(TilesetNode, 'firstgid'), firstgid, code);
  if code = 0 then
    Result.firstgid := firstgid
  else
    Result.firstgid := -1;

end;

{ 문자열을 구분문자로 나누기 }
function SplitString(const S, Delimiter: unicodestring): TStringList;
begin
  Result := TStringList.Create;
  Result.StrictDelimiter := True;
  Result.Delimiter := Delimiter[1]; // 단일 문자 Delimiter만 가능
  Result.DelimitedText := ansistring(S);
end;

{ layer 정보를 분석하여 RLayer 을 반환한다. }
function AnalyzeLayer(IOrder: integer; LayerNode: TDOMNode): RLayer;
var
  DataNode: TDomNode;
  MapData: unicodestring;
  MapCells: TStringList;
  I: integer;
  Cell: integer;
  Code: integer;
  tmpData: array of integer;
begin
  // layer 노드는 반드시 하나의 data노드를 갖는다.
  DataNode := LayerNode.FirstChild;

  MapData := unicodestring(StringReplace(ansistring(DataNode.FirstChild.NodeValue),
    #10, '', [rfReplaceAll]));
  MapCells := SplitString(MapData, ',');

  SetLength(tmpData, MapCells.Count);
  for I := 0 to MapCells.Count - 1 do
  begin
    Val(MapCells[I], Cell, Code);

    if Code = 0 then
    begin
      tmpData[I] := Cell;
    end
    else
      tmpData[I] := 0;
  end;
  MapCells.Free;

  Result.Data := tmpData;
  Result.order := IOrder;
  Result.layer_name := FindAttributeValue(LayerNode, 'name');
end;

procedure DebugLayer(ALayer: RLayer);
var
  I: integer;
begin
  WriteLn('Order of Layer : ', ALayer.order);
  WriteLn(' High Index of cell : ', High(ALayer.Data));
  WriteLn(' Low Index of cell : ', Low(ALayer.Data));
  WriteLn(' name of layer : ', ALayer.layer_name);
  for I := Low(ALayer.Data) to High(ALayer.Data) do
  begin
    Write(ALayer.Data[I]);
    Write(' ');
  end;
  WriteLn();
end;

function ParseTileset(APath: string): RTileset;
var
  Doc: TXMLDocument;
  Child: TDOMNode;
  code: integer;
  tilewidth: integer;
  tileheight: integer;
  FilePath: string;
  image_source: string;
begin
  try
    ReadXMLFile(Doc, APath);
    FilePath := ExtractFileDir(APath);

    // name, tilewidth, tileheight, columns
    Result.tilesetname := Format('%s',
      [FindAttributeValue(Doc.DocumentElement, 'name')]);

    Val(FindAttributeValue(Doc.DocumentElement, 'tilewidth'), tilewidth, code);
    Val(FindAttributeValue(Doc.DocumentElement, 'tileheight'), tileheight, code);


    Result.tilewidth := tilewidth;
    Result.tileheight := tileheight;
    Child := Doc.DocumentElement.FirstChild;

    while Assigned(Child) do
    begin
      if Child.NodeName = 'image' then
      begin
        image_source := Format('%s', [FindAttributeValue(Child, 'source')]);
        ;
        Result.image_source := image_source;
        Result.image_path := ConcatPaths([FilePath, image_source]);
      end;
      Child := Child.NextSibling;
    end;
  finally
    Doc.Free;
  end;
end;

function ParseTilemap(APath: string): RTilemap;
var
  Doc: TXMLDocument;
  Child: TDOMNode;
  i: integer;
  layer: RLayer;
  layers: specialize TList<RLayer>;
  FilePath: string;
  ATileset: RTileset;
  tilesets: specialize TList<RTileset>;
  code: integer;
  firstgid: integer;
  TilesetLocation: string;
  Width: integer;
  Height: integer;
begin
  tilesets := specialize TList<RTileset>.Create;
  layers := specialize TList<RLayer>.Create;
  try
    ReadXMLFile(Doc, APath);

    FilePath := ExtractFileDir(APath);
    Child := Doc.DocumentElement.FirstChild;
    I := 1;

    Val(FindAttributeValue(Doc.DocumentElement, 'width'), Width, code);
    Val(FindAttributeValue(Doc.DocumentElement, 'height'), Height, code);

    Result.FWidth := Width;
    Result.FHeight := Height;
    while Assigned(Child) do
    begin

      if Child.NodeName = 'tileset' then
      begin
        { 타일셋 처리 }
        // 타일셋에 source 가 있다면 별도의 파일을 열어야하니
        // 일단 패스
        if FindAttribute(Child, 'source') then
        begin
          TilesetLocation := FindAttributeValue(Child, 'source');
          ATileset := ParseTileset(ConcatPaths([FilePath, TilesetLocation]));
          Val(FindAttributeValue(Child, 'firstgid'), firstgid, code);

          if code = 0 then
            ATileset.firstgid := firstgid
          else
            ATileset.firstgid := -1;

          tilesets.Add(ATileset);
          // DebugTileset(ATileset);
        end
        else
        begin
          // 해당 타일셋 정보
          ATileset := AnalyzeTileset(Child);
          ATileset.image_path := ConcatPaths([FilePath, ATileset.image_source]);
          // DebugTileset(AnalyzeTileset(Child));

          tilesets.Add(ATileset);
        end;
      end
      else if Child.NodeName = 'layer' then
      begin
        {레이어 처리}
        Layer := AnalyzeLayer(I, Child);
        layers.add(Layer);
        //DebugLayer(Layer);
        Inc(I);
      end;
      Child := Child.NextSibling;
    end;
  finally
    Doc.Free;
  end;

  Result.FLayers := layers;
  Result.FTilesets := tilesets;
end;

function getTilesetIndex(Tilesets: specialize TList<RTileset>; gid: Integer): integer;
var
  I: integer;
  ATileset: RTileset;
begin
  // 주어진 gid 가 들어가야할 Tileset 인덱스를 돌려준다.
  Result := Tilesets.Count - 1; // 기본 값은 가장 마지막 인덱스

  for I := Tilesets.Count - 1 downto 0 do
  begin
    // 가장 처음에 만나는 작거나 같은 수에 해당하는 인덱스를 반환
    ATileset := Tilesets[I];
    if ATileset.firstgid <= gid then
    begin
      Result := I;
      break;
      ;
    end;
  end;
end;

end.
