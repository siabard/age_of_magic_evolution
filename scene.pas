unit scene;

{$mode ObjFPC}{$H+}{$J-}

interface

uses
  Classes, SysUtils, asset_manager, sdl2, entity_manager, entity, Generics.Collections,
  camera, KeyInput, xml_reader;

type

  EActionName = (move_left, move_right, move_up, move_down);
  EActionType = (action_start, action_stop);
  ESceneType = (void_scene, title_scene, map_scene, battle_scene, end_scene);

  TScene = class
  protected
    FCamera: TCamera;
    FAssetManager: TAssetManager;
    FSceneType: ESceneType;
    FRenderer: PSDL_Renderer;
    FKeyInput: TKeyInput;
    FEntityManager: TEntityManager;
    FActionMap: specialize THashMap<integer, EActionName>;
    FTileMap: specialize THashMap<string, RTilemap>;
    FCurrentSceneName: string;
  public
    constructor Create(AM: TAssetManager; AR: PSDL_Renderer; AK: TKeyInput);
    destructor Destroy; override;
    procedure SceneInit(APath: string);
    procedure SceneUpdate(dt: real);
    procedure SceneRender;
    procedure RegisterAction(ACode: integer; AName: EActionName);
    procedure DoAction(ACode: integer; AAct: EActionType); virtual;
    property Renderer: PSDL_Renderer read FRenderer write FRenderer;
    property AssetManager: TassetManager read FAssetManager write FAssetManager;
    property SceneType: ESceneType read FSceneType;
    property EntityManager: TEntityManager read FEntityManager write FEntityManager;
  end;

implementation

uses
  component, LogUtil, animation, StrUtils, atlas;

constructor TScene.Create(AM: TAssetManager; AR: PSDL_Renderer; AK: TKeyInput);
begin
  FAssetManager := AM;
  FRenderer := AR;
  FSceneType := void_scene;
  FEntityManager := TEntityManager.Create;
  FCamera := TCamera.Create('main_camera', 0, 0, 320, 400);
  FActionMap := specialize THashMap<integer, EActionName>.Create;
  if Assigned(AK) then
    FKeyInput := AK;
  FTileMap := specialize THashMap<string, RTilemap>.Create;
end;


destructor TScene.Destroy;
var
  ATileMap: RTilemap;
  ATileset: RTileset;
  TilesetName: string;
  AEntity: TEntity;
  AListEntity: TListEntity;
  I: integer;
begin

  // 타일 맵 지우기
  // 타일맵에는 각 레이어와 타일셋이 리스트로 되어있으므로
  // 삭제해주어야한다.
  // 그리고 AssetManager 에서 타일셋에 대한 텍스쳐와 아틀라스도 삭제한다.
  for ATileMap in FTileMap.Values do
  begin
    for ATileset in ATileMap.FTilesets do
    begin
      // 개별 타일셋에서 AtlasId 와 TextureId 는 동일하다.
      // 타일셋의 이름이 해당 아이디이다.
      TilesetName := ATileset.tilesetname;

      Self.AssetManager.RemoveAtlas(TilesetName);
      Self.AssetManager.RemoveTexture(TilesetName);
    end;

    ATileMap.FLayers.Free;
    ATileMap.FTilesets.Free;
  end;
  FTileMap.Free;

  FEntityManager.Free;
  FCamera.Free;
  FActionMap.Free;
  inherited;
end;



procedure TScene.SceneInit(APath: string);
var
  AEntity: TEntity;
  AAnimation: TAnimation;
  ACompMove: TMovementComponent;
  ACompInput: TInputComponent;
  ACompPos: TPositionComponent;
  ACompAnim: TAnimationComponent;
  ACompColl: TCollideComponent;
  ADepthComponent: TDepthComponent;
  configFile: TextFile;
  config: string;
  Fields: TStringList;
  ValX: integer;
  ValY: integer;
  ValW: integer;
  ValH: integer;
  ValCode: integer;
  ATileMap: RTilemap;
  TilemapName: string;
  ATileset: RTileset;
  TilesetName: string;
  AHashMapKey: integer;
  ValDepth: integer;

  { Layer 엔터티 생성용 }
  ALayer: RLayer;
  LI: integer;
  LCI: integer;
  LayerGid: integer;
  LCid: string;
  LGroupName: string;
  LEntity: TEntity;
  LCol: integer;
  LRow: integer;
  TI: integer;
  AnimationName: string;
  LAtlas: TAtlas;
  AI: integer;
  firstgid: integer;
begin
  { APath에서 설정파일을 읽어 Scene 에 Entity 등을 구성한다. }
  Fields := TStringList.Create;
  Fields.Delimiter := #9; // 탭 문자
  Fields.StrictDelimiter := False; // 여러 구분자 허용

  AssignFile(configFile, APath);
  try
    Reset(configFile);
    try
      while not EOF(configFile) do
      begin
        ReadLn(configFile, config);

        if IsEmptyStr(config, []) then
          continue;


        // Config를 Split 한다.
        Fields.DelimitedText := config;

        case Fields[0] of
          'entity': begin
            AEntity := FEntityManager.AddEntity;
            AEntity.Tag := Fields[1];
            FEntityManager.Update;
          end;
          'set': begin
            { set entity [entity_id] movement x y }
            {
            set entity [entity_id] movement x y
            set entity player movement 0 0
            # set entity [entity_id] position x y
            set entity player position 0 0
            # set entity [entity_id] animation [animation_id]
            set entity player animation sword
            set entity player animation armor

            # set entity [entity_id] current_animation [animation_id]
            set entity player current_animation sword

            }
            case Fields[1] of
              'entity': begin
                AEntity := EntityManager.GetEntity(Fields[2]);
                case Fields[3] of
                  'movement': begin
                    if AEntity.movement = nil then
                    begin
                      ACompMove :=
                        TMovementComponent.Create(Format('%s_%s', ['mov', Fields[2]]));
                      AEntity.movement := ACompMove;
                    end
                    else
                      ACompMove := AEntity.movement;

                    Val(Fields[4], ValX, ValCode);
                    Val(Fields[5], ValY, ValCode);
                    ACompMove.X := ValX;
                    ACompMove.Y := ValY;

                  end;
                  'position': begin
                    if AEntity.position = nil then
                    begin
                      ACompPos :=
                        TPositionComponent.Create(Format('%s_%s', ['pos', Fields[2]]));
                      AEntity.position := ACompPos;
                    end
                    else
                      ACompPos := AEntity.position;

                    Val(Fields[4], ValX, ValCode);
                    Val(Fields[5], ValY, ValCode);
                    ACompPos.X := ValX;
                    ACompPos.Y := ValY;
                    ACompPos.PX := ValX;
                    ACompPos.PY := ValY;

                  end;
                  'animation': begin
                    AAnimation := AssetManager.GetAnimation(Fields[4]);
                    if AEntity.animation = nil then
                    begin
                      ACompAnim :=
                        TAnimationComponent.Create(Format('%s_%s', ['ani', Fields[2]]));
                      AEntity.animation := ACompAnim;
                    end
                    else
                      ACompAnim := AEntity.animation;

                    ACompAnim.SetAnimation(Fields[4], AAnimation);
                    ACompAnim.Duration := 300;
                  end;
                  'current_animation': begin
                    if AEntity.animation <> nil then
                      AEntity.animation.CurrentAnimation := Fields[4];

                  end;
                  'collision': begin
                    if AEntity.collide = nil then
                    begin
                      ACompColl :=
                        TCollideComponent.Create(Format('%s_%s', ['coll', Fields[2]]));
                      AEntity.collide := ACompColl;
                    end
                    else
                      ACompColl := AEntity.collide;

                    Val(Fields[4], ValX, ValCode);
                    Val(Fields[5], ValY, ValCode);
                    Val(Fields[6], ValW, ValCode);
                    Val(Fields[7], ValH, ValCode);

                    with ACompColl.BoundBox do
                    begin
                      RX := ValX;
                      RY := ValY;
                      RW := ValW;
                      RH := ValH;
                    end;
                  end;
                  'input': begin
                    if AEntity.input = nil then
                    begin
                      ACompInput :=
                        TInputComponent.Create(Format('%s_%s', ['input', Fields[2]]));
                      AEntity.input := ACompInput;
                    end;
                  end;
                  'depth': begin
                    if not Assigned(AEntity.Depth) then

                      ADepthComponent :=
                        TDepthComponent.Create(Format('%s_%s_%s', ['depth', Fields[2], Fields[4]]))
                    else
                      ADepthComponent := AEntity.Depth;

                    Val(Fields[4], ValDepth, ValCode);
                    ADepthComponent.depth := ValDepth;
                    AEntity.depth := ADepthComponent;

                  end;
                end;
              end;
              'map': begin
                FCurrentSceneName := Fields[2];
              end;

            end;
          end;
          'map': begin
            { TODO 아래의 코드를 이용하여 Entity를 Scene::Init 에서
              생성할 수 있으면 된다.
              Entity는 Animation, Position 이 있으며, Collide 타일셋에 한해
              Animation 대신 BoundingBox 가 추가되면 된다.
              FTileMap 을 삭제했으면 좋겠는데, 데이터양이 아직 크기 않으므로
              이후에 Map에 대한 Entity조작이 안정화되면 삭제한다.
            }
            // 지도를 읽는다.
            ATilemap := xml_reader.ParseTilemap(Fields[2]);
            TilemapName := Fields[1];

            Self.FTileMap.Add(TilemapName, ATilemap);

            // 타일셋과 아틀라스를 등록한다.
            for ATileset in ATileMap.FTilesets do
            begin
              // 개별 타일셋에서 AtlasId 와 TextureId 는 동일하다.
              // 타일셋의 이름이 해당 아이디이다.
              TilesetName := ATileset.tilesetname;

              // 타일셋에서 Texture를 생성한다.
              FAssetManager.LoadTexture(TilesetName,
                pansichar(ansistring(ATileset.image_path)));

              // 텍스쳐에서 아틀라스를 생성한다.
              FAssetManager.AddAtlas(TilesetName, TilesetName,
                ATileset.tilewidth, ATileset.tileheight);

              // 해당 타일맵에 맞는 애니메이션을 만든다.
              // 모든 아틀라스에 대해 만들면 된다.
              LAtlas := FAssetManager.GetAtlas(TilesetName);
              for  AI := 0 to LAtlas.Rects.Count - 1 do
              begin
                // AAnimation := TAnimation.Create(AnimationName, AAtlas.TextureName, FrameStart, FrameSize, AAtlas.Rects);
                AnimationName := Format('tileset_%s_%d', [TilesetName, AI]);
                AAnimation := TAnimation.Create(AnimationName,
                  LAtlas.TextureName, AI, 1, LAtlas.Rects);
                FAssetManager.AddAnimation(AnimationName, AAnimation);
              end;
            end;

            // 모든 생성이 완료된 경우 entity를 생성한다.
            // Animation은 texture 와 atlas 가 생성되었기때문에 만들어낼 수 있다.
            //  Entity를 만든다. (단 layer 항목의 0은 제외..)
            // Entity 를 생성할 때 태그는 Layer_[tilename]_[layer_index] 식으로 만든다.
            for LI := 0 to ATilemap.FLayers.Count - 1 do
            begin
              ALayer := Atilemap.FLayers[LI];
              LGroupName := Format('layer_%s_%d', [TilemapName, LI]);
              for LCI := Low(ALayer.Data) to High(ALayer.Data) do
              begin
                LayerGid := ALayer.Data[LCI];
                LRow := LCI div ATilemap.FWidth;
                LCol := LCI mod ATilemap.FWidth;

                if LayerGid > 0 then
                begin
                  LEntity := FEntityManager.AddEntity(LGroupName);
                  LEntity.Tag := LGroupName;
                  TI := getTilesetIndex(ATileMap.FTilesets, LayerGid);
                  firstgid := ATileMap.FTilesets[TI].firstgid;
                  // RenderSystem을 참고해서 필요 Component 들을 만든다. (Animation / Position)
                  ADepthComponent :=
                    TDepthComponent.Create(Format('depth_%s_%d', [LGroupName, LCI]));
                  ADepthComponent.depth := LI;
                  ACompPos :=
                    TPositionComponent.Create(Format('pos_%s_%d', [LGroupName, LCI]));
                  ACompPos.X := LCol * ATileMap.FTilesets[TI].tilewidth;
                  ACompPos.Y := LRow * ATileMap.FTilesets[TI].tileheight;
                  ACompPos.PX := 0;
                  ACompPos.PY := 0;
                  ACompAnim :=
                    TAnimationComponent.Create(Format('ani_%s_%d', [LGroupName, LCI]));

                  AAnimation :=
                    FAssetManager.GetAnimation(Format('tileset_%s_%d',
                    [ATileMap.FTilesets[TI].tilesetname, LayerGid - firstgid]));

                  ACompAnim.SetAnimation(Format('ani_%s_%d',
                    [ATileMap.FTilesets[TI].tilesetname, LayerGid - firstgid]),
                    AAnimation);
                  ACompAnim.Duration := 300;
                  LEntity.depth := ADepthComponent;
                  LEntity.animation := ACompAnim;
                  LEntity.position := ACompPos;
                end;
              end;
            end;

            FEntityManager.Update;

          end;
        end;
      end;
    except
      on E: EInOutError do
      begin

        LogDebug('Failed Open File');

      end;
    end;
  finally
    CloseFile(configFile);
  end;
end;


procedure TScene.SceneUpdate(dt: real);
begin
  FEntityManager.Update();
end;

procedure TScene.SceneRender();
var
  itemTexture: PSDL_Texture;
begin
  SDL_RenderClear(FRenderer);

  itemTexture := FAssetManager.GetTexture('items');
  if itemTexture <> nil then
    SDL_RenderCopy(FRenderer, itemTexture, nil, nil);

  SDL_RenderPresent(FRenderer);

end;


procedure TScene.RegisterAction(ACode: integer; AName: EActionName);
begin
  FActionMap.Add(ACode, AName);
end;

procedure TScene.DoAction(ACode: integer; AAct: EActionType);
begin

end;

end.
