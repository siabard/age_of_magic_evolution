unit scene;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, asset_manager, sdl2, entity_manager, Generics.Collections,
  camera, KeyInput, action;

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
  entity, component, LogUtil, animation, StrUtils;

constructor TScene.Create(AM: TAssetManager; AR: PSDL_Renderer; AK: TKeyInput);
begin
  FAssetManager := AM;
  FRenderer := AR;
  FSceneType := void_scene;
  FEntityManager := TEntityManager.Create;
  FCamera := TCamera.Create('main_camera', 0, 0, 640, 480);
  FActionMap := specialize THashMap<integer, EActionName>.Create;
  if Assigned(AK) then
    FKeyInput := AK;

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
  configFile: TextFile;
  config: string;
  Fields: TStringList;
  ValX: integer;
  ValY: integer;
  ValW: integer;
  ValH: integer;
  ValCode: integer;
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
        WriteLn('Start Config', config);

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
                WriteLn('entity setup');
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
                end;
              end;
            end;
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

destructor TScene.Destroy;
begin
  FEntityManager.Free;
  FCamera.Free;
  FActionMap.Free;
  inherited;
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
