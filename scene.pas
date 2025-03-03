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
    FActionMap: specialize THashMap<Integer, EActionName>;
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

constructor TScene.Create(AM: TAssetManager; AR: PSDL_Renderer; AK: TKeyInput);
begin
  FAssetManager := AM;
  FRenderer := AR;
  FSceneType := void_scene;
  FEntityManager := TEntityManager.Create;
  FCamera := TCamera.Create('main_camera', 0, 0, 640, 480);
  FActionMap := specialize THashMap<Integer, EActionName>.Create;
  if Assigned(AK) then
    FKeyInput := AK;

end;


procedure TScene.SceneInit(APath: string);
begin
  { APath에서 설정파일을 읽어 Scene 에 Entity 등을 구성한다. }
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
