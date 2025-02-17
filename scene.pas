unit scene;

{$mode ObjFPC}{$H+}

interface

uses
  Classes, SysUtils, asset_manager, sdl2, entity, Generics.Collections;

type

  ESceneType = (void_scene, title_scene, map_scene, battle_scene, end_scene);

  TScene = class
  protected
    AAssetManager: TAssetManager;
    ASceneType: ESceneType;
    ARenderer: PSDL_Renderer;
    AEntities: specialize TList<TEntity>;
    AddedEntities: specialize TList<TEntity>;
  public
    constructor Create(AM: TAssetManager; AR: PSDL_Renderer);
    destructor Destroy; override;
    procedure SceneUpdate(dt: real);
    procedure SceneRender;
    property Renderer: PSDL_Renderer read ARenderer write ARenderer;
    property AssetManager: TassetManager read AAssetManager write AAssetManager;
    property SceneType: ESceneType read ASceneType;

  end;

implementation

constructor TScene.Create(AM: TAssetManager; AR: PSDL_Renderer);
begin
  AAssetManager := AM;
  ARenderer := AR;
  AEntities := specialize TList<TEntity>.Create;
  AddedEntities := specialize TList<TEntity>.Create;
  ASceneType:= void_scene;

end;

destructor TScene.Destroy;
var
  AEntity: TEntity;
begin
  for AEntity in AEntities do
  begin
    AEntity.Free;
  end;
  AEntities.Free;
  inherited;
end;

procedure TScene.SceneUpdate(dt: real);
var
  AEntity: TEntity;
  SubEntities: specialize TList<TEntity>;
begin


  // 삭제된 Entity 항목을 모두 지운다.
  SubEntities := specialize TList<TEntity>.Create;
  for AEntity in AEntities do
  begin
    if AEntity.IsLive = False then
    begin
      AEntity.Free;
    end
    else
    begin
      SubEntities.Add(AEntity);
    end;
  end;

  // 추가된 Entity 항목을 모두 더한다.
  for AEntity in AddedEntities do
  begin
    SubEntities.Add(AEntity);
  end;

  AEntities.Free;
  AEntities := SubEntities;

end;

procedure TScene.SceneRender();
var
  itemTexture: PSDL_Texture;
begin
  SDL_RenderClear(ARenderer);

  itemTexture := AAssetManager.GetTexture('items');
  if itemTexture <> nil then
    SDL_RenderCopy(ARenderer, itemTexture, nil, nil);

  SDL_RenderPresent(ARenderer);

end;

end.
